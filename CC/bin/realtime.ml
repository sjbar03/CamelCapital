open Printf
open Batteries
open CC
open Unix

let state_file = "trader_state.txt"

(* Function to read the last state from a file *)
let read_last_state () =
  if Sys.file_exists state_file then (
    let ic = open_in state_file in
    let line = input_line ic in
    close_in ic;
    match String.split_on_char ',' line with
    | [ date_str; balance_str ] -> (date_str, float_of_string balance_str)
    | _ -> failwith "State file is corrupted")
  else
    let today = Unix.localtime (Unix.time ()) in
    let date_str =
      Printf.sprintf "%04d-%02d-%02d" (today.tm_year + 1900) (today.tm_mon + 1)
        today.tm_mday
    in
    (date_str, 100000.0)

(* Function to write the current state to a file *)
let write_state date balance =
  let oc = open_out state_file in
  Printf.fprintf oc "%s,%f\n" date balance;
  close_out oc

(* Reads stock data from a CSV file into a list of stock data records *)
let read_stock_data file_name : StockData.t list =
  let lines = File.lines_of file_name in
  let data_lines = Enum.skip 1 lines in
  Enum.fold
    (fun acc line ->
      try
        let stock_data = StockData.parse_stock_data line in
        stock_data :: acc
      with _ -> acc (* If parsing fails, skip the line *))
    [] data_lines
  |> List.rev (* Because we've built the list in reverse order *)

let download_and_process_data ticker start_date end_date historical =
  let command =
    if historical then
      Printf.sprintf "python download_data.py %s %s %s" ticker start_date
        end_date
    else Printf.sprintf "python download_data.py %s --realtime" ticker
  in
  match Sys.command command with
  | 0 -> if historical then "historical_data.csv" else "minute_data.csv"
  | _ -> failwith (Printf.sprintf "Failed to download data using %s" command)

let execute_trading_strategy file_name starting_balance =
  let stock_data_list = read_stock_data file_name in
  let updated_stock_data_list =
    List.fold_right
      (fun sd acc ->
        let updated_sd =
          StockData.update_prev_close sd
            (match acc with
            | [] -> 0.0
            | hd :: _ -> hd.StockData.prev_close)
        in
        updated_sd :: acc)
      stock_data_list []
  in
  let tr_data =
    List.map (fun sd -> (sd, StockData.true_range sd)) updated_stock_data_list
  in
  let tr_75th_percentile =
    StockData.moving_percentile (List.map snd tr_data) 75.0
  in
  let _, final_balance =
    StockData.calc_final_bal tr_75th_percentile 0.7 starting_balance tr_data
  in
  final_balance (* Return just the balance as a float *)

(* Helper function to get the last element of a list *)
let last_element lst =
  match List.rev lst with
  | hd :: _ -> Some hd
  | [] -> None

(* Realtime GUI *)
let style_border =
  Bogue.Style.create ~border:(Bogue.Style.mk_border (Bogue.Style.mk_line ())) ()

let status_msg status =
  let msg = if status = true then "BUYING" else "NOT BUYING" in
  Printf.sprintf "STATUS: %s" msg

let status_label = Bogue.Widget.label (status_msg false)
let tr_msg tr = Printf.sprintf "True Range (Threshold): %f" tr
let tr_label = Bogue.Widget.label (tr_msg 0.0)
let ticker_msg ticker = Printf.sprintf "Ticker: %s" ticker
let ticker_label = Bogue.Widget.label (ticker_msg "TICK")

let status_block =
  Bogue.Layout.tower_of_w [ status_label; tr_label; ticker_label ]

let terminal_content old_content new_msg =
  Printf.sprintf "%s\n%s" old_content new_msg

let terminal = Bogue.Widget.text_display (terminal_content "" "Hello World!")
let ticker_input = Bogue.Widget.text_input ~prompt:"Input Ticker" ()
let start_button = Bogue.Widget.button "Start"
let input_and_button = Bogue.Layout.flat_of_w [ ticker_input; start_button ]

let control_block =
  Bogue.Layout.tower
    [
      Bogue.Layout.resident
        ~background:
          (Bogue.Layout.color_bg Bogue.Draw.(transp (find_color "#FFEFBE")))
        terminal;
      input_and_button;
    ]

let info_block = Bogue.Layout.flat [ control_block; status_block ]
let chart = Bogue.Widget.sdl_area ~w:600 ~h:300 ~style:style_border ()
let chart_graphic = Bogue.Widget.get_sdl_area chart
let last_15_points = Array.make 15 None

let get_coordinates i value =
  let chart_layout = Bogue.Layout.resident chart in
  let x =
    fst (Bogue.Layout.get_physical_size chart_layout)
    / Array.length last_15_points
    * i
  in
  let raw_numbers =
    Array.map
      (fun x ->
        match x with
        | Some y -> y
        | None -> 0.0)
      last_15_points
  in
  let range = StockData.arr_range raw_numbers in
  let y = StockData.gen_y_coord_from_range range value chart_layout in
  (x, y + 50)

let draw_chart () =
  Bogue.Sdl_area.clear chart_graphic;
  try
    for index = 0 to Array.length last_15_points do
      let coords = get_coordinates index (Option.get last_15_points.(index)) in
      Bogue.Sdl_area.draw_circle chart_graphic
        ~color:Bogue.Draw.(opaque black)
        ~thick:5 ~radius:5 coords
    done
  with Invalid_argument _ -> ignore ()

let main_layout = Bogue.Layout.tower [ info_block; Bogue.Layout.resident chart ]
let board = Bogue.Bogue.create [ Bogue.Window.create main_layout ]

let increment_arr arr new_entry =
  try
    for i = 0 to Array.length arr - 1 do
      match arr.(i) with
      | None ->
          arr.(i) <- Some new_entry;
          raise Exit
      | Some _ -> ()
    done;
    StockData.shift_in arr (Some new_entry)
  with Exit -> ignore ()

(* End GUI *)

(* Updated function to handle both historical and minute-level data *)
let monitor_and_trade (ticker : string) : unit =
  Bogue.Sync.push (fun _ -> draw_chart ());
  let today = Unix.time () in
  let start_date = today -. (86400. *. 100.) |> Unix.localtime in
  let end_date = today |> Unix.localtime in
  let start_date_str =
    Printf.sprintf "%04d-%02d-%02d"
      (start_date.tm_year + 1900)
      (start_date.tm_mon + 1) start_date.tm_mday
  in
  let end_date_str =
    Printf.sprintf "%04d-%02d-%02d" (end_date.tm_year + 1900)
      (end_date.tm_mon + 1) end_date.tm_mday
  in

  (* Download historical data and minute-level data *)
  let historical_file =
    download_and_process_data ticker start_date_str end_date_str true
  in
  let minute_file =
    download_and_process_data ticker end_date_str end_date_str false
  in

  (* Load and merge data *)
  let historical_data = read_stock_data historical_file in
  let minute_data = read_stock_data minute_file in
  let stock_data_list = List.append historical_data minute_data in

  (* Merge data lists *)
  match last_element stock_data_list with
  | Some last_data ->
      let tr_data = List.map StockData.true_range stock_data_list in
      let tr_75th_percentile = StockData.moving_percentile tr_data 75.0 in
      increment_arr last_15_points last_data.StockData.close;
      if last_data.StockData.close <= tr_75th_percentile then (
        let buy_price = last_data.StockData.close in
        let balance = snd (read_last_state ()) in
        let shares_to_buy = balance *. 0.10 /. buy_price in
        (* Investing 10% of balance *)
        let cost = shares_to_buy *. buy_price in
        Printf.printf
          "Stock Bought: %s\n\
           Amount: %.2f shares at $%.2f each.\n\
           Total Cost: $%.2f\n\
           Selling at opening price tomorrow morning.\n"
          ticker shares_to_buy buy_price
          (shares_to_buy *. buy_price);
        write_state start_date_str (balance -. cost);
        Stdlib.flush Stdlib.stdout)
      else
        Printf.printf
          "Current price: $%.2f, True Range: $%.2f\n\
           Not buying. Current price higher than TR 75th percentile: $%.2f\n"
          last_data.StockData.close tr_75th_percentile tr_75th_percentile;
      Stdlib.flush Stdlib.stdout;
      Unix.sleep 1
  | None ->
      Printf.printf "No data available\n";
      Stdlib.flush Stdlib.stdout;
      Unix.sleep 1

(* More GUI, add and button connection *)

let reset_gui () =
  Bogue.Widget.(
    set_text ticker_label (ticker_msg "TICK");
    set_text tr_label (tr_msg 0.0);
    set_text status_label (status_msg false))

let start_trading button ticker_input ev =
  if Bogue.Button.is_pressed (Bogue.Widget.get_button button) then (
    let ticker = Bogue.Widget.get_text ticker_input in
    try
      while true do
        if Bogue.Trigger.should_exit ev then raise Exit
        else monitor_and_trade ticker;
        Unix.sleep 1;
        draw_chart ()
      done
    with Exit ->
      Printf.eprintf "Program Exited by GUI";
      exit 0)

let go_connection =
  Bogue.Widget.connect start_button ticker_input start_trading
    Bogue.Trigger.buttons_down

let _ =
  Bogue.Widget.add_connection start_button go_connection;
  reset_gui ()

(* End GUI *)

(* Update main function to use the correct function call *)
let main () =
  if Array.length Sys.argv < 3 then
    eprintf "Usage: %s <ticker> [--realtime | <start_date> <end_date>]\n"
      Sys.argv.(0)
  else
    let ticker = Sys.argv.(1) in
    if Sys.argv.(2) = "--realtime" then Bogue.Main.run board
    else if Array.length Sys.argv = 4 then (
      let start_date = Sys.argv.(2) in
      let end_date = Sys.argv.(3) in
      let last_date, last_balance = read_last_state () in
      let file_name =
        download_and_process_data ticker start_date end_date
          true (* Ensure to pass 'true' for historical data fetching *)
      in
      let final_balance = execute_trading_strategy file_name last_balance in
      let today = localtime (time ()) in
      let date_str =
        sprintf "%04d-%02d-%02d" (today.tm_year + 1900) (today.tm_mon + 1)
          today.tm_mday
      in
      write_state date_str final_balance;
      printf "Last trading session was on: %s with a balance of: $%.2f\n"
        last_date last_balance;
      printf "Current balance after trading: $%.2f\n" final_balance)
    else
      eprintf
        "Invalid usage. Please use: %s <ticker> [--realtime | <start_date> \
         <end_date>]\n"
        Sys.argv.(0)

let () =
  Sys.catch_break true;
  try main () with
  | Sys.Break -> Printf.printf "\nProgram interrupted by user. Exiting...\n"
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
