open Printf
open Batteries
open CC
open Unix

let state_file = "trader_state.txt"
let global_ticker = ref ""
let global_bought = ref false
let global_tr = ref 0.0
let global_price = ref 0.0
let gui = ref false

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
let tr_msg tr = Printf.sprintf "True Range: %.2f" tr
let tr_label = Bogue.Widget.label (tr_msg 0.0)
let ticker_msg ticker = Printf.sprintf "Ticker: %s" ticker
let ticker_label = Bogue.Widget.label (ticker_msg "TICK")
let price_msg price = Printf.sprintf "Current Price: %.2f" price
let price_label = Bogue.Widget.label (price_msg 0.0)

(* Status Block : Layout containing status message, ticker message, price
   message, and tr message*)
let status_block =
  Bogue.Layout.tower_of_w ~w:300
    [ status_label; price_label; tr_label; ticker_label ]

let ticker_input = Bogue.Widget.text_input ~prompt:"Input Ticker" ()
let start_button = Bogue.Widget.button "Start"
let refresh_button = Bogue.Widget.button "Refresh"

(* Control Block: Layout containing input text box and button controls. *)
let control_block =
  Bogue.Layout.flat_of_w [ ticker_input; start_button; refresh_button ]

(* Info Block: Top half of GUI, all text information. *)
let info_block = Bogue.Layout.flat [ control_block; status_block ]
let chart = Bogue.Widget.sdl_area ~w:600 ~h:300 ~style:style_border ()
let chart_graphic = Bogue.Widget.get_sdl_area chart
let last_15_points = Array.make 15 0.0

(* Get coordinates corresponding to the ith entry in [last_15_points],
   translated using StockData utils.*)
let get_ith_coordinates i value =
  let chart_layout = Bogue.Layout.resident chart in
  let x =
    fst (Bogue.Layout.get_physical_size chart_layout)
    / Array.length last_15_points
    * i
  in
  let range = StockData.arr_range last_15_points in
  let y =
    StockData.gen_y_coord_from_range
      (!global_tr *. 0.9, snd range *. 1.1)
      value
      (snd (Bogue.Layout.get_physical_size chart_layout))
  in
  (x, y)

(* An array containing ordered pairs, coordinates corresponding to the last 15
   data points *)
let get_all_coords () = Array.mapi get_ith_coordinates last_15_points

(* Iterate over all coordinates, plotting them on the graph. Also draw tr
   (threshold) line. *)
let draw_chart () =
  Bogue.Sdl_area.clear chart_graphic;
  let coords = get_all_coords () in
  let range = StockData.arr_range last_15_points in
  let tr_line =
    StockData.gen_y_coord_from_range
      (!global_tr *. 0.9, snd range *. 1.1)
      !global_tr
      (snd (Bogue.Layout.get_physical_size (Bogue.Layout.resident chart)))
  in
  Bogue.Sdl_area.draw_line chart_graphic
    ~color:Bogue.Draw.(opaque red)
    ~thick:5 (0, tr_line) (1500, tr_line);
  for index = 0 to Array.length coords - 1 do
    Bogue.Sdl_area.draw_circle chart_graphic
      ~color:Bogue.Draw.(opaque black)
      ~thick:5 ~radius:5 coords.(index);
    if index <> Array.length coords - 1 && snd coords.(index + 1) < tr_line then
      Bogue.Sdl_area.draw_line chart_graphic
        ~color:Bogue.Draw.(opaque black)
        ~thick:5 coords.(index)
        coords.(index + 1)
  done

let main_layout = Bogue.Layout.tower [ info_block; Bogue.Layout.resident chart ]
let board = Bogue.Bogue.create [ Bogue.Window.create main_layout ]

(* First grow array from left to right, then when it is 'full' (no entries are
   0.0) shift in from right. *)
let increment_arr arr new_entry =
  try
    for i = 0 to Array.length arr - 1 do
      if arr.(i) = 0.0 then (
        arr.(i) <- new_entry;
        raise Exit)
    done;
    StockData.shift_in arr new_entry
  with Exit -> ignore ()

(* End GUI *)

(* Updated function to handle both historical and minute-level data *)
let monitor_and_trade (ticker : string) : unit =
  global_ticker := ticker;
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
      global_tr := tr_75th_percentile;
      global_price := last_data.StockData.close;
      increment_arr last_15_points last_data.StockData.close;
      if last_data.StockData.close <= tr_75th_percentile then (
        let buy_price = last_data.StockData.close in
        let balance = snd (read_last_state ()) in
        let shares_to_buy = balance *. 0.10 /. buy_price in
        (* Investing 10% of balance *)
        let cost = shares_to_buy *. buy_price in
        if not !gui then
          Printf.printf
            "Stock Bought: %s\n\
             Amount: %.2f shares at $%.2f each.\n\
             Total Cost: $%.2f\n\
             Selling at opening price tomorrow morning.\n"
            ticker shares_to_buy buy_price
            (shares_to_buy *. buy_price);
        write_state start_date_str (balance -. cost);
        Stdlib.flush Stdlib.stdout;
        global_bought := true)
      else if not !gui then
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

let gui_start_trading button ticker_input ev =
  if Bogue.Button.is_pressed (Bogue.Widget.get_button button) then (
    let ticker = Bogue.Widget.get_text ticker_input in
    try
      while not !global_bought do
        if Bogue.Trigger.should_exit ev then raise Exit
        else monitor_and_trade ticker
      done
    with Exit ->
      Printf.eprintf "Program Exited by GUI";
      exit 0)

let refresh_gui refresh_button _ ev =
  if Bogue.Trigger.should_exit ev then raise Exit
  else if Bogue.Button.is_pressed (Bogue.Widget.get_button refresh_button) then
    draw_chart ();
  Bogue.Widget.(
    set_text ticker_label (ticker_msg !global_ticker);
    set_text tr_label (tr_msg !global_tr);
    set_text status_label (status_msg !global_bought);
    set_text price_label (price_msg !global_price))

let go_connection =
  Bogue.Widget.connect start_button ticker_input gui_start_trading
    Bogue.Trigger.buttons_down

let refresh_connection =
  Bogue.Widget.connect refresh_button chart refresh_gui
    Bogue.Trigger.buttons_down

let start_gui () =
  gui := true;
  Bogue.Widget.add_connection start_button go_connection;
  Bogue.Widget.add_connection refresh_button refresh_connection;
  reset_gui ();
  Bogue.Main.run board

(* End GUI *)

(* Update main function to use the correct function call *)
let no_gui_loop ticker =
  gui := false;
  while not !global_bought do
    monitor_and_trade ticker
  done

let main_new () =
  match Array.to_list Sys.argv with
  | [ _; ticker; "--realtime" ] -> no_gui_loop ticker
  | [ _; "--realtime"; "gui" ] -> start_gui ()
  | [ _; ticker; start_date; end_date ] ->
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
      printf "Current balance after trading: $%.2f\n" final_balance
  | _ ->
      eprintf
        "Usage: %s <ticker> [--realtime | --gui <start_date> <end_date>]\n"
        Sys.argv.(0)

let () =
  Sys.catch_break true;
  try main_new () with
  | Sys.Break -> Printf.printf "\nProgram interrupted by user. Exiting...\n"
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
