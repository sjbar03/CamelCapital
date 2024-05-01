open Batteries
open CC

(** [insert_char lst ch index] inserts ch at intervals of 3 in lst, which should
    be a reversed list of chars that represent a floating point value with two
    decimal digits. *)
let rec insert_char lst ch index =
  match lst with
  | [] -> []
  | h :: t ->
      if index - 3 > 0 && (index - 3) mod 3 = 0 then
        ch :: h :: insert_char t ch (index + 1)
      else h :: insert_char t ch (index + 1)

(** [format_large_number f] returns a string representation of a monetary value
    f, complete with commas and dollar signs. *)
let format_large_number f =
  let d = Printf.sprintf "%.*f" 2 f in
  let lst = String.to_list d in
  "$" ^ String.of_list (List.rev (insert_char (List.rev lst) ',' 0))

let execute_trading_strategy tr_data percentile_threshold buy_signal_multiplier
    starting_balance =
  let tr_75th_percentile =
    StockData.moving_percentile (List.map snd tr_data) percentile_threshold
  in
  let final_balance =
    StockData.calc_final_bal tr_75th_percentile buy_signal_multiplier
      starting_balance tr_data
  in
  print_endline ("Starting balance: " ^ format_large_number starting_balance);
  print_endline ("Final balance: " ^ format_large_number (snd final_balance))
(* for i = 0 to Array.length StockData.last_1000_days - 1 do print_endline
   (string_of_float StockData.last_1000_days.(i)) done *)

let rec update_list lst prev_close =
  match lst with
  | [] -> []
  | hd :: tl ->
      let updated_hd = StockData.update_prev_close hd prev_close in
      updated_hd :: update_list tl updated_hd.close

let trading_algorithm file_name =
  let lines = File.lines_of file_name in
  (* Skip the first line which contains the headers *)
  let data_lines = Enum.skip 1 lines in
  let stock_data_list =
    Enum.map StockData.parse_stock_data data_lines |> List.of_enum
  in
  let updated_stock_data_list =
    (* Assume this list is sorted by date for each ticker *)
    update_list stock_data_list 0.0
  in
  let tr_data =
    List.map (fun sd -> (sd, StockData.true_range sd)) updated_stock_data_list
  in
  execute_trading_strategy tr_data 75. 0.7

let download_data ticker =
  let command = Printf.sprintf "python download_data.py %s" ticker in
  match Sys.command command with
  | 0 -> Printf.printf "Downloaded data for %s\n" ticker
  | _ -> failwith "Failed to download data"

let starting_balance balance_arg default_starting_balance =
  match balance_arg with
  | [] -> default_starting_balance (* Use default if no balance is specified *)
  | balance_str :: _ ->
      float_of_string balance_str (* Convert specified balance to float *)

let rec get_valid_int _ =
  ANSITerminal.printf [ Foreground Red ] "%s" "Please enter an integer: \n";
  let input = read_int_opt () in
  if input != None then Option.get input else get_valid_int ()

let rec get_risk _ =
  ANSITerminal.printf [ Foreground Red ] "%s"
    "Please enter '1', '2', or '3': \n";
  let input = read_int_opt () in
  if input != None && List.mem (Option.get input) [ 1; 2; 3 ] then
    Option.get input
  else get_risk ()

let start_ui _ =
  ANSITerminal.printf [ Bold; Foreground Green ] "%s"
    "Welcome to Camel Capital! \n";
  Printf.printf "%s" "  To begin, first enter your desired starting capital: \n";
  let capital = get_valid_int () in
  Printf.printf
    " Next, enter either 1, 2, or 3 to select your risk profile. A higher \
     number (I.E. 3) carries a higer risk factor. \n";
  let risk = get_risk () in
  Printf.printf "%s%i%s%i" "The starting capital and risk are as follows: "
    capital " / " risk

(* GUI *)

let enter_balance = Bogue.Widget.text_input ~prompt:"Enter starting balance" ()

let final_balance_label =
  Bogue.Widget.text_display
    (string_of_float
       StockData.last_1000_days.(Array.length StockData.last_1000_days - 1))

let button =
  Bogue.Widget.button "GO"
    ?bg_on:Bogue.Style.(Some (color_bg Bogue.Draw.(opaque green)))

let prompt_box =
  Bogue.Layout.(flat_of_w [ enter_balance; button; final_balance_label ])

let chart = Bogue.Widget.sdl_area ~w:500 ~h:500 ()
let chart_graphic = Bogue.Widget.get_sdl_area chart

let chart_layout =
  Bogue.Layout.resident ~name:"Graph"
    ~background:Bogue.(Layout.color_bg Draw.(lighter (transp yellow)))
    chart

let main_layout =
  Bogue.Layout.tower ~name:"CamelCapital" [ prompt_box; chart_layout ]

let gen_x_coord starting current =
  Bogue.Layout.height main_layout
  - int_of_float ((current -. starting) /. (starting /. 100.))

let draw_graph starting =
  Bogue.Sdl_area.clear chart_graphic;
  let width = Bogue.Layout.width main_layout * 2 in
  let tick_spacing = width / Array.length StockData.last_1000_days in
  for i = 0 to Array.length StockData.last_1000_days - 1 do
    let bal = StockData.last_1000_days.(i) in
    let color = Bogue.Draw.(opaque (if bal < starting then red else green)) in
    Bogue.Sdl_area.draw_circle chart_graphic ~color ~thick:5 ~radius:5
      (tick_spacing * i, gen_x_coord starting bal)
  done;
  Bogue.Sdl_area.update chart_graphic

let board = Bogue.Bogue.of_layout main_layout

let submit_balance button enter_balance ev =
  if Bogue.Trigger.should_exit ev then raise Exit
  else if Bogue.Button.is_pressed (Bogue.Widget.get_button button) then
    try
      let input =
        String.split
          (Bogue.Text_input.text (Bogue.Widget.get_text_input enter_balance))
          ~by:" "
      in
      Printf.printf "%s%s" (fst input) (snd input);
      let bal = float_of_string (snd input) in
      let ticker = fst input in
      download_data ticker;
      let csv_file = "stock_data.csv" in
      trading_algorithm csv_file bal;
      draw_graph bal;
      Bogue.Bogue.refresh_custom_windows board
    with _ -> ()

let balance_button_connection =
  Bogue.Widget.connect button enter_balance submit_balance
    Bogue.Trigger.buttons_down

let () = Bogue.Widget.add_connection button balance_button_connection

(* Modify the entry point to handle stock ticker and an optional starting
   balance *)
let () =
  let default_starting_balance = 100000.0 in
  (* Default starting balance *)
  let argv = Array.to_list Sys.argv in
  match argv with
  | [ _; "risk" ] -> start_ui ()
  | _ :: "csv" :: file_name :: balance_arg ->
      trading_algorithm file_name
        (starting_balance balance_arg default_starting_balance)
  | [ _; "gui" ] -> Bogue.Main.run board
  | _ :: "ticker" :: ticker :: balance_arg ->
      download_data ticker;
      (* Call Python script to download data *)
      let csv_file = "stock_data.csv" in
      (* Updated to use the fixed output file name from the Python script *)
      trading_algorithm csv_file
        (starting_balance balance_arg default_starting_balance)
  | _ ->
      Printf.eprintf
        "Usage: %s <csv|ticker> <file_path|ticker_symbol> [starting_balance]\n"
        (List.hd argv)
