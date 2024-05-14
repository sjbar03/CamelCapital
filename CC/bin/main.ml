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

let rec get_risk () =
  let lines = File.lines_of "stock_data.csv" in
  let data_lines = Enum.skip 1 lines in
  let stock_data_list =
    Enum.map StockData.parse_stock_data data_lines |> List.of_enum
  in
  let stock_data_array = Array.of_list stock_data_list in
  let base_kelly = Garch.kelly_criterion stock_data_array in

  ANSITerminal.printf [ Foreground Red ] "Please enter '1', '2', or '3': \n";
  match read_int_opt () with
  | Some 1 -> log base_kelly
  | Some 2 -> base_kelly
  | Some 3 -> base_kelly *. base_kelly
  | _ -> get_risk ()

let start_ui _ =
  ANSITerminal.printf [ Bold; Foreground Green ] "%s"
    "Welcome to Camel Capital! \n";
  Printf.printf "%s" "  To begin, first enter your desired starting capital: \n";
  let capital = get_valid_int () in
  Printf.printf
    " Next, enter either 1, 2, or 3 to select your risk profile. A higher \
     number (I.E. 3) carries a higer risk factor. \n";
  let risk = get_risk () in
  Printf.printf "The starting capital and risk are as follows: %f / %f\n"
    (float_of_int capital) risk

(* GUI *)

let enter_balance = Bogue.Widget.text_input ~prompt:"Enter starting balance" ()
let example_usage_label = Bogue.Widget.label "Example: AAPL 100000"

let interface_box =
  Bogue.Layout.tower_of_w [ enter_balance; example_usage_label ]

let final_balance_label =
  Bogue.Widget.text_display
    (string_of_float
       StockData.last_1000_day_bal.(Array.length StockData.last_1000_day_bal - 1))

let button =
  Bogue.Widget.button "GO"
    ?bg_on:Bogue.Style.(Some (color_bg Bogue.Draw.(opaque green)))

let prompt_box =
  Bogue.Layout.(
    flat ~align:Bogue.Draw.Center ~scale_content:true
      [
        interface_box;
        Bogue.Layout.resident button;
        Bogue.Layout.resident final_balance_label;
      ])

let balance_chart_label = Bogue.Widget.label "Balance (Last 1000 Days)"

let value_chart_label =
  Bogue.Widget.label "Stock Price at Open (Last 1000 Days)"

let balance_sdl_area = Bogue.Widget.sdl_area ~w:500 ~h:500 ()
let bal_graphic = Bogue.Widget.get_sdl_area balance_sdl_area
let value_sdl_area = Bogue.Widget.sdl_area ~w:500 ~h:500 ()
let value_graphic = Bogue.Widget.get_sdl_area value_sdl_area

let bal_border =
  Bogue.Style.mk_border
    (Bogue.Style.mk_line ~color:Bogue.Draw.(opaque pale_grey) ~width:5 ())

let val_border =
  Bogue.Style.mk_border
    (Bogue.Style.mk_line ~color:Bogue.Draw.(opaque black) ~width:5 ())

(* Bogue layout for the balace chart (last 1000 days) *)
let bal_chart =
  Bogue.Layout.tower_of_w ~name:"Balance History (Last 1000 Days)"
    ~background:Bogue.Layout.(style_bg (Bogue.Style.of_border bal_border))
    [ balance_chart_label; balance_sdl_area ]

(* Bogue layout for the value chart (last 1000 days) *)
let val_chart =
  Bogue.Layout.tower_of_w ~name:"Value History (Last 1000 Days)"
    ~background:Bogue.Layout.(style_bg (Bogue.Style.of_border val_border))
    [ value_chart_label; value_sdl_area ]

let chart_layout = Bogue.Layout.flat ~name:"Charts" [ bal_chart; val_chart ]

let main_layout =
  Bogue.Layout.tower ~name:"CamelCapital" [ prompt_box; chart_layout ]

let arr_range arr =
  let range = (arr.(0), arr.(0)) in
  Array.fold_left
    (fun range a -> (Float.min (fst range) a, Float.max (snd range) a))
    range arr

let translate_value_to_y height difference distance_from_min =
  int_of_float (height -. (height /. difference *. distance_from_min))

let gen_y_coord_from_range range value chart =
  let high = snd range in
  let low = fst range in
  let difference = high -. low in
  let height = float_of_int (snd (Bogue.Layout.get_physical_size chart)) in
  let distance_from_min = value -. low in
  translate_value_to_y height difference distance_from_min

let draw_graph chart graphic color data =
  Bogue.Sdl_area.clear graphic;
  let width = fst (Bogue.Layout.get_physical_size chart) in
  let tick_spacing = width / Array.length data in
  let range = arr_range data in
  for i = 0 to Array.length data - 1 do
    let curr_val = data.(i) in
    Bogue.Sdl_area.draw_circle graphic ~color ~thick:5 ~radius:5
      (tick_spacing * i, gen_y_coord_from_range range curr_val chart)
  done;
  Bogue.Sdl_area.update graphic

let board = Bogue.Bogue.of_layout main_layout

let draw_all_graphs _ =
  draw_graph bal_chart bal_graphic
    Bogue.Draw.(opaque green)
    StockData.last_1000_day_bal;
  draw_graph val_chart value_graphic
    Bogue.Draw.(opaque blue)
    StockData.last_1000_day_val

let parse_input enter_balance =
  String.split
    (Bogue.Text_input.text (Bogue.Widget.get_text_input enter_balance))
    ~by:" "

let run_algoritm_from_gui ticker bal =
  download_data ticker;
  let csv_file = "stock_data.csv" in
  trading_algorithm csv_file bal

let update_info_display starting_bal =
  let msg =
    Printf.sprintf "Starting Balance: %s \nFinal Balance %s"
      (format_large_number starting_bal)
      (format_large_number StockData.last_1000_day_bal.(999))
  in
  Bogue.Widget.set_text final_balance_label msg

let submit_balance button enter_balance ev =
  if Bogue.Trigger.should_exit ev then raise Exit
  else if Bogue.Button.is_pressed (Bogue.Widget.get_button button) then
    try
      let input = parse_input enter_balance in
      let bal = float_of_string (snd input) in
      let ticker = fst input in
      run_algoritm_from_gui ticker bal;
      draw_all_graphs ();
      update_info_display bal;
      Bogue.Bogue.refresh_custom_windows board
    with _ -> ()

let balance_button_connection =
  Bogue.Widget.connect button enter_balance submit_balance
    Bogue.Trigger.buttons_down

let () =
  update_info_display 0.0;
  Bogue.Widget.add_connection button balance_button_connection

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
