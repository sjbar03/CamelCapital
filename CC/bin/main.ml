open Printf
open Batteries
open CC

let rec insert_char lst char index =
  match lst with
  | [] -> []
  | h :: t ->
      if index - 3 > 0 && (index - 3) mod 3 = 0 then
        char :: h :: insert_char t char (index + 1)
      else h :: insert_char t char (index + 1)

let format_large_number f =
  let d = sprintf "%.*f" 2 f in
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

(* Modify the entry point to handle stock ticker and an optional starting
   balance *)
let () =
  let default_starting_balance = 100000.0 in
  (* Default starting balance *)
  let argv = Array.to_list Sys.argv in
  match argv with
  | _ :: "csv" :: file_name :: balance_arg ->
      trading_algorithm file_name
        (starting_balance balance_arg default_starting_balance)
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
