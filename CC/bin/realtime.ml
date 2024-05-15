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

(* Updated function to handle both historical and minute-level data *)
let rec monitor_and_trade ticker =
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
        (* Update balance after purchase *)
        exit 0 (* Exit the program after purchase to prevent further actions *))
      else
        Printf.printf
          "Current price: $%.2f, True Range: $%.2f\n\
           Not buying. Current price higher than TR 75th percentile: $%.2f\n"
          last_data.StockData.close tr_75th_percentile tr_75th_percentile;
      Stdlib.flush Stdlib.stdout;
      Unix.sleep 1;
      monitor_and_trade ticker
  | None ->
      Printf.printf "No data available\n";
      Stdlib.flush Stdlib.stdout;
      Unix.sleep 1;
      monitor_and_trade ticker

(* Update main function to use the correct function call *)
let main () =
  if Array.length Sys.argv < 3 then
    eprintf "Usage: %s <ticker> [--realtime | <start_date> <end_date>]\n"
      Sys.argv.(0)
  else
    let ticker = Sys.argv.(1) in
    if Sys.argv.(2) = "--realtime" then monitor_and_trade ticker
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
