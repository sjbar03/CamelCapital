open Batteries
open Printf

(* A record to hold the stock data *)
type stock_data = {
  date : string;
  ticker : string;
  high : float;
  low : float;
  open_ : float; (* OCaml keyword avoidance *)
  close : float;
  prev_close : float;
}

let safe_float_of_string str line =
  try float_of_string (String.trim str)
  with Failure _ ->
    failwith
      (Printf.sprintf "float_of_string failure on: '%s' in line: %s" str line)

(* Function to parse a line of CSV into stock_data, with a dummy prev_close for now *)
let parse_stock_data line =
  match Str.split (Str.regexp ",") line with
  | [ date; open_; high; low; close; _adj_close; _volume ] ->
      {
        date;
        ticker = "AAPL";
        (* Directly assign 'AAPL' as the ticker *)
        high = safe_float_of_string high line;
        low = safe_float_of_string low line;
        open_ = safe_float_of_string open_ line;
        close = safe_float_of_string close line;
        prev_close = 0.0;
        (* This will be updated based on logic to handle previous close *)
      }
  | _ -> failwith (Printf.sprintf "Incorrect CSV format for line: %s" line)

(* Update stock_data with previous day's close price *)
let update_prev_close stock_data prev_close = { stock_data with prev_close }

(* Calculate True Range *)
let true_range sd =
  List.fold_left max (sd.high -. sd.low)
    [ sd.high -. sd.prev_close; sd.prev_close -. sd.low ]

(* Calculate Moving Percentile *)
let moving_percentile tr_list percentile =
  let sorted_list = List.sort compare tr_list in
  let index =
    int_of_float (float_of_int (List.length sorted_list) *. percentile /. 100.)
  in
  List.nth sorted_list index

(* Trading Strategy Function *)
let execute_trading_strategy tr_data percentile_threshold buy_signal_multiplier
    starting_balance =
  let tr_75th_percentile =
    moving_percentile (List.map snd tr_data) percentile_threshold
  in
  let _, final_balance =
    List.fold_left
      (fun (prev_close, balance) (stock, _tr) ->
        let buy_price =
          stock.close -. (tr_75th_percentile *. buy_signal_multiplier)
        in
        if stock.low <= buy_price && prev_close < buy_price then
          (stock.close, balance -. buy_price)
        else (stock.close, balance))
      (0.0, starting_balance) tr_data
  in
  printf "Starting balance: %f\n" starting_balance;
  printf "Ending balance: %f\n" final_balance

(* Trading Algorithm *)
let trading_algorithm file_name =
  let lines = File.lines_of file_name in
  (* Skip the first line which contains the headers *)
  let data_lines = Enum.skip 1 lines in
  let stock_data_list = Enum.map parse_stock_data data_lines |> List.of_enum in
  let updated_stock_data_list =
    (* Assume this list is sorted by date for each ticker *)
    let rec update_list lst prev_close =
      match lst with
      | [] -> []
      | hd :: tl ->
          let updated_hd = update_prev_close hd prev_close in
          updated_hd :: update_list tl updated_hd.close
    in
    update_list stock_data_list 0.0
  in
  let tr_data =
    List.map (fun sd -> (sd, true_range sd)) updated_stock_data_list
  in
  execute_trading_strategy tr_data 75. 0.7

(* Entry Point *)
let () =
  let starting_balance = 100000.0 (* Example starting balance *) in
  let argv = Array.to_list Sys.argv in
  match argv with
  | _ :: file_name :: _ -> trading_algorithm file_name starting_balance
  | _ ->
      Printf.eprintf "Usage: %s <csv_file_path> <starting_balance>\n"
        (List.hd argv)
