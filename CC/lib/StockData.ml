type t = {
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

let parse_stock_data line =
  match String.split_on_char ',' line with
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

let update_prev_close stock_data prev_close = { stock_data with prev_close }

let true_range sd =
  List.fold_left max (sd.high -. sd.low)
    [ sd.high -. sd.prev_close; sd.prev_close -. sd.low ]

let moving_percentile tr_list percentile =
  let sorted_list = List.sort compare tr_list in
  let index =
    int_of_float (float_of_int (List.length sorted_list) *. percentile /. 100.)
  in
  List.nth sorted_list index

(* Add a new parameter to carry the number of shares bought *)
let determine_buy_sell tr_75th_percentile buy_signal_multiplier
    (prev_close, balance, shares_bought) (stock, _tr) =
  let buy_price =
    stock.close -. (tr_75th_percentile *. buy_signal_multiplier)
  in
  if stock.low <= buy_price && prev_close < buy_price then
    let shares_to_buy = balance *. 0.15 /. buy_price in
    (* Assuming 15% of balance is used for buying *)
    ( stock.close,
      balance -. (shares_to_buy *. buy_price),
      shares_bought +. shares_to_buy )
  else
    (* Sell all shares at the opening price of the current day and update the
       balance *)
    (stock.close, balance +. (shares_bought *. stock.open_), 0.0)
(* Reset shares_bought to 0 after selling *)

let calc_final_bal tr_75th_percentile buy_signal_multiplier starting_balance
    tr_data =
  List.fold_left
    (determine_buy_sell tr_75th_percentile buy_signal_multiplier)
    (0.0, starting_balance, 0.0)
    (* Initialize shares_bought to 0 *) tr_data
  |> fun (_, balance, _) -> (0.0, balance)
(* Only return the final balance, discarding shares_bought *)
