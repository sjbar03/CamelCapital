type t = {
  date : string;
  ticker : string;
  high : float;
  low : float;
  open_ : float;
  close : float;
  prev_close : float;
}
(** A record type that represents a single day of trading information for a
    specific stock with [ticker]*)

val last_1000_day_bal : float array
(* Global array for GUI chart data. Last 1000 days of backtesting balaces. *)

val last_1000_day_val : float array
(* Global array for GUI chart data. Last 1000 days of backtesting stock
   values. *)

val shift_in : 'a array -> 'a -> unit
(** [shift_in arr value] shifts value into the rightmost index of arr, shifting
    each previous entry down one position. Discards the entry at index 0
    Requires: Array.length arr > 0 *)

val parse_stock_data : string -> t
(** [parse_stock_data line] returns a [StockData.t] with information provided by
    [line]. Requires: the information in line must be in the following order
    "<date>,<open>,<high>,<low>,<close>,<adj_close>,<volume>" *)

val update_prev_close : t -> float -> t
(** [update_prev_close stock_data prev_close] is [stock_data] with its field for
    prev_close updated to contain [prev_close]*)

val true_range : t -> float
(** [true_range sd] is the true range calculated from the day of stock data
    represented by [sd]. Read more about true range and how to calculate it here
    https://www.linnsoft.com/techind/true-range-tr#:~:text=Welles%20Wilder%20des
    cribed%20these%20calculations,today's%20high%20to%20today's%20low. *)

val moving_percentile : float list -> float -> float
(** [moving_percentile tr_list percentile] calculates the moving percentile for
    the list of true ranges [tr_list] with threshold [percentile] *)

val calc_final_bal : float -> float -> float -> (t * 'a) list -> float * float
(** [calc_final_bal tr_75th_percentile buy_signal_multiplier starting_balance tr_data]
    is the final balance accumulated after considering each day of stock data
    and determining whether or not to buy. If a stock is bought, it sells
    according to next morning's opening price. *)

val calculate_daily_returns : t list -> float list
(** [calculate_daily_returns stock_data_list] computes the daily returns for a
    list of stock data. It returns a list of daily returns calculated as the
    percentage change from the previous close to the close of each day. If the
    previous close is zero, returns zero to avoid division by zero. *)

val average : float list -> float
(** [average lst] calculates the average of a list of float values. Returns zero
    if the list is empty. *)

val expected_return : t array -> float
(** [expected_return data] calculates the expected return of a list of stock
    data by first calculating daily returns with [calculate_daily_returns] and
    then averaging those returns using [average]. *)

val calculate_buy_price : t -> float -> float -> float
(** Calculates the desired buy price for a stock based on its last close price,
    the 75th percentile true range, and a buy signal multiplier. This price is
    what the algorithm will use to compare against the current price to decide
    whether to buy. *)

val arr_range : float array -> float * float
(** [arr_range arr] is the range of values in the array, in the form (min, max) *)

val gen_y_coord_from_range : float * float -> float -> int -> int
(** [gen_y_coord_from_range] is a translation from a raw float value to a pixel
    y value, based on the size of the SDL area*)
