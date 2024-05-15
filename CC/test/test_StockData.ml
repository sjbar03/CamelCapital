open OUnit2
open CC.StockData

let string_of_t record =
  Printf.sprintf
    "{ date = %s; ticker = %s; high = %.2f; low = %.2f; open_ = %.2f; close = \
     %.2f; prev_close = %.2f }"
    record.date record.ticker record.high record.low record.open_ record.close
    record.prev_close

let test_parse_stock_data _ =
  let valid_line = "2020-01-01,100.0,105.0,95.0,102.0,102.0,2000" in
  let invalid_line = "2020-01-01,100.0" in
  let expected_output =
    {
      date = "2020-01-01";
      ticker = "AAPL";
      high = 105.0;
      low = 95.0;
      open_ = 100.0;
      close = 102.0;
      prev_close = 0.0;
    }
  in
  assert_equal ~printer:string_of_t expected_output
    (parse_stock_data valid_line);
  assert_raises (Failure "Incorrect CSV format for line: 2020-01-01,100.0")
    (fun () -> parse_stock_data invalid_line)

let test_true_range _ =
  let stock_data =
    {
      date = "2020-01-01";
      ticker = "AAPL";
      high = 105.0;
      low = 95.0;
      open_ = 100.0;
      close = 102.0;
      prev_close = 100.0;
    }
  in
  let expected_tr = 10.0 in
  assert_equal ~printer:string_of_float expected_tr (true_range stock_data)

let test_calculate_buy_price _ =
  let stock_data =
    {
      date = "2020-01-01";
      ticker = "AAPL";
      high = 105.0;
      low = 95.0;
      open_ = 100.0;
      close = 102.0;
      prev_close = 100.0;
    }
  in
  let tr_75th_percentile = 10.0 in
  let buy_signal_multiplier = 0.5 in
  let expected_price = 102.0 -. (10.0 *. 0.5) in
  assert_equal ~printer:string_of_float expected_price
    (calculate_buy_price stock_data tr_75th_percentile buy_signal_multiplier)

let suite =
  "Stock Functions Tests"
  >::: [
         "test_parse_stock_data" >:: test_parse_stock_data;
         "test_true_range" >:: test_true_range;
         "test_calculate_buy_price" >:: test_calculate_buy_price;
       ]

let () = run_test_tt_main suite
