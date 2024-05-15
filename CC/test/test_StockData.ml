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

let test_parse_stock_data_extra_fields _ =
  let line =
    "2020-01-01,100.0,105.0,95.0,102.0,102.0,2000,ExtraField1,ExtraField2"
  in
  let _ =
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
  assert_raises
    (Failure
       "Incorrect CSV format for line: \
        2020-01-01,100.0,105.0,95.0,102.0,102.0,2000,ExtraField1,ExtraField2")
    (fun () -> parse_stock_data line)

let test_parse_stock_data_missing_non_critical_fields _ =
  let line = "2020-01-01,100.0,105.0,95.0,102.0" in
  let _ =
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
  assert_raises
    (Failure "Incorrect CSV format for line: 2020-01-01,100.0,105.0,95.0,102.0")
    (fun () -> parse_stock_data line)

let test_parse_stock_data_non_numeric_values _ =
  let line = "2020-01-01,abc,105.0,95.0,102.0,102.0,2000" in
  assert_raises
    (Failure
       "float_of_string failure on: 'abc' in line: \
        2020-01-01,abc,105.0,95.0,102.0,102.0,2000") (fun () ->
      parse_stock_data line)

let test_parse_stock_data_edge_case_values _ =
  let line = "2020-01-01,0.0,0.0001,0.0001,0.0,0.0,2000" in
  let expected_output =
    {
      date = "2020-01-01";
      ticker = "AAPL";
      high = 0.0001;
      low = 0.0001;
      open_ = 0.0;
      close = 0.0;
      prev_close = 0.0;
    }
  in
  assert_equal ~printer:string_of_t expected_output (parse_stock_data line)

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

let test_true_range_close_lower_than_prev_close _ =
  let stock_data =
    {
      date = "2020-01-02";
      ticker = "AAPL";
      high = 98.0;
      low = 90.0;
      open_ = 95.0;
      close = 92.0;
      prev_close = 100.0;
    }
  in
  let expected_tr = 10.0 in
  assert_equal ~printer:string_of_float expected_tr (true_range stock_data)

let test_true_range_high_price_peaking _ =
  let stock_data =
    {
      date = "2020-01-03";
      ticker = "AAPL";
      high = 115.0;
      low = 95.0;
      open_ = 100.0;
      close = 105.0;
      prev_close = 100.0;
    }
  in
  let expected_tr = 20.0 in
  assert_equal ~printer:string_of_float expected_tr (true_range stock_data)

let test_true_range_all_prices_equal _ =
  let stock_data =
    {
      date = "2020-01-04";
      ticker = "AAPL";
      high = 100.0;
      low = 100.0;
      open_ = 100.0;
      close = 100.0;
      prev_close = 100.0;
    }
  in
  let expected_tr = 0.0 in
  assert_equal ~printer:string_of_float expected_tr (true_range stock_data)

let test_true_range_close_and_low_same _ =
  let stock_data =
    {
      date = "2020-01-05";
      ticker = "AAPL";
      high = 108.0;
      low = 95.0;
      open_ = 105.0;
      close = 95.0;
      prev_close = 103.0;
    }
  in
  let expected_tr = 13.0 in
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

let test_calculate_buy_price_zero_tr _ =
  let stock_data =
    {
      date = "2020-01-02";
      ticker = "AAPL";
      high = 100.0;
      low = 100.0;
      open_ = 100.0;
      close = 100.0;
      prev_close = 100.0;
    }
  in
  let tr_75th_percentile = 0.0 in
  let buy_signal_multiplier = 0.5 in
  let expected_price = 100.0 in
  assert_equal ~printer:string_of_float expected_price
    (calculate_buy_price stock_data tr_75th_percentile buy_signal_multiplier)

let test_calculate_buy_price_high_multiplier _ =
  let stock_data =
    {
      date = "2020-01-03";
      ticker = "AAPL";
      high = 105.0;
      low = 95.0;
      open_ = 100.0;
      close = 102.0;
      prev_close = 100.0;
    }
  in
  let tr_75th_percentile = 10.0 in
  let buy_signal_multiplier = 1.0 in
  let expected_price = 102.0 -. 10.0 in
  assert_equal ~printer:string_of_float expected_price
    (calculate_buy_price stock_data tr_75th_percentile buy_signal_multiplier)

let test_calculate_buy_price_negative_tr _ =
  let stock_data =
    {
      date = "2020-01-04";
      ticker = "AAPL";
      high = 105.0;
      low = 95.0;
      open_ = 100.0;
      close = 102.0;
      prev_close = 100.0;
    }
  in
  let tr_75th_percentile = -10.0 in
  let buy_signal_multiplier = 0.5 in
  let expected_price = 102.0 +. (10.0 *. 0.5) in
  assert_equal ~printer:string_of_float expected_price
    (calculate_buy_price stock_data tr_75th_percentile buy_signal_multiplier)

let test_calculate_buy_price_varying_close _ =
  let stock_data =
    {
      date = "2020-01-05";
      ticker = "AAPL";
      high = 110.0;
      low = 90.0;
      open_ = 100.0;
      close = 105.0;
      prev_close = 95.0;
    }
  in
  let tr_75th_percentile = 15.0 in
  let buy_signal_multiplier = 0.5 in
  let expected_price = 105.0 -. (15.0 *. 0.5) in
  assert_equal ~printer:string_of_float expected_price
    (calculate_buy_price stock_data tr_75th_percentile buy_signal_multiplier)

let suite4 =
  "Buy Price Calculation Tests"
  >::: [
         "Zero True Range" >:: test_calculate_buy_price_zero_tr;
         "High Multiplier" >:: test_calculate_buy_price_high_multiplier;
         "Negative True Range" >:: test_calculate_buy_price_negative_tr;
         "Varying Close Prices" >:: test_calculate_buy_price_varying_close;
       ]

let suite2 =
  "Stock Functions Tests"
  >::: [
         "test_parse_stock_data" >:: test_parse_stock_data;
         "test_true_range" >:: test_true_range;
         "test_calculate_buy_price" >:: test_calculate_buy_price;
       ]

let suite1 =
  "Stock Data Parsing Tests"
  >::: [
         "Valid Line" >:: test_parse_stock_data;
         "Extra Fields" >:: test_parse_stock_data_extra_fields;
         "Missing Non-critical Fields"
         >:: test_parse_stock_data_missing_non_critical_fields;
         "Non-numeric Values" >:: test_parse_stock_data_non_numeric_values;
         "Edge Case Values" >:: test_parse_stock_data_edge_case_values;
       ]

let suite3 =
  "True Range Tests"
  >::: [
         "Initial Test" >:: test_true_range;
         "Close Lower than Previous Close"
         >:: test_true_range_close_lower_than_prev_close;
         "High Price Peaking" >:: test_true_range_high_price_peaking;
         "All Prices Equal" >:: test_true_range_all_prices_equal;
         "Close and Low at Same Level" >:: test_true_range_close_and_low_same;
       ]

let _ = run_test_tt_main suite2
let () = run_test_tt_main suite3
let () = run_test_tt_main suite1
let _ = run_test_tt_main suite4
