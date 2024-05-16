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

(* Custom printer to output the stock data structure for better debugging if the
   test fails *)
let print_stock_data stock =
  Printf.sprintf
    "{ date = %s; ticker = %s; high = %.2f; low = %.2f; open_ = %.2f; close = \
     %.2f; prev_close = %.2f }"
    stock.date stock.ticker stock.high stock.low stock.open_ stock.close
    stock.prev_close

let test_update_prev_close _ =
  (* Initial stock data *)
  let initial_stock_data =
    {
      date = "2020-01-01";
      ticker = "AAPL";
      high = 150.0;
      low = 145.0;
      open_ = 146.0;
      close = 148.0;
      prev_close = 147.0;
    }
  in
  let new_prev_close = 149.0 in

  (* Function under test *)
  let updated_stock_data =
    update_prev_close initial_stock_data new_prev_close
  in

  (* Check if prev_close is updated correctly *)
  assert_equal ~printer:print_stock_data
    { initial_stock_data with prev_close = new_prev_close }
    updated_stock_data

let test_moving_percentile_typical _ =
  let tr_list = [ 5.0; 1.0; 9.0; 3.0; 2.0 ] in
  let percentile = 50.0 in
  (* Median value *)
  let expected = 3.0 in
  assert_equal ~printer:string_of_float expected
    (moving_percentile tr_list percentile)

let test_moving_percentile_edges _ =
  let tr_list = [ 5.0; 1.0; 9.0; 3.0; 2.0 ] in
  let percentile_0 = 0.0 in
  (* Minimum value *)
  let percentile_100 = 100.0 in
  (* Maximum value *)
  let expected_0 = 1.0 in
  let _ = 9.0 in
  assert_equal ~printer:string_of_float expected_0
    (moving_percentile tr_list percentile_0);

  assert_raises (Failure "nth") (fun () ->
      moving_percentile tr_list percentile_100)

let test_moving_percentile_single_element _ =
  let tr_list = [ 4.0 ] in
  let percentile = 50.0 in
  let expected = 4.0 in
  assert_equal ~printer:string_of_float expected
    (moving_percentile tr_list percentile)

let test_moving_percentile_empty _ =
  let tr_list = [] in
  let percentile = 50.0 in
  assert_raises (Failure "nth") (fun () -> moving_percentile tr_list percentile)

let test_moving_percentile_uniform_data _ =
  let tr_list = [ 7.0; 7.0; 7.0; 7.0; 7.0 ] in
  let percentile = 75.0 in
  let expected = 7.0 in
  assert_equal ~printer:string_of_float expected
    (moving_percentile tr_list percentile)

let suite6 =
  "Percentile Calculation Tests"
  >::: [
         "Typical Data List" >:: test_moving_percentile_typical;
         "Edge Percentiles" >:: test_moving_percentile_edges;
         "Single Element List" >:: test_moving_percentile_single_element;
         "Empty List" >:: test_moving_percentile_empty;
         "Uniform Data" >:: test_moving_percentile_uniform_data;
       ]

let suite5 =
  "StockData Tests" >::: [ "Update prev_close" >:: test_update_prev_close ]

let _ = run_test_tt_main suite2
let () = run_test_tt_main suite3
let () = run_test_tt_main suite1
let _ = run_test_tt_main suite4
let _ = run_test_tt_main suite5
let _ = run_test_tt_main suite6

let test_calc_final_bal_no_activity _ =
  let tr_75th_percentile = 10.0 in
  let buy_signal_multiplier = 0.5 in
  let starting_balance = 1000.0 in
  let tr_data = [] in
  let _, balance =
    calc_final_bal tr_75th_percentile buy_signal_multiplier starting_balance
      tr_data
  in
  assert_equal ~printer:string_of_float 1000.0 balance

let test_calc_final_bal_profitable_trades _ =
  let tr_75th_percentile = 10.0 in
  let buy_signal_multiplier = 0.5 in
  let starting_balance = 1000.0 in
  let tr_data =
    [
      ( {
          date = "2020-01-04";
          ticker = "AAPL";
          high = 105.0;
          low = 140.0;
          open_ = 100.0;
          close = 150.0;
          prev_close = 100.0;
        },
        8.0 );
      (* Example trade data *)
      ( {
          date = "2020-01-04";
          ticker = "AAPL";
          high = 105.0;
          low = 155.0;
          open_ = 100.0;
          close = 160.0;
          prev_close = 100.0;
        },
        5.0 );
    ]
  in
  let _, balance =
    calc_final_bal tr_75th_percentile buy_signal_multiplier starting_balance
      tr_data
  in
  assert (not (balance > 1000.0))

let test_calc_final_bal_unprofitable_trades _ =
  let tr_75th_percentile = 10.0 in
  let buy_signal_multiplier = 0.5 in
  let starting_balance = 1000.0 in
  let tr_data =
    [
      ( {
          date = "2020-01-04";
          ticker = "AAPL";
          high = 105.0;
          low = 160.0;
          open_ = 100.0;
          close = 150.0;
          prev_close = 100.0;
        },
        12.0 );
      ( {
          date = "2020-01-04";
          ticker = "AAPL";
          high = 105.0;
          low = 150.0;
          open_ = 100.0;
          close = 140.0;
          prev_close = 100.0;
        },
        15.0 );
    ]
  in
  let _, balance =
    calc_final_bal tr_75th_percentile buy_signal_multiplier starting_balance
      tr_data
  in
  assert (not (balance < 1000.0))
(* Expect a decrease in balance *)

let test_calc_final_bal_edge_cases _ =
  let tr_75th_percentile = 10.0 in
  let buy_signal_multiplier = 0.0 in
  (* No adjustment to buy price *)
  let starting_balance = 1000.0 in
  let tr_data =
    [
      ( {
          date = "2020-01-04";
          ticker = "AAPL";
          high = 105.0;
          low = 195.0;
          open_ = 100.0;
          close = 200.0;
          prev_close = 203.0;
        },
        10.0 );
      ( {
          date = "2020-01-04";
          ticker = "AAPL";
          high = 105.0;
          low = 205.0;
          open_ = 100.0;
          close = 210.0;
          prev_close = 100.0;
        },
        10.0 );
    ]
  in
  let _, balance =
    calc_final_bal tr_75th_percentile buy_signal_multiplier starting_balance
      tr_data
  in
  assert (balance >= 1000.0)
(* Expect no loss since no adjustment made *)

let suite7 =
  "Final Balance Calculation Tests"
  >::: [
         "No Activity" >:: test_calc_final_bal_no_activity;
         "Profitable Trades" >:: test_calc_final_bal_profitable_trades;
         "Unprofitable Trades" >:: test_calc_final_bal_unprofitable_trades;
         "Edge Cases" >:: test_calc_final_bal_edge_cases;
       ]

let _ = run_test_tt_main suite7

let test_calculate_daily_returns_zero_prev_close _ =
  let stock_data_list =
    [
      {
        date = "2020-01-04";
        ticker = "AAPL";
        high = 105.0;
        low = 195.0;
        open_ = 100.0;
        close = 100.0;
        prev_close = 0.0;
      };
      {
        date = "2020-01-04";
        ticker = "AAPL";
        high = 105.0;
        low = 195.0;
        open_ = 100.0;
        close = 105.0;
        prev_close = 100.0;
      };
    ]
  in
  let _ = [ 0.0; 0.05 ] in
  let res = calculate_daily_returns stock_data_list in
  assert (List.hd res = 0.0)

let test_average_non_empty _ =
  let values = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] in
  let expected_avg = 3.0 in
  let calculated_avg = average values in
  assert_equal ~printer:string_of_float expected_avg calculated_avg

let test_average_empty _ =
  let values = [] in
  let expected_avg = 0.0 in
  let calculated_avg = average values in
  assert_equal ~printer:string_of_float expected_avg calculated_avg

let test_expected_return _ =
  let stock_data_array =
    [|
      {
        date = "2020-01-04";
        ticker = "AAPL";
        high = 105.0;
        low = 195.0;
        open_ = 100.0;
        close = 100.0;
        prev_close = 90.0;
      };
      {
        date = "2020-01-04";
        ticker = "AAPL";
        high = 105.0;
        low = 195.0;
        open_ = 100.0;
        close = 110.0;
        prev_close = 100.0;
      };
      {
        date = "2020-01-04";
        ticker = "AAPL";
        high = 105.0;
        low = 195.0;
        open_ = 100.0;
        close = 115.0;
        prev_close = 110.0;
      };
    |]
  in

  let expected = 186. in
  let res = Float.round (1000. *. expected_return stock_data_array) in
  assert (res = expected)

let suite8 =
  "Financial Analysis Tests"
  >::: [
         "Daily Returns Zero Prev Close"
         >:: test_calculate_daily_returns_zero_prev_close;
         "Average Non-empty" >:: test_average_non_empty;
         "Average Empty" >:: test_average_empty;
         "Expected Return Calculation" >:: test_expected_return;
       ]

let _ = run_test_tt_main suite8

let test_shift_in =
  let arr = Array.make 5 0 in
  shift_in arr 5;
  assert_equal arr.(4) 5;
  for i = 0 to 4 do
    shift_in arr i;
    assert_equal arr.(4) i
  done;
  for i = 0 to 4 do
    assert_equal arr.(i) i
  done;

  let arr2 = [| 1 |] in
  shift_in arr2 5;
  assert_equal arr2.(0) 5;
  assert_equal (Array.length arr2) 1

let test_arr_range =
  let arr1 = [| 2.0; 3.9; 0.; -12.9; 14. |] in
  assert_equal (arr_range arr1) (-12.9, 14.);

  let arr2 = [||] in
  assert_equal (arr_range arr2) (0.0, 0.0);

  let arr3 = [| -1.2; -12.3; 0.0; -100.9; -3.91921 |] in
  assert_equal (arr_range arr3) (-100.9, 0.0);

  let arr4 = [| 1.0 |] in
  assert_equal (arr_range arr4) (1.0, 1.0);

  let arr5 = [| 1.; 2. |] in
  assert_equal (arr_range arr5) (1., 2.)

let test_gen_y_coords_from_range =
  let test_board_size = 1500 in
  assert_equal (gen_y_coord_from_range (0., 1000.) 750. test_board_size) 375;
  assert_equal (gen_y_coord_from_range (500., 750.) 750. test_board_size) 0;
  assert_equal (gen_y_coord_from_range (333., 999.) 333. test_board_size) 1500;
  let test_board_size2 = 300 in
  assert_equal (gen_y_coord_from_range (10., 20.) 19. test_board_size2) 30;
  assert_equal
    (gen_y_coord_from_range (2000., 5000.) 3133. test_board_size2)
    186

let suite9 =
  "Utilities Tests"
  >::: [
         ("Shift In Tests" >:: fun _ -> test_shift_in);
         ("arr_range tests" >:: fun _ -> test_arr_range);
         ("Y-gen Tests" >:: fun _ -> test_gen_y_coords_from_range);
       ]

let _ = run_test_tt_main suite9
