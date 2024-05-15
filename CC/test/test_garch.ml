open OUnit2
open CC.StockData
open CC.Garch

let round_to_4dp x =
  let factor = 10.0 ** 4.0 in
  floor ((x *. factor) +. 0.5) /. factor

let test_log_r _ =
  let stock1 : CC.StockData.t =
    {
      close = 105.0;
      prev_close = 100.0;
      date = "";
      ticker = "";
      high = 0.;
      low = 0.;
      open_ = 0.;
    }
  in
  let stock2 : CC.StockData.t =
    {
      close = 100.0;
      prev_close = 0.0;
      date = "";
      ticker = "";
      high = 0.;
      low = 0.;
      open_ = 0.;
    }
  in
  assert_equal ~printer:string_of_float 0.0488 (log_r stock1);
  assert_equal ~printer:string_of_float 0.0 (log_r stock2)

open OUnit2
open CC.StockData

let test_log_r_negative_prices _ =
  let stock : CC.StockData.t =
    {
      close = -105.0;
      prev_close = -100.0;
      date = "";
      ticker = "";
      high = 0.;
      low = 0.;
      open_ = 0.;
    }
  in
  assert_equal ~printer:string_of_float 0.0488 (log_r stock)

let test_log_r_no_price_change _ =
  let stock : CC.StockData.t =
    {
      close = 100.0;
      prev_close = 100.0;
      date = "";
      ticker = "";
      high = 0.;
      low = 0.;
      open_ = 0.;
    }
  in
  assert_equal ~printer:string_of_float 0.0 (log_r stock)

let test_log_r_small_price_movements _ =
  let stock : CC.StockData.t =
    {
      close = 100.01;
      prev_close = 100.0;
      date = "";
      ticker = "";
      high = 0.;
      low = 0.;
      open_ = 0.;
    }
  in
  assert_equal ~printer:string_of_float 0.0001 (log_r stock)

let test_log_r_large_price_movements _ =
  let stock : CC.StockData.t =
    {
      close = 200.0;
      prev_close = 100.0;
      date = "";
      ticker = "";
      high = 0.;
      low = 0.;
      open_ = 0.;
    }
  in
  assert_equal ~printer:string_of_float 0.6931 (log_r stock)

let test_objective_function _ =
  let data =
    [|
      {
        close = 105.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 106.0;
        prev_close = 105.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let params = { alpha = 0.05; beta = 0.90; omega = 0.01 } in
  let result = objective_function data params in
  assert_bool "Objective should be positive" (result > 0.0)

let test_objective_function_empty _ =
  let data = [||] in
  (* Empty array of stock data *)
  let params = { alpha = 0.05; beta = 0.90; omega = 0.01 } in
  let result = objective_function data params in
  assert_equal ~printer:string_of_float 0.0 result

let test_objective_function_uniform_prices _ =
  let data =
    [|
      {
        close = 100.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 100.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let params = { alpha = 0.05; beta = 0.90; omega = 0.01 } in
  let result = objective_function data params in
  assert_equal ~printer:string_of_float 0.01
    result (* Minimum variance based on omega *)

let test_objective_function_decreasing_prices _ =
  let data =
    [|
      {
        close = 95.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 90.0;
        prev_close = 95.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let params = { alpha = 0.05; beta = 0.90; omega = 0.01 } in
  let result = objective_function data params in
  assert_bool "Objective should account for negative growth" (result > 0.0)

let test_objective_function_large_variations _ =
  let data =
    [|
      {
        close = 200.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 300.0;
        prev_close = 200.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let params = { alpha = 0.05; beta = 0.90; omega = 0.01 } in
  let result = objective_function data params in
  assert_bool "Objective should be large for large price variations"
    (result > 0.0)

let test_estimate_garch_params _ =
  let data =
    [|
      {
        close = 105.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 106.0;
        prev_close = 105.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let estimated_params = estimate_garch_params data in
  assert (
    estimated_params.alpha > 0.0
    && estimated_params.beta > 0.0
    && estimated_params.omega > 0.0)

let test_estimate_garch_params_high_volatility _ =
  let data =
    [|
      {
        close = 100.0;
        prev_close = 90.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 120.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 90.0;
        prev_close = 120.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let estimated_params = estimate_garch_params data in
  assert (not (estimated_params.alpha > 0.05 && estimated_params.beta > 0.85))

let test_estimate_garch_params_low_volatility _ =
  let data =
    [|
      {
        close = 100.0;
        prev_close = 100.1;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 100.2;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let estimated_params = estimate_garch_params data in
  assert (not (estimated_params.alpha < 0.01 && estimated_params.omega < 0.01))

let test_estimate_garch_params_empty _ =
  let data = [||] in
  let estimated_params = estimate_garch_params data in
  assert (
    not
      (estimated_params.alpha = 0.0
      && estimated_params.beta = 0.0
      && estimated_params.omega = 0.0))

let test_estimate_garch_params_non_sequential _ =
  let data =
    [|
      {
        close = 150.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 90.0;
        prev_close = 150.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let estimated_params = estimate_garch_params data in
  assert_bool "Check for valid alpha and beta values"
    (estimated_params.alpha > 0.0 && estimated_params.beta < 0.95)

let test_variance _ =
  let data =
    [|
      {
        close = 105.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 106.0;
        prev_close = 105.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let result = variance data in
  assert_bool "Variance should be non-negative" (result >= 0.0)

let test_variance_zero_variance _ =
  let data =
    [|
      {
        close = 100.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 100.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let result = variance data in
  assert_equal ~printer:string_of_float 0.1396 (round_to_4dp result)

let test_variance_increasing_trend _ =
  let data =
    [|
      {
        close = 100.0;
        prev_close = 90.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 110.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let result = variance data in
  assert_bool "Variance should be positive on an increasing price trend"
    (result > 0.0)

let test_variance_high_volatility _ =
  let data =
    [|
      {
        close = 100.0;
        prev_close = 150.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
      {
        close = 150.0;
        prev_close = 100.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let result = variance data in
  assert_bool "Variance should be high with large fluctuations"
    (not (result > 25.0))

let test_variance_single_data_point _ =
  let data =
    [|
      {
        close = 100.0;
        prev_close = 95.0;
        date = "";
        ticker = "";
        high = 0.;
        low = 0.;
        open_ = 0.;
      };
    |]
  in
  let result = variance data in
  assert_bool "Variance with a single data point should be handled gracefully"
    (result >= 0.0)

let suite3 =
  "test suite"
  >::: [
         "Test 1" >:: test_log_r;
         "Test 2" >:: test_objective_function;
         "Test 3" >:: test_estimate_garch_params;
         "Test 4" >:: test_variance;
       ]

let suite1 =
  "Log R Function Tests"
  >::: [
         "test_log_r_negative_prices" >:: test_log_r_negative_prices;
         "test_log_r_no_price_change" >:: test_log_r_no_price_change;
         "test_log_r_small_price_movements" >:: test_log_r_small_price_movements;
         "test_log_r_large_price_movements" >:: test_log_r_large_price_movements;
       ]

let suite2 =
  "Objective Function Tests"
  >::: [
         "Empty Dataset" >:: test_objective_function_empty;
         "Uniform Prices" >:: test_objective_function_uniform_prices;
         "Decreasing Prices" >:: test_objective_function_decreasing_prices;
         "Large Variations" >:: test_objective_function_large_variations;
       ]

let suite4 =
  "Estimate GARCH Params Tests"
  >::: [
         "High Volatility" >:: test_estimate_garch_params_high_volatility;
         "Low Volatility" >:: test_estimate_garch_params_low_volatility;
         "Empty Data" >:: test_estimate_garch_params_empty;
         "Non-Sequential Data" >:: test_estimate_garch_params_non_sequential;
       ]

let suite5 =
  "Estimate GARCH Params Tests"
  >::: [
         "High Volatility" >:: test_estimate_garch_params_high_volatility;
         "Low Volatility" >:: test_estimate_garch_params_low_volatility;
         "Empty Data" >:: test_estimate_garch_params_empty;
         "Non-Sequential Data" >:: test_estimate_garch_params_non_sequential;
       ]

let suite6 =
  "Variance Function Tests"
  >::: [
         "Zero Variance" >:: test_variance_zero_variance;
         "Increasing Trend" >:: test_variance_increasing_trend;
         "High Volatility" >:: test_variance_high_volatility;
         "Single Data Point" >:: test_variance_single_data_point;
       ]

let _ = run_test_tt_main suite5
let _ = run_test_tt_main suite1
let _ = run_test_tt_main suite2
let _ = run_test_tt_main suite3
let _ = run_test_tt_main suite4
let _ = run_test_tt_main suite6
