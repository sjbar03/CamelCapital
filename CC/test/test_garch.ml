open OUnit2
open CC.StockData
open CC.Garch

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

let tests =
  "test suite"
  >::: [
         "Test 1" >:: test_log_r;
         "Test 2" >:: test_objective_function;
         "Test 3" >:: test_estimate_garch_params;
         "Test 4" >:: test_variance;
       ]

let _ = run_test_tt_main tests
