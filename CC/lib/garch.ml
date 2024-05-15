(*@author Carson Wolber ctw54*)
open StockData

type garch_params = {
  alpha : float;
  beta : float;
  omega : float;
}

let log_r stock =
  if stock.prev_close <> 0.0 then log (stock.close /. stock.prev_close) else 0.0

let objective_function data params =
  let residuals = Array.map log_r data in
  let variances = Array.make (Array.length residuals) 0.0 in
  variances.(0) <- residuals.(0) ** 2.0;
  for i = 1 to Array.length residuals - 1 do
    variances.(i) <-
      params.omega
      +. (params.alpha *. (residuals.(i - 1) ** 2.0))
      +. (params.beta *. variances.(i - 1))
  done;
  Array.fold_left ( +. ) 0.0 variances

let estimate_garch_params data =
  let initial_params = { alpha = 0.05; beta = 0.90; omega = 0.01 } in
  let rec optimize params best_score iteration =
    if iteration <= 0 then params
    else
      let tested_params =
        [
          { params with alpha = params.alpha +. 0.01 };
          { params with beta = params.beta +. 0.01 };
          { params with omega = params.omega +. 0.01 };
        ]
      in
      let scores = List.map (objective_function data) tested_params in
      let best_index, best_new_score =
        List.fold_left
          (fun (bi, bs) (i, s) -> if s < bs then (i, s) else (bi, bs))
          (0, best_score)
          (List.mapi (fun i s -> (i, s)) scores)
      in
      if best_new_score < best_score then
        optimize
          (List.nth tested_params best_index)
          best_new_score (iteration - 1)
      else params
  in
  optimize initial_params (objective_function data initial_params) 100

let variance data =
  let params = estimate_garch_params data in
  let residuals = Array.map log_r data in
  let n = Array.length residuals in
  let variances = Array.make (n + 1) 0.0 in
  variances.(0) <- residuals.(0) ** 2.0;
  for i = 1 to n - 1 do
    variances.(i) <-
      params.omega
      +. (params.alpha *. (residuals.(i - 1) ** 2.0))
      +. (params.beta *. variances.(i - 1))
  done;
  variances.(n) <-
    params.omega
    +. (params.alpha *. variances.(n - 1))
    +. (params.beta *. variances.(n - 1));
  sqrt variances.(n)

let kelly_criterion data = variance data /. expected_return data
