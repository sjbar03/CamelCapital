(*@author Carson Wolber*)
open StockData

type garch_params = {
  alpha: float;
  beta: float;
  omega: float;
}

(** log of return *)
val log_r : t -> float

(** objective func for model fit score *)
val objective_function : t array -> garch_params -> float

(** Estimates garch model parameters alpha, beta, and omega*)
val estimate_garch_params : t array -> garch_params

(** the "main" function that gives variance for each stock*)
val forecast_volatility : t array -> garch_params -> int -> float array

