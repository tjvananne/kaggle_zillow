




xgb_param_grid <- expand.grid(
    "objective"="reg:linear",
    "eval_metric"="mae",
    "eta"=c(0.05, 0.75, 0.1, 0.5),
    "max_depth"=c(3, 5, 7),
    "alpha"=c(0, 1),
    "lambda"=c(0, 1),
    "subsample"=c(0.25, 0.5, 0.9),
    "colsample_bytree"=c(0.25, 0.5, 0.9),
    "max_delta_step"=c(0, 1)
)



