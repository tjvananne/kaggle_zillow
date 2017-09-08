
# function defs

print("loading GBL_zil_config file")


# function definitions


    # modeling metric
    calc_rmse <- function(param_actuals, param_preds) {
        err <- param_actuals - param_preds
        err_sqrd <- err^2
        mean_err_sqrd <- mean(err_sqrd)
        rt_mean_err_sqrd <- sqrt(mean_err_sqrd)
        return(rt_mean_err_sqrd)
    }
    
    
    # min max scaler (0-1)
    min_max_scale <- function(vec) {
        vec <- (vec - min(vec, na.rm=T)) / ((max(vec, na.rm=T)) - min(vec, na.rm=T))    
        return(vec)
    }
    
    
    # undo the min max scaler (for calculating real rmse metric)
    undo_min_max_scale <- function(vec, prior_min, prior_max) {
        unscaled <- (vec * (prior_max - prior_min)) + prior_min
        return(unscaled)
    }



