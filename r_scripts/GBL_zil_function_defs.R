
# function defs

print("loading GBL_zil_config file")


# function definitions

    # cut numeric fields into quantiled categories
    cut2_rename <- function(p_vec, p_grps, p_name, p_verb=T) {
        
        # # for testing:
        # p_vec <- c(1, 4, 2, NA, 7, 5, 4, 2, NA, 5, 3, 3, 2, 4, 4, NA, 3, 2)
        # unique(p_vec)
        # p_grps <- 5
        # p_name <- "test_vec"
        # p_verb <- T
        
        post_cut <- cut2(p_vec, g=p_grps)
        if(p_verb) {print(table(post_cut)) }
        post_cut_num <- as.numeric(post_cut)
        if(p_verb) {print(table(post_cut_num))}
        post_cut_num_char <- paste0(p_name, "_", as.character(post_cut_num))
        if(p_verb) {print(table(post_cut_num_char))}
        post_cut_num_char[grepl("_NA$", post_cut_num_char)] <- NA
        return(post_cut_num_char)
    }



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



