
# function defs

print("loading GBL_zil_config file")



# rapid dt merge
    fast_dt_merge <- function(p_df1, p_df2, p_keys) {
        
        # this fails based on referential integrity rules - need more practice with substitute and quote
        require(data.table)
        setDT(p_df1); setDT(p_df2)
        setkey(p_df1, p_keys); setkey(p_df2, p_keys)
        p_df3 <- merge(x=p_df1, y=p_df2, by=p_keys, all.x=T, all.y=F, sort=F)
        setDF(p_df3)
        sapply(p_df3, class) == "factor"
        return(p_df3)
            
    }


    
    
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




# XGB sweeper for classification    
tjv_xgb_classi_sweep <- function(param_train, param_grid, param_resultpath=NULL, 
                                     param_append=F, param_nrounds=10000, param_folds=5, 
                                     param_early_stop=100, param_randseed=1776, param_maximize=F) {
        
        
        # # for rapid dev
        # param_train <- dmat_train
        # param_grid <- xgb_grid
        # param_resultpath <- "xgb_sweep_results.csv"  # <-- keeping this simple for now
        # param_nrounds = 10000
        # param_folds = 5
        # param_early_stop=100
        
        # param_append logic is untested
        set.seed(param_randseed)
        
        # if a file path was supplied, create or overwrite the file, otherwise, start a data.frame
        if(!is.null(param_resultpath)) {
            
            # if user specified they want to append to existing file:
            if(param_append) {
                
                # if the file to append to actually exists:
                if(file.exists(param_resultpath)) {
                    print(paste0("appending to the file already created at: ", param_resultpath))
                    existing_results_file <- read.csv(param_resultpath, stringsAsFactors = F)
                    
                } else {
                    # file doesn't exist, cannot append to it
                    stop(paste0(param_resultpath, " - file does not exist, cannot append to this file"))
                }
                
            } else {
                # user doesn't want to append, so write to the file and overwrite if necessary
                cat("parameterkey, cv_score, best_nrounds\n", file=param_resultpath, append = FALSE)
            }
        } 
        
        
        # iterate for every row in the parameter grid
        for(i in 1:nrow(param_grid)) {
            
            # parameter key
            this_parakey <- paste0(names(param_grid[i, ]), "_", as.list(param_grid[i, ]), collapse="|")
            
            # if user wishes to append to existing results file:
            if(param_append) {
                if(this_parakey %in% existing_results_file$parameterkey) {
                    print(paste0(this_parakey, ": already evaluated..."))
                    next
                }
            }
            
            print(paste0("running this paramkey: ", this_parakey, "..."))
            
            # execute cv
            this_cv <- xgb.cv(
                data = param_train,
                nrounds = param_nrounds,
                nfold = param_folds,
                early_stopping_rounds = param_early_stop,
                params = as.list(param_grid[i,]),
                print_every_n = 100,
                maximize = param_maximize
            )
            
            
            # capture nrounds and score for min error
            ev_log <- data.frame(this_cv$evaluation_log)
            ev_log_metric <- names(ev_log)[grepl("^test_", names(ev_log)) & grepl("_mean", names(ev_log))]
            if(param_maximize) {
                this_best_nrounds <- which.max(ev_log[, ev_log_metric])
            } else {
                this_best_nrounds <- which.min(ev_log[, ev_log_metric])
            }
            
            this_best_score <- ev_log[this_best_nrounds, ev_log_metric]
            
            
            # if path isn't null, cat to it, if it is, then either create the df (i is 1) or concat to the df (i > 1)
            if(!is.null(param_resultpath)) {
                cat(paste0(this_parakey, ", ", this_best_score, ", ", this_best_nrounds, "\n"), 
                    file = param_resultpath, append = TRUE)
            } else {
                if(i == 1) {
                    results_df <- data.frame("parameterkey"=this_parakey, "cv_score"=this_best_score, "best_nrounds"=this_best_nrounds)
                } else {
                    this_df <- data.frame("parameterkey"=this_parakey, "cv_score"=this_best_score, "best_nrounds"=this_best_nrounds)
                    results_df <- bind_rows(results_df, this_df)
                }
            }
            
            
        } # end for loop
        # if result path is null then return it as a data.frame
        if(is.null(param_resultpath)) {
            return(results_df)
        }
        
        
} # end the function

    
    
    
    
    
