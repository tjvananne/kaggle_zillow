

# same as 002 (sparse) but using the new helper functions to help build xgboost dmatrices quicker.
# I'd like to start experimenting with different forms of ensembling in this experiment


# source in config and function defs
source('r_scripts/GBL_zil_config.R')         # config is loading our libraries as well
source('r_scripts/GBL_zil_function_defs.R')  # function definitions


# config for this script / experiment:
exp_seed <- 1776
exp_number <- "004"
rdata_file <- file.path(GBL_PATH_TO_CACHE, paste0("all_files_for_00_zil_baseline", exp_number, ".RData"))
rdata_exp_file <- file.path(GBL_PATH_TO_CACHE, paste0("experiment_files_00_zil_baseline", exp_number, ".RData"))
read_in_file <- file.path(GBL_PATH_TO_DATA, "joined_checkpoint1.rds")
exp_target <- "logerror"  # <-- this isn't hooked up to anything yet, but this is what we need to start predicting on features
    
    
    
# load data
    list.files(GBL_PATH_TO_DATA)
    joined <- readRDS(read_in_file)
    joined <- joined[!duplicated(joined$id), ]
    assert_that(sum(duplicated(joined$id)) == 0)
    
    
    
# identify train/test + holdout split -------------------------------------
    set.seed(exp_seed)
    joined <- joined %>% arrange(id)
    y <- joined[, c("id", "logerror")]
    y_test <- y[is.na(y$logerror), ]
    y_train <- y[!is.na(y$logerror), ]
    holdout_indx <- caret::createDataPartition(y=y_train$logerror, p=0.15, list=F)
    y_holdout <- y_train[holdout_indx, ]
    y_train <- y_train[-holdout_indx, ]
    rm(holdout_indx, y)
    gc()
        
            # design quality assertions
            assert_that(length(intersect(y_train$id, y_holdout$id)) == 0)
            assert_that(length(intersect(y_train$id, y_test$id)) == 0)
            assert_that(length(intersect(y_test$id, y_holdout$id)) == 0)
            
            
# mod1 (all data) ----------------------------------------------
        
            
    # manually select what cols you want from "joined"
    # joined <- joined  # <-- this model we'll use all of joined
            
    # we have file (experiment) number, and then a model (within file) number
    mod1_nbr <- "01"
    mod1_longcache_fp <- paste0("cache/jlong_yids_file", exp_number, "_mod", mod1_nbr, ".RData")
    mod1_dmatcache_fp <- paste0("cache/dmats_file", exp_number, "_mod", mod1_nbr, ".RData")
    
    # either load in cache file or build experiment files from scratch
    if(file.exists(mod1_dmatcache_fp)) {
        print("mod1 dmatcache file exists; loading it now...")
        load(file=mod1_dmatcache_fp)
    } else if(file.exists(mod1_longcache_fp)) { 
        print("mod1 dmatcache file doesn't exist; loading in longcache file though...")
        load(file=mod1_longcache_fp) 
        mod1_exp <- tv_gen_exp_sparsemats(p_longdf=mod1_jlong, p_id=id, p_tr_ids=y_train, p_te_ids=y_test, p_ho_ids=y_holdout, p_target="logerror")
        gc()
        save(mod1_exp, file=mod1_dmatcache_fp)
    } else {
        print("mod1 didn't have either cash file, generating dmat files now...")
        
        # split numeric / categorical features (features only)
        mod1_feats_name_num <- setdiff(names(joined)[sapply(joined, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
        mod1_feats_name_cat <- setdiff(names(joined)[sapply(joined, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
        
        # generate long data
        mod1_jlong <- tv_gen_numcat_long(p_df=joined, p_id=id, p_numcols=mod1_feats_name_num, p_catcols=mod1_feats_name_cat)
        rm(joined)
        gc()
        
        # save or load cache
        # save(mod1_jlong, y_train, y_test, y_holdout, file=mod1_longcache_fp)  # <-- skip this to save space
        
        # generate dmat files
        mod1_exp <- tv_gen_exp_sparsemats(p_longdf=mod1_jlong, p_id=id, p_tr_ids=y_train, p_te_ids=y_test, p_ho_ids=y_holdout, p_target="logerror")
        save(mod1_exp, file=mod1_dmatcache_fp)
        lapply(mod1_exp, dim)
        gc()
    }
        
    # split exp files into separate dmats
    mod1_dmat_tr <- mod1_exp$train
    mod1_dmat_te <- mod1_exp$test
    mod1_dmat_ho <- mod1_exp$holdout
    mod1_dist_feats <- mod1_exp$features
    
    
    # set up params search space and run it!
    mod1_params <- list("objective" = "reg:linear", 
                       "eval_metric" = "mae",
                       "eta" = 0.01, 
                       "max_depth" = 7, 
                       "subsample" = 0.5, 
                       "colsample_bytree" = 0.5,
                       "lambda" = 0, 
                       "alpha" = 1,
                       "max_delta_step" = 1,
                       "nthread" = 4)     
    mod1_obj_min <- T
    
    # run CV
    set.seed(exp_seed)
    mod1_cv <- xgboost::xgb.cv(
        data=mod1_dmat_tr,
        params=mod1_params,
        nrounds=10000,
        nfold=5,
        early_stopping_rounds=200
    )
    
    
    # identify best number of rounds
    mod1_eval_log <- data.frame(mod1_cv$evaluation_log)
    mod1_metric <- names(mod1_eval_log)[grepl("^test_", names(mod1_eval_log)) & grepl("_mean$", names(mod1_eval_log))]
    if(mod1_obj_min) {
        mod1_bestn_rounds <- which.min(mod1_eval_log[, mod1_metric])
    } else {
        mod1_bestn_rounds <- which.max(mod1_eval_log[, mod1_metric])
    }
    
    
    # run the real model
    mod1_xgb <- xgboost::xgb.train(
        data=mod1_dmat_tr,
        params=mod1_params,
        nrounds=mod1_bestn_rounds,
        print_every_n=1
    )
    
    
        # feature importance
        # dim(x_train_sp); length(unique(x_train$feature_name))
        mod1_xgb_imp <- xgboost::xgb.importance(feature_names = as.character(mod1_dist_feats$feature_name), model=mod1_xgb)
        xgboost::xgb.plot.importance(mod1_xgb_imp[1:20,])
        xgboost::xgb.plot.importance(mod1_xgb_imp[21:40,])
        
        
        
    
    # predict
    yhat_holdout <- predict(this_xgb, x_holdout_sp)
    y_holdout$yhat <- yhat_holdout
    mean(abs(y_holdout$logerror - y_holdout$yhat))
    y_train$yhat <- predict(this_xgb, x_train_sp)
    y_test$yhat <- predict(this_xgb, x_test_sp)
    
    
        # compare prediction distribution vs real distribution
        hist(yhat_holdout, breaks=50, col='light blue')
        hist(y_train$logerror, breaks=50, col='light green')
    
    
    # generate subs
    sub <- bind_rows(
            select(y_test, ParcelId=id, `201610`=yhat),
            select(y_train, ParcelId=id, `201610`=logerror),
            select(y_holdout, ParcelId=id, `201610`=yhat)
        ) %>%
        mutate(
            `201611`=`201610`,
            `201612`=`201610`,
            `201710`=`201610`,
            `201711`=`201610`,
            `201712`=`201610`
            )
        
    
    sub$ParcelId <- gsub("pid_", "", sub$ParcelId)
    head(sub$ParcelId)
    
    write.csv(sub, paste0("../subs/sub_baseline_", exp_number, "001.csv"), row.names = F)
    head(sub)
    # baseline001. xgboost. cv=0.06916229; holdout=0.065937; PLB=0.0655428
    
    #' solid! these scores are pretty terrible, but at least they are all
    #' tracking well together. That is good that holdout and public leader
    #' board scores are so close. CV is lacking a bit but that is to be
    #' expected. Tradeoffs.
    #' 
    