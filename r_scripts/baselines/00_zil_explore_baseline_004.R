

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
        
            gc()
        
            # design quality assertions
            assert_that(length(intersect(y_train$id, y_holdout$id)) == 0)
            assert_that(length(intersect(y_train$id, y_test$id)) == 0)
            assert_that(length(intersect(y_test$id, y_holdout$id)) == 0)
            
            
# exp1 (all data) ----------------------------------------------
        
    # this is really just a test of the functions that have been developed
            
    # identify numeric vs categorical features
    feats_name_num <- setdiff(names(joined)[sapply(joined, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
    feats_name_cat <- setdiff(names(joined)[sapply(joined, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
    
    jlong <- tv_gen_numcat_long(p_df=joined, p_id=id, p_numcols=feats_name_num, p_catcols=feats_name_cat)
    rm(joined)
    gc()
    
    exp <- tv_gen_exp_sparsemats(p_longdf=jlong, p_id=id, p_tr_ids=y_train, p_te_ids=y_test, p_ho_ids=y_holdout, p_target="logerror")
    

    # set up params search space and run it!
    exp_params <- list("objective" = "reg:linear", 
                       "eval_metric" = "mae",
                       "eta" = 0.01, 
                       "max_depth" = 7, 
                       "subsample" = 0.3, 
                       "colsample_bytree" = 0.3,
                       "lambda" = 1, 
                       "alpha" = 1,
                       "max_delta_step" = 1,
                       "nthread" = 4)     
    
    # run CV
    set.seed(exp_seed)
    x_cv <- xgboost::xgb.cv(
        data=x_dmt_train,
        params=params,
        nrounds=10000,
        nfold=5,
        early_stopping_rounds=200
    )
    
    
    # identify best number of rounds
    this_eval_log <- data.frame(x_cv$evaluation_log)
    this_metric <- names(this_eval_log)[grepl("^test_", names(this_eval_log)) & grepl("_mean$", names(this_eval_log))]
    this_bestn_rounds <- which.min(this_eval_log[, this_metric])
    
    
    # run the real model
    this_xgb <- xgboost::xgboost(
        data=x_dmt_train,
        params=params,
        nrounds=this_bestn_rounds
    )
    
    
        # feature importance
        # dim(x_train_sp); length(unique(x_train$feature_name))
        this_xgb_imp <- xgboost::xgb.importance(feature_names = as.character(dist_feats$feature_name), model=this_xgb)
        xgboost::xgb.plot.importance(this_xgb_imp[1:20,])
        
        
        
        
    
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
    
    
    
###############################################################################
    
    # three different models for three different months
    
    #' Next, I'd like to see if we can improve THIS submission at all by 
    #' training three separate models for month 10, 11, 12, respectively.
    #' Then we can predict the months separately as opposed to just building
    #' one giant generalized model.
    
    
    # CHECKPOINT pipeline
    
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    getwd()
    # "C:/Users/tvananne/Documents/personal/github/kaggles/zillow_zestimate/r_scripts"
    
    
    # source in config and function defs
    source('GBL_zil_config.R')
    source('GBL_zil_function_defs.R')
    
    # load from checkpoint
    load(file=file.path(GBL_PATH_TO_CACHE, "all_files_for_00_zil_baseline01.RData"))
    gc()
    
        
    # first step: determine how to figure out how to separate them into the three months they're closest to
    tdates <- joined_sub$transactiondate[!is.na(joined_sub$transactiondate)]
    tdates <- lubridate::ymd(tdates)
    min(tdates); max(tdates)
    
    
    
    joined_sub$transactiondate <- lubridate::ymd(joined_sub$transactiondate)   
    
    
    joined_sub_10 <- joined_sub %>% 
        filter(transactiondate >= ymd('2016-10-01')) %>%
        filter(transactiondate <= ymd('2016-10-31'))
    
    joined_sub_11 <- joined_sub %>%
        filter(transactiondate >= ymd('2016-11-01')) %>%
        filter(transactiondate <= ymd('2016-11-30'))
    
    joined_sub_12 <- joined_sub %>%
        filter(transactiondate >= ymd('2016-12-01')) %>%
        filter(transactiondate <= ymd('2016-12-31'))
    
    
    # set up params search space and run it
    params <- list("objective" = "reg:linear", "eval_metric" = "mae",
                   "eta" = 0.01, 
                   "max_depth" = 6, 
                   "subsample" = 0.7, 
                   "colsample_bytree" = 0.6,
                   # "lambda" = 1.0, 
                   # "min_child_weight" = 6, 
                   # "gamma" = 10,
                   "alpha" = 1.0, 
                   "nthread" = 6)     
    
    
    # train with this, predict with this plus everything else?
    feats_all_10 <- feats_all %>%
        filter(id %in% joined_sub_10$id)
    
    
    
    x_cv <- xgboost::xgb.cv(
        data=x_dmt_train,
        params=params,
        nrounds=10000,
        nfold=7,
        early_stopping_rounds=50
    )
    
    min(x_cv$evaluation_log$test_mae_mean)
    which.min(x_cv$evaluation_log$test_mae_mean)
    
    myxgb <- xgboost::xgboost(
        data=x_dmt_train,
        params=params,
        nrounds=638
    )
    
    
    
    