

# baseline joined data so far

# 
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()
# # "C:/Users/tvananne/Documents/personal/github/kaggles/zillow_zestimate/r_scripts"


# source in config and function defs
source('r_scripts/GBL_zil_config.R')
source('r_scripts/GBL_zil_function_defs.R')

rebuild1 <- F

if(rebuild1) {
    # THIS WAS A CHECKPOINT from the preprocessing file
    joined <- readRDS(file.path(GBL_PATH_TO_DATA, "joined2.rds"))
    joined <- joined[!duplicated(joined$id), ]
    assert_that(sum(duplicated(joined$id)) == 0)
    
        # we don't have transaction dates for ANY of test
        sum(is.na(joined$transactiondate))
        sum(is.na(joined$logerror))
    
    
    # first baseline will ignore transactiondates
    joined_sub <- joined[, names(joined)[names(joined) %in% c("id", "logerror", "transactiondate") | grepl("tv_", names(joined))]]
    rm(joined); gc()
    
    
    # identify train/test + holdout
    set.seed(1776)
    joined_sub <- joined_sub %>% arrange(id)
    y <- joined_sub[, c("id", "logerror")]
    y_test <- y[is.na(y$logerror), ]
    y_train <- y[!is.na(y$logerror), ]
    y_holdout <- y_train[sample(1:nrow(y_train), size = ceiling(0.15 * nrow(y_train))), ]
    y_train <- y_train[!y_train$id %in% y_holdout$id, ]
    
    gc()
    
    assert_that(!any(y_train$id %in% y_holdout$id))
    assert_that(!any(y_train$id %in% y_test$id))
    
    
    feats_name_num <- setdiff(names(joined_sub)[sapply(joined_sub, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
    feats_name_cat <- setdiff(names(joined_sub)[sapply(joined_sub, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
    
    joined_num <- joined_sub[, feats_name_num]
    joined_cat <- joined_sub[, feats_name_cat]
    
    
    # normalize numeric variables
    myproc_scaler_center <- caret::preProcess(joined_num, method=c("scale", "center"))
    # myproc_range <- caret::preProcess(joined_num, method=c("range"))
    
    joined_num_scale_center <- cbind(id=joined_sub$id, predict(myproc_scaler_center, joined_num))
    # joined_num_rng <- cbind(id=joined_sub$id, predict(myproc_range, joined_num))
    # joined_num <- cbind(id=joined_sub$id, joined_num)
    
    
    # gather all numeric features into feature_name / column format
    feats_num <- tidyr::gather(joined_num_scale_center[, c(feats_name_num, "id")], key=feature_name, value=value, -id) %>%
        arrange(id)
    
    
    
    
    # writing loop to handle ALL feature / categoricals
    feats_cat <- data.frame()
    
    for(feat in feats_name_cat) {
        
        # feat <- "tv_cat_latitude_five"
        
        print(feat)
        # isolate this feat
        df_feats_ <- joined_sub[, c("id", feat)]
        # assign(x = paste0("pos_values_", feat), value = as.character(unique(df_feats_[, 2])))
        # df_feats_[, 2] <- paste0(feat, "_", as.character(df_feats_[, 2]))
        
        df_feats_$value <- 1
        names(df_feats_) <- c("id", "feature_name", "value")
        feats_cat <- bind_rows(feats_cat, df_feats_)
        
    }
    
    gc()
    feats_cat <- feats_cat[!is.na(feats_cat$feature_name), ]
    feats_num <- feats_num[!is.na(feats_num$value), ]
    
    
    
    rm(joined, df_feats_, joined_cat, joined_num, joined_num_rng, joined_num_scale_center); gc()
    
    feats_all <- bind_rows(feats_num, feats_cat)
    rm(feats_cat, feats_num); gc()
    
    
    # feature ids need to be at the "all" level
    gc()
    distinct_feats_all <- data.frame(feature_name = unique(feats_all$feature_name))
    distinct_feats_all$feature_num <- as.numeric(as.factor(distinct_feats_all$feature_name))
    
    setDT(feats_all); setDT(distinct_feats_all)
    setkey(feats_all, feature_name); setkey(distinct_feats_all, feature_name)
    feats_all <- merge(x=feats_all, y=distinct_feats_all, by=c("feature_name"), all.x=T, all.y=F, sort=F)
    
    rm(distinct_feats_all)
    setDF(feats_all)
    gc()
    
    save(y, y_test, y_train, y_holdout, joined_sub, feats_all, file=file.path(GBL_PATH_TO_CACHE, "all_files_for_00_zil_baseline01.RData"))
    
    
}


    
    
####################################################################################
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
    
    
    
    
    # split back into groups
    x_train <- feats_all[feats_all$id %in% y_train$id, ]  # <-- challenge from past taylor who doesn't know data.table -- try doing this in data.table to learn how to!
    x_test <- feats_all[feats_all$id %in% y_test$id, ]    # <-- build shiny apps. publish them on own web server. show what they can do with pre-aggregated data for free. charge for real thing.
    x_holdout <- feats_all[feats_all$id %in% y_holdout$id, ]

    # still in order?
    head(joined_sub$id, 9); head(x_train$id, 9); head(x_test$id, 9); head(x_holdout$id, 9)    
    
    
        # need to number each distinct id within each dataset
        dist_x_train <- data.frame(id = unique(x_train$id)) %>% 
            arrange(id) %>%
            mutate(id_num = row_number())
            
        setDT(x_train); setDT(dist_x_train); setkey(x_train, id); setkey(dist_x_train, id)
        x_train <- merge(x=x_train, y=dist_x_train, by=c("id"), all.x=T, all.y=F, sort=F) 
        assert_that(sum(is.na(x_train$id_num)) == 0)
        rm(dist_x_train); gc()
        
        # now test...
        dist_x_test <- data.frame(id = unique(x_test$id)) %>%
            arrange(id) %>%
            mutate(id_num = row_number())
        
        setDT(x_test); setDT(dist_x_test); setkey(x_test, id); setkey(dist_x_test, id)
        x_test <- merge(x=x_test, y=dist_x_test, by=c("id"), all.x=T, all.y=F, sort=F)
        assert_that(sum(is.na(x_test$id_num)) == 0)
        rm(dist_x_test); gc()
                
        # now holdout...
        dist_x_holdout <- data.frame(id = unique(x_holdout$id)) %>%
            arrange(id) %>%
            mutate(id_num = row_number())
        
        setDT(x_holdout); setDT(dist_x_holdout); setkey(x_holdout, id); setkey(dist_x_holdout, id)
        x_holdout <- merge(x=x_holdout, y=dist_x_holdout, by=c("id"), all.x=T, all.y=F, sort=F)
        assert_that(sum(is.na(x_holdout$id_num)) == 0)
        rm(dist_x_holdout); gc()

        # more comfortable with dfs
        setDF(x_train); setDF(x_test); setDF(x_holdout)
        
    
    # identify intersecting features between our three datasets
    feat_intersect_1 <- dplyr::intersect(x_holdout$feature_num, x_train$feature_num)
    feat_intersect_2 <- dplyr::intersect(feat_intersect_1, x_test$feature_num)
    x_test <- x_test %>% dplyr::filter(feature_num %in% feat_intersect_2)
    x_train <- x_train %>% dplyr::filter(feature_num %in% feat_intersect_2)
    x_holdout <- x_holdout %>% dplyr::filter(feature_num %in% feat_intersect_2)
    rm(feat_intersect_1, feat_intersect_2)
    gc()
        
    
    # let's manually merge the y value into the various "x_" datasets here
    setDT(x_train); setDT(y_train); setkey(x_train, id); setkey(y_train, id)
    x_train <- merge(x_train, y_train, by='id', all.x=T, all.y=F, sort=F)
    setDF(x_train); setDF(y_train)
    setDT(x_holdout); setDT(y_holdout); setkey(x_holdout, id); setkey(y_holdout, id)
    x_holdout <- merge(x_holdout, y_holdout, by='id', all.x=T, all.y=F, sort=F)
    setDF(x_holdout); setDF(y_holdout)
    head(x_holdout); head(x_train)
    
    # one final sort to make sure
    x_train <- x_train %>% arrange(id)
    x_holdout <- x_holdout %>% arrange(id)
    x_test <- x_test %>% arrange(id)
    y_train <- y_train %>% arrange(id)
    y_holdout <- y_holdout %>% arrange(id)
    gc()
    
    # generate sparse matrices
    assert_that(sum(sapply(x_train, function(x) sum(is.na(x)))) == 0)
    x_train_sp <- sparseMatrix(
        i=x_train$id_num,
        j=x_train$feature_num,
        x=x_train$value
    )
     
    
    # now test
    assert_that(sum(sapply(x_test, function(x) sum(is.na(x)))) == 0)
    x_test_sp <- sparseMatrix(
        i=x_test$id_num,
        j=x_test$feature_num,
        x=x_test$value
    ) 
    
    # now holdout
    assert_that(sum(sapply(x_holdout, function(x) sum(is.na(x)))) == 0)
    x_holdout_sp <- sparseMatrix(
        i=x_holdout$id_num,
        j=x_holdout$feature_num,
        x=x_holdout$value
    )
    
    
    # construct dmats
    x_dmt_train <- xgboost::xgb.DMatrix(x_train_sp, label=y_train$logerror)
    x_dmt_holdout <- xgboost::xgb.DMatrix(x_holdout_sp, label=y_holdout$logerror)  # doesn't hurt to put the label on the holdout?
    x_dmt_test <- xgboost::xgb.DMatrix(x_test_sp)
        
    
    
# this will likely be a grid search over a wide area of xgb parameters

    
    # set up params search space and run it!
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
    
    yhat_holdout <- predict(myxgb, x_holdout_sp)
    y_holdout$yhat <- yhat_holdout
    mean(abs(y_holdout$logerror - y_holdout$yhat))
    y_train$yhat <- predict(myxgb, x_train_sp)
    y_test$yhat <- predict(myxgb, x_test_sp)
    
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
    
    write.csv(sub, "../subs/sub_baseline_001.csv", row.names = F)
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
    
    
    
    