

# level 1 loop stacks

#' steps:
#' split into train test holdout per usual
#' LOOP START:
#'     randomly sample feature columns (the number of columns to sample should also be random)
#'     from this i-th loop sample, split numeric / char cols and pass into long data per usual
#'     pass all that data into sparse mat generator (explicitly keep/set-aside the distinct features)
#'     randomly sample rows from the training sparse matrix output from above function (# of rows should be random as well)
#'     randomly pick one of ~6 or so pre-set xgb parameter sets that I create from the beginning (be careful with ETAs here)
#'     run the xgb.cv for this i-th round to determine the optimal # of rounds 
#'     run the real model on the subset of train rows for this round
#'     predict on holdout, capture MAE of holdout and set aside just for curiosity sake
#'     now limit a copy of "joined" (all data) to just the distinct features of this i-th example
#'     then generate a longdata / sparse mat from THAT copy from above (same method as inside of tv_gen_exp_sparsemats)
#'     pass ALL data through the model, generate predictions, and save those to disk in an RDS or something
#'     garbage collection()
#' NEXT iteration


# anything written to disk should have this style of file formatting:
    # level_01_round_0001_preds  # <-- predictions in .rds format
    # level_01_round_0001_feats  # <-- 1-col data.frame of the features used in .csv format
    # level_01_train_results     # <-- 4-col data.frame, one row per loop iteration
        # col1: level_01_round_001  # what level of model and what round of loop iteration?
        # col2: xgboost parameter key so we know what params generated this model
        # col3: cv_score
        # col4: holdout score
        # col5: best_nrounds


# manual reset:
# rm(list=setdiff(ls(), "joined"))



list.files('r_scripts')
source("r_scripts/GBL_zil_config.R")
source("r_scripts/GBL_zil_function_defs.R")

joined <- readRDS(file="input/joined_checkpoint1.rds")
joined <- joined[!duplicated(joined$id), ]


    # # one-time proc
    # joined_name_num <- setdiff(names(joined)[sapply(joined, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
    # joined_name_cat <- setdiff(names(joined)[sapply(joined, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
    # joined_long <- tv_gen_numcat_long(joined, id, joined_name_num, joined_name_cat)
    # saveRDS(joined_long, file="cache/joined_long.rds")
joined_long <- readRDS(file="cache/joined_long.rds")
gc()


lvl <- "01"
fp_preds <- "r_scripts/loopstacks/level_01_preds/"
fp_feats <- "r_scripts/loopstacks/level_01_feats/"
fp_errors <- "r_scripts/loopstacks/level_01_errors/01_errors.txt"
fp_results <- "r_scripts/loopstacks/level_01_train_results.csv"
myseed <- 1873



xgb_param_grid <- expand.grid(
    "objective"="reg:linear",
    "eval_metric"="mae",
    "eta"=c(0.02, 0.5, 0.75),
    "max_depth"=c(3, 5, 7),
    "alpha"=c(0, 1),
    "lambda"=c(0, 1),
    "subsample"=c(0.25, 0.5, 0.9),
    "colsample_bytree"=c(0.25, 0.5, 0.9),
    "max_delta_step"=c(0, 1),
    "nthread" = 6,
    stringsAsFactors = F)





set.seed(myseed)
i <- 1
while(TRUE) {
    # do stuff... forever
    gc()
    print(paste0("ON ITERATION: ", i))
    set.seed(i)  # <-- will ensure different splits of train/holdout every iteration
    iter <- sprintf("%04.0f", i)
    iter_rnd <- paste0("_", lvl, '_round_', iter, "_")
    
    all_feats <- names(joined)[grepl("^tv_", names(joined))]
    num_cols_to_samp <- ceiling(runif(1, ceiling(.10 * length(all_feats)), ceiling(.50 * length(all_feats))))
    this_feats <- all_feats[sample(1:length(all_feats), num_cols_to_samp)]
    this_j <- bind_cols(id=joined$id, logerror=joined$logerror, joined[, this_feats])
    
        # # investigate number of columns being sampled
        # set.seed(myseed)
        # hist(replicate(n=1000, ceiling(runif(1, ceiling(.10 * length(all_feats)), ceiling(.50 * length(all_feats))))), col='light blue')

    
    y_ <- this_j[, c("id", "logerror")]
    y_test_ <- y_[is.na(y_$logerror), ]
    y_train_ <- y_[!is.na(y_$logerror), ]
    holdout_indx_ <- caret::createDataPartition(y=y_train_$logerror, p=0.25, list=F)
    y_holdout_ <- y_train_[holdout_indx_, ]
    y_train_ <- y_train_[-holdout_indx_, ]
    rm(holdout_indx_, y_)
    gc()
    
    feats_name_num_ <- setdiff(names(this_j)[sapply(this_j, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
    feats_name_cat_ <- setdiff(names(this_j)[sapply(this_j, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
    
    if(length(feats_name_num_) == 0) {feats_name_num_ <- NULL}
    if(length(feats_name_cat_) == 0) {feats_name_cat_ <- NULL}
    
    gc()
    jlong_ <- tv_gen_numcat_long(p_df=this_j, p_id=id, p_numcols=feats_name_num_, p_catcols=feats_name_cat_)
    gc()
    this_exp_ <- tv_gen_exp_sparsemats(p_longdf=jlong_, p_id=id, p_tr_ids=y_train_, p_te_ids=y_test_, p_ho_ids=y_holdout_, p_target="logerror")
    gc()
    
    dmat_tr_ <- this_exp_$train
    dmat_te_ <- this_exp_$test
    dmat_ho_ <- this_exp_$holdout
    dist_feats_ <- this_exp_$features
    
    num_rows_to_sample <- ceiling(runif(1, ceiling(.30 * nrow(dmat_tr_)), ceiling(.90 * nrow(dmat_tr_))))
    rows_to_sample <- sample(1:nrow(dmat_tr_), num_rows_to_sample)
    dmat_tr_sub_ <- xgboost::slice(dmat_tr_, rows_to_sample)
    
        # proper "xgb" way of obtaining the labels attached to the subset dmatrix
        # xgboost::getinfo(dmat_tr_sub_, "label")
    
    xgb_param_indx_ <- floor(runif(1, 1, (nrow(xgb_param_grid)+1)))
    xgb_params_ <- as.list(xgb_param_grid[xgb_param_indx_, ])
    xgb_param_key_ <- paste0(  paste0(names(xgb_params_), "$")   , xgb_params_, collapse="|")
    
    
    # cross val to get number of rounds
    print("running the cross validation...")
    cv_results_ <-  xgb.cv(
        data=dmat_tr_sub_,
        nfold=5,
        nrounds=2000,
        params=xgb_params_,
        early_stopping_rounds=50,
        print_every_n=2001
    )
    
    eval_log <- data.frame(cv_results_$evaluation_log)
    eval_metric <- names(eval_log)[grepl("^test_", names(eval_log)) & grepl("_mean$", names(eval_log))]
    cv_score <- min(eval_log[, eval_metric], na.rm=T)
    best_nrounds <- max(which.min(eval_log[, eval_metric]), 5)  # <-- at least 5 iterations...
    
    
    # build actual model
    print("training the model...")
    xgb_mdl_ <- xgboost(
        data=dmat_tr_,
        nrounds=best_nrounds,
        params = xgb_params_,
        print_every_n=2001)
    
    
    # collect holdout results
    holdout_results_ <- data.frame(
        id=y_holdout_$id,
        actuals=y_holdout_$logerror,
        preds=predict(xgb_mdl_, dmat_ho_)
    )
    
    # calc holdout score
    holdout_score_ <- mean( abs(holdout_results_$actuals - holdout_results_$preds))
    
    
    # prepare joined_long for preds on ALL data
    print("preparing all data to be passed through model...")
    joined_long_ <- joined_long %>% filter(feature_name %in% dist_feats_$feature_name)
    
            if(length(unique(joined_long_$feature_name)) != nrow(dist_feats_)  |  
               length(unique(joined_long_$id)) != length(unique(joined$id))) {
                cat(paste0(i, "\n"), file=fp_errors, append=T)
                print(paste0("*****************There's a problem with iteration: ", i))
                next
            }
            # assert_that(length(unique(joined_long_$feature_name)) == nrow(dist_feats_))
            # assert_that(length(unique(joined_long_$id)) == length(unique(joined$id)))
        
        
        # map in feature numbers
        setDT(joined_long_); setDT(dist_feats_)
        setkey(joined_long_, feature_name); setkey(dist_feats_, feature_name)
        joined_long_ <- merge(x=joined_long_, y=dist_feats_, by="feature_name", all.x=T, all.y=F, sort=F)
        gc()
        
        # create distinct id / id_nums
        joined_long_ <- joined_long_ %>% arrange(id)
        dist_ids_ <- data.frame(
            id=unique(joined_long_$id),
            id_num=1:length(unique(joined_long_$id)))
        
        
        # map in id_nums
        setDT(joined_long_); setDT(dist_ids_)
        setkey(joined_long_, id); setkey(dist_ids_, id)
        joined_long_ <- merge(x=joined_long_, y=dist_ids_, by="id", all.x=T, all.y=F, sort=F)
        gc()
    
              
    # create sparse matrix
    print("creating all-data sparse matrices...")
    joined_long_ <- joined_long_ %>% arrange(id, feature_name)
    joined_spmat_ <- Matrix::sparseMatrix(
        i=joined_long_$id_num,
        j=joined_long_$feature_num,
        x=joined_long_$value)
    
    rm(joined_long_)  # can we do this yet?
    
    joined_dmat_ <- xgboost::xgb.DMatrix(data=joined_spmat_)
    gc()
    
    all_preds <- data.frame(id=unique(joined$id), preds=predict(xgb_mdl_, joined_dmat_), stringsAsFactors = F)
    
    
        if(nrow(all_preds) != length(unique(joined$id))) {
            cat(paste0(i, "\n"), file=fp_errors, append=T)
            print(paste0("*****************There's a problem with iteration: ", i))
            next
        }
        # assert_that(nrow(all_preds) == length(unique(joined$id)))
        
    
    print("writing everything to disk...")
    # save all_pred
    prediction_fp_ <- paste0(fp_preds, "level", iter_rnd, "preds.rds")
    saveRDS(all_preds, file=prediction_fp_)
    
    # save dist_features
    features_fp_ <- paste0(fp_feats, "level", iter_rnd, "feats.rds")
    saveRDS(as.data.frame(dist_feats_), file=features_fp_)
    
    # concat to results file
    if(!file.exists(fp_results)) {
        cat("level_iteration,xgb_params,cv_score,holdout_score,best_nrounds\n", file=fp_results, append=F)
    }
    
    # write out results to the results file
    cat(paste(iter_rnd, xgb_param_key_, cv_score, holdout_score_, paste0(best_nrounds, "\n"), sep=','), file=fp_results, append = T)
    gc()
    
    # increment
    i <- i + 1
}













