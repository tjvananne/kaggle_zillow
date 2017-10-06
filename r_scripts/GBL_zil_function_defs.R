
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
    cut2_rename <- function(p_vec, p_grps, p_name="", p_verb=T) {
        
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
        
        if(p_name=="") {
            post_cut_num_char <- as.character(post_cut_num)
        } else {
            post_cut_num_char <- paste0(p_name, "_", as.character(post_cut_num))
        }
        
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





# convert wide data into long data for easier sparse matrix generation
tv_gen_numcat_long <- function(p_df, p_id, p_numcols=NULL, p_catcols=NULL,  p_scale=T) {
    
    # convert the p_id parameter into something we can use in dplyr functions
    this_id <- enquo(p_id)
    
    # NUMERIC
    print("processing numeric columns....")
    if(!is.null(p_numcols)) {
        # isolate numeric features, scale/center (if applicable), combine
        df_num <- select(p_df, p_numcols)
        
        if(p_scale) {
            print("scaling numeric columns....")
            
            myproc_scaler_center <- caret::preProcess(df_num, method=c("scale", "center"))
            df_num <- predict(myproc_scaler_center, df_num)  # <-- overwriting the existing non-scaled numeric values
        }    
        # add ID back in
        df_num <- cbind(id=select(p_df, !!this_id), df_num)
        gc()
        # gather all numeric features into feature_name / column format
        print("gathering numeric columns....")
        df_num <- tidyr::gather(df_num, key=feature_name, value=value, -!!this_id) %>%
            filter(!is.na(value)) %>%
            arrange(!!this_id)
        gc()
    }
    
    
    # CATEGORICAL
    print("processing categorical columns....")
    if(!is.null(p_catcols)) {
        # create a collector dataframe
        df_cats <- vector("list", length(p_catcols))
        
        print("looping through categorical columns....")
        # not the most efficient way, but it'll get the job done
        for(i in 1:length(p_catcols)) {
            
            # keep track of where ew're at
            print(p_catcols[i])  # <-- keep for now
            # isolate this feat
            df_feats_ <- select(p_df, !!this_id, p_catcols[i])
            # one hot encoding
            df_feats_$value <- 1
            names(df_feats_) <- c("id", "feature_name", "value")
            df_feats_ <- df_feats_ %>%
                filter(!is.na(feature_name))
            df_cats[[i]] <- df_feats_
        }
        gc() # <-- move this into the loop if things get crazy and we need more memory
        df_cat <- bind_rows(df_cats); rm(df_cats); gc()
    }
    print("finished looping through categorical columns....")
    
    if(!is.null(p_numcols) & is.null(p_catcols)) {
        # p_numcols provided but not p_catcols
        feats_all_long <- df_num 
    } else if(!is.null(p_catcols) & is.null(p_numcols)) {
        # p_catcols is provided but not p_numcols
        feats_all_long <- df_cat
    } else {
        # both are provided -- there's a final option that neither are provided, but wth would that even mean?
        feats_all_long <- bind_rows(df_num, df_cat)
    }
    
    print("final sort on the data's id field....")
    # for consistency sake, let's arrange it by the id
    feats_all_long <- feats_all_long %>%
        arrange(!!this_id)
    
    return(feats_all_long)
}

    # # testing function
    # x <- tv_gen_numcat_long(p_df=joined_small, p_id=id, p_numcols=numcols, p_catcols=catcols)
    #     assert_that(all(unique(x$id) == joined_small$id))
    # x2 <- tv_gen_numcat_long(p_df=joined_small, p_id=id, p_numcols=numcols) # <-- just nums
    #     assert_that(all(unique(x$id) == joined_small$id))        
    #     head(x2); unique(x2$feature_name)
    # x3 <- tv_gen_numcat_long(p_df=joined_small, p_id=id, p_catcols=catcols) # <-- just cats
    #     assert_that(all(unique(x3$id) == joined_small$id))
    #     head(x3); unique(x3$feature_name)




    


# takes in long df, id (unquoted), tr/te/ho ids, target var (quoted)
tv_gen_exp_sparsemats <- function(p_longdf, p_id, p_tr_ids, p_te_ids, p_ho_ids, p_target) {
    # p_longdf: longform of the dataset
    # p_id: name of the "id" field
    # p_target: name of the target variable
    # p_yids: 2 column dataframe, one column is the id, other is the target variable
    # p_tr_ids: 2 column df, <target var name> and id
    # p_te_ids: 2 column df, <target var name> and id
    # p_ho_ids: 2 column df, <target var name> and id
    
    # p_longdf should be 3 column df with names of (id, feature_name, value)
    # p_id should be id (unquoted)
    # this will be the case if you used tv_gen_numcat_long function to feed this one
    
    print("separating long data into tr, te, ho...")
    train_ <- p_longdf %>% filter(id %in% p_tr_ids$id)
    test_ <- p_longdf %>% filter(id %in% p_te_ids$id) 
    holdout_ <- p_longdf %>% filter(id %in% p_ho_ids$id)
    rm(p_te_ids, p_longdf); gc()
    
    # feature mapping table to create feature numbers
    print("distinct feature number mapping...")
    dist_feats <- intersect(train_$feature_name, test_$feature_name)
    dist_feats <- intersect(dist_feats, holdout_$feature_name)
    dist_feats <- dist_feats[order(dist_feats)]
    df_dist_feats <- data.frame(feature_name=dist_feats, feature_num=1:length(dist_feats))
    gc()
    
    # merge
    print("merging in the distinct feature numbers...")
    setDT(test_); setkey(test_, feature_name)
    setDT(df_dist_feats); setkey(df_dist_feats, feature_name)
    setDT(train_); setkey(train_, feature_name)
    setDT(holdout_); setkey(holdout_, feature_name)
    train_ <- merge(x=train_, y=df_dist_feats, by="feature_name", all.x=T, all.y=F, sort=F)
    test_ <- merge(x=test_, y=df_dist_feats, by="feature_name", all.x=T, all.y=F, sort=F)
    holdout_ <- merge(x=holdout_, y=df_dist_feats, by="feature_name", all.x=T, all.y=F, sort=F)
    gc()
    # rm(df_dist_feats)  # <-- let's keep this for feature importance metrics later on
    print("filtering out features that don't exist in all three sets...")
    train_ <- train_ %>% filter(!is.na(feature_num))
    test_ <- test_ %>% filter(!is.na(feature_num))
    holdout_ <- holdout_ %>% filter(!is.na(feature_num))
    gc()
    
    # Numeric value mapping tables for IDs
    print("creating distinct numeric id mapping tables...")
    train_ids_ <- train_ %>% select(id) %>% unique() %>% arrange(id) %>% mutate(id_num = 1:nrow(.))
    test_ids_ <- test_ %>% select(id) %>% unique() %>% arrange(id) %>% mutate(id_num = 1:nrow(.))
    holdout_ids_ <- holdout_ %>% select(id) %>% unique() %>% arrange(id) %>% mutate(id_num = 1:nrow(.))
    gc()
    
    # merge
    print("merging the distinct id numbers into data sets...")
    setDT(train_ids_); setDT(test_ids_); setDT(holdout_ids_)
    setDT(train_); setDT(test_); setDT(holdout_)
    setkey(train_ids_, id); setkey(test_ids_, id); setkey(holdout_ids_, id)
    setkey(train_, id); setkey(test_, id); setkey(holdout_, id)
    gc()
    test_ <- merge(x=test_, y=test_ids_, by="id", all.x=T, all.y=F, sort=F)
    rm(test_ids_); gc()
    train_ <- merge(x=train_, y=train_ids_, by="id", all.x=T, all.y=F, sort=F)
    rm(train_ids_); gc()
    holdout_ <- merge(x=holdout_, y=holdout_ids_, by="id", all.x=T, all.y=F, sort=F)
    rm(holdout_ids_); gc()
    
    
    # sparse mats
    print("generating sparse matrices...")
    spmat_tr_ <- sparseMatrix(i = train_$id_num, j = train_$feature_num, x = train_$value)
    spmat_te_ <- sparseMatrix(i = test_$id_num, j = test_$feature_num, x = test_$value)
    spmat_ho_ <- sparseMatrix(i = holdout_$id_num, j = holdout_$feature_num, x = holdout_$value)
    rm(test_)
    gc()
    
    # sort labels
    print("arranging labels before attaching to xgb.DMatrices...")
    p_tr_ids <- p_tr_ids %>% filter(id %in% train_$id) %>% arrange(id)
    p_ho_ids <- p_ho_ids %>% filter(id %in% holdout_$id) %>% arrange(id)
    rm(train_, holdout_)
    gc()
    
    # construct dmats
    print("generating xgb.DMatrices...")
    dmat_tr_ <- xgboost::xgb.DMatrix(spmat_tr_, label=p_tr_ids[, p_target])
    dmat_te_ <- xgboost::xgb.DMatrix(spmat_te_)
    dmat_ho_ <- xgboost::xgb.DMatrix(spmat_ho_, label=p_ho_ids[, p_target])
    gc()
    
    return_list <- list(train=dmat_tr_, test=dmat_te_, holdout=dmat_ho_, features=df_dist_feats) 
    return(return_list)
}

    # testing (FULL EXAMPLE)
    # # setup - let's iris for now because it's simpler?
    # set.seed(1)
    # myiris <- iris[sample(1:nrow(iris), nrow(iris)), ]
    # myiris$id <- 1:nrow(myiris)            
    # names(myiris) <- gsub("\\.", "_", tolower(names(myiris)))
    # myiris_yid <- myiris[, c("petal_length", "id")]
    # myiris$petal_length <- NULL
    # myiris_remcols <- "id"
    # myiris_numcols <- names(myiris)[sapply(myiris, class) %in% c("numeric", "integer")]
    # myiris_catcols <- names(myiris)[sapply(myiris, class) %in% c("character", "factor")]
    # myiris_numcols <- myiris_numcols[!myiris_numcols %in% myiris_remcols]
    # myiris_catcols <- myiris_catcols[!myiris_catcols %in% myiris_remcols]
    # 
    # myiris_numcols
    # names(myiris)
    # 
    # tr_idx <- caret::createDataPartition(y=myiris_yid$petal_length, p=0.7, list=F)
    # myiris_tr_ids <- myiris_yid[tr_idx,]
    # myiris_te_ids <- myiris_yid[-tr_idx,] 
    # ho_idx <- caret::createDataPartition(y=myiris_tr_ids$petal_length, p=0.2, list=F)
    # myiris_ho_ids <- myiris_tr_ids[ho_idx, ]
    # myiris_tr_ids <- myiris_tr_ids[-ho_idx, ]
    # 
    # assert_that(length(intersect(myiris_tr_ids$id, myiris_ho_ids$id)) == 0)
    # assert_that(length(intersect(myiris_tr_ids$id, myiris_te_ids$id)) == 0)
    # assert_that(length(intersect(myiris_te_ids$id, myiris_ho_ids$id)) == 0)
    # 
    # 
    # # generate long form
    # myiris_long <- tv_gen_numcat_long(myiris, id, myiris_numcols, myiris_catcols)
    # 
    # 
    # exp_dmats <- tv_gen_exp_sparsemats(p_longdf=myiris_long, p_id=id, p_target="petal_length", 
    #                                    p_tr_id=myiris_tr_ids, p_te_id=myiris_te_ids, p_ho_id=myiris_ho_ids)
    
    
    
    
