

# baseline joined data so far.

#' sparse matrices are too difficult to keep track of, next experiments will be exactly the same
#' but with dense matrices so we can actually see what the hell is going on.
#' Ok nevermind, sparse matrices are very necessary.


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()
# # "C:/Users/tvananne/Documents/personal/github/kaggles/zillow_zestimate/r_scripts"


# source in config and function defs
source('r_scripts/GBL_zil_config.R')         # config is loading our libraries as well
source('r_scripts/GBL_zil_function_defs.R')  # function definitions


# config for this script / experiment:
exp_seed <- 1776
exp_number <- "002"
rdata_file <- file.path(GBL_PATH_TO_CACHE, paste0("all_files_for_00_zil_baseline", exp_number, ".RData"))
rdata_exp_file <- file.path(GBL_PATH_TO_CACHE, paste0("experiment_files_00_zil_baseline", exp_number, ".RData"))
read_in_file <- file.path(GBL_PATH_TO_DATA, "joined_checkpoint1.rds")
exp_target <- "logerror"  # <-- this isn't hooked up to anything yet, but this is what we need to start predicting on features


# control flow flag
rebuild1 <- T


# set up data for train/test split and whatnot
if(rebuild1) {
    
    # what would it take for this whole block to be built into a function?
    # that would allow us to quickly iterate over different datasets without 
    # the pain of manipulating the long data which can be challenging
    
    
    # THIS WAS A CHECKPOINT from the preprocessing file
    list.files(GBL_PATH_TO_DATA)
    joined <- readRDS(read_in_file)
    joined <- joined[!duplicated(joined$id), ]
    assert_that(sum(duplicated(joined$id)) == 0)
    
        # we don't have transaction dates for ANY of test
        sum(is.na(joined$transactiondate))
        sum(is.na(joined$logerror))
    
    
# function dev  ------------------------------------------------------------------    
        
        
        
        
        
        
    #' ok, we're going to start dev on a new function which takes in the dense
    #' data frame, names of numeric cols, and names of categorical cols, and
    #' returns you with a dataframe of long form. Bonus if we can supply the name
    #' of the unique record identifier to be used in the "tidyr::gather()" function.
    #' That would require non-standard evaluation
    
        
    joined <- arrange(joined, id)
        
        
    # joined is the dataset we're passing in; these are inputs we're responsible for to generate the long-form dataset
    numcols <- names(joined)[startsWith(names(joined), "tv_rawreg") | startsWith(names(joined), "tv_logreg")]
    catcols <- names(joined)[startsWith(names(joined), "tv_cat_")]
    
    
    # for quicker development and testing
    joined_small <- joined[1:10000,]
    
    
    
    # what if some of these are blank?
    
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
            
            
                
                
                
                
                
    # setup - let's iris for now because it's simpler?
    set.seed(1)
    myiris <- iris[sample(1:nrow(iris), nrow(iris)), ]
    myiris$id <- 1:nrow(myiris)            
    names(myiris) <- gsub("\\.", "_", tolower(names(myiris)))
    myiris_yid <- myiris[, c("petal_length", "id")]
    myiris$petal_length <- NULL
    myiris_remcols <- "id"
    myiris_numcols <- names(myiris)[sapply(myiris, class) %in% c("numeric", "integer")]
    myiris_catcols <- names(myiris)[sapply(myiris, class) %in% c("character", "factor")]
    myiris_numcols <- myiris_numcols[!myiris_numcols %in% myiris_remcols]
    myiris_catcols <- myiris_catcols[!myiris_catcols %in% myiris_remcols]
    
        myiris_numcols
        names(myiris)
    
    tr_idx <- caret::createDataPartition(y=myiris_yid$petal_length, p=0.7, list=F)
    myiris_tr_ids <- myiris_yid[tr_idx,]
    myiris_te_ids <- myiris_yid[-tr_idx,] 
    ho_idx <- caret::createDataPartition(y=myiris_tr_ids$petal_length, p=0.2, list=F)
    myiris_ho_ids <- myiris_tr_ids[ho_idx, ]
    myiris_tr_ids <- myiris_tr_ids[-ho_idx, ]
    
        assert_that(length(intersect(myiris_tr_ids$id, myiris_ho_ids$id)) == 0)
        assert_that(length(intersect(myiris_tr_ids$id, myiris_te_ids$id)) == 0)
        assert_that(length(intersect(myiris_te_ids$id, myiris_ho_ids$id)) == 0)
    
            
    # generate long form
    myiris_long <- tv_gen_numcat_long(myiris, id, myiris_numcols, myiris_catcols)
    
        
    
    
    
    
    
    tv_gen_exp_sparsemats <- function(p_longdf, p_id, p_target, p_yids, p_tr_ids, p_te_ids, p_ho_ids=NULL) {
        # p_longdf: longform of the dataset
        # p_id: name of the "id" field
        # p_target: name of the target variable
        # p_yids: 2 column dataframe, one column is the id, other is the target variable
        # p_tr_ids: character vector of train id values
        # p_te_ids: character vector of test id values
        # p_ho_ids: character vector of holdout id values
        
        # this function renames your ID field to "id" - deal with it.
        # stop pretending like you're making a package and just write the function with constraints
        
        
        
        this_id <- enquo(p_id)
    
        p_longdf <- p_longdf %>% rename(id = !!this_id)
        
        train_ <- p_longdf %>% filter(id %in% p_tr_ids)
        test_ <- p_longdf %>% filter(id %in% p_te_ids) 
        holdout_ <- p_longdf %>% filter(id %in% p_ho_ids)
        
        browser()
        
        # combine feature names to create the distinct features mapping
        dist_feats <- intersect(train_$feature_name, test_$feature_name)
        dist_feats <- intersect(dist_feats, holdout_$feature_name)
        dist_feats <- dist_feats[order(dist_feats)]
        
        # i had just decided to do the feature mapping before id number mapping
        
        
        # Numeric value mapping tables for IDs
        train_ids_ <- train_ %>% arrange(id) %>% select(id) %>% unique() %>% mutate(id_num = 1:nrow(.))
        test_ids_ <- test_ %>% arrange(id) %>% select(id) %>% unique() %>% mutate(id_num = 1:nrow(.))
        holdout_ids_ <- holdout_ %>% arrange(id) %>% select(id) %>% unique() %>% mutate(id_num = 1:nrow(.))
        
        # merge
        setDT(train_ids_); setDT(test_ids_); setDT(holdout_ids_)
        setkey(train_ids_, id); setkey(test_ids_, id); setkey(holdout_ids_, id)
        train_ <- merge(x=train_, y=train_ids_, by="id", all.x=T, all.y=F, sort=F)
            rm(train_ids_)
        test_ <- merge(x=test_, y=test_ids_, by="id", all.x=T, all.y=F, sort=F)
            rm(test_ids_)
        holdout_ <- merge(x=holdout_, y=holdout_ids_, by="id", all.x=T, all.y=F, sort=F)
            rm(holdout_ids_)
        gc()
        
        
        
        
        
        
        
        
        return(1)
    }
    
    
    
    
    tv_gen_exp_sparsemats(p_longdf=myiris_long, p_id=id, p_target=petal_length, 
                          p_tr_ids=myiris_tr_ids$id, p_te_ids=myiris_te_ids$id, p_ho_ids=myiris_ho_ids$id)
    
    
    
    
    
    
    
    
# identify train/test + holdout split -------------------------------------
        
        # this doesn't necessarily have to happen first, this could have been done later after the long df creation
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
            
            
    # numeric / categorical split ----------------------------------------------
            
        # identify numeric vs categorical features
        feats_name_num <- setdiff(names(joined)[sapply(joined, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
        feats_name_cat <- setdiff(names(joined)[sapply(joined, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
        
        
            # did we leave any out?
            assert_that(length(feats_name_num) + length(feats_name_cat) == length(names(joined)) - 3)
        
        # separate them into two dataframes
        joined_num <- joined[, feats_name_num]
        joined_cat <- joined[, feats_name_cat]
        
    
    # numerical preprocessing ---------------------------------------------------
        
        # normalize numeric variables (scaling occurs with all data combined)
        myproc_scaler_center <- caret::preProcess(joined_num, method=c("scale", "center"))
        joined_num_scaled <- cbind(id=joined$id, predict(myproc_scaler_center, joined_num))
        
        
        # gather all numeric features into feature_name / column format
        feats_num <- tidyr::gather(joined_num_scaled[, c(feats_name_num, "id")], key=feature_name, value=value, -id) %>%
            arrange(id)
        
            # inspect:
            head(feats_num)
        
    # categorical preprocessing ---------------------------------------------------
        
        # create a collector dataframe
        feats_cat <- data.frame()
        
        # not the most efficient way, but it'll get the job done
        for(feat in feats_name_cat) {
            
            # keep track of where ew're at
            print(feat)
            
            # isolate this feat
            df_feats_ <- joined[, c("id", feat)]
            
            # one hot encoding
            df_feats_$value <- 1
            names(df_feats_) <- c("id", "feature_name", "value")
            feats_cat <- bind_rows(feats_cat, df_feats_)
        }
        
            # inspect:
            head(feats_cat)
    
        
            gc()
            
        # remove NAs
        feats_cat <- feats_cat[!is.na(feats_cat$feature_name), ]
        feats_num <- feats_num[!is.na(feats_num$value), ]
        
        
        # clean up environment
        rm(joined, df_feats_, joined_cat, joined_num, joined_num_scaled); gc()
        
        
        # combine feature again
        feats_all <- bind_rows(feats_num, feats_cat)
        rm(feats_cat, feats_num); gc()
        
        
        
        # here 9/30/2017 - 5:21 pm
        
        
        # don't do this here, need to do this later
        # # feature ids need to be at the "all" level
        # distinct_feats_all <- data.frame(feature_name = unique(feats_all$feature_name))
        # distinct_feats_all$feature_num <- as.numeric(as.factor(distinct_feats_all$feature_name))
        # 
        # # join the feature numeric id into the real data
        # setDT(feats_all); setDT(distinct_feats_all)
        # setkey(feats_all, feature_name); setkey(distinct_feats_all, feature_name)
        # feats_all <- merge(x=feats_all, y=distinct_feats_all, by=c("feature_name"), all.x=T, all.y=F, sort=F)
        # 
        # rm(distinct_feats_all)
        # setDF(feats_all)
        # gc()
        
        
        # save snapshot of this experiment's data so we don't have to go through all of that again
        save(y, y_test, y_train, y_holdout, feats_all, file=
            file.path(rdata_file))
        
        
}


  
  

# start experiment 002 here --------------------------

    # source in config and function defs
    source('r_scripts/GBL_zil_config.R')
    source('r_scripts/GBL_zil_function_defs.R')

    # load from checkpoint
    load(file=rdata_file)
    gc()
    
    # make sure it's still in order
    feats_all <- feats_all %>%
        arrange(id)
    
    
    # split back into groups
    x_train <- feats_all[feats_all$id %in% y_train$id, ]  
    x_test <- feats_all[feats_all$id %in% y_test$id, ]    
    x_holdout <- feats_all[feats_all$id %in% y_holdout$id, ]

    
    
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
        sapply(x_train, class)        
        
        
        # switch factors to character
        x_train$id <- as.character(x_train$id)
        x_train$feature_name <- as.character(x_train$feature_name)
        x_test$id <- as.character(x_test$id)
        x_test$feature_name <- as.character(x_test$feature_name)
        x_holdout$id <- as.character(x_holdout$id)
        x_holdout$feature_name <- as.character(x_holdout$feature_name)
        
        
    # identify intersecting features between our three datasets
    feat_intersect_1 <- dplyr::intersect(x_holdout$feature_name, x_train$feature_name)
    feat_intersect_2 <- dplyr::intersect(feat_intersect_1, x_test$feature_name)
    x_test <- x_test %>% dplyr::filter(feature_name %in% feat_intersect_2)
    x_train <- x_train %>% dplyr::filter(feature_name %in% feat_intersect_2)
    x_holdout <- x_holdout %>% dplyr::filter(feature_name %in% feat_intersect_2)
    rm(feat_intersect_1, feat_intersect_2)
    gc()
     
       
# at this point, features have been aligned across all three datsets
# use one of the datsaets to generate feature id's then map them in to each dataset
    dist_feats <- data.frame(feature_name = unique(x_holdout$feature_name)) %>%
        arrange(feature_name) %>%
        mutate(feature_num = 1:nrow(.))
    
    setDT(x_holdout); setDT(dist_feats); setkey(x_holdout, feature_name); setkey(dist_feats, feature_name)
    x_holdout <- merge(x=x_holdout, y=dist_feats, by="feature_name", all.x=T, all.y=T, sort=F)
    setDF(x_holdout)
    
    setDT(x_train); setkey(x_train, feature_name); 
    x_train <- merge(x=x_train, y=dist_feats, by="feature_name", all.x=T, all.y=F, sort=F)
    setDF(x_train)
    
    setDT(x_test); setkey(x_test, feature_name); 
    x_test <- merge(x=x_test, y=dist_feats, by="feature_name", all.x=T, all.y=F, sort=F)
    setDF(x_test)
    
    
    # let's manually merge the y value into the various "x_" datasets here
    setDT(x_train); setDT(y_train); setkey(x_train, id); setkey(y_train, id)
    x_train <- merge(x_train, y_train, by='id', all.x=T, all.y=F, sort=F)
    setDF(x_train); setDF(y_train)
    
    setDT(x_holdout); setDT(y_holdout); setkey(x_holdout, id); setkey(y_holdout, id)
    x_holdout <- merge(x_holdout, y_holdout, by='id', all.x=T, all.y=F, sort=F)
    setDF(x_holdout); setDF(y_holdout)
    
    # the above processes sorted train and holdout by id by default (setting key)
    # we need to sort test that was as well
    gc()
    x_test <- x_test %>% arrange(id)
    
    
    
        # assert that we're still in order here
        assert_that(all(y_train$id == unique(x_train$id)))
        assert_that(all(y_test$id == unique(x_test$id)))
        assert_that(all(y_holdout$id == unique(x_holdout$id)))
    
        # can we add more assertions to determine if feature/id's are all in order as well?
    
        # # one final sort to make sure
        # x_train <- x_train %>% arrange(id)
        # x_holdout <- x_holdout %>% arrange(id)
        # x_test <- x_test %>% arrange(id)
        # y_train <- y_train %>% arrange(id)
        # y_holdout <- y_holdout %>% arrange(id)
        gc()
    
        
    # generate sparse matrices
    assert_that(sum(sapply(x_train, function(x) sum(is.na(x)))) == 0)
    length(unique(x_train$feature_num))
    x_train_sp <- sparseMatrix(
        i=x_train$id_num,
        j=x_train$feature_num,
        x=x_train$value
    )
    dim(x_train_sp)
    max(x_train$feature_num) 
    
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
    
    rm(x_holdout, x_train, x_test); gc()
    
    # construct dmats
    x_dmt_train <- xgboost::xgb.DMatrix(x_train_sp, label=y_train$logerror)
    x_dmt_holdout <- xgboost::xgb.DMatrix(x_holdout_sp, label=y_holdout$logerror) 
    x_dmt_test <- xgboost::xgb.DMatrix(x_test_sp)
        
        
        
# this will likely be a grid search over a wide area of xgb parameters

    # set up params search space and run it!
    params <- list("objective" = "reg:linear", 
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
    
    
    
    