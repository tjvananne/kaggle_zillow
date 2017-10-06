


# almost exact same as 002, but we're going to try using dense matrices. sparse is greate for
# keeping a low RAM profile, but it's much more difficult to see what the model is doing.
# KISS - keep it simple, stupid.


# source in config and function defs
source('r_scripts/GBL_zil_config.R')         # config is loading our libraries as well
source('r_scripts/GBL_zil_function_defs.R')  # function definitions


# config for this script / experiment:
this_seed <- 1776
exp_number <- "003"
rdata_file <- file.path(GBL_PATH_TO_CACHE, paste0("all_files_for_00_zil_baseline", exp_number, ".RData"))
read_in_file <- file.path(GBL_PATH_TO_DATA, "joined_checkpoint1.rds")



# THIS WAS A CHECKPOINT from the preprocessing file
list.files(GBL_PATH_TO_DATA)
joined <- readRDS(read_in_file)
joined <- joined[!duplicated(joined$id), ]
assert_that(sum(duplicated(joined$id)) == 0)

    
# dummies ----------------------------------------------------------------

    # heavy dev in here right now. much of this needs to move to function defs.

    
    # take in character vector which is supposed to be factor, give back onehot encoding of the categories in data.frame
    char_vec_to_dummy <- function(p_character_vector, p_colname, na.rm=F) {
        # this function will convert a character vector to a dummy variable data.frame
        if(is.factor(p_character_vector)) {p_character_vector <- as.character(p_character_vector)}
        if(!grepl("_$", p_colname)) {p_colname <- paste0(p_colname, "_")}
        p_character_vector[is.na(p_character_vector)] <- "__NA"
        modmat_df <- data.frame(model.matrix(~p_character_vector + 0))
        names(modmat_df) <- gsub("p_character_vector", p_colname, names(modmat_df))
        if(na.rm) {
            modmat_df <- modmat_df[, !grepl("__NA$", names(modmat_df))]
        }
        return(modmat_df)
    }
    
    sapply(joined, class)


    # for each column that starts with "tv_cat_", convert it to one hot encodings and append it to a list
    # then turn that list into a data.frame so we can concat it to the main table
    cols_to_dummy <- names(joined)[grepl("^tv_cat_", names(joined))]
    len_dumcols <- length(cols_to_dummy)
    ohe_list <- vector(mode='list', length = len_dumcols)
    
    ohe_df <- data.frame()
    for(i in 1:len_dumcols) {
        print(i)
        ohe_df <- bind_rows(ohe_df, char_vec_to_dummy(p_character_vector = joined[, cols_to_dummy[i], drop=T],
                                           p_colname = paste0(cols_to_dummy[i], "_ohe")))
    }
    
    
    
    
    
    
    myvec <- c("yes", "no", "yes", "no", NA, NA, "maybe")    
    
    x <- data.frame(char_vec_to_dummy(myvec, "myvec", na.rm=T))
    x
    
    
    myvec2 <- myvec
    myvec2[is.na(myvec2)] <- "__NA"
    myfac <- as.factor(myvec2)
    myfac[is.na(myfac)] <- "__NA"
    
    is.character(myfac)
    
    myvec_df <- model.matrix(~myfac + 0)
    
        
        
    
# identify train/test + holdout split -------------------------------------
    set.seed(this_seed)
    dat_tr <- joined[!is.na(joined$logerror), ]
    dat_te <- joined[is.na(joined$logerror), ]
    indx_ho <- caret::createDataPartition(y=dat_tr$logerror, p=0.15, list=F)
    dat_ho <- dat_tr[indx_ho, ]    
    dat_tr <- dat_tr[-indx_ho, ]    
    
        # assertions for our machine learning design
        assert_that(length(intersect(dat_tr$id, dat_te$id)) == 0)
        assert_that(length(intersect(dat_tr$id, dat_ho$id)) == 0)
  
    
# various dmat creations (do multiple and ensemble them) ------------------
        
    # this is the "stock" dmat
    exp1_remcols <- c("id", "logerror", "transactiondate")
        assert_that(all(exp1_remcols %in% names(dat_tr)))
    
    exp1_mat_tr <- dat_tr[, !names(dat_tr) %in% exp1_remcols]       
    exp1_dmat_tr <- xgb.DMatrix(matrix(dat_tr[, !names(dat_tr) %in% exp1_remcols]), label = dat_tr$logerror)
            
        

        
        
        
        
# old below this line -------------------------
            
            
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
        x_test <- x_test %>% arrange(id)
        
        head(x_holdout); head(x_train)
        
        
            # assert that we're still in order here
            assert_that(all(y_train$id == unique(x_train$id)))
            assert_that(all(y_test$id == unique(x_test$id)))
            assert_that(all(y_holdout$id == unique(x_holdout$id)))
        
        
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
        
        
        # construct dmats
        x_dmt_train <- xgboost::xgb.DMatrix(x_train_sp, label=y_train$logerror)
        x_dmt_holdout <- xgboost::xgb.DMatrix(x_holdout_sp, label=y_holdout$logerror) 
        x_dmt_test <- xgboost::xgb.DMatrix(x_test_sp)
            
            
            # what is going on
            length(unique(x_train$feature_name))
            length(unique(x_train$id))
            length(unique(x_train$feature_num))
            class(x_train$feature_name)
            dim(x_train_sp)    
            sapply(x_train, function(x) sum(is.na(x)))
            sapply(x_train, function(x) sum(is.null(x)))
            sapply(x_train, function(x) sum(is.infinite(x)))
            attributes(x_dmt_train)
            
            
            dim(x_train_sp)
            dim(x_test_sp)
            dim(x_holdout_sp)
            
            length(unique(x_test$feature_name))
            length(unique(x_holdout$feature_name))
            
            x_inspect <- x_train %>%
                group_by(id, feature_name) %>%
                summarise(this_feat_same_id = n())
                
            head(x_inspect)
            x_inspect2 <- x_inspect %>%
                arrange(desc(this_feat_same_id))
        
            head(x_inspect2)
            max(x_inspect2$this_feat_same_id)        
            
            
    # this will likely be a grid search over a wide area of xgb parameters
    
        # set up params search space and run it!
        params <- list("objective" = "reg:linear", 
                       "eval_metric" = "mae",
                       "eta" = 0.01, 
                       "max_depth" = 7, 
                       "subsample" = 0.8, 
                       "colsample_bytree" = 0.4,
                       "lambda" = 0, 
                       "alpha" = 1, 
                       "nthread" = 6)     
        
        # run CV
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
            dim(x_train_sp); length(unique(x_train$feature_name))
            this_xgb_imp <- xgboost::xgb.importance(unique(as.character(x_train$feature_name)), model=this_xgb)
            xgboost::xgb.plot.importance(this_xgb_imp[1:20,])
            
            
            # from joined
            ggplot(data=joined %>% filter(id %in% y_train$id), aes(x=tv_cat_poolsize_ten, y=logerror)) +
                geom_boxplot() +
                coord_flip()
            
            cat_poolsize_ten_8 <- x_train %>%
                filter(feature_name == "tv_cat_poolsize_ten_8")
            
            
            
        
        # predict
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
        
        
        
        