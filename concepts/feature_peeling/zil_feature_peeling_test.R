

# feature peeling


# source in config and function defs
source('r_scripts/GBL_zil_config.R')         # config is loading our libraries as well
source('r_scripts/GBL_zil_function_defs.R')  # function definitions


# change this on each iteration
feature_peeling_fp <- 'concepts/feature_peeling/'
feature_peeling_iteration <- '01_all_features_predictions'



# config for this script / experiment:
exp_seed <- 1776
exp_number <- "feat_peel"
rdata_file <- file.path(GBL_PATH_TO_CACHE, paste0("all_files_for_00_zil_baseline", exp_number, ".RData"))
rdata_exp_file <- file.path(GBL_PATH_TO_CACHE, paste0("experiment_files_00_zil_baseline", exp_number, ".RData"))
read_in_file <- file.path(GBL_PATH_TO_DATA, "joined_checkpoint1.rds")
exp_target <- "logerror"  # <-- this isn't hooked up to anything yet, but this is what we need to start predicting on features


# load data ---------
    list.files(GBL_PATH_TO_DATA)
    joined <- readRDS(read_in_file)
    joined <- joined[!duplicated(joined$id), ]
    assert_that(sum(duplicated(joined$id)) == 0)
    joined <- joined %>% arrange(id)


# identify train/test + holdout split -------------------------------------
    set.seed(exp_seed)
    joined <- joined %>% arrange(id)
    y <- joined[, c("id", "logerror")]
    y_test <- y[is.na(y$logerror), ]
    y_train <- y[!is.na(y$logerror), ]
    holdout_indx <- caret::createDataPartition(y=y_train$logerror, p=0.25, list=F)
    y_holdout <- y_train[holdout_indx, ]
    y_train <- y_train[-holdout_indx, ]
    rm(holdout_indx, y)
    gc()

            # design quality assertions
            assert_that(length(intersect(y_train$id, y_holdout$id)) == 0)
            assert_that(length(intersect(y_train$id, y_test$id)) == 0)
            assert_that(length(intersect(y_test$id, y_holdout$id)) == 0)


# mod1 (all data) ----------------------------------------------

            
# we have file (experiment) number, and then a model (within file) number
mod1_nbr <- "01"
mod1_longcache_fp <- paste0("cache/jlong_yids_file", exp_number, "_mod", mod1_nbr, ".RData")
mod1_dmatcache_fp <- paste0("cache/dmats_file", exp_number, "_mod", mod1_nbr, ".RData")

# # either load in cache file or build experiment files from scratch
# if(file.exists(mod1_dmatcache_fp)) {
#     print("mod1 dmatcache file exists; loading it now...")
#     load(file=mod1_dmatcache_fp)
# } else if(file.exists(mod1_longcache_fp)) {
#     print("mod1 dmatcache file doesn't exist; loading in longcache file though...")
#     load(file=mod1_longcache_fp)
#     mod1_exp <- tv_gen_exp_sparsemats(p_longdf=mod1_jlong, p_id=id, p_tr_ids=y_train, p_te_ids=y_test, p_ho_ids=y_holdout, p_target="logerror")
#     gc()
#     save(mod1_exp, file=mod1_dmatcache_fp)
# } else {
    print("mod1 didn't have either cash file, generating dmat files now...")

    joined <- joined %>% select(-tv_logreg_finsqft12)
    
    # split numeric / categorical features (features only)
    mod1_feats_name_num <- setdiff(names(joined)[sapply(joined, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
    mod1_feats_name_cat <- setdiff(names(joined)[sapply(joined, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))

    # generate long data
    mod1_jlong <- tv_gen_numcat_long(p_df=joined, p_id=id, p_numcols=mod1_feats_name_num, p_catcols=mod1_feats_name_cat)
    rm(joined)
    gc()

    # generate dmat files
    mod1_exp <- tv_gen_exp_sparsemats(p_longdf=mod1_jlong, p_id=id, p_tr_ids=y_train, p_te_ids=y_test, p_ho_ids=y_holdout, p_target="logerror")
    save(mod1_exp, file=mod1_dmatcache_fp)
    lapply(mod1_exp, dim)
    gc()
    
# }

# split exp files into separate dmats
mod1_dmat_tr <- mod1_exp$train
mod1_dmat_te <- mod1_exp$test
mod1_dmat_ho <- mod1_exp$holdout
mod1_dist_feats <- mod1_exp$features


# first run of feature_peeling will
    mod1_params <- list(
    "objective" = "reg:linear",
    "eval_metric" = "mae",
    "eta" = 0.01,
    "max_depth" = 4,  
    "subsample" = 0.9,
    "colsample_bytree" = 0.9,
    "lambda" = 0,
    "alpha" = 1,
    "max_delta_step" = 1,
    "nthread" = 6)
mod1_obj_min <- T

# run CV
set.seed(exp_seed)
mod1_cv <- xgboost::xgb.cv(
    data=mod1_dmat_tr,
    params=mod1_params,
    nrounds=10000,
    nfold=5,
    early_stopping_rounds=100,
    print_every_n = 3
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
mod1_xgb <- xgboost::xgboost(
    data=mod1_dmat_tr,
    params=mod1_params,
    nrounds=mod1_bestn_rounds,
    print_every_n=1,
    save_period = NULL
)


    # feature importance
    # dim(x_train_sp); length(unique(x_train$feature_name))
    mod1_xgb_imp <- xgboost::xgb.importance(feature_names = as.character(mod1_dist_feats$feature_name), model=mod1_xgb)
    mod1_xgb_imp[1:20, ]
    xgboost::xgb.plot.importance(mod1_xgb_imp[1:20,])
    xgboost::xgb.plot.importance(mod1_xgb_imp[21:40,])
    xgboost::xgb.plot.importance(mod1_xgb_imp[41:60,])




# predict
mod1_predictions <- predict(mod1_xgb, mod1_dmat_ho)
mod1_results <- data.frame(mod1_preds=mod1_predictions, actuals=y_holdout$logerror, id=y_holdout$id)
mod1_results$residuals <- mod1_results$mod1_preds - mod1_results$actuals
mod1_results$abs_res <- abs(mod1_results$residuals)
hist(mod1_results$residuals, col='light green', breaks=60)
hist(mod1_predictions, col='light blue', breaks=60)
hist(y_holdout$logerror, col='light blue', breaks=60)
mod1_mae <- mean(abs(mod1_results$residuals))
print(mod1_mae)
print(head(mod1_results[order(-mod1_results$abs_res), ], 20))
# print(mean(mod1_results$actuals))
# print(mean(mod1_results$mod1_preds))


    


# mod2 - categorical -----------------------------------------------------------------

#' 1)   read in joined
#' 2)   up/down sample the various values (if categorical - well you can do this with regression.. not the point)
#' 2.5) if upsampled, make a new temp id and map the existing id to the new one (functions must obtain unique "id" values)
#' 3)   split out the id/target combos into train, test will be the missing/NAs, then split a holdout off of train
#' 4)   split numeric and character columns
#' 5)   pass in relevant info to generate our long data file
#' 5)   pass that long data file in with our target/ids to generate dmatrix objects
#' 6)   run cv to determine best number of rounds to use for building actual model
#' 7)   use best nrounds from prev step to build actual model
#' 7.5) highly suggest caching the model at this point...
#' 8)   


#' ok, so this 005 / 02 model will be my first attempt at modeling a categorical variable
#' to be used as a feature itself. Ideally, we'd find one that is already fairly important
#' to logerror, hasn't been used in numeric form, and doesn't have too many missing values
#'     airconditioningtypeid
#'     regionidcity
#'     building_age_five
#'     bedroomcnt
#'     count_toilets
#'     longitude_twenty
#'     finsqft12

#' understanding what we're doing here...
#'    1) Interactively inspect and build the model and check for cheaters (and upsample / downsample where necessary)
#'    2) when satisfied, cache the model (and char vector of distinct features used)
#'    3) then we can just read in the cached model, filter data to distinct features that
#'       should be used in the model, and use it to predict on all data     


#  if we're testing / inspecting this model for results, make this flag true
# if we're ready to apply a model to the data and save the results out somewhere, make it False
mod2_test <- T

# read in joined data
joined <- readRDS(read_in_file)
joined <- joined[!duplicated(joined$id), ]
assert_that(sum(duplicated(joined$id)) == 0)
joined <- joined %>% arrange(id)

assert_that(sum(duplicated(joined$id)) == 0)

sum(is.na(joined$tv_cat_airconditioningtypeid)) / nrow(joined)  # 72% missing


# joined is arranged by id
joined$tv_cat_airconditioningtypeid %>% table()  # 742k 1, 58k 2, 8.7k 3
mod2_num_classes <- 3
# ac_id 1 downsampled, ac_id 2 and 3 are upsampled, all NA are included
set.seed(exp_seed)
mod2_joined <- bind_rows(joined %>% filter(tv_cat_airconditioningtypeid == 'tv_cat_airconditioningtypeid_1') %>% sample_n(100000),
                         joined %>% filter(tv_cat_airconditioningtypeid == 'tv_cat_airconditioningtypeid_2') %>% sample_n(100000, replace=T),
                         joined %>% filter(tv_cat_airconditioningtypeid == 'tv_cat_airconditioningtypeid_3') %>% sample_n(100000, replace=T),
                         joined %>% filter(is.na(tv_cat_airconditioningtypeid))   # %>% sample_n(300000, replace=F)  # <-- should we even limit this at all?
) %>% sample_n(nrow(.), replace=F)

mod2_joined <- mod2_joined %>% arrange(id)
mod2_joined_mapping <- data.frame(real_id = mod2_joined$id, id = 1:nrow(mod2_joined))
mod2_joined$id <- 1:nrow(mod2_joined)

assert_that(sum(duplicated(mod2_joined$id)) == 0)

set.seed(exp_seed)
mod2_y <- mod2_joined[, c("id", "tv_cat_airconditioningtypeid")]
mod2_y$target <- as.integer(as.factor(mod2_y$tv_cat_airconditioningtypeid)) - 1
table(mod2_y$target, mod2_y$tv_cat_airconditioningtypeid)
mod2_y$tv_cat_airconditioningtypeid <- NULL
mod2_y_train <- mod2_y[ !is.na(mod2_y$target), ]
mod2_y_test <- mod2_y[ is.na(mod2_y$target), ]
mod2_ho_indx <- caret::createDataPartition(mod2_y_train$target, p=0.2, list=F)
mod2_y_holdout <- mod2_y_train[mod2_ho_indx, ]
mod2_y_train <- mod2_y_train[-mod2_ho_indx, ]
rm(mod2_ho_indx)

assert_that(length(intersect(mod2_y_train$id, mod2_y_holdout$id)) == 0)
assert_that(length(intersect(mod2_y_test$id, mod2_y_train$id)) == 0)

# remove target (and any/all cheaters), heating appeared to be a cheater
# keep in mind, we don't want these models to be perfect, we want some rough edges to distinguish what's going on
mod2_remcols <- c("tv_cat_airconditioningtypeid", "logerror", "tv_cat_heatingorsystemtypeid") 
mod2_joined <- mod2_joined[, !names(mod2_joined) %in% mod2_remcols]
gc()


# we have file (experiment) number, and then a model (within file) number
mod2_nbr <- "02"
mod2_longcache_fp <- paste0("cache/jlong_yids_file", exp_number, "_mod", mod2_nbr, ".RData")
mod2_dmatcache_fp <- paste0("cache/dmats_file", exp_number, "_mod", mod2_nbr, ".RData")

# either load in cache file or build experiment files from scratch
if(file.exists(mod2_dmatcache_fp)) {
    print("mod2 dmatcache file exists; loading it now...")
    load(file=mod2_dmatcache_fp)
} else {
    print("mod2 didn't have either cash file, generating dmat files now...")
    
    # split numeric / categorical features (features only)
    mod2_feats_name_num <- setdiff(names(mod2_joined)[sapply(mod2_joined, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
    mod2_feats_name_cat <- setdiff(names(mod2_joined)[sapply(mod2_joined, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
    
    # generate long data
    gc()
    mod2_jlong <- tv_gen_numcat_long(p_df=mod2_joined, p_id=id, p_numcols=mod2_feats_name_num, p_catcols=mod2_feats_name_cat)
    rm(mod2_joined)
    gc()
    
    # generate dmat files
    mod2_exp <- tv_gen_exp_sparsemats(p_longdf=mod2_jlong, p_id=id, p_tr_ids=mod2_y_train, p_te_ids=mod2_y_test, 
                                      p_ho_ids=mod2_y_holdout, p_target="target")
    save(mod2_exp, file=mod2_dmatcache_fp)
    lapply(mod2_exp, dim)
    gc()
}

# split exp files into separate dmats
mod2_dmat_tr <- mod2_exp$train
mod2_dmat_te <- mod2_exp$test
mod2_dmat_ho <- mod2_exp$holdout
mod2_dist_feats <- mod2_exp$features


# set up params search space and run it!
# for the rapid dev/test of these first level stackers, increase the "eta"
mod2_params <- list(
    "objective" = "multi:softprob",  # softmax will pick one, softprob will report all probabilities
    "eval_metric" = "mlogloss",
    "num_class" = mod2_num_classes,
    "eta" = 0.05,  # 0.01 is way too slow
    "max_depth" = 7, 
    "subsample" = 0.6, 
    "colsample_bytree" = 0.4,
    "lambda" = 1, 
    "alpha" = 0,
    "max_delta_step" = 1,
    # scale_pos_weight     # can you use this in multi class?
    "nthread" = 4)     

# bestnrounds=2164 / cv test-mlogloss: 0.0366 / holdout mlogloss: 
# mod2_params <- list(
#     "objective" = "multi:softprob",  # softmax will pick one, softprob will report all probabilities
#     "eval_metric" = "mlogloss",
#     "num_class" = mod2_num_classes,
#     "eta" = 0.05,  # 0.01 is way to slow
#     "max_depth" = 7,                 # let's use lower complexity 1st round models for quicker models
#     "subsample" = 0.6, 
#     "colsample_bytree" = 0.4,
#     "lambda" = 1, 
#     "alpha" = 0,
#     "max_delta_step" = 1,
#     # scale_pos_weight     # can you use this in multi class?
#     "nthread" = 4)     


mod2_obj_min <- T

# run CV
set.seed(exp_seed)
mod2_cv <- xgboost::xgb.cv(
    data=mod2_dmat_tr,
    params=mod2_params,
    nrounds=10000,
    nfold=5,
    early_stopping_rounds=50
)


# identify best number of rounds
mod2_eval_log <- data.frame(mod2_cv$evaluation_log)
mod2_metric <- names(mod2_eval_log)[grepl("^test_", names(mod2_eval_log)) & grepl("_mean$", names(mod2_eval_log))]
if(mod2_obj_min) {
    mod2_bestn_rounds <- which.min(mod2_eval_log[, mod2_metric])
} else {
    mod2_bestn_rounds <- which.max(mod2_eval_log[, mod2_metric])
}


# run the real model
mod2_xgb <- xgboost::xgboost(
    data=mod2_dmat_tr,
    params=mod2_params,
    
    # nrounds=mod2_bestn_rounds,
    nrounds=2164,
    
    print_every_n=1,
    save_period = NULL
)


# feature importance
# dim(x_train_sp); length(unique(x_train$feature_name))
mod2_xgb_imp <- xgboost::xgb.importance(feature_names = as.character(mod2_dist_feats$feature_name), model=mod2_xgb)
xgboost::xgb.plot.importance(mod2_xgb_imp[1:20,])
xgboost::xgb.plot.importance(mod2_xgb_imp[21:40,])


# make predictions (this is generally kinda weird with multiple classes)
mod2_predictions <- as.data.frame(matrix(predict(mod2_xgb, mod2_dmat_ho), ncol=mod2_num_classes, byrow=T))
class(mod2_predictions)
length(mod2_predictions); dim(mod2_dmat_ho)

# things to look for, are we just predicting it to be the most common class?
# are we just giving every record the same score arbitrarily?
hist(mod2_predictions$V1)
hist(mod2_predictions$V2)
hist(mod2_predictions$V3)
mod2_predictions_combo <- cbind(mod2_y_holdout, mod2_predictions)    


# make predictions on test
mod2_test_preds <- as.data.frame(matrix(predict(mod2_xgb, mod2_dmat_te), ncol=mod2_num_classes, byrow=T))
hist(mod2_test_preds$V1, col='light blue', breaks=40)
hist(mod2_test_preds$V2, col='light blue', breaks=40)
hist(mod2_test_preds$V3, col='light blue', breaks=40)



#' ok, damn. Now I get it. I've built my functions too large and can't use the
#' pieces of them because they're too intertwined. 


# submission generation --------------------------------------------------------------------------

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


