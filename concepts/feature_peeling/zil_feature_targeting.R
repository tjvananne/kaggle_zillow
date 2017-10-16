

# feature targeting:
#' setting a feature (ideally an important one) as the temporary target variable. predicting values 
#' for that feature, and then using the predicted values as features for the real target variable
#' of the overall project.


    # manual reset:
    rm(list=setdiff(ls(), "joined"))

    
# source in config and function defs
source('r_scripts/GBL_zil_config.R')         # config is loading our libraries as well
source('r_scripts/GBL_zil_function_defs.R')  # function definitions

# list.files('input')
joined <- readRDS('input/joined_checkpoint1.rds')
joined <- joined[!duplicated(joined$id), ]
joined <- joined %>% arrange(id)



# split up train (target not NA), test (target is NA), holdout (a portion split off of train)
exp_seed <- 1776
set.seed(exp_seed)
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


# split numeric / categorical features (features only) and generate long data
mod1_feats_name_num <- setdiff(names(joined)[sapply(joined, class) %in% c("numeric", "integer")], c("id", "logerror", "transactiondate"))
mod1_feats_name_cat <- setdiff(names(joined)[sapply(joined, class) %in% c("character", "factor")], c("id", "logerror", "transactiondate"))
mod1_jlong <- tv_gen_numcat_long(p_df=joined, p_id=id, p_numcols=mod1_feats_name_num, p_catcols=mod1_feats_name_cat)
gc()


# generate sparse matrices
mod1_exp <- tv_gen_exp_sparsemats(p_longdf=mod1_jlong, p_id=id, p_tr_ids=y_train, p_te_ids=y_test, p_ho_ids=y_holdout, p_target="logerror")
lapply(mod1_exp, dim)
gc()


# split out what gets returned
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


# importance
mod1_xgb_imp <- xgboost::xgb.importance(feature_names = as.character(mod1_dist_feats$feature_name), model=mod1_xgb)
mod1_xgb_imp[1:20, ]



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





# mod1 begin the targeting... starting with the most important feature:
# tv_logreg_finsqft12
# so this one doesn't have many NA values, let's see if this helps much...
mod2_target <- "tv_logreg_finsqft12"
mod2_y <- joined[, c("id", "tv_logreg_finsqft12")]
mod2_y_train <- mod2_y[!is.na(mod2_y$tv_logreg_finsqft12), ]
mod2_y_test <- mod2_y[is.na(mod2_y$tv_logreg_finsqft12), ]
mod2_remcols <- c("id", "logerror", "transactiondate", "tv_logreg_finsqft12", "tv_rawreg_finsqft12")

mod2_feats_name_num <- setdiff(names(joined)[sapply(joined, class) %in% c("numeric", "integer")], mod2_remcols)
mod2_feats_name_cat <- setdiff(names(joined)[sapply(joined, class) %in% c("character", "factor")], mod2_remcols)

mod2_jlong <- tv_gen_numcat_long(p_df=joined, p_id=id, p_numcols=mod2_feats_name_num, p_catcols=mod2_feats_name_cat)
gc()

# generate sparse matrices
mod2_exp <- tv_gen_exp_sparsemats(p_longdf=mod2_jlong, p_id=id, p_tr_ids=mod2_y_train, p_te_ids=mod2_y_test, p_target=mod2_target)
lapply(mod2_exp, dim)
gc()


# split out what gets returned
mod2_dmat_tr <- mod2_exp$train
mod2_dmat_te <- mod2_exp$test
# mod2_dmat_ho <- mod2_exp$holdout
mod2_dist_feats <- mod2_exp$features



# first run of feature_peeling will
mod2_params <- list(
    "objective" = "reg:linear",
    "eval_metric" = "mae",
    "eta" = 0.1,  # <-- because this data set is huge
    "max_depth" = 6,  
    "subsample" = 0.3,
    "colsample_bytree" = 0.25,
    "lambda" = 0,
    "alpha" = 1,
    "max_delta_step" = 1,
    "nthread" = 6)
mod2_obj_min <- T


mod2_cv <- xgb.cv(
    nfold=5,
    data=mod2_dmat_tr,
    params=mod2_params,
    nrounds=10000,
    early_stopping_rounds = 100
)


