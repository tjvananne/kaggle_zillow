

# baseline joined data so far


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# "C:/Users/tvananne/Documents/personal/github/kaggles/zillow_zestimate/r_scripts"


# source in config and function defs
source('GBL_zil_config.R')
source('GBL_zil_function_defs.R')


list.files(GBL_PATH_TO_DATA)

joined <- readRDS(file.path(GBL_PATH_TO_DATA, "joined2.rds"))


# first baseline will ignore transactiondates
joined_sub <- joined[, names(joined)[names(joined) %in% c("id", "logerror") | grepl("tv_", names(joined))]]
rm(joined); gc()


# identify train/test + holdout
set.seed(1776)
y <- joined_sub[, c("id", "logerror")]
y_test <- y[is.na(y$logerror), ]
y_train <- y[!is.na(y$logerror), ]
y_holdout <- y_train[sample(1:nrow(y_train), size = ceiling(0.15 * nrow(y_train))), ]
y_train <- y_train[!y_train$id %in% y_holdout$id, ]

    assert_that(!any(y_train$id %in% y_holdout$id))


feats_name_num <- setdiff(names(joined_sub)[sapply(joined_sub, class) %in% c("numeric", "integer")], c("id", "logerror"))
feats_name_cat <- setdiff(names(joined_sub)[sapply(joined_sub, class) %in% c("character", "factor")], c("id", "logerror"))

joined_num <- joined_sub[, feats_name_num]
joined_cat <- joined_sub[, feats_name_cat]


    # normalize numeric variables
    myproc_scaler_center <- caret::preProcess(joined_num, method=c("scale", "center"))
    myproc_range <- caret::preProcess(joined_num, method=c("range"))
    
    joined_num_scale_center <- cbind(id=joined$id, predict(myproc_scaler_center, joined_num))
    joined_num_rng <- cbind(id=joined$id, predict(myproc_range, joined_num))
    joined_num <- cbind(id=joined$id, joined_num)
    
    
# gather all numeric features into feature_name / column format
feats_num <- tidyr::gather(joined_num_scale_center[, c(feats_name_num, "id")], key=feature_name, value=value, -id) %>%
    arrange(id)




# writing loop to handle ALL feature / categoricals
feats_cat <- data.frame()

for(feat in feats_name_cat) {
    
    print(feat)
    # isolate this feat
    df_feats_ <- joined_sub[, c("id", feat)]
    # assign(x = paste0("pos_values_", feat), value = as.character(unique(df_feats_[, 2])))
    df_feats_[, 2] <- paste0(feat, "_", as.character(df_feats_[, 2]))
    df_feats_$value <- 1
    names(df_feats_) <- c("id", "feature_name", "value")
    feats_cat <- bind_rows(feats_cat, df_feats_)
    
}


rm(joined, df_feats_, joined_cat, joined_num, joined_num_rng, joined_num_scale_center, joined_sub)
gc()

feats_all <- bind_rows(feats_num, feats_cat)
rm(feats_cat, feats_num); gc()



# I want lowest CV, holdout score, and submission

# this will likely be a grid search over a wide area of xgb parameters







