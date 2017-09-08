
# zil explore

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

GBL_PATH_TO_DATA <- '../input'


library(data.table)
library(dplyr)


# read data
prop <- fread(file.path(GBL_PATH_TO_DATA, 'properties_2016.csv'))
train <- fread(file.path(GBL_PATH_TO_DATA, 'train_2016_v2.csv'))


# Analysis of count of NA values
NAs <- sapply(prop, function(x) sum(is.na(x))) %>% data.frame()
NAs <- bind_cols(features=row.names(NAs), NAs)
names(NAs) <- c('features', 'count_NAs')
NAs <- arrange(NAs, desc(count_NAs))


# Blanks
blanks <- sapply(prop, function(x) sum(trimws(as.character(x)) == '', na.rm = T)) %>% 
    data.frame() %>% bind_cols(features=names(prop))
names(blanks) <- c('blank_values', 'features')


#' Plan # 1, use the sparse matrix stacking strategy from airbnb to 
#' bootstrap our way from heavily NA values to significantly less NA values


# Unique values per field
unq_vals <- sapply(prop, function(x) length(unique(x))) %>% data.frame() %>% bind_cols(features=names(prop), values=.)
names(unq_vals) <- c('features', 'unq_values')



prop_summary <- merge(x=NAs, y=blanks, by=c('features'), all.x=T, all.y=T)
prop_summary <- merge(x=prop_summary, y=unq_vals, by=c('features'), all.x=T, all.y=T)
prop_summary$blank_or_NA <- prop_summary$blank_values + prop_summary$count_NAs
prop_summary$perc_NA <- round(prop_summary$count_NAs / nrow(prop) * 100, 2)
prop_summary$perc_blank <- round(prop_summary$blank_values / nrow(prop) * 100, 2)
prop_summary$perc_NA_or_blank <- round((prop_summary$count_NAs + prop_summary$blank_values) / nrow(prop) * 100, 2) 


dir.create('../cache/summary')
prop_summary <- prop_summary %>% arrange(desc(perc_NA_or_blank))
write.csv(prop_summary, '../cache/summary/prop_summary.csv', row.names = F)


table(prop$bedroomcnt, prop$buildingclasstypeid) %>% data.frame() %>% arrange(desc(Freq))




# how do the id's stack up in terms of train/test
# could be the beginning of some preprocessing stuff:
ids <- data.frame(parcelid = unique(c(prop$parcelid, train$parcelid)))
ids$dataset <- 'test'
ids$dataset[ids$parcelid %in% train$parcelid] <- 'train'
ids <- ids %>% arrange(parcelid)
ids$train_cumsum <- cumsum(ids$dataset == 'train')
ids$row_id <- 1:nrow(ids)
plot(ids$row_id, y=ids$train_cumsum, pch=16)
plot(ids$parcelid, y=ids$train_cumsum, pch=16)


names(prop)
paste0(names(prop)[grepl('id', names(prop))], collapse = ', ')

coltype_id <- c(
    "airconditioningtypeid", 
    "architecturalstyletypeid", 
    "buildingclasstypeid", 
    "buildingqualitytypeid", 
    "decktypeid", 
    "heatingorsystemtypeid", 
    "pooltypeid10", 
    "pooltypeid2", 
    "pooltypeid7", 
    "propertylandusetypeid", 
    "regionidcity", 
    "regionidcounty", 
    "regionidneighborhood", 
    "regionidzip", 
    "storytypeid", 
    "typeconstructiontypeid"
)


coltype_id_in_datadict <- c(
    "heatingorsystemtypeid",
    "propertylandusetypeid",
    "storytypeid",
    "airconditioningtypeid",
    "architecturalstyletypeid",
    "typeconstructiontypeid",
    "buildingclasstypeid"
)

coltype_id_not_in_datadict <- setdiff(coltype_id, coltype_id_in_datadict)

!coltype_id_in_datadict %in% names(prop)
!coltype_id %in% names(prop)

