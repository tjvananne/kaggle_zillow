

# keep the structure, but rename this script to be very exploratory

# VERY next steps:
# - rename all the features so names are more manageable
# - 



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# "C:/Users/tvananne/Documents/personal/github/kaggles/zillow_zestimate/r_scripts"



source('GBL_zil_config.R')
source('GBL_zil_function_defs.R')



# read data
prop <- fread(file.path(GBL_PATH_TO_DATA, 'properties_2016.csv'))
train <- fread(file.path(GBL_PATH_TO_DATA, 'train_2016_v2.csv'))
zzzsampsub <- fread(file.path(GBL_PATH_TO_DATA, 'sample_submission.csv'))

    # make sure they are DFs
    setDF(prop)
    setDF(train)
    setDF(zzzsampsub)

    
# summary
prop_summary <- read.csv('../cache/summary/prop_summary.csv')


# adhoc stuff that will later be in it's own script once I figure out what it should be...
prop$parcelid <- paste0("pid_", as.character(prop$parcelid))
train$parcelid <- paste0("pid_", as.character(train$parcelid))


# use this for everything -- remove logerror / transactiondate for all feature stacking
joined <- merge(x=train, y=prop, by='parcelid', all.x=T, all.y=T, sort=F)


    # one by one feature analysis:
    # look at everything except for things with "id" in the name -- those we'll look at on their own

    #' might want to come back and revisit all of these and flag all of the records where they are
    #' in the top 1 or 0.5 % or something. Some of these right-hand tails are absurd... very skewed data
    #' Maybe we should also look into log transformations AND categorizing data AFTER log transformations


        names_not_procd <- function(para_df, para_col_prefix_exclude) {
            names_w_pref <- names(para_df)[grepl(para_col_prefix_exclude, names(para_df))]
            names_wo_pref <- names(para_df)[!grepl(para_col_prefix_exclude, names(para_df))]
            return(names_wo_pref)
        }


        # basementsqft is integer, that is fine
        # create 5-class category
        joined$tv_cat_basementsqft <- cut2(joined$basementsqft, g=5)
        levels(joined$tv_cat_basementsqft)  # for manual inspection
        joined$tv_cat_basementsqft <- as.numeric(joined$tv_cat_basementsqft)
        joined$tv_cat_basementsqft <- paste0("tv_cat_basementsqft_", as.character(joined$tv_cat_basementsqft))
        table(joined$tv_cat_basementsqft)
        # rename numeric field
        joined$tv_reg_basementsqft <- joined$basementsqft
        # remove original
        joined$basementsqft <- NULL
        
        
        
        
        # bathroomcnt
        # create "has_halfbath" feature
        joined$tv_cat_has_halfbath <- as.integer(joined$bathroomcnt %% 1 == 0.5)
        joined$tv_cat_has_halfbath <- paste0("tv_cat_has_halfbath_", as.character(joined$tv_cat_has_halfbath))
        # create "toilets_count" categorical feature
        joined$tv_cat_count_toilets <- cut2(ceiling(joined$bathroomcnt), g=5)
        levels(joined$tv_cat_count_toilets); table(joined$tv_cat_count_toilets)
        joined$tv_cat_count_toilets <- as.numeric(joined$tv_cat_count_toilets)
        joined$tv_cat_count_toilets <- paste0("tv_cat_count_toilets_", as.character(joined$tv_cat_count_toilets))
        table(joined$tv_cat_count_toilets)
        # create bathroomcnt regression feature
        joined$tv_reg_bathroomcnt <- joined$bathroomcnt
        # remove original
        joined$bathroomcnt <- NULL
        
        
        
        # bedroomcnt
        quantile(joined$bedroomcnt, seq(0, 1, .10), na.rm=T)
        # create binned categorical bedroom count
        joined$tv_cat_bedroomcnt <- cut2(joined$bedroomcnt, g=9)
        levels(joined$tv_cat_bedroomcnt); table(joined$tv_cat_bedroomcnt)
        joined$tv_cat_bedroomcnt <- as.numeric(joined$tv_cat_bedroomcnt)
        joined$tv_cat_bedroomcnt <- paste0("tv_cat_bedroomcnt_", joined$tv_cat_bedroomcnt)
        table(joined$tv_cat_bedroomcnt)
        # create regression bedroom count
        joined$tv_reg_bedroomcnt <- joined$bedroomcnt
        # remove original
        joined$bedroomcnt <- NULL
        
        
        
        # calulatedbathnbr -- more missing values than other bathrm number ignore for now
        class(joined$calculatedbathnbr)
        quantile(joined$calculatedbathnbr, seq(0, 1, 0.1), na.rm=T)
        # I DON'T THINK THIS GIVES US ANY ADDITIONAL INFO, GOING TO REMOVE THIS FIELD
        joined$calculatedbathnbr <- NULL
        
        
        
        
        # finishedfloor1squarefeet -- this one has some serious detail, lets make two cats and one reg
        quantile(joined$finishedfloor1squarefeet, seq(0, 1, 0.10), na.rm=T)
        # five categories
        joined$tv_cat_finflr1_five <- cut2(joined$finishedfloor1squarefeet, g=5)
        levels(joined$tv_cat_finflr1_five); table(joined$tv_cat_finflr1_five)
        joined$tv_cat_finflr1_five <- as.numeric(joined$tv_cat_finflr1_five)
        joined$tv_cat_finflr1_five <- paste0("tv_cat_finflr1_five_", as.character(joined$tv_cat_finflr1_five))
        table(joined$tv_cat_finflr1_five)  # 40k in each of the five bins, nice!
        # ten categories
        joined$tv_cat_finflr1_ten <- cut2(joined$finishedfloor1squarefeet, g=10)
        levels(joined$tv_cat_finflr1_ten); table(joined$tv_cat_finflr1_ten)
        joined$tv_cat_finflr1_ten <- as.numeric(joined$tv_cat_finflr1_ten)
        joined$tv_cat_finflr1_ten <- paste0("tv_cat_finflr1_ten_", as.character(joined$tv_cat_finflr1_ten))
        table(joined$tv_cat_finflr1_ten)
        # regression
        joined$tv_reg_finflr1 <- joined$finishedfloor1squarefeet
        joined$finishedfloor1squarefeet <- NULL
        
        
        # calculatedfinishedsquarefeet
        quantile(joined$calculatedfinishedsquarefeet, seq(0, 1, .10), na.rm=T)
        
        
        
        # calculatedfinishedsquarefeet -- sqr ft of all finished area in home
        # 1) regression
        quantile(joined$calculatedfinishedsquarefeet, seq(0, 1, 0.01), na.rm=T)
        
        
    
    
# isolate the ids and what dataset they came from
ids <- data.frame(parcelid = unique(c(prop$parcelid, train$parcelid)))
ids$dataset <- 'test'
ids$dataset[ids$parcelid %in% train$parcelid] <- 'train'
ids <- ids %>% arrange(parcelid)





names(prop)
paste0(names(prop)[grepl('id', names(prop))], collapse = ', ')



coltype_id <- c(
    "airconditioningtypeid", "architecturalstyletypeid", "buildingclasstypeid", "buildingqualitytypeid", 
    "decktypeid", "heatingorsystemtypeid", "pooltypeid10", "pooltypeid2", "pooltypeid7", 
    "propertylandusetypeid", "regionidcity", "regionidcounty", "regionidneighborhood", 
    "regionidzip", "storytypeid", "typeconstructiontypeid"
)




coltype_id_in_datadict <- c(
    # with intensive research, we make ordinal copies of some of these (heating, least to most advanced?)
    "heatingorsystemtypeid",  # <-- 13 "None"  (76 in train, 1266 in test)
    "propertylandusetypeid",  
    "storytypeid",
    "airconditioningtypeid",  # <-- 5 "None"   (215 in train, 8580 in test)
    "architecturalstyletypeid",
    "typeconstructiontypeid",
    "buildingclasstypeid"
)




(paste0(setdiff(coltype_id, coltype_id_in_datadict), collapse=', '))

coltype_id_not_in_datadict <- c(
    
    # should be numeric
    "buildingqualitytypeid", # 1 is best, 12 is worst (treat as numerical)
    
    # should be categorical
    "decktypeid",            # NA or 66, let's treat 66 as a TRUE or something? boolean?
    "pooltypeid10",          # NA or 1, a one means spa or hottub
    "pooltypeid2",           # NA or 1, a one means pool with spa or hottub
    "pooltypeid7",           # NA or 1, a one means pool without spa or hottub
    "regionidcounty",        # 4 unique county values (including missing) -- categorical
    
    # still categorical, may require binning of some sort:
    "regionidcity",          # 187 unique city values -- categorical (any relation to Lat/Long?)
    "regionidneighborhood",  # 529 unique values -- categorical (any relation to Lat/Long?)
    "regionidzip"            # 406 unique values -- categorical
)

# overlap?
assert_that(!any(duplicated(c(coltype_id_in_datadict, coltype_id_not_in_datadict))))

# any missing?
assert_that(!any(!coltype_id %in% (c(coltype_id_in_datadict, coltype_id_not_in_datadict))))


# are the pool ids mutually exclusive? or do they add to each other?
pools <- unique(prop[, c('pooltypeid10', 'pooltypeid2', 'pooltypeid7')])
print(pools)



    # yes, mutually exclusive
    # pooltypeid10 pooltypeid2 pooltypeid7
    # 1               NA          NA          NA
    # 111             NA          NA           1
    # 1341             1          NA          NA
    # 10337           NA           1          NA





