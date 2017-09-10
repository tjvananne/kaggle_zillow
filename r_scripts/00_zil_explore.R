

# keep the structure, but rename this script to be very exploratory

# VERY next steps:
# - rename all the features so names are more manageable
# - 



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
# "C:/Users/tvananne/Documents/personal/github/kaggles/zillow_zestimate/r_scripts"


# source in config and function defs
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


# add prefix to ids
prop$parcelid <- paste0("pid_", as.character(prop$parcelid))
train$parcelid <- paste0("pid_", as.character(train$parcelid))


# use this for everything -- remove logerror / transactiondate for all feature stacking
joined <- merge(x=train, y=prop, by='parcelid', all.x=T, all.y=T, sort=F)

    # reset
    joined_filepath <- file.path(GBL_PATH_TO_DATA, 'joined.rds')
    if(file.exists(joined_filepath)) {
        joined <- readRDS(joined_filepath)
    } else {
        saveRDS(joined, joined_filepath)  # <-- run 
    }; rm(joined_filepath)
    
    # one by one feature analysis:
    # look at everything except for things with "id" in the name -- those we'll look at on their own

    #' might want to come back and revisit all of these and flag all of the records where they are
    #' in the top 1 or 0.5 % or something. Some of these right-hand tails are absurd... very skewed data
    #' Maybe we should also look into log transformations AND categorizing data AFTER log transformations


        
        
        
        # basementsqft is integer, that is fine
            # this is great... taking the log helps normalize quite a bit
            # hist(joined$basementsqft, col='light blue', breaks=30)
            # hist(log(joined$basementsqft), col='light blue', breaks=30)
        # create 5-class category; rename numeric; log the original; remove original
        joined$tv_cat_basementsqft <- cut2_rename(joined$basementsqft, 5, "tv_cat_basementsqft")
        joined$tv_reg_basementsqft <- joined$basementsqft
        joined$tv_logreg_basementsqft <- log(joined$basementsqft)
        joined$basementsqft <- NULL
        
        
        
        # bathroomcnt
            hist(joined$bathroomcnt, col='light blue')
            hist(log(joined$bathroomcnt + 1), col='light blue')
        # create "has_halfbath" feature
        joined$tv_cat_has_halfbath <- as.integer(joined$bathroomcnt %% 1 == 0.5)
        joined$tv_cat_has_halfbath <- paste0("tv_cat_has_halfbath_", as.character(joined$tv_cat_has_halfbath))
        joined$tv_cat_has_halfbath[grepl("_NA$", joined$tv_cat_has_halfbath)] <- NA
        sum(is.na(joined$tv_cat_has_halfbath))
        # 5-cat; rename numeric; log original; remove original
        # joined$tv_cat_count_toilets <- cut2_rename(joined$bathroomcnt, 5, "tv_cat_count_toilets")
        joined$tv_cat_count_toilets <- cut2(ceiling(joined$bathroomcnt), cuts = c(
            min(ceiling(joined$bathroomcnt), na.rm=T), 1, 2, 3, 4, 5, max(ceiling(joined$bathroomcnt), na.rm=T)))
        joined$tv_cat_count_toilets <- as.numeric(joined$tv_cat_count_toilets)
        joined$tv_cat_count_toilets <- paste0("tv_cat_count_toilets_", as.character(joined$tv_cat_count_toilets))
        joined$tv_cat_count_toilets[grepl("_NA$", joined$tv_cat_count_toilets)] <- NA
            sum(is.na(joined$tv_cat_count_toilets))
            table(joined$tv_cat_count_toilets)
        joined$tv_reg_bathroomcnt <- joined$bathroomcnt
        joined$tv_logreg_bathroomcnt <- log(joined$bathroomcnt)
        joined$bathroomcnt <- NULL
        
        
        
        # bedroomcnt
            hist(joined$bedroomcnt, col='light blue')
            hist(log(joined$bedroomcnt + 1), col='light blue')
        # create binned categorical bedroom count
        # joined$tv_cat_bedroomcnt <- cut2_rename(joined$bedroomcnt, 5, "tv_cat_bedroomcnt")
        joined$tv_cat_bedroomcnt <- cut2(joined$bedroomcnt, cuts = c(
            min(joined$bedroomcnt, na.rm=T), 1, 2, 3, 4, 5, max(joined$bedroomcnt, na.rm=T)))
        joined$tv_cat_bedroomcnt <- as.numeric(joined$tv_cat_bedroomcnt)
        joined$tv_cat_bedroomcnt <- paste0("tv_cat_bedroomcnt_", as.character(joined$tv_cat_bedroomcnt))
        joined$tv_cat_bedroomcnt[grepl("_NA$", joined$tv_cat_bedroomcnt)] <- NA
            sum(is.na(joined$tv_cat_bedroomcnt))
            table(joined$tv_cat_bedroomcnt)
        joined$tv_reg_bedroomcnt <- joined$bedroomcnt
        joined$tv_logreg_bedroomcnt <- log(joined$bedroomcnt + 1)
        joined$bedroomcnt <- NULL
        
        
        
        # calulatedbathnbr -- more missing values than other bathrm number ignore for now
        # I DON'T THINK THIS GIVES US ANY ADDITIONAL INFO, GOING TO REMOVE THIS FIELD
        joined$calculatedbathnbr <- NULL
        
        
        
        
        # finishedfloor1squarefeet -- this one has some serious detail, lets make two cats and one reg
            hist(joined$finishedfloor1squarefeet, col='light blue')
            hist(log(joined$finishedfloor1squarefeet + 500), col='light blue')  # 500 shift to improve normalcy
        # make features
        joined$tv_cat_finflr1_five <- cut2_rename(joined$finishedfloor1squarefeet, 5, "tv_cat_finflr1_five")
        joined$tv_cat_finflr1_ten <- cut2_rename(joined$finishedfloor1squarefeet, 10, "tv_cat_finflr1_ten")    
        joined$tv_reg_finflr1 <- joined$finishedfloor1squarefeet
        joined$tv_logreg_finflr1 <- log(joined$finishedfloor1squarefeet + 500)
        joined$finishedfloor1squarefeet <- NULL
        
        
        # calculatedfinishedsquarefeet
            hist(joined$calculatedfinishedsquarefeet, col='light blue')
            hist(log(joined$calculatedfinishedsquarefeet), col='light blue')        
        # make features
        joined$tv_cat_totsqft_five <- cut2_rename(joined$calculatedfinishedsquarefeet, 5, "tv_cat_totsqft_five")
        joined$tv_cat_totsqft_ten <- cut2_rename(joined$calculatedfinishedsquarefeet, 10, "tv_cat_totsqft_ten")        
        joined$tv_reg_totsqft <- joined$calculatedfinishedsquarefeet
        joined$tv_logreg_totsqft <- log(joined$calculatedfinishedsquarefeet)
        joined$calculatedfinishedsquarefeet <- NULL
        
        
        # finishedsquarefeet12
            hist(joined$finishedsquarefeet12, col='light blue')
            hist(log(joined$finishedsquarefeet12), col='light blue')
        # make features
        joined$tv_cat_finsqft12_five <- cut2_rename(joined$finishedsquarefeet12, 5, "tv_cat_finsqft12_five")
        joined$tv_cat_finsqft12_ten <- cut2_rename(joined$finishedsquarefeet12, 10, "tv_cat_finsqft12_ten")
        joined$tv_reg_finsqft12 <- joined$finishedsquarefeet12    
        joined$tv_logreg_finsqft12 <- log(joined$finishedsquarefeet12)
        joined$finishedsquarefeet12 <- NULL
        
        
        # finishedsquarefeet13
            hist(joined$finishedsquarefeet13, col='light blue')
            # hist(log(joined$finishedsquarefeet13), col='light blue')  # <-- don't make log
        # make features
        joined$tv_cat_finsqft13_five <- cut2_rename(joined$finishedsquarefeet13, 5, "tv_cat_finsqft13_five")
        joined$tv_cat_finsqft13_ten <- cut2_rename(joined$finishedsquarefeet13, 10, "tv_cat_finsqft13_ten")
        joined$tv_reg_finsqft13 <- joined$finishedsquarefeet13    
        joined$finishedsquarefeet13 <- NULL    
        
        
        # finishedsquarefeet15
            hist(joined$finishedsquarefeet15, col='light blue')
            hist(log(joined$finishedsquarefeet15), col='light blue')
        # make features
        joined$tv_cat_finsqft15_five <- cut2_rename(joined$finishedsquarefeet15, 5, "tv_cat_finsqft15_five")
        joined$tv_cat_finsqft15_ten <- cut2_rename(joined$finishedsquarefeet15, 10, "tv_cat_finsqft15_ten")    
        joined$tv_reg_finsqft15 <- joined$finishedsquarefeet15
        joined$tv_logreg_finsqft15 <- log(joined$finishedsquarefeet15)
        joined$finishedsquarefeet15 <- NULL
        
        # finishedsquarefeet50
            hist(joined$finishedsquarefeet50, col='light blue')
            hist(log(joined$finishedsquarefeet50 + 250), col='light blue')
        # make features
        joined$tv_cat_finsqft50_five <- cut2_rename(joined$finishedsquarefeet50, 5, "tv_cat_finsqft50_five")
        joined$tv_cat_finsqft50_ten <- cut2_rename(joined$finishedsquarefeet50, 10, "tv_cat_finsqft50_ten")
        joined$tv_reg_finsqft50 <- joined$finishedsquarefeet50
        joined$tv_logreg_finsqft50 <- log(joined$finishedsquarefeet50 + 250)
        joined$finishedsquarefeet50 <- NULL
        
        
        
        # finishedsquarefeet6
            hist(joined$finishedsquarefeet6, col='light blue')
            hist(log(joined$finishedsquarefeet6), col='light blue', breaks=80)
        # make features
        joined$tv_cat_finsqft6_five <- cut2_rename(joined$finishedsquarefeet6, 5, "tv_cat_finsqft6_five")
        joined$tv_cat_finsqft6_ten <- cut2_rename(joined$finishedsquarefeet6, 10, "tv_cat_finsqft6_ten")    
        joined$tv_reg_finsqft6 <- joined$finishedsquarefeet6
        joined$tv_logreg_finsqft6 <- log(joined$finishedsquarefeet6)
        joined$finishedsquarefeet6 <- NULL
            
        
        
        # fips -- categorical location data I believe
        unique(joined$fips)
        joined$tv_cat_fips <- paste0("fips_", as.character(joined$fips))
        joined$tv_cat_fips[joined$tv_cat_fips == "fips_NA"] <- NA
            table(joined$tv_cat_fips); sum(is.na(joined$tv_cat_fips))
            sum(is.na(joined$tv_cat_fips))
        joined$fips <- NULL
        
        
        # fireplacecnt
        # min is 1, should we assume NAs are zeros? I don't think we can make that assumption? 
            hist(joined$fireplacecnt, col='light blue', breaks=70)
            hist(log(joined$fireplacecnt + 1000), col='light blue', breaks=70)
            table(joined$fireplacecnt); sum(is.na(joined$fireplacecnt))
        # there are no zeros, let's assume zero if NA?
        joined$tv_cat_fireplacecnt <- ifelse(is.na(joined$fireplacecnt), 0, joined$fireplacecnt)
        joined$tv_cat_fireplacecnt <- cut2(joined$tv_cat_fireplacecnt, cuts = c(
            min(joined$tv_cat_fireplacecnt, na.rm=T), 1, 2, 3, 4, max(joined$tv_cat_fireplacecnt, na.rm=T)))
            table(joined$tv_cat_fireplacecnt); sum(is.na(joined$tv_cat_fireplacecnt))
        joined$tv_cat_fireplacecnt <- as.numeric(joined$tv_cat_fireplacecnt)
        joined$tv_cat_fireplacecnt <- paste0("tv_cat_fireplacecnt_", as.character(joined$tv_cat_fireplacecnt))
        joined$tv_cat_fireplacecnt[grepl("_NA$", joined$tv_cat_fireplacecnt)] <- NA
            sum(is.na(joined$tv_cat_fireplacecnt))
            table(joined$tv_cat_fireplacecnt)
        joined$tv_reg_fireplacecnt <- ifelse(is.na(joined$fireplacecnt), 0, joined$fireplacecnt)
            table(joined$tv_reg_fireplacecnt)    
        joined$tv_logreg_fireplacecnt <- log(joined$tv_reg_fireplacecnt + 1)    
            min(joined$tv_logreg_fireplacecnt)
            table(joined$tv_logreg_fireplacecnt)
        joined$fireplacecnt <- NULL
        
        
        # full bath cnt -- Remove, this will just muddy the water
        sum(is.na(joined$fullbathcnt))  # 128k NAs
        min(joined$fullbathcnt, na.rm=T) # 1 is min
        quantile(joined$fullbathcnt, seq(0, 1, 0.1), na.rm=T)
        joined$fullbathcnt <- NULL
        
        
        # garagecarcnt -- I want to manually do the groupings here to include "0" count category
            hist(joined$garagecarcnt, col='light blue')
            hist(log(joined$garagecarcnt + 1), col='light blue')
        sum(is.na(joined$garagecarcnt))  # 2.1 Mil NAs
        min(joined$garagecarcnt, na.rm=T) # 0 -- don't assume NA are zero
        quantile(joined$garagecarcnt, seq(0, 1, 0.2), na.rm=T)
        joined$tv_cat_garagecarcnt <- cut2(joined$garagecarcnt, cuts = c(
            min(joined$garagecarcnt, na.rm=T), 1, 2, 3, 4, 5, max(joined$garagecarcnt, na.rm=T)))
            levels(joined$tv_cat_garagecarcnt)
        joined$tv_cat_garagecarcnt <- as.numeric(joined$tv_cat_garagecarcnt)
        joined$tv_cat_garagecarcnt <- paste0("tv_cat_garagecarcnt_", as.character(joined$tv_cat_garagecarcnt))
        joined$tv_cat_garagecarcnt[grepl("_NA$", joined$tv_cat_garagecarcnt)] <- NA
            sum(is.na(joined$tv_cat_garagecarcnt))  # 2.1 Mil NAs -- good
            table(joined$tv_cat_garagecarcnt)
        joined$tv_reg_garagecarcnt <- joined$garagecarcnt
        joined$tv_logreg_garagecarcnt <- log(joined$garagecarcnt + 1)
        joined$garagecarcnt <- NULL
        
        
        
        # garage total sqft
            sum(is.na(joined$garagetotalsqft))  # 2.1 Mil NAs
            min(joined$garagetotalsqft, na.rm=T) # 0 -- don't assume NAs are zero
            hist(joined$garagetotalsqft, col='light blue', breaks = 50)
            hist(log(joined$garagetotalsqft + 100), col='light blue', breaks = 50)
        joined$tv_cat_garagetotalsqft_five <- cut2_rename(joined$garagetotalsqft, 5, "tv_cat_garagetotalsqft_five")
        joined$tv_cat_garagetotalsqft_ten <- cut2_rename(joined$garagetotalsqft, 10, "tv_cat_garagetotalsqft_ten")
        joined$tv_reg_garagetotalsqft <- joined$garagetotalsqft
        joined$tv_logreg_garagetotalsqft <- log(joined$garagetotalsqft + 100)
        joined$garagetotalsqft <- NULL
        
        
        
        # hashottuborspa  -- "" or "true"
        joined$tv_cat_has_hottub <- as.integer(joined$hashottuborspa == 'true')
            table(joined$tv_cat_has_hottub)
            sum(is.na(joined$tv_cat_has_hottub))
        joined$tv_cat_has_hottub <- paste0("tv_cat_has_hottub_", as.character(joined$tv_cat_has_hottub))
        table(joined$tv_cat_has_hottub)
        joined$hashottuborspa <- NULL
        
        
        
        
        
        # LAT and LON -- going to make lat-lon combo buckets
            # not going to be able to use randomforest for some of these categorical features (over max num classes)
            sum(is.na(joined$lat))
            hist(joined$latitude, col='light blue')
            sum(is.na(joined$longitude))
            hist(joined$longitude, col='light blue')
        # lat lon five
        joined$tv_cat_latitude_five <- as.character(as.numeric(cut2(joined$latitude, g=5)))
        joined$tv_cat_longitude_five <- as.character(as.numeric(cut2(joined$longitude, g=5)))
        joined$tv_cat_latlong_five <- paste0("tv_cat_latlong_five_", joined$tv_cat_latitude_five, '_', joined$tv_cat_longitude_five)
        table(joined$tv_cat_latlong_five)
        joined$tv_cat_latlong_five[grepl("NA", joined$tv_cat_latlong_five)] <- NA
            sum(is.na(joined$tv_cat_latlong_five))
        joined$tv_cat_latitude_five <- ifelse(is.na(joined$tv_cat_latitude_five), NA, 
                                              paste0("tv_cat_latitude_five_", joined$tv_cat_latitude_five))
        joined$tv_cat_longitude_five <- ifelse(is.na(joined$tv_cat_longitude_five), NA,
                                               paste0("tv_cat_longitude_five_", joined$tv_cat_longitude_five))
        # lat long ten
        joined$tv_cat_latitude_ten <- as.character(as.numeric(cut2(joined$latitude, g=10)))
        joined$tv_cat_longitude_ten <- as.character(as.numeric(cut2(joined$longitude, g=10)))
        joined$tv_cat_latlong_ten <- paste0("tv_cat_latlong_ten_", joined$tv_cat_latitude_ten, '_', joined$tv_cat_longitude_ten)
        table(joined$tv_cat_latlong_ten)
        joined$tv_cat_latlong_ten[grepl("NA", joined$tv_cat_latlong_ten)] <- NA
        sum(is.na(joined$tv_cat_latlong_ten))
        joined$tv_cat_latitude_ten <- ifelse(is.na(joined$tv_cat_latitude_ten), NA, 
                                              paste0("tv_cat_latitude_ten_", joined$tv_cat_latitude_ten))
        joined$tv_cat_longitude_ten <- ifelse(is.na(joined$tv_cat_longitude_ten), NA,
                                               paste0("tv_cat_longitude_ten_", joined$tv_cat_longitude_ten))
        # lat long twenty
        joined$tv_cat_latitude_twenty <- as.character(as.numeric(cut2(joined$latitude, g=20)))
        joined$tv_cat_longitude_twenty <- as.character(as.numeric(cut2(joined$longitude, g=20)))
        joined$tv_cat_latlong_twenty <- paste0("tv_cat_latlong_twenty_", joined$tv_cat_latitude_twenty, '_', joined$tv_cat_longitude_twenty)
        table(joined$tv_cat_latlong_twenty)
        joined$tv_cat_latlong_twenty[grepl("NA", joined$tv_cat_latlong_twenty)] <- NA
        sum(is.na(joined$tv_cat_latlong_twenty))
        joined$tv_cat_latitude_twenty <- ifelse(is.na(joined$tv_cat_latitude_twenty), NA, 
                                             paste0("tv_cat_latitude_twenty_", joined$tv_cat_latitude_twenty))
        joined$tv_cat_longitude_twenty <- ifelse(is.na(joined$tv_cat_longitude_twenty), NA,
                                              paste0("tv_cat_longitude_twenty_", joined$tv_cat_longitude_twenty))
        # regs
        joined$tv_reg_latitude <- joined$latitude
        joined$tv_reg_longitude <- joined$longitude
        
        joined$longitude <- NULL
        joined$latitude <- NULL
        
        
        
        # lotsizesquarefeet
            sum(is.na(joined$lotsizesquarefeet))  # 276k
            hist(joined$lotsizesquarefeet, col='light green', breaks=50)
            hist(log(joined$lotsizesquarefeet, base=3), col='light green', breaks=50)
            min(joined$lotsizesquarefeet, na.rm=T)  # 100
        # cats
        joined$tv_cat_lotsizesqft_five <- cut2_rename(joined$lotsizesquarefeet, 5, "tv_cat_lotsizesqft_five")
        joined$tv_cat_lotsizesqft_ten <- cut2_rename(joined$lotsizesquarefeet, 10, "tv_cat_lotsizesqft_ten")
        # regs
        joined$tv_reg_lotsizesqft <- joined$lotsizesquarefeet
        joined$tv_logreg_lotsizesqft <- log(joined$lotsizesquarefeet, base=3)
        joined$lotsizesquarefeet <- NULL
        
        
        
        # poolcnt -- pooltypeid's will have this covered.. delete this as it's a risk of being a cheater
            sum(is.na(joined$poolcnt))  # 246k
            hist(joined$poolcnt, col='red', breaks=50)
        joined$poolcnt <- NULL
        
        
        
        # poolsizesum
            sum(is.na(joined$poolsizesum))  # 2.96 mill NAs
            hist(joined$poolsizesum, col='red', breaks=50)
            hist(log(joined$poolsizesum + 500), col='red', breaks=50)
        joined$tv_cat_poolsize_five <- cut2_rename(joined$poolsizesum, 5, "tv_cat_poolsize_five")
        joined$tv_cat_poolsize_ten <- cut2_rename(joined$poolsizesum, 10, "tv_cat_poolsize_ten")
        joined$tv_reg_poolsize <- joined$poolsizesum
        joined$tv_logreg_poolsize <- log(joined$poolsizesum + 500)
        joined$poolsizesum <- NULL
            
        
        
        # propertycountylandusecode
            sum(is.na(joined$propertycountylandusecode))  # 0
            joined$propertycountylandusecode[joined$propertycountylandusecode == ''] <- 'blank'
        joined$tv_cat_proplanduse <- paste0("tv_cat_proplanduse_", joined$propertycountylandusecode)
        joined$propertycountylandusecode <- NULL
        
        
        table(joined$tv_cat_proplanduse) %>% data.frame() %>% arrange(desc(Freq))
        
        
        # catch        
        
        
    
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





