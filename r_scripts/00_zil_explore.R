

# keep the structure, but rename this script to be very exploratory

# VERY next steps:
# - rename all the features so names are more manageable
# - 



#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()
# "C:/Users/tvananne/Documents/personal/github/kaggles/zillow_zestimate/r_scripts"


# source in config and function defs
source('r_scripts/GBL_zil_config.R')
source('r_scripts/GBL_zil_function_defs.R')

recalculate_features <- FALSE
if(recalculate_features) {
    
    
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
    
    
    # rename parcelid to id and add prefix to ids
    
    prop$parcelid <- paste0("pid_", as.character(prop$parcelid))
    train$parcelid <- paste0("pid_", as.character(train$parcelid))
    
    
    # use this for everything -- remove logerror / transactiondate for all feature stacking
    joined <- merge(x=train, y=prop, by='parcelid', all.x=T, all.y=T, sort=F)
    joined$id <- joined$parcelid; joined$parcelid <- NULL


    # reset
    # joined_filepath <- file.path(GBL_PATH_TO_DATA, 'joined.rds')
    # saveRDS(joined, joined_filepath) 
    
    
    # one by one feature analysis:
    # look at everything except for things with "id" in the name -- those we'll look at on their own

    #' might want to come back and revisit all of these and flag all of the records where they are
    #' in the top 1 or 0.5 % or something. Some of these right-hand tails are absurd... very skewed data
    #' Maybe we should also look into log transformations AND categorizing data AFTER log transformations


        
    
    
    # basementsqft is integer, that is fine
        hist(joined$basementsqft, col='light blue', breaks=30)
        hist(log(joined$basementsqft + 100), col='light blue', breaks=30)
        hist(scale(log(joined$basementsqft + 100)), col='light blue', breaks=30)
        moments::skewness(joined$basementsqft, na.rm=T)
        moments::skewness(log(joined$basementsqft), na.rm=T)
        moments::skewness(log(joined$basementsqft + 100), na.rm=T)  #<-- 100 is magic skewness minimizing constant
    # create 5-class category; rename numeric; log the original; remove original
    joined$tv_cat_basementsqft <- cut2_rename(joined$basementsqft, 5, "tv_cat_basementsqft")
    joined$tv_rawreg_basementsqft <- joined$basementsqft
    joined$tv_logreg_basementsqft <- log(joined$basementsqft + 100)
        quantile(joined$basementsqft, seq(.8, 1, 0.005), na.rm=T) 
        sum(joined$basementsqft >= quantile(joined$basementsqft, .995, na.rm=T), na.rm=T) # <-- check for how many extreme outliers we have
        assert_that(sum(is.na(joined$basementsqft)) == sum(is.na(joined$tv_cat_basementsqft)))
        assert_that(sum(is.na(joined$basementsqft)) == sum(is.na(joined$tv_logreg_basementsqft)))
        assert_that(sum(is.na(joined$basementsqft)) == sum(is.na(joined$tv_rawreg_basementsqft)))
    joined$basementsqft <- NULL
    
        
    
    
    # bathroomcnt
        hist(joined$bathroomcnt, col='light blue')
        hist(log(joined$bathroomcnt), col='light blue')
        hist(scale(log(joined$bathroomcnt + 3.6)), col='light green')
        moments::skewness(joined$bathroomcnt, na.rm=T)
        moments::skewness(log(joined$bathroomcnt + 3.6), na.rm=T) # <-- 3.6 magic skewness minimizer
    # create "has_halfbath" feature
    joined$tv_cat_has_halfbath <- as.integer(joined$bathroomcnt %% 1 == 0.5)
    joined$tv_cat_has_halfbath <- paste0("tv_cat_has_halfbath_", as.character(joined$tv_cat_has_halfbath))
    joined$tv_cat_has_halfbath[grepl("_NA$", joined$tv_cat_has_halfbath)] <- NA
    joined$tv_cat_count_toilets <- cut2(ceiling(joined$bathroomcnt), cuts = c(
        min(ceiling(joined$bathroomcnt), na.rm=T), 1, 2, 3, 4, 5, max(ceiling(joined$bathroomcnt), na.rm=T)))
    joined$tv_cat_count_toilets <- as.numeric(joined$tv_cat_count_toilets)
    joined$tv_cat_count_toilets <- paste0("tv_cat_count_toilets_", as.character(joined$tv_cat_count_toilets))
    joined$tv_cat_count_toilets[grepl("_NA$", joined$tv_cat_count_toilets)] <- NA
        table(joined$tv_cat_count_toilets)
    # joined$tv_rawreg_bathroomcnt <- joined$bathroomcnt
    joined$tv_logreg_bathroomcnt <- log(joined$bathroomcnt + 3.6)
    joined$tv_rawreg_bathroomcnt <- joined$bathroomcnt
        assert_that(sum(is.na(joined$tv_logreg_bathroomcnt)) == sum(is.na(joined$bathroomcnt)))
        assert_that(sum(is.na(joined$tv_cat_count_toilets)) == sum(is.na(joined$bathroomcnt)))
        assert_that(sum(is.na(joined$tv_logreg_bathroomcnt)) == sum(is.na(joined$bathroomcnt)))
        assert_that(sum(is.na(joined$tv_rawreg_bathroomcnt)) == sum(is.na(joined$bathroomcnt)))
    joined$bathroomcnt <- NULL
    
    
   
    
    
    # bedroomcnt
        hist(joined$bedroomcnt, col='light blue')
        hist(log(joined$bedroomcnt + 1), col='light blue', breaks=20)
        hist(scale(log(joined$bedroomcnt + 15)), col='light green')
        moments::skewness(joined$bedroomcnt, na.rm=T)
        moments::skewness(log(joined$bedroomcnt + 1), na.rm=T)
        moments::skewness(log(joined$bedroomcnt + 15), na.rm=T) # <-- 15 is the magic skewness minimizer
        summary(joined$bedroomcnt)
        summary(log(joined$bedroomcnt + 1))
        summary(scale(log(joined$bedroomcnt + 2)))
    # joined$tv_cat_bedroomcnt <- cut2_rename(joined$bedroomcnt, 5, "tv_cat_bedroomcnt")  #<-- wanted this to be manual
    joined$tv_cat_bedroomcnt <- cut2(joined$bedroomcnt, cuts = c(
        min(joined$bedroomcnt, na.rm=T), 1, 2, 3, 4, 5, max(joined$bedroomcnt, na.rm=T)))
    joined$tv_cat_bedroomcnt <- as.numeric(joined$tv_cat_bedroomcnt)
    joined$tv_cat_bedroomcnt <- paste0("tv_cat_bedroomcnt_", as.character(joined$tv_cat_bedroomcnt))
    joined$tv_cat_bedroomcnt[grepl("_NA$", joined$tv_cat_bedroomcnt)] <- NA
        table(joined$tv_cat_bedroomcnt)
    joined$tv_rawreg_bedroomcnt <- joined$bedroomcnt
    joined$tv_logreg_bedroomcnt <- log(joined$bedroomcnt + 15)
        assert_that(sum(is.na(joined$tv_cat_bedroomcnt)) == sum(is.na(joined$bedroomcnt)))
        assert_that(sum(is.na(joined$tv_rawreg_bedroomcnt)) == sum(is.na(joined$bedroomcnt)))
        assert_that(sum(is.na(joined$tv_logreg_bedroomcnt)) == sum(is.na(joined$bedroomcnt)))
    joined$bedroomcnt <- NULL
    
        
    
    
    # calulatedbathnbr -- more missing values than other bathrm number ignore for now
    # I DON'T THINK THIS GIVES US ANY ADDITIONAL INFO, GOING TO REMOVE THIS FIELD
    joined$calculatedbathnbr <- NULL
    
    
    
    
    # finishedfloor1squarefeet -- this one has some serious detail, lets make two cats and one reg
        hist(joined$finishedfloor1squarefeet, col='light blue')
        hist(log(joined$finishedfloor1squarefeet + 259), col='light blue')  # 500 shift to improve normalcy
        min(joined$finishedfloor1squarefeet, na.rm=T)
        summary(scale(log(joined$finishedfloor1squarefeet + 259)))
        moments::skewness(log(joined$finishedfloor1squarefeet + 259), na.rm=T)
    # make features
    joined$tv_cat_finflr1_five <- cut2_rename(joined$finishedfloor1squarefeet, 5, "tv_cat_finflr1_five")
    joined$tv_cat_finflr1_ten <- cut2_rename(joined$finishedfloor1squarefeet, 10, "tv_cat_finflr1_ten")    
    joined$tv_rawreg_finflr1 <- joined$finishedfloor1squarefeet
    joined$tv_logreg_finflr1 <- log(joined$finishedfloor1squarefeet + 259)
        assert_that(sum(is.na(joined$tv_cat_finflr1_five)) == sum(is.na(joined$finishedfloor1squarefeet)))
        assert_that(sum(is.na(joined$tv_cat_finflr1_ten)) == sum(is.na(joined$finishedfloor1squarefeet)))
        assert_that(sum(is.na(joined$tv_rawreg_finflr1)) == sum(is.na(joined$finishedfloor1squarefeet)))
        assert_that(sum(is.na(joined$tv_logreg_finflr1)) == sum(is.na(joined$finishedfloor1squarefeet)))
    joined$finishedfloor1squarefeet <- NULL
    
    
    
    
    
    # calculatedfinishedsquarefeet
        hist(joined$calculatedfinishedsquarefeet, col='light blue')
        hist(log(joined$calculatedfinishedsquarefeet), col='light blue') 
        min(joined$calculatedfinishedsquarefeet, na.rm=T)
        sum(joined$calculatedfinishedsquarefeet < 1000, na.rm=T)
        moments::skewness(log(joined$calculatedfinishedsquarefeet), na.rm=T)  # <-- can't reduce skew by adding a constant
    # make features
    joined$tv_cat_totsqft_five <- cut2_rename(joined$calculatedfinishedsquarefeet, 5, "tv_cat_totsqft_five")
    joined$tv_cat_totsqft_ten <- cut2_rename(joined$calculatedfinishedsquarefeet, 10, "tv_cat_totsqft_ten")        
    joined$tv_rawreg_totsqft <- joined$calculatedfinishedsquarefeet
    joined$tv_logreg_totsqft <- log(joined$calculatedfinishedsquarefeet)
        assert_that(sum(is.na(joined$calculatedfinishedsquarefeet)) == sum(is.na(joined$tv_cat_totsqft_five)))
        assert_that(sum(is.na(joined$calculatedfinishedsquarefeet)) == sum(is.na(joined$tv_cat_totsqft_ten)))
        assert_that(sum(is.na(joined$calculatedfinishedsquarefeet)) == sum(is.na(joined$tv_rawreg_totsqft)))
        assert_that(sum(is.na(joined$calculatedfinishedsquarefeet)) == sum(is.na(joined$tv_logreg_totsqft)))
    joined$calculatedfinishedsquarefeet <- NULL
    
    
    
    
    # finishedsquarefeet12
        hist(joined$finishedsquarefeet12, col='light blue')
        hist(log(joined$finishedsquarefeet12), col='light blue')
        min(joined$finishedsquarefeet12, na.rm=T)
        summary(joined$finishedsquarefeet12)
        moments::skewness(log(joined$finishedsquarefeet12), na.rm=T)
        moments::skewness(log(joined$finishedsquarefeet12 + 1), na.rm=T)  # <-- can't minimize skewness by adding a constant
    # make features
    joined$tv_cat_finsqft12_five <- cut2_rename(joined$finishedsquarefeet12, 5, "tv_cat_finsqft12_five")
    joined$tv_cat_finsqft12_ten <- cut2_rename(joined$finishedsquarefeet12, 10, "tv_cat_finsqft12_ten")
    joined$tv_cat_finsqft12_twenty <- cut2_rename(joined$finishedsquarefeet12, 20, "tv_cat_finsqft12_twenty")
    joined$tv_rawreg_finsqft12 <- joined$finishedsquarefeet12    
    joined$tv_logreg_finsqft12 <- log(joined$finishedsquarefeet12)
        assert_that(sum(is.na(joined$tv_cat_finsqft12_five)) == sum(is.na(joined$finishedsquarefeet12)))
        assert_that(sum(is.na(joined$tv_cat_finsqft12_ten)) == sum(is.na(joined$finishedsquarefeet12)))
        assert_that(sum(is.na(joined$tv_rawreg_finsqft12)) == sum(is.na(joined$finishedsquarefeet12)))
        assert_that(sum(is.na(joined$tv_logreg_finsqft12)) == sum(is.na(joined$finishedsquarefeet12)))
    joined$finishedsquarefeet12 <- NULL
    
    
    
    
    
    # finishedsquarefeet13
        hist(joined$finishedsquarefeet13, col='light blue')
        hist(log(joined$finishedsquarefeet13), col='light blue')  # <-- don't make log, this makes skew worse
        moments::skewness(joined$finishedsquarefeet13, na.rm=T)
        moments::skewness(log(joined$finishedsquarefeet13), na.rm=T)    
    # make features - not enough density to create a ten-category feature
    joined$tv_cat_finsqft13_five <- cut2_rename(joined$finishedsquarefeet13, 5, "tv_cat_finsqft13_five")
    joined$tv_rawreg_finsqft13 <- joined$finishedsquarefeet13    
    joined$finishedsquarefeet13 <- NULL    
    
    
    
    
    # finishedsquarefeet15
        hist(joined$finishedsquarefeet15, col='light blue', breaks=50)
        hist(log(joined$finishedsquarefeet15 - 111), col='light blue', breaks=50)
        min(joined$finishedsquarefeet15, na.rm=T)
        moments::skewness(log(joined$finishedsquarefeet15), na.rm=T)
        moments::skewness(log(joined$finishedsquarefeet15 - 111), na.rm=T)
    # make features
    joined$tv_cat_finsqft15_five <- cut2_rename(joined$finishedsquarefeet15, 5, "tv_cat_finsqft15_five")
    joined$tv_cat_finsqft15_ten <- cut2_rename(joined$finishedsquarefeet15, 10, "tv_cat_finsqft15_ten")    
    joined$tv_rawreg_finsqft15 <- joined$finishedsquarefeet15
    joined$tv_logreg_finsqft15 <- log(joined$finishedsquarefeet15 - 111)
        assert_that(sum( is.infinite(joined$tv_logreg_finsqft15) | is.nan(joined$tv_logreg_finsqft15)) == 0)
    joined$finishedsquarefeet15 <- NULL
    
    
    
    # finishedsquarefeet50
        hist(joined$finishedsquarefeet50, col='light blue')
        hist(log(joined$finishedsquarefeet50 + 211), col='light blue')
        moments::skewness(log(joined$finishedsquarefeet50), na.rm=T)
        moments::skewness(log(joined$finishedsquarefeet50 + 211), na.rm=T)
    # make features
    joined$tv_cat_finsqft50_five <- cut2_rename(joined$finishedsquarefeet50, 5, "tv_cat_finsqft50_five")
    joined$tv_cat_finsqft50_ten <- cut2_rename(joined$finishedsquarefeet50, 10, "tv_cat_finsqft50_ten")
    joined$tv_rawreg_finsqft50 <- joined$finishedsquarefeet50
    joined$tv_logreg_finsqft50 <- log(joined$finishedsquarefeet50 + 211)
    joined$finishedsquarefeet50 <- NULL
    
    
    
    # finishedsquarefeet6
        hist(joined$finishedsquarefeet6, col='light blue')
        hist(log(joined$finishedsquarefeet6), col='light blue', breaks=80)
        hist(log(joined$finishedsquarefeet6 + 72), col='light green', breaks=80)
        moments::skewness(log(joined$finishedsquarefeet6 + 72), na.rm=T)
    # make features
    joined$tv_cat_finsqft6_five <- cut2_rename(joined$finishedsquarefeet6, 5, "tv_cat_finsqft6_five")
    # joined$tv_cat_finsqft6_ten <- cut2_rename(joined$finishedsquarefeet6, 10, "tv_cat_finsqft6_ten")  #  not enough density
    joined$tv_rawreg_finsqft6 <- joined$finishedsquarefeet6
    joined$tv_logreg_finsqft6 <- log(joined$finishedsquarefeet6 + 72)
    joined$finishedsquarefeet6 <- NULL
        
    
    
    # fips -- categorical location data I believe
    unique(joined$fips)
    joined$tv_cat_fips <- paste0("fips_", as.character(joined$fips))
    joined$tv_cat_fips[joined$tv_cat_fips == "fips_NA"] <- NA
        table(joined$tv_cat_fips); sum(is.na(joined$tv_cat_fips))
        assert_that(sum(is.na(joined$fips)) == sum(is.na(joined$tv_cat_fips)))
    joined$fips <- NULL
    
    
    
    
    # fireplacecnt
    # min is 1, should we assume NAs are zeros? I don't think we can make that assumption? 
        hist(joined$fireplacecnt, col='light blue', breaks=70)
        hist(log(joined$fireplacecnt), col='light blue', breaks=70)
        moments::skewness(joined$fireplacecnt, na.rm=T)
        moments::skewness(log(joined$fireplacecnt + 1), na.rm=T)
        table(joined$fireplacecnt); sum(is.na(joined$fireplacecnt))
    # there are no zeros, let's assume zero if NA?
    joined$tv_cat_fireplacecnt <- ifelse(is.na(joined$fireplacecnt), 0, joined$fireplacecnt)
    joined$tv_cat_fireplacecnt <- cut2(joined$tv_cat_fireplacecnt, cuts = c(
        min(joined$tv_cat_fireplacecnt, na.rm=T), 1, 2, 3, max(joined$tv_cat_fireplacecnt, na.rm=T)))
        table(joined$tv_cat_fireplacecnt); sum(is.na(joined$tv_cat_fireplacecnt))
    joined$tv_cat_fireplacecnt <- as.numeric(joined$tv_cat_fireplacecnt)
    joined$tv_cat_fireplacecnt <- paste0("tv_cat_fireplacecnt_", as.character(joined$tv_cat_fireplacecnt))
    joined$tv_cat_fireplacecnt[grepl("_NA$", joined$tv_cat_fireplacecnt)] <- NA
        sum(is.na(joined$tv_cat_fireplacecnt))
        table(joined$tv_cat_fireplacecnt)
    joined$tv_rawreg_fireplacecnt <- ifelse(is.na(joined$fireplacecnt), 0, joined$fireplacecnt)
        table(joined$tv_rawreg_fireplacecnt)    
    joined$tv_logreg_fireplacecnt <- log(joined$tv_rawreg_fireplacecnt + 1)    
        min(joined$tv_logreg_fireplacecnt)
        table(joined$tv_logreg_fireplacecnt)
    joined$fireplacecnt <- NULL
    
    
    
    
    # full bath cnt -- Remove, this will just muddy the water
    sum(is.na(joined$fullbathcnt))  # 128k NAs
    min(joined$fullbathcnt, na.rm=T) # 1 is min
    quantile(joined$fullbathcnt, seq(0, 1, 0.1), na.rm=T)
    joined$fullbathcnt <- NULL
    
    
    
    
    # garagecarcnt -- I want to manually do the groupings here to include "0" count category
        hist(joined$garagecarcnt, col='light blue', breaks=40)
        hist(log(joined$garagecarcnt + 1), col='light blue', breaks=40)
        hist(log(joined$garagecarcnt + 5), col='light blue', breaks=40)
        sum(is.na(joined$garagecarcnt))  # 2.1 Mil NAs
        min(joined$garagecarcnt, na.rm=T) # 0 -- don't assume NA are zero
        moments::skewness(joined$garagecarcnt, na.rm=T)
        moments::skewness(log(joined$garagecarcnt + 5), na.rm=T)
    quantile(joined$garagecarcnt, seq(0, 1, 0.2), na.rm=T)
    joined$tv_cat_garagecarcnt <- cut2(joined$garagecarcnt, cuts = c(
        min(joined$garagecarcnt, na.rm=T), 1, 2, 3, 4, max(joined$garagecarcnt, na.rm=T)))
        table(joined$tv_cat_garagecarcnt)
        levels(joined$tv_cat_garagecarcnt)
    joined$tv_cat_garagecarcnt <- as.numeric(joined$tv_cat_garagecarcnt)
    joined$tv_cat_garagecarcnt <- paste0("tv_cat_garagecarcnt_", as.character(joined$tv_cat_garagecarcnt))
    joined$tv_cat_garagecarcnt[grepl("_NA$", joined$tv_cat_garagecarcnt)] <- NA
        sum(is.na(joined$tv_cat_garagecarcnt))  # 2.1 Mil NAs -- good
        table(joined$tv_cat_garagecarcnt)
    joined$tv_rawreg_garagecarcnt <- joined$garagecarcnt
    joined$tv_logreg_garagecarcnt <- log(joined$garagecarcnt + 5)
    joined$garagecarcnt <- NULL
    
    
    
    
    # garage total sqft
        sum(is.na(joined$garagetotalsqft))  # 2.1 Mil NAs
        min(joined$garagetotalsqft, na.rm=T) # 0 -- don't assume NAs are zero
        hist(joined$garagetotalsqft, col='light blue', breaks = 50)
        hist(log(joined$garagetotalsqft + 3660), col='light blue', breaks = 50)
        moments::skewness(log(joined$garagetotalsqft + 1), na.rm=T)
        moments::skewness(log(joined$garagetotalsqft + 3660), na.rm=T)
    joined$tv_cat_garagetotalsqft_five <- cut2_rename(joined$garagetotalsqft, 5, "tv_cat_garagetotalsqft_five")
    joined$tv_cat_garagetotalsqft_ten <- cut2_rename(joined$garagetotalsqft, 10, "tv_cat_garagetotalsqft_ten")
    joined$tv_rawreg_garagetotalsqft <- joined$garagetotalsqft
    joined$tv_logreg_garagetotalsqft <- log(joined$garagetotalsqft + 3660)
    joined$garagetotalsqft <- NULL
    
    
    
    # hashottuborspa  -- "" or "true"
    joined$tv_cat_has_hottub <- as.integer(joined$hashottuborspa == 'true')
        table(joined$tv_cat_has_hottub)
        sum(is.na(joined$tv_cat_has_hottub)); sum(is.na(joined$hashottuborspa))
    joined$tv_cat_has_hottub <- paste0("tv_cat_has_hottub_", as.character(joined$tv_cat_has_hottub))
    table(joined$tv_cat_has_hottub)
    joined$hashottuborspa <- NULL
    
    
    
    
    
    # LAT and LON -- going to make lat-lon combo buckets
        # not going to be able to use randomforest for some of these categorical features (over max num classes)
        sum(is.na(joined$lat))
        hist(joined$latitude, col='light blue')
        sum(is.na(joined$longitude))
        hist(joined$longitude, col='light blue')
    # quantile groups
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
        # too many groups
        # joined$tv_cat_latlong_twenty <- paste0("tv_cat_latlong_twenty_", joined$tv_cat_latitude_twenty, '_', joined$tv_cat_longitude_twenty)
        # table(joined$tv_cat_latlong_twenty)
        # joined$tv_cat_latlong_twenty[grepl("NA", joined$tv_cat_latlong_twenty)] <- NA
        # sum(is.na(joined$tv_cat_latlong_twenty))
    joined$tv_cat_latitude_twenty <- ifelse(is.na(joined$tv_cat_latitude_twenty), NA, 
                                         paste0("tv_cat_latitude_twenty_", joined$tv_cat_latitude_twenty))
    joined$tv_cat_longitude_twenty <- ifelse(is.na(joined$tv_cat_longitude_twenty), NA,
                                          paste0("tv_cat_longitude_twenty_", joined$tv_cat_longitude_twenty))
    table(joined$tv_cat_latlong_twenty)
    
    # equal interval binning - 10 groups
    (latmax_ <- max(joined$latitude, na.rm=T))
    (latmin_ <- min(joined$latitude, na.rm=T))
    latcut_10_ <- seq(latmin_, latmax_, by=((latmax_ - latmin_) / 10))
    joined$tv_cat_latitude_eib_10 <- as.character(as.numeric(cut2(joined$latitude, cuts=latcut_10_)))
        table(joined$tv_cat_latitude_eib_10)
    (lonmax_ <- max(joined$longitude/1000, na.rm=T))  # <-- division by 1000 for longitude to fix numeric precision issues
    (lonmin_ <- min(joined$longitude/1000, na.rm=T))
    loncut_10_ <- seq(lonmin_, lonmax_, by=((lonmax_ - lonmin_) / 10))
    joined$tv_cat_longitude_eib_10 <- as.character(as.numeric(cut2(joined$longitude/1000, cuts=loncut_10_)))
        table(joined$tv_cat_longitude_eib_10)
    joined$tv_cat_latlong_eib_10 <- paste0("tv_cat_latlong_eib_10_", joined$tv_cat_latitude_eib_10, '_', joined$tv_cat_longitude_eib_10)
    joined$tv_cat_latlong_eib_10[grepl("NA", joined$tv_cat_latlong_eib_10)] <- NA
    joined$tv_cat_latitude_eib_10 <- ifelse(is.na(joined$tv_cat_latitude_eib_10), NA,
                                            paste0("tv_cat_latitude_eib_10_", joined$tv_cat_latitude_eib_10))
    joined$tv_cat_longitude_eib_10 <- ifelse(is.na(joined$tv_cat_longitude_eib_10), NA,
                                             paste0("tv_cat_longitude_eib_10_", joined$tv_cat_longitude_eib_10))
        assert_that(sum(is.na(joined$latitude)) == sum(is.na(joined$tv_cat_latitude_eib_10)))
        assert_that(sum(is.na(joined$longitude)) == sum(is.na(joined$tv_cat_longitude_eib_10)))
        assert_that(sum(is.na(joined$longitude) | sum(is.na(joined$latitude))) == sum(is.na(joined$tv_cat_latlong_eib_10)))
    
    tv_cat_latlong_eib_10_grp <- table(joined$tv_cat_latlong_eib_10) %>% data.frame()
    remove_sparse_latlong_eib10 <- as.character(tv_cat_latlong_eib_10_grp$Var1[tv_cat_latlong_eib_10_grp$Freq < 10000])
    joined$tv_cat_latlong_eib_10[joined$tv_cat_latlong_eib_10 %in% remove_sparse_latlong_eib10] <- NA
    # I WAS RIGHT HERE 9/32/2017
    
    # equal interval binning - 20 groups
    latcut_20_ <- seq(latmin_, latmax_, by=((latmax_ - latmin_) / 20))
    
    
    
    
    # regs
    joined$tv_rawreg_latitude <- joined$latitude
    joined$tv_rawreg_longitude <- joined$longitude
    
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
    joined$tv_rawreg_lotsizesqft <- joined$lotsizesquarefeet
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
    joined$tv_rawreg_poolsize <- joined$poolsizesum
    joined$tv_logreg_poolsize <- log(joined$poolsizesum + 500)
    joined$poolsizesum <- NULL
        
    
    
    # propertycountylandusecode -- come back and check this
        sum(is.na(joined$propertycountylandusecode))  # 0
        joined$propertycountylandusecode[joined$propertycountylandusecode == ''] <- 'blank'
    joined$tv_cat_proplanduse <- paste0("tv_cat_proplanduse_", joined$propertycountylandusecode)
    
    proplanduse_df_ <- table(joined$tv_cat_proplanduse) %>% data.frame() %>% arrange(desc(Freq)) %>% top_n(20)
    joined$tv_cat_proplanduse <- ifelse(joined$tv_cat_proplanduse %in% proplanduse_df_$Var1, 
                                        joined$tv_cat_proplanduse, NA)
    table(joined$tv_cat_proplanduse)
    sum(is.na(joined$tv_cat_proplanduse))  # 92k NAs
    joined$propertycountylandusecode <- NULL
    
    
    
    # propertyzoningdesc -- not sure, let's go ahead and remove this for now?
        joined$propertyzoningdesc[200]  # 5,639 unique values
        table(joined$propertyzoningdesc) %>% data.frame() %>% arrange(desc(Freq))
    joined$propertyzoningdesc <- NULL
    
    
    # rawcensustractandblock -- numeric?
        hist(joined$rawcensustractandblock, col='red', breaks=50)
        joined$rawcensustractandblock %>% unique() %>% length()
        quantile(joined$rawcensustractandblock, seq(0, 1, 0.1), na.rm=T)
    joined$tv_cat_rawcensus_five <- cut2_rename(joined$rawcensustractandblock, 5, "tv_cat_rawcensus_five")
    joined$tv_cat_rawcensus_twenty <- cut2_rename(joined$rawcensustractandblock, 20, "tv_cat_rawcensus_twenty")
    joined$tv_rawreg_rawcensus <- joined$rawcensustractandblock
    joined$rawcensustractandblock <- NULL
    
    
    # roomcnt
        sum(is.na(joined$roomcnt))  # 11k
        hist(joined$roomcnt, col='red', breaks=50)
        hist(log(joined$roomcnt), col='red', breaks=50)
        min(joined$roomcnt, na.rm=T)  # 0
    joined$tv_cat_roomcnt <- cut2(joined$roomcnt, cuts = c(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, max(joined$roomcnt, na.rm=T)))
    table(joined$tv_cat_roomcnt)
    joined$tv_cat_roomcnt <- as.numeric(joined$tv_cat_roomcnt)
    joined$tv_cat_roomcnt <- ifelse(is.na(joined$tv_cat_roomcnt), NA, 
                                    paste0("tv_cat_roomcnt_", as.character(joined$tv_cat_roomcnt)))
    table(joined$tv_cat_roomcnt)
    
    joined$tv_rawreg_roomcnt <- joined$roomcnt
    joined$tv_logreg_roomcnt <- log(joined$roomcnt + 1)
    joined$roomcnt <- NULL
    
    
    
    # threequarterbathnbr - I don't like this feature, let's remove it
    class(joined$threequarterbathnbr)
    quantile(joined$threequarterbathnbr, seq(0, 1, 0.1), na.rm=T)
    table(joined$threequarterbathnbr)
    joined$threequarterbathnbr <- NULL
    
    
    # unitcnt
    class(joined$unitcnt)
    quantile(joined$unitcnt, seq(0, 1, 0.1), na.rm=T)
    hist(joined$unitcnt, col='light blue', breaks=40)
    hist(log(joined$unitcnt), col='light blue', breaks=40)
    joined$tv_cat_unitcnt_three <- cut2(joined$unitcnt, cuts=c(1, 2, 3))
    joined$tv_cat_unitcnt_three <- as.integer(joined$tv_cat_unitcnt_three)
    joined$tv_cat_unitcnt_three <- ifelse(is.na(joined$tv_cat_unitcnt_three), NA,
                                          paste0("tv_cat_unitcnt_three_", joined$tv_cat_unitcnt_three))
    unique(joined$tv_cat_unitcnt_three)
    table(joined$tv_cat_unitcnt_three)
    
    joined$tv_rawreg_unitcnt <- joined$unitcnt
    joined$tv_logreg_unitcnt <- log(joined$unitcnt)
    joined$unitcnt <- NULL
    
    
    # yardbuildingsqft17
    hist(joined$yardbuildingsqft17, col='light blue')
    hist(log(joined$yardbuildingsqft17 + 20), col='light blue')
    quantile(joined$yardbuildingsqft17, seq(0, 1, 0.1), na.rm=T)
    joined$tv_cat_yardbuildingsqft17_five 
    joined$tv_cat_yardbuildingsqft17_five <- cut2_rename(joined$yardbuildingsqft17, 5, "tv_cat_yardbuildingsqft17_five")
    joined$tv_cat_yardbuildingsqft17_ten <- cut2_rename(joined$yardbuildingsqft17, 10, "tv_cat_yardbuildingsqft17_ten")
    joined$tv_rawreg_yardbuildingsqft17 <- joined$yardbuildingsqft17
    joined$tv_logreg_yardbuildingsqft17 <- log(joined$yardbuildingsqft17 + 20)
    joined$yardbuildingsqft17 <- NULL
    
    
    # yardbuildingsqft26
    hist(joined$yardbuildingsqft26, col='light blue')
    hist(log(joined$yardbuildingsqft26), col='light blue')
    joined$tv_cat_yardbuildingsqft26_five <- cut2_rename(joined$yardbuildingsqft26, 5, "tv_cat_yardbuildingsqft26_five")
    joined$tv_cat_yardbuildingsqft26_ten <- cut2_rename(joined$yardbuildingsqft26, 10, "tv_cat_yardbuildingsqft26_ten")
    joined$tv_rawreg_yardbuildingsqft26 <- joined$yardbuildingsqft26
    joined$tv_logreg_yardbuildingsqft26 <- log(joined$yardbuildingsqft26)
    joined$yardbuildingsqft26 <- NULL
    
    
    # yearbuilt
    hist(joined$yearbuilt, col='light blue')
    max(joined$yearbuilt, na.rm=T) # max is 2015
    joined$tv_rawreg_building_age <- 2015 - joined$yearbuilt
    hist(joined$tv_rawreg_building_age, col='light blue')
    hist(log(joined$tv_rawreg_building_age + 35), col='light blue')
    joined$tv_logreg_building_age <- log(joined$tv_rawreg_building_age + 35)
    quantile(joined$tv_rawreg_building_age, seq(0, 1, 0.2), na.rm=T)

    joined$tv_cat_building_age_five <- cut2_rename(joined$tv_rawreg_building_age, 5, "tv_cat_building_age_five")
    joined$tv_cat_building_age_ten <- cut2_rename(joined$tv_rawreg_building_age, 10, "tv_cat_building_age_ten")
    joined$yearbuilt <- NULL
    
    
    # numberofstories -- I think all we can / should do here is a boolean feature
    hist(joined$numberofstories, col='light blue')
    hist(log(joined$numberofstories), col='light blue')
    joined$tv_cat_bool_is_multi_story <- as.integer(joined$numberofstories > 1)
    joined$tv_cat_bool_is_multi_story <- ifelse(is.na(joined$tv_cat_bool_is_multi_story), NA,
                                                paste0("tv_cat_bool_is_multi_story_", joined$tv_cat_bool_is_multi_story))
    unique(joined$tv_cat_bool_is_multi_story)
    table(joined$tv_cat_bool_is_multi_story)
    joined$numberofstories <- NULL
    
    
    
    # fireplaceflag - we've already done fireplace stuff, lets remove this
    table(joined$fireplaceflag)
    joined$fireplaceflag <- NULL
    
    
    # assessmentyear -- when this isn't 2015, then NA
    hist(joined$assessmentyear, col='light blue')
    joined$taxamount[joined$assessmentyear != 2015 | is.na(joined$assessmentyear)] <- NA
    joined$taxdelinquencyflag[joined$assessmentyear != 2015 | is.na(joined$assessmentyear)] <- NA
    joined$taxdelinquencyyear[joined$assessmentyear != 2015 | is.na(joined$assessmentyear)] <- NA
    joined$taxvaluedollarcnt[joined$assessmentyear != 2015 | is.na(joined$assessmentyear)] <- NA
    joined$landtaxvaluedollarcnt[joined$assessmentyear != 2015 | is.na(joined$assessmentyear)] <- NA
    joined$structuretaxvaluedollarcnt[joined$assessmentyear != 2015 | is.na(joined$assessmentyear)] <- NA
    table(joined$assessmentyear)
    joined$assessmentyear <- NULL
    
    
    # structuretaxvaluedollarcnt
    hist(joined$structuretaxvaluedollarcnt, col='light blue')
    hist(log(joined$structuretaxvaluedollarcnt + 100), col='light blue') # <-- so beautiful...
    joined$tv_rawreg_structuretaxvalue <- joined$structuretaxvaluedollarcnt
    joined$tv_logreg_structuretaxvalue <- log(joined$structuretaxvaluedollarcnt + 100)
    
    quantile(joined$structuretaxvaluedollarcnt, seq(0, 1, 0.2), na.rm=T)
    joined$tv_cat_structuretaxvalue_five <- cut2_rename(joined$structuretaxvaluedollarcnt, 5, "tv_cat_structuretaxvalue_five")
    joined$tv_cat_structuretaxvalue_ten <- cut2_rename(joined$structuretaxvaluedollarcnt, 10, "tv_cat_structuretaxvalue_ten")
    joined$tv_cat_structuretaxvalue_twenty <- cut2_rename(joined$structuretaxvaluedollarcnt, 20, "tv_cat_structuretaxvalue_twenty")
    
    joined$structuretaxvaluedollarcnt <- NULL
    
    
    # taxvaluedollarcnt
    hist(joined$taxvaluedollarcnt, col='light blue')
    hist(log(joined$taxvaluedollarcnt + 900), col='red')
    joined$tv_rawreg_taxvalue <- joined$taxvaluedollarcnt
    joined$tv_logreg_taxvalue <- log(joined$taxvaluedollarcnt + 900)
    
    quantile(joined$taxvaluedollarcnt, seq(0, 1, 0.05), na.rm=T)
    joined$tv_cat_taxvalue_ten <- cut2_rename(joined$taxvaluedollarcnt, 10, "tv_cat_taxvalue_ten")
    joined$tv_cat_taxvalue_twenty <- cut2_rename(joined$taxvaluedollarcnt, 20, "tv_cat_taxvalue_twenty")
    joined$taxvaluedollarcnt <- NULL
    
    
    
    # landtaxvaluedollarcnt
    hist(joined$landtaxvaluedollarcnt, col='light blue')
    hist(log(joined$landtaxvaluedollarcnt + 500), col='light blue')
    joined$tv_cat_landtaxvalue_five <- cut2_rename(joined$landtaxvaluedollarcnt, 5, "tv_cat_landtaxvalue_five")
    joined$tv_cat_landtaxvalue_ten <- cut2_rename(joined$landtaxvaluedollarcnt, 10, "tv_cat_landtaxvalue_ten")
    joined$tv_cat_landtaxvalue_twenty <- cut2_rename(joined$landtaxvaluedollarcnt, 20, "tv_cat_landtaxvalue_twenty")
    joined$tv_rawreg_landtaxvalue <- joined$landtaxvaluedollarcnt
    joined$tv_logreg_landtaxvalue <- log(joined$landtaxvaluedollarcnt + 500)
    joined$landtaxvaluedollarcnt <- NULL
    
    
    # taxamount
    hist(joined$taxamount, col='light blue')
    hist(log(joined$taxamount + 100), col='red')
    joined$tv_cat_taxamount_five <- cut2_rename(joined$taxamount, 5, "tv_cat_taxamount_five")
    joined$tv_cat_taxamount_ten <- cut2_rename(joined$taxamount, 10, "tv_cat_taxamount_ten")
    joined$tv_cat_taxamount_twenty <- cut2_rename(joined$taxamount, 20, "tv_cat_taxamount_twenty")
    joined$tv_rawreg_taxamount <- joined$taxamount
    joined$tv_logreg_taxamount <- log(joined$taxamount + 100)
    joined$taxamount <- NULL
    
    
    # taxdelinquencyflag
    table(joined$taxdelinquencyflag)
    joined$tv_cat_bool_taxdelinquency <- as.integer(joined$taxdelinquencyflag == "Y")
    joined$tv_cat_bool_taxdelinquency <- ifelse(is.na(joined$tv_cat_bool_taxdelinquency), NA,
                                                paste0("tv_cat_bool_taxdelinquency_", joined$tv_cat_bool_taxdelinquency))
    joined$taxdelinquencyflag <- NULL
    
    
    
    # taxdelinquencyyear -- meh, let's leave this out for now
    table(joined$taxdelinquencyyear)
    joined$taxdelinquencyyear <- NULL
    
    
    # censustractandblock
    class(joined$censustractandblock)
    hist(joined$censustractandblock * 10e307 * 10e2, col='gray')
    quantile(joined$censustractandblock * 10e307, seq(0, 1, 0.1), na.rm=T)
    
    joined$tv_rawreg_censustractandblock <- joined$censustractandblock * 10e307 * 10e2
        hist(joined$tv_rawreg_censustractandblock, col='red')
    joined$tv_cat_censustractandblock_five <- cut2_rename(joined$tv_rawreg_censustractandblock, 5, "tv_cat_censustractandblock_five")
    joined$tv_cat_censustractandblock_ten <- cut2_rename(joined$tv_rawreg_censustractandblock, 10, "tv_cat_censustractandblock_ten")
    joined$tv_cat_censustractandblock_twenty <- cut2_rename(joined$tv_rawreg_censustractandblock, 20, "tv_cat_censustractandblock_twenty")
    joined$censustractandblock <- NULL
    
    names(joined)[!grepl("^tv_", names(joined))]
        
}
        
        
############################################################################        
# checkpoint to make sure our pipeline is good:
saveRDS(joined, file=file.path(GBL_PATH_TO_DATA, "joined2.rds"))
joined <- readRDS(file.path(GBL_PATH_TO_DATA, "joined2.rds"))


# what's left? 
names(joined)[!grepl("^tv_", names(joined))]
        

# airconditioningtypeid
table(joined$airconditioningtypeid)
class(joined$airconditioningtypeid)
thisdf <- data.frame(airconditioningtypeid = c(1, 3, 5, 9, 11, 12, 13),
                     tv_cat_airconditioningtypeid = c('1', 'other', '3', 'other', 'other', 'other', '2'))
joined <- merge(x=joined, y=thisdf, by='airconditioningtypeid', all.x=T, all.y=F)
joined$tv_cat_airconditioningtypeid <- ifelse(is.na(joined$tv_cat_airconditioningtypeid), NA,
                                              paste0("tv_cat_airconditioningtypeid_", joined$tv_cat_airconditioningtypeid))
table(joined$tv_cat_airconditioningtypeid)
sum(is.na(joined$tv_cat_airconditioningtypeid))
joined$airconditioningtypeid <- NULL; rm(thisdf)



# architecturalstyletypeid - this isi pretty pitiful, let's just remove it
table(joined$architecturalstyletypeid)
joined$architecturalstyletypeid <- NULL


# buildingclasstypeid
table(joined$buildingclasstypeid)
joined$buildingclasstypeid <- NULL


# buildingqualitytypeid  <-- potentially an "ordinal" variable
table(joined$buildingqualitytypeid)




# decktypeid
# heatingorsystemtypeid
# pooltypeid10
# pooltypeid2
# pooltypeid7
# propertylandusetypeid
# regionidcity
# regionidcounty
# regionidneighborhood
# regionidzip
# storytypeid
# typeconstructiontypeid



     



# variables with "id" in them are explored below



   
        
# # isolate the ids and what dataset they came from
# ids <- data.frame(parcelid = unique(c(prop$parcelid, train$parcelid)))
# ids$dataset <- 'test'
# ids$dataset[ids$parcelid %in% train$parcelid] <- 'train'
# ids <- ids %>% arrange(parcelid)



# names(prop)
# paste0(names(prop)[grepl('id', names(prop))], collapse = ', ')



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





