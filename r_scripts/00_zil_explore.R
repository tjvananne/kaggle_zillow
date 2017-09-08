

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

    # common transformation is to combine cut and quantile to bin numeric values into categories:
    # https://stackoverflow.com/questions/11728419/using-cut-and-quartile-to-generate-breaks-in-r-function
    # Hmisc::cut2() has a "g" parameter for number of quantile groups (can "g" and "m" params be give?)


        # basementsqft is integer, that is fine
        # 1) basic regression
        # 2) categorical on "pentiles?" 20% quantiles (5)
        class(joined$basementsqft)
        quantile(joined$basementsqft, seq(0, 1, .20), na.rm=T) # <-- cut these quantiles
        joined$basementsqft %>% unique() %>% head(10)
        joined$basementsqft %>% min(na.rm=T)
    
    
        # bathroomcnt
        # this will be a good feature:
        # 1) ceiling() will give us count of whole toilets (categorical: <=1, 2, 3, 4, >4)
        # 2) then also run a regression on the raw number of bathrooms with the .5 halves
        # 3) binary categorical for whether a half-bath is present (bathroomcnt %% 1 == 0.5)
        class(joined$bathroomcnt)
        joined$bathroomcnt %>% unique() %>% head(10)
        summary(joined$bathroomcnt)
        quantile(joined$bathroomcnt, probs=seq(0, 1, 0.05), na.rm=T)
        unique(joined$bathroomcnt) %% 1 == 0.5  # <-- new feature: has_a_half_bathrm (binary categorical)
        # joined$bathroomcnt %>% unique() %>% floor() # <-- showers
        joined$bathroomcnt %>% ceiling() %>% summary() # <-- toilets (categorical: 0, 1, 2, 3, 4, >4)

        
        # bedroomcnt
        # 1) regression
        # 2) categorical (<=1, 2, 3, 4, >5)
        quantile(joined$bedroomcnt, seq(0, 1, 0.05), na.rm=T)
    
        
        # calulatedbathnbr -- more missing values than other bathrm number ignore for now
        
        # finishedfloor1squarefeet
        # 1) regression
        # 2) categorical pentiles
        quantile(joined$finishedfloor1squarefeet, seq(0, 1, 0.20), na.rm=T)
        
        
        # calculatedfinishedsquarefeet -- sqr ft of all finished area in home
        # 1) regression
        
        quantile(joined$calculatedfinishedsquarefeet, seq(0, 1, 0.10), na.rm=T)
        
        
    
    
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





