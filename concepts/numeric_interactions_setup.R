

# testing the setup of multiple interactions

library(gtools)     # <-- combinations and permutations (for column name combos)
library(assertthat) # <-- for testing
library(xgboost)    # <-- for testing the creation of DMatrices
library(caret)      # <-- for easier scaling 



# example with iris data:
df <- head(iris)
names(df) <- gsub("\\.", "_", tolower(names(df)))

# isolate numeric fields
df_num <- df[, 1:4]
df_num
df_num_na <- data.frame(df_num)  # <-- data.frame() constructor to create a copy of object
df_num_na[1, 3] <- NA

    # don't do this, it won't line up with how model matrix handles the interactions
        # # identify the interaction column names, combine those with the original column names
        # combo_names_norep_mat <- gtools::combinations(n=ncol(df_num), r=2, v=names(df_num), set=T, repeats.allowed = F)
        # combo_names <- apply(combo_names_norep_mat, 1, FUN=paste0, collapse="-")
        # allnames <- c(names(df_num), combo_names)
        # allnames


#' ideas for retaining column names while also leveraging the efficiency of sparse matrices...
#' take the initial dataframe of numeric values... 
#' 1) limit it to only a single row (ideally one where
#' cases complete is equal to true, or at least test what happens with NA values). 
#' 2) Run this 1-row version of the data.frame through stats::model.matrix to create a dense 
#' matrix that will have the column names stored in "attr(<matrix name>, "dimnames")[[2]]"
#' 3) Save the column names somewhere (being sure to gsub out the ":" between interaction cols)
#' 4) generate the sparse matrix




# concise function ---------------------------------------------------------------------------------------------
    
    #' one thing to keep in mind, model matrix with formula of (~ .^4) will make all of the 
    #' interactions UP THROUGH four variables. You'll be returned all of the raw numeric 
    #' variables, all of the two-way interactions, all of the three-way interactions, AND
    #' finally all of the four-way interactions. So don't do a "^2" and a "^3" and a "^4" 
    #' because you'll have a significant amount of duplication. Just do the maximum amount
    #' of interaction that you plan on having at all, then feature select from there. I would
    #' suggest doing something high at first like "^7" (crazy, I know) and then remove all of
    #' the non-interaction variables.




# write function ---------------------------------------------------------------------------

    calc_7_way_interaction <- function(p_df, just_interactions=T, sparse=T) {
        # pass in a data.frame
        # defaults to pre-scaling & pre-centering your numeric features being passed in
            # it doesn't hurt to rescale/recenter features twice, it just won't do anything
        # defaults to only returning interactions and not the raw values
        # defaults to sparse matrix being returned
        # will also return feature names per sparse matrix "Dimnames"[[2]] values
        
        # no scaling or normalization, do that stuff outside of here if you want it
        
        # generate either the sparse or dense 
        if(sparse) {
            mat <- Matrix::sparse.model.matrix(~ .^7 - 1, p_df)
        } else {
            mat <- model.matrix(~ . ^7 - 1, p_df)
        }
        
        # limit to just interactions if that's what we're after
        if(just_interactions) {
            nbr_cols <- ncol(p_df)
            mat <- mat[, (nbr_cols + 1):ncol(mat)]
        } 
        
        # standard interface for column names regardless of if it's sparse or dense matrix
        if(sparse) {
            col_names <- attr(mat, "Dimnames")[[2]]
        } else {
            col_names <- attr(mat, "dimnames")[[2]]
        }
        
        # replace the ":" with something else to separate interaction features
        col_names <- gsub(":", "_i_", col_names)
        
        # return 
        return_list <- list(spmat, col_names)
        return(return_list)
    } 

    

    pprange <- preProcess(airquality, method="range")
    aq_ranged <- predict(pprange, airquality)
    
    hist(aq_ranged$Ozone, col='light blue', breaks=20)
    hist(airquality$Ozone, col='light green', breaks=20)
    head(aq_ranged$Ozone)
    summary(aq_ranged$Ozone)
    summary(airquality$Ozone)
    
    x <- calc_7_way_interaction(airquality)
    
    
    head(x)
    
    dim(x[[1]])
    x[[2]]

    



# Cross validated question:
    
    # taking a small sample of "airquality" data
    set.seed(2)
    my_aq <- data.frame(airquality[sample(1:nrow(airquality), 100), ])
    
    # create a scaled/centered version
    my_aq_pp_scaler <- caret::preProcess(my_aq, method=c("center", "scale"))
    my_aq_scaled <- predict(my_aq_pp_scaler, my_aq)
    
    
    # computing interactions with pre-scaled data
    denmat_prescaled <- as.data.frame(model.matrix(~ .^2 - 1, data=my_aq_scaled))
    hist(denmat_prescaled$`Ozone:Solar.R`, col='light blue', main="Pre-interaction-scale: Not Rescaled")  
    
        # 1) do I need to scale/center again?
        denmat_pp_scaler <- caret::preProcess(denmat_prescaled, method=c("center", "scale"))
        denmat_prescaled_scaled <- predict(denmat_pp_scaler, denmat_prescaled)
        hist(denmat_prescaled_scaled$`Ozone:Solar.R`, col='light pink', main="Pre-interaction-scale: Also Rescaled")
        
        
    
    # postscaled - not scaling until AFTER interactions have been computed
    denmat2 <- model.matrix(~ .^3 - 1, data=my_aq)
    denmat3 <- denmat2[, (ncol(my_aq) + 1):ncol(denmat2)]
    df3 <- as.data.frame(denmat3)
    
    
    
    denmat2_pp_scaler <- caret::preProcess(denmat2, method=c("center", "scale"))
    denmat_postscaled <- as.data.frame(predict(denmat2_pp_scaler, denmat2))
    hist(denmat_postscaled$`Ozone:Solar.R`, col='light green', main="No Pre-scale: Just Post-interaction-scale")        
    
    
    # examine difference
    denmat_scaled[1, 7:17]
    denmat2_scaled[1, 7:17]
    
    denmat_df <- as.data.frame(denmat_scaled)
    denmat2_df <- as.data.frame(denmat2_scaled)
    
    hist(denmat_df$`Ozone:Solar.R`, col='light blue', breaks=40)
    hist(denmat2_df$`Ozone:Solar.R`, col='light blue', breaks=40)
    


# end cross validated question:




    
    my_aq <- data.frame(airquality)
    sapply(my_aq, function(x) sum(is.na(x))) 
    sum(complete.cases(my_aq)) # 111 complete cases; 153 rows total
    
    my_aq_pp_scaler <- caret::preProcess(my_aq, method=c("center", "scale"))
    my_aq_scaled <- predict(my_aq_pp_scaler, my_aq)
    
    
    
    # prescaled
    denmat <- model.matrix(~ .^4 - 1, data=my_aq_scaled)
    denmat_pp_scaler <- caret::preProcess(denmat, method=c("center", "scale"))
    denmat_scaled <- predict(denmat_pp_scaler, denmat)
    
    
    # postscaled
    denmat2 <- model.matrix(~ .^4 - 1, data=my_aq)
    denmat2_pp_scaler <- caret::preProcess(denmat2, method=c("center", "scale"))
    denmat2_scaled <- predict(denmat2_pp_scaler, denmat2)
    
    # examine difference
    denmat_scaled[1, 7:17]
    denmat2_scaled[1, 7:17]
    
    denmat_df <- as.data.frame(denmat_scaled)
    denmat2_df <- as.data.frame(denmat2_scaled)
    
    hist(denmat_df$`Ozone:Solar.R`, col='light blue', breaks=40)
    hist(denmat2_df$`Ozone:Solar.R`, col='light blue', breaks=40)
    
    
    denmat_scaled[1, 1:10] == denmat2_scaled[1, 1:10]
    
    
    round(denmat_scaled, 3) == round(denmat2_scaled, 3)
        
            
    
# Clean Procedural code ----------------------------------------------------------------------------------------
    
    # 1: limit data to single row
        df_num_onerow <- df_num[1,]
        df_num_onerow_na <- df_num_na[1, ]
        
    
            # # 2: run the single row version through stats::model.matrix to create dense matrix with interactions
            #     
            #     # DEFAULT options()$na.action is "na.omit" -- I want to change it to "na.pass" to retain NAs in the resulting model matrix
            #     options()$na.action
            #     options(na.action="na.pass")
            #     
            #     # run the single row through to get dense version of matrix
            #     df_num_densemat <- stats::model.matrix(~ .^2 - 1, data=df_num_onerow)
            #     df_num_densemat_na <- stats::model.matrix(~ .^2 - 1, data=df_num_onerow_na)
            #     
            #         assert_that(all(dim(df_num_densemat) == dim(df_num_densemat)))
            #     
            # # 3: save the names of the dense matrix to a character vector object
            #     
            #     df_num_densemat_names <- attr(df_num_densemat, "dimnames")[[2]]
            #     df_num_densemat_names <- gsub(":", "-", df_num_densemat_names)  # <-- prefer dash over colon
            #     df_num_densemat_names_justinter <- df_num_densemat_names[(ncol(df_num) + 1):ncol(df_num_densemat)]
            #     df_num_densemat_names  # <-- all names including the raw columns
            #     df_num_densemat_names_justinter  # <-- just the names of the interaction columns
    
                    
    # 4: run the full data through the sparse model matrix
        
        #' ok so steps 2 and 3 are not necessary... even sparse matrices capture their respective column names
        
        df_num_sparmat <- Matrix::sparse.model.matrix(~ .^2 - 1, data=df_num)
        df_num_sparmat_na <- Matrix::sparse.model.matrix(~ .^2 - 1, data=df_num_na)
        
        # get the column names
        attributes(df_num_sparmat)
        attr(df_num_sparmat, "Dimnames")[[2]]
        
            dim(df_num_sparmat)
            assert_that(all(dim(df_num_sparmat) == dim(df_num_sparmat_na)))    
            assert_that(all(ncol(df_num_sparmat) == ncol(df_num_densemat)))
            
            
    # 5: generate xgb.DMatrix
        df_num_dmat <- xgb.DMatrix(df_num_sparmat)
        df_num_dmat        
        attributes(df_num_dmat)
        
        #' oh wait, 
        
                
# Dirty procedural stuff down here -------------------------------------------------------------
    
    
    # dense matrix generation (minus 1 is to remove the intercept of all 1's)
    mat_inter <- stats::model.matrix(~ .^3 - 1, data=df_num)
    mat_just_inter <- mat_inter[, (ncol(df_num) + 1):ncol(mat_inter)]
    mat_inter
    attributes(mat_inter)
    colnames <- attr(mat_inter, "dimnames")[[2]]
    mat_just_inter
    
        # convert to df
        df_inter <- as.data.frame(mat_inter)
        names(df_inter) <- allnames
        df_inter
        
        # convert only the interactions to df
        df_just_inter <- as.data.frame(mat_just_inter)
        names(df_just_inter) <- combo_names
        df_just_inter
    
    # sparse matrix generation (minus 1 is to remove the intercept of all 1's)
    spmat_inter <- Matrix::sparse.model.matrix(~ .^2 - 1, data=df_num)
    spmat_inter
        
        # equal dimensions; equal values
        assert_that(all(dim(mat_inter) == dim(spmat_inter)))
        assert_that(all(spmat_inter == mat_inter))
    
    
    
    # four columns, six rows; I want multiplication interaction between all of these to start with
    combo_indx_norep <- gtools::combinations(n=ncol(df), r=2, v=1:ncol(df), set=T, repeats.allowed=F)
    combo_indx_norep
    
    for(i in 1:nrow(mymat)) {
        print(i)
    }
    
    

