

# testing the setup of multiple interactions

library(gtools)     # <-- combinations and permutations (for column name combos)
library(assertthat) # <-- for testing



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


# 1: limit data to single row
    df_num_onerow <- df_num[1,]
    df_num_onerow_na <- df_num_na[1, ]
    

# 2: run the single row version through stats::model.matrix to create dense matrix with interactions
    
    # DEFAULT options()$na.action is "na.omit" -- I want to change it to "na.pass" to retain NAs in the resulting model matrix
    options()$na.action
    options(na.action="na.pass")
    
    # run the single row through to get dense version of matrix
    df_num_densemat <- stats::model.matrix(~ .^2 - 1, data=df_num_onerow)
    df_num_densemat_na <- stats::model.matrix(~ .^2 - 1, data=df_num_onerow_na)
    
        assert_that(all(dim(df_num_densemat) == dim(df_num_densemat)))
    
# 3: save the names of the dense matrix to a character vector object
    
    df_num_densemat_names <- attr(df_num_densemat, "dimnames")[[2]]
    df_num_densemat_names <- gsub(":", "-", df_num_densemat_names)  # <-- prefer dash over colon
    df_num_densemat_names_justinter <- df_num_densemat_names[(ncol(df_num) + 1):ncol(df_num_densemat)]
    df_num_densemat_names  # <-- all names including the raw columns
    df_num_densemat_names_justinter  # <-- just the names of the interaction columns

                
# 4: run the full data through the sparse model matrix
    
    df_num_sparmat <- Matrix::sparse.model.matrix(~ .^2 - 1, data=df_num)
    df_num_sparmat_na <- Matrix::sparse.model.matrix(~ .^2 - 1, data=df_num_na)
    
        assert_that(all(dim(df_num_sparmat) == dim(df_num_sparmat_na)))    
        assert_that(all(dim(df_num_sparmat) == dim(df_num_densemat)))
        
        
        
        
        
# Procedural stuff down here -------------------------------------------------------------


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



