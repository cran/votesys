#' Check Ballots with Duplicated Values, Mistakes, or without Any Valid Entry
#'
#' The function simply checks validity of ballots and shows the check result. If 
#' you want a one-step clean, set \code{clean} to TRUE and a set of cleaned ballots
#' will be returned. Here, duplicated values mean that the voter write the 
#' same candidate more than one time, or, when 
#' he assigns scores, he assigns the same score
#' to more than one candidates. Mistakes are names that do not appear in the candidate 
#' list, or score values that are illegal (e.g., if voters are required to assign 1-5 to candidates, 
#' then 6 is an illegal value). Ballots without a valid entry (that is, all entries are NAs) are also
#' to be picked out. Different formats can be input into the function, see Details.
#'
#' The function accepts the following input:
#' \itemize{
#'  \item (1) when \code{xtype} is 1, x must be a matrix. Column names are candidate names (if
#' column names are NULL, they will be created: x1, x2, x3...). Candidate number is the number 
#' of columns of the matrix. Entry ij is the numeric score assigned 
#' by the ith voter to the jth candidate.
#'   \item (2) when \code{xtype} is 2, x can be a matrix or data.frame. Candidate 
#' number is the length of \code{candidate}.
#' Entries are names (character or numeric) of candidates. 
#' The i1, i2, i3... entries are the 1st, 2nd, 
#' 3rd... preferences of voter i.
#'   \item (3) when \code{xtype} is 3, \code{x} should be a list. 
#' Each element of the list is a ballot, a vector 
#' contains the names (character or numeric) of candidates. The 1st 
#' preference is in the 1st position of 
#' the vector, the 2nd preference is in the 2nd position... The number of candidates is the length 
#' of \code{candidate}; as a result, a ballot with number of names larger than candidate number is 
#' labelled as wrong.
#' }
#'
#' @param x a data.frame, matrix or list of raw ballots. See Details.
#' @param xtype should be 1, 2 (default) or 3, designating the type of x. See Details.
#' @param candidate if \code{xtype} is 1, this argument is ignored. 
#' If \code{xtype} is 2 or3, candidate names
#' must be given as a character or numeric vector. If a name is not given, 
#' but is still on a ballot, then the ballot is labelled as wrong.
#' @param vv if \code{xtype} is 2 or 3, it is ignored. 
#' If \code{xtype} is 1, this gives the valid score values for x.
#' @param isna entries which should be taken as NAs. \code{NA} in \code{x}  
# will always 
#' be taken as missing value, 
#' however, you can add more (e.g., you may use 99, 999 as missing values). 
#' If x contains characters, 
#' this argument should also be provided with a character vector, and 
#' if numeric, then numeric vector. Do 
#' not add NA to \code{isna}, because the default (NULL) 
#' means \code{NA} is already included.
#' @param clean the default is FALSE, that is, it does not return the cleaned data. If it is TRUE, 
#' a set of ballots without duplicated values, without mistakes and with 
#' at least one valid value, is 
#' returned.
#' 
#' @return a list with 3 or 4 elements: \code{row_with_dup} is 
#' the rows (not row names) of rows that have 
#' duplicated values; \code{row_with_wrong} is the rows with illegal names or the 
#' lengths of them are larger than candidate number (this could only happen when x 
#' is a list). \code{row_all_na} is the rows the entries of which are all NAs. For a list, 
#' elements with NULL are also taken as all-NA ballots.
#' 
#' @export
#' @examples
#' raw=list(
#' 	c('a', 'e', 'c', 'd', 'b'), 
#' 	c('b', 'a', 'e'), 
#' 	c('c', 'd', 'b'), 
#' 	c('d', 'a', 'b'), 
#' 	c('a', 'a', 'b', 'b', 'b'), 
#' 	c(NA, NA, NA, NA), 
#' 	v7=NULL, 
#' 	v8=c('a', NA, NA, NA, NA, NA, NA), 
#' 	v9=rep(" ", 3)
#' )
#' y=check_dup_wrong(raw, xtype=3, candidate=letters[1: 5])
#' y=check_dup_wrong(raw, xtype=3, candidate=letters[1: 4]) 
check_dup_wrong <-
function(x, xtype = 2, candidate = NULL, vv = NULL, isna = NULL, clean = FALSE) {
    class1 <- class(x)[1]
    cat("Class of raw data is: ", class1, "\n")
    if (!class1 %in% c("matrix", "data.frame", "list")) 
        stop("x must be a matrix, data.frame or list.")
    
    # xtype is 2, 3 check
    if (xtype %in% c(2, 3)) {
        if (xtype == 2) 
            cat("xtype is 2 means each row of the raw data contains the preference of a voter.\n")
        if (xtype == 3) 
            cat("xtype is 3 means each element of the raw data list contains the preference of a voter.\n")
        lengthcand <- length(candidate)
        if (lengthcand < 2) 
            stop("Less than 2 candidates.")
        if (any(is.na(candidate))) 
            stop("candidate should not have NA.")
        isna <- if (class1 == "matrix") 
            c(isna, NA) else c(as.character(isna), NA, "", " ")
        if (!class1 == "matrix") 
            candidate <- as.character(candidate)
        
        dup_row <- c()
        wrong_row <- c()
        all_na <- c()
        
        if (class1 == "list") {
            for (i in 1:length(x)) {
                ii <- as.character(x[[i]])
                lengthii <- length(ii)
                if (lengthii == 0) 
                  ii <- rep(NA, length(candidate))  # list element is NULL
                ii <- ii[!ii %in% isna]
                len_ii <- length(ii)
                if (len_ii == 0) {
                  all_na <- append(all_na, i)
                  if (lengthii > lengthcand) 
                    wrong_row <- append(wrong_row, i)
                } else {
                  if (any(!ii %in% candidate) | lengthii > lengthcand) 
                    wrong_row <- append(wrong_row, i)
                  if (anyDuplicated(ii) != 0) 
                    dup_row <- append(dup_row, i)
                }
            }
        }
        if (class1 == "data.frame") {
            for (j in 1:ncol(x)) {
                if (class(x[, j]) != "character") 
                  x[, j] <- as.character(x[, j])
            }
            for (i in 1:nrow(x)) {
                ii <- as.character(x[i, ])
                ii <- ii[!ii %in% isna]
                len_ii <- length(ii)
                if (len_ii > 0) {
                  if (any(!ii %in% candidate)) 
                    wrong_row <- append(wrong_row, i)
                  if (anyDuplicated(ii) != 0) 
                    dup_row <- append(dup_row, i)
                } else {
                  all_na <- append(all_na, i)
                }
            }
        }
        if (class1 == "matrix") {
            if (!class(x[1, 1]) %in% c("integer", "numeric", "character")) 
                stop("If x is a matrix, cells must be of class numeric, integer, character.")
            if (class(x[1, 1]) %in% c("integer", "numeric")) {
                if (!class(candidate) %in% c("integer", "numeric")) 
                  stop("If x is a matrix, cells must be of the same class as candidate.")
            } else {
                if (class(candidate) != "character") 
                  stop("If x is a matrix, cells must be of the same class as candidate.")
            }
            
            for (i in 1:nrow(x)) {
                ii <- x[i, ]
                ii <- ii[!ii %in% isna]
                len_ii <- length(ii)
                if (len_ii > 0) {
                  if (any(!ii %in% candidate)) 
                    wrong_row <- append(wrong_row, i)
                  if (anyDuplicated(ii) != 0) 
                    dup_row <- append(dup_row, i)
                } else {
                  all_na <- append(all_na, i)
                }
            }
        }
        
        # xtype is 1 check
    } else if (xtype == 1) {
        cat("xtype is 1 mean the colnames are candidates and cells contain the numeric values given by voters.\n")
        if (class1 == "matrix") {
            if (!is.numeric(x[1, 1])) 
                stop("x must be with numeric cells.")
        }
        if (class1 == "data.frame") 
            stop("If xtype is 1, x must be a matrix.")
        if (ncol(x) < 2) 
            stop("Less than 2 candidates.")
        if (is.null(vv)) {
            vv <- 1:ncol(x)
        } else {
            if (!is.numeric(vv)) 
                stop("If xtype is 1, vv must be numeric.")
            if (any(is.na(vv))) 
                stop("vv must not have NA.")
        }
        isna <- c(isna, NA)
        
        dup_row <- c()
        wrong_row <- c()
        all_na <- c()
        
        for (i in 1:nrow(x)) {
            ii <- x[i, ]
            ii <- ii[!ii %in% isna]
            len_ii <- length(ii)
            if (len_ii > 0) {
                if (any(! ii %in% vv)) 
                  wrong_row <- append(wrong_row, i)
                if (anyDuplicated(ii) != 0) 
                  dup_row <- append(dup_row, i)
            } else {
                all_na <- append(all_na, i)
            }
        }
    } else {
        stop("Sorry, currently xtype can only be 1, 2 or 3.")
    }
    
    result <- list(row_with_dup = dup_row, row_with_wrong = wrong_row, row_all_na = all_na)
    if (clean == TRUE) {
        rrr <- c(dup_row, wrong_row, all_na)
        rrr <- unique(rrr)
        if (length(rrr) > 0) {
            if (class1 == "list") {
                result$clean <- x[-rrr]
                if (length(result$clean) == 0) {
                  warning("No vote is left.")
                  result$clean <- NULL
                }
            } else {
                result$clean <- x[-rrr, ]
                if (nrow(result$clean) == 0) {
                  warning("No vote is left.")
                  result$clean <- NULL
                }
            }
        }
    }
    result
}
