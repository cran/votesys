#' Convert Incomplete ranking/rating matrix into full matrix
#' 
#' This function deals with incomplete ranking and rating matrix 
#' (e. g., created by \code{create_vote} and stored in \code{$ballot}), 
#' so as to convert it into full ranking and rating. 
#' In each row of the score matrix, the 
#' smallest value represents the most preferred 
#' and the biggest value represents the most hated.
#' For the methods used by this function, see Details.
#' See Examples for how to modify an object of class 
#' vote created with incomplete data.
#' 
#' Three methods are used and you should choose 
#' according to your need.
#' \itemize{
#'  \item (1) "valid": the default method. For the vector 
#' \code{c(3, 1, 2, 2, NA, NA)}, as there should be 6 values 
#' but only 4 are given, 4 is the valid number, and the NAs 
#' will be converted to 4. However, if the argument 
#' \code{plus} is a value other than 0, than NAs will be 
#' equal to the valid number plus that value. For example, 
#' if \code{plus = 10}, the NAs will be 14 (4 + 10).
#'  \item (2) "max": the maximum value in each row plus the value 
#' given by \code{plus}. So for \code{c(3, 1, 2, 2, NA, NA)}, 
#' and \code{plus = 0}, NAs will be 3 (3 + 0).
#'  \item (3) "len": In the case of topKlist, 
#' interviewees may, for example, choose 4 or 5 items 
#' from a 20-item list. When the method is "len", use \code{n} to 
#' indicate the total number of items or any other number. 
#' The default value of \code{n} is \code{ncol(x)}, 
#' which is equivalent to the way \code{create_vote} used to 
#' convert NAs so as to 
#' calculate the Condorcet matrix.
#' }
#' 
#' @param x the score matrix, should be a matrix, data.frame, 
#' or data.table.
#' @param method see Details, default is "valid".
#' @param plus see Details, default is 0.
#' @param n see Details, default is 0.
#' 
#' @return Always a matrix. NAs are converted to numbers. 
#' However, if all entries in a row of the input data are NAs, 
#' then that row will NOT be modified. NOTE: the order of 
#' the returned matrix (the 1st row, the 2nd row, the 3rd row, etc) 
#' is DIFFERENT from the input data.
#' 
#' @export
#' @examples
#' raw <- list2ballot(string = c("1: a, b, c", "2: b, c", "3: a, b"))
#' vote <- create_vote(raw, xtype = 3, candidate = c("a", "b", "c"))
#' ballot <- as_complete(vote$ballot, method = "max", plus = 5)
#' ballot <- as_complete(vote$ballot, method = "len", n = 10)
#' # Now re-create the vote object
#' vote <- create_vote(ballot, xtype = 1)
#' 
#' m <- matrix(c(
#'     1, 2, 3, NA, NA, NA,
#'     1, 1.1, 2.2, 8.8, NA, NA, 
#'     1, 1.1, 2.2, 8.8, NA, NA, 
#'     1, 1.1, 2.2, 8.8, NA, NA, 
#'     1, 1.1, 2.2, 8.8, NA, NA, 		
#'     NA, NA, NA, NA, NA, NA,
#'     3, 2, NA, NA, NA, NA, 
#'     3, 2, NA, NA,NA,NA,
#'     1, 2, 3, 4, 5, 6), ncol = 6, byrow = TRUE)
#' colnames(m) <- LETTERS[1: 6]
#' y <- as_complete(m, method = "valid", plus = 30)
as_complete <- function(x, method = c("valid", "max", "len"), plus = 0, n=NULL){
	classx <- class(x)[1]
	if (! classx %in% c("matrix", "data.table", "data.frame")) stop("x must be of class matrix, data.frame or data.table.")
	xnc <- ncol(x)
	nmx <- colnames(x)
	method <- method[1]
	if (! method %in% c("valid", "max", "len")) stop("method must be one of valid, max or len.")
	plus <- plus[1]
	if (! is.numeric(plus)) stop ("plus must be numeric.")
	LONG <- if (is.null(n)) xnc else n
	rownames(x) <- NULL
	if (classx != "data.table") x <- data.table::data.table(x)
    R <- data.table::frankv(x, ties.method = "dense")
    RT <- table(R)
    tlen <- length(RT)
    ttn <- as.numeric(names(RT))
    RT <- as.numeric(RT)
    xx <- matrix(NA, nrow = tlen, ncol = ncol(x))
    for (i in 1:tlen) {
        instance <- match(ttn[i], R)
        xx[i, ] <- as.numeric(x[instance, ])  # must add as.numeric
    }
    rm(x)
	
	InnEr_vAlId=function(v, ME, PL, vlen, long){
		posna=which(is.na(v))
		if (length(posna) %in% c(0, vlen)){
			return(v)
		} else {
			if (ME=="len"){
				v[posna]=long
			} else if(ME=="valid"){
				v[posna]=vlen-length(posna)+PL
			} else if (ME=="max"){
				v[posna]=max(v, na.rm=TRUE)+PL
			}
			return(v)
		}
	}
	y=t(apply(xx, 1, InnEr_vAlId, ME=method, PL=plus, vlen=xnc, long=LONG))
	y=y[rep(1: nrow(y), RT), ]
	colnames(y)=nmx
	y
}
