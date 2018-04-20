#' Repeat ith element of list x or row of matrix/data.frames 
#' for j times
#'
#' Suppose you have 3 different unique ballots and the amount 
#' of each ballot is 10, 20, 30. Now you want to create raw 
#' ballots as a list. Then you can use this function. See examples
#' for usage.
#' @param x a list, each element of which should be a vector. Note: 
#' only one of \code{x}, \code{m} and \code{string} 
#' can be a non-NULL object
#' @param n how many times each element of \code{x} 
#' or each row of \code{m} should be replicated.
#' It should be a numeric vector of non-negative integers and the 
#' length of it should be equal to that of \code{x} or the row number 
#' of \code{m}. 
#' The default is 1 for each element of \code{x}.
#' @param m a matrix or dataframe, the number of rows should be 
#' equal to the length of n.
#' @param string default is NULL. If it is not NULL,  \code{x}, \code{m} 
#' and \code{n} are ignored. It should be a character vector. Each 
#' one contains two parts, the 1st is the amount of 
#' that ballot, and the 2nd part contains the names. The 1st and 
#' 2nd parts, as well as the names, should be split by spaces 
#' or punctuations. But no space and punctuation is allowed 
#' inside the names ("_" is not taken to be 
#' a punctuation). See examples.
#' 
#' @return a list with replicated vectors, if \code{x} is not NULL, 
#' or a matrix/data.frame with duplicated rows, if 
#' \code{m} is not NULL.
#' @export
#' @examples
#' # Use x and n
#' unique_ballot <- list(
#'     c("A", "B", "C"), c("F", "A", "B"),
#'     c("E", "D", "C", "B", "F", "A"), c("x","x", "A")
#' )
#' r <- c(1, 2, 3, 0)
#' y <- list2ballot(unique_ballot, r)
#' 
#' # Use string, x and n will be ignored.
#' # The characters can be written in a very loose way as follows, 
#' # for the function will automatically delete unwanted parts.
#' # But do make sure there is no space or punctuation
#' # inside the names.
#' unique_ballot <- c(
#' 	"2, Bob, Mike Jane", 	"3: barack_obama;;Bob>Jane", 
#' 	"0 Smith   Jane", "  1 Mike???!!!"
#' )
#' y <- list2ballot(string = unique_ballot)
#' # Use a matrix.
#' m <- matrix(c(1, 2, 3, 3, 1, 2), nrow = 2, byrow = TRUE)
#' colnames(m) <- c("p1", "p2", "p3")
#' r <- c(3, 5)
#' y <- list2ballot(m = m, n = r)
list2ballot <- function(x = NULL, n = rep(1, length(x)), m = NULL, string = NULL) {
	# 1 is not null, other all null
	stringnull <- ! is.null(string)
	mnull <- ! is.null(m)
	xnnull <- ! is.null(x) & ! is.null(n)
	null3 <- sum(stringnull+mnull+xnnull)
	if (null3 != 1) stop("One and only one of x/n, m, string should not be NULL.")

    if (xnnull) {
        if (!is.list(x)) 
            stop("x must be a list.")
        if (!is.numeric(n)) 
            stop("n must be numeric.")
        if (length(x) == 0) 
            stop("x must be of length larger than 0.")
        if (length(x) != length(n)) 
            stop("x and n must be of the same length.")
		all_cl <- sapply(x, is.vector)
		if (any(all_cl == FALSE)) stop("Every element of list x should be of class vector.")
    }
    if (stringnull) {
        if (!is.character(string)) 
            stop("string must be a character vector.")
        if (!is.vector(string)) 
            stop("string must be a character vector.")
        if (length(string) == 0) 
            stop("string must have at least 1 element.")
        string <- gsub("^\\W+|\\W+$", "", string)
        n <- c()
        x <- list()
        for (i in 1:length(string)) {
            ii <- unlist(strsplit(string[i], "\\W+"))
            if (length(ii) < 2) 
                stop("Each string must have two parts, the 1st is the amount of the ballots, the 2nd contains candidate names.")
            n[i] <- ii[1]
			iii <- ii[-1]
			iii[iii == "NA"] <- NA
            x[[i]] <- iii
        }
        n <- suppressWarnings(as.numeric(n))
        if (any(is.na(n))) 
            stop("Coercion fails. You should make sure the 1st valid part of each string is something numeric.")
    }
    nis0 <- which(n == 0)
    if (length(nis0) == length(n)) stop("There is actually no ballot.")
	if (xnnull == TRUE | stringnull == TRUE){
		if (length(nis0) > 0) {
			n <- n[-nis0]
			x <- x[-nis0]
		}
		LL=x[rep(seq_len(length(x)), n)]
		names(LL) <- NULL
	}
	if (mnull == TRUE){
		LL=m[rep(seq_len(nrow(m)), n), ]
		rownames(LL) <- NULL
		colnames(LL) <- colnames(m)
	}
	LL
}
	