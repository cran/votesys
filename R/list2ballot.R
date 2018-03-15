#' Repeat ith element of list x for j times to form a larger list
#'
#' Suppose you have 3 different unique ballots and the amount 
#' of each ballot is 10, 20, 30. Now you want to create raw 
#' ballots as a list. Then you can use this function. See examples
#' for usage.
#' @param x a list, each element of which should be a vector.
#' @param n how many times each element of x should be replicated.
#' It should be a numeric vector of non-negative integers and the 
#' length of it should be equal to that of \code{x}. 
#' The default is 1 for each element of \code{x}.
#' @param string default is NULL. If it is not NULL,  \code{x} 
#' and \code{n} are ignored. It should be a character vector. Each 
#' one contains two parts, the 1st is the amount of 
#' that ballot, and the 2nd part contains the names. The 1st and 
#' 2nd parts, as well as the names, should be split by spaces 
#' or punctuations. But no space and punctuation is allowed 
#' inside the names ("_" is not taken to be 
#' a punctuation). See examples.
#' 
#' @return a list with replicated vectors.
#' @export
#' @examples
#' # Use x and n
#' unique_ballot <- list(c("A", "B", "C"), c("F", "A", "B"),
#' 		c("E", "D", "C", "B", "F", "A"), c("x","x", "A")
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
list2ballot <- function(x, n = rep(1, length(x)), string = NULL) {
    if (is.null(string)) {
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
    if (!is.null(string)) {
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
            x[[i]] <- ii[-1]
        }
        n <- suppressWarnings(as.numeric(n))
        if (any(is.na(n))) 
            stop("Coercion fails. You should make sure the 1st valid part of each string is something numeric.")
    }
    nis0 <- which(n == 0)
    if (length(nis0) == length(n)) 
        stop("There is actually no ballot.")
    if (length(nis0) > 0) {
        n <- n[-nis0]
        x <- x[-nis0]
    }
    
    v2list2many <- function(x, ni) rep(list(x), times = ni)
    LL <- mapply(v2list2many, x, n, SIMPLIFY = FALSE)
    unlist(LL, recursive = FALSE)
}
	