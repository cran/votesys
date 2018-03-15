#' Approval Method
#' 
#' In approval method, each voter is required to mention 
#' one or more candidates, and the winner is the one 
#' who gets the top frequency. For this function, a ballot 
#' with candidates more than required and 
#' different scores is also valid. For a score matrix, the function will 
#' check the positions j, k...which have the 
#' lowest scores (in a \code{vote} object, 
#' the lower, the better) in the ith row. However, the function will 
#' first check the \code{approval_able} element of 
#' the \code{vote} object. If it is FALSE, the winner will be NULL.
#'
#' @param x an object of class \code{vote}.
#' @param min_valid default is 1. If the number of valid entries of 
#' a ballot is less than this value, the ballot will not be used.
#' @param n the number of candidates written down by a 
#' voter should not larger than this value. 
#' 
#' @return a list object.
#' \itemize{
#'   \item (1) \code{call} the function call.
#'   \item (2) \code{method} the counting method.
#'   \item (3) \code{candidate} candidate names.
#'   \item (4) \code{candidate_num} number of candidate.
#'   \item (5) \code{ballot_num} number of ballots in x.
#'   \item (6) \code{valid_ballot_num} number of ballots that are 
#' used to compute the result.
#'   \item (7) \code{winner} the winners, may be one, more than one or NULL.
#'   \item (8) \code{n} equal to the argument \code{n}.
#'   \item (9) \code{other_info} frequencies of candidates mentioned 
#' by voters.
#' }
#' 
#' @export
#' @examples
#' raw <- matrix(NA, nrow = 22, ncol = 5)
#' for (i in 1: 20){
#'    set.seed(i)
#'    raw[i, ] <- sample(c(1: 5, NA, NA, NA), 5)
#' }
#' raw[21, ] <- c(4, 5, 3, 1, 2)
#' raw[22, ] <- c(3, 5, 1, 2, 4)
#' vote <- create_vote(raw, xtype = 1)
#' y <- approval_method(vote, n = 3)
#' y <- approval_method(vote, n = 3, min_valid = 5)
#' y <- approval_method(vote, n = 4, min_valid = 3)
approval_method <-
function(x, min_valid = 1, n) {
    method <- "approval"
    if (!class(x)[1] == "vote") 
        stop("x must be a vote object.")
	
	do_ok <- if (x$approval_able == FALSE) FALSE else TRUE 
		
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    candidate <- x$candidate
    NBALLOT <- x$ballot_num
    candidate_num <- x$candidate_num
    if (n < 1 | n > candidate_num) 
        stop("n should be >=1 and <= the number of candidates.")
    
	if (do_ok == TRUE){
		should_del <- c()
		if (length(x$row_with_na) > 0) {
			get_na_ok <- which(x$num_non_na < min_valid)
			if (length(get_na_ok) > 0) 
				should_del <- x$row_with_na[get_na_ok]
		}
		length_should_del <- length(should_del)
		VALID_BALLOT_NUM <- NBALLOT - length_should_del
		if (VALID_BALLOT_NUM == 0) 
			stop("No ballot is OK.")
		x <- if (length_should_del > 0) 
			x$ballot[-should_del, ] else x$ballot
		
		message("SELECTING")
		COUNT <- rep(0, candidate_num)
		for (i in 1:nrow(x)) {
			ii <- x[i, ]
			len_not_na <- length(ii[!is.na(ii)])
			ii <- order(ii)[1:min(len_not_na, n)]
			COUNT[ii] <- COUNT[ii] + 1
		}
		
		winner <- which(COUNT == max(COUNT))
		winner <- candidate[winner]
		names(COUNT) <- candidate
	}
	
	message("COLLECTING RESULT")
	if (do_ok == TRUE){
		over <- list(call = match.call(), method = method, candidate = candidate, 
		candidate_num = candidate_num, ballot_num = NBALLOT, valid_ballot_num = VALID_BALLOT_NUM, 
		winner = winner, n = n, other_info = list(count = COUNT))
	} else {
		over <- list(call = match.call(), method = method, candidate = candidate, 
		candidate_num = candidate_num, ballot_num = NBALLOT, valid_ballot_num = NULL, 
		winner = NULL, n = n, other_info = NULL)
		message("The winner is NULL, for approval can only be used when x$approval_able is TRUE.\n")
		message("You can ckeck x$row_with_dup to delete rows with duplicated values.\n")
	}
    message("DONE")
    return(over)
}
