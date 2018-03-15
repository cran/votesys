#' Copeland Method
#' 
#' Candidates enter into pairwise comparison. 
#' if the number of voters who prefer a is larger than the 
#' number of voters who prefer b, then a wins b, a gets 1 
#' point, b gets -1 point. If the numbers are equal, then both 
#' of them gets 0 point.
#' Then, sum up each one's comparison points. 
#' For example, a wins 3 times, loses 1 time, has equal 
#' votes with 2 candidate, his score is 
#' 3 * 1 + (-1) * 1 + 0 * 2 = 2. 
#' The one gets the most points wins. Essentially, this 
#' is a way to solve ties in ordinary Condorcet method.
#' However, there may be 2 or more winners. The other 
#' type of Copeland method is to count only the times of wins, 
#' that is, the loser in pairwise comparison gets 0 point 
#' rather than -1 point.
#' 
#' @param x it accepts the following types of input:
#' 1st, it can be an object of class \code{vote}. 
#' 2nd, it can be a user-given Condorcet matrix, 
#' 3rd, it can be a result of another Condorcet method, 
#' which is of class \code{condorcet}.
#' @param allow_dup whether ballots with duplicated score values 
#' are taken into account. Default is TRUE.
#' @param min_valid default is 1. If the number of valid entries of 
#' a ballot is less than this value, it will not be used.
#' @param lose the point the pairwise loser gets, should be 
#' -1 (default) or 0.
#' 
#' @return a \code{condorcet} object, which is essentially 
#' a list.
#' \itemize{
#'   \item (1) \code{call} the function call.
#'   \item (2) \code{method} the counting method.
#'   \item (3) \code{candidate} candidate names.
#'   \item (4) \code{candidate_num} number of candidate.
#'   \item (5) \code{ballot_num} number of ballots in \code{x}. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (6) \code{valid_ballot_num} number of ballots that are 
#' actually used to compute the result. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (7) \code{winner} the winners.
#'   \item (8) \code{input_object} the class of \code{x}. 
#'   \item (9) \code{cdc} the Condorcet matrix which is actually used.
#'   \item (10) \code{dif} the score difference matrix. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (11) \code{binary} win and loss recorded with 1 (win), 
#' 0 (equal) and -1 (loss).
#'   \item (12) \code{summary_m} times of win (1), equal (0) 
#' and loss (-1).
#'   \item (13) \code{other_info} a list with 2 elements, the 1st is the point
#' the loser gets, it is equal to \code{lose}. The 2nd contains the scores.
#' }
#' 
#' @export
#' @examples
#' raw <- c(
#' 	rep(c('m', 'n', 'c', 'k'), 42), rep(c('n', 'c', 'k', 'm'), 26), 
#' 	rep(c('c', 'k', 'n', 'm'), 15), rep(c('k', 'c', 'n', 'm'), 17)
#' ) 
#' raw <- matrix(raw, ncol = 4, byrow = TRUE)
#' vote <- create_vote(raw, xtype = 2, candidate = c('m', 'n', 'k', 'c'))
#' win1 <- cdc_simple(vote)
#' win2 <- cdc_copeland(vote) # winner is n
#' win2 <- cdc_copeland(win1$cdc)
#' win3 <- cdc_copeland(win2, lose = 0)
cdc_copeland <-
function(x, allow_dup = TRUE, min_valid = 1, lose = -1) {
    method <- "copeland1"
    if (!class(x)[1] %in% c("vote", "matrix", "condorcet")) 
        stop("x must be a vote, condorcet or matrix object.")
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    stopifnot(allow_dup %in% c(TRUE, FALSE))
	stopifnot(lose %in% c(0, -1))
    
    CORE_M <- fInd_cdc_mAtrIx(x, dup_ok = allow_dup, available = min_valid)
    
    message("EXTRACTING INFO")
    class1 <- CORE_M$input_object
    candidate <- CORE_M$candidate
    candidate_num <- CORE_M$candidate_num
    ballot_num <- CORE_M$ballot_num
    valid_ballot_num <- CORE_M$valid_ballot_num
    cdc_matrix <- CORE_M$cdc  # find cdc matrix
    dif_matrix <- CORE_M$dif
    binary_m <- CORE_M$binary  # find binary
    
    message("SELECTING")
    summary_m <- sUmmAry_101(x = binary_m, rname = candidate)
	
	if (lose == -1){
		net_m <- t(apply(summary_m, 1, `*`, c(-1, 0, 1)))
		row_sumup <- rowSums(net_m)
   } else {
		to_zero <- binary_m
		to_zero[to_zero == -1] <- 0
		row_sumup <- rowSums(to_zero)
	}
    winner <- which(row_sumup == max(row_sumup))
    winner <- candidate[winner]
 
    message("COLLECTING RESULT")
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = winner, input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, 
		other_info = list(lose = lose, copeland_score = row_sumup))
    class(over) <- "condorcet"
    message("DONE")
    if (over$candidate_num < 5 & lose == -1) 
        message("It is better to have at least 5 candidates when lose = -1.")
    return(over)
}
