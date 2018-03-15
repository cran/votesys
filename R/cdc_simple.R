#' Ordinary Condorcet Method
#' 
#' Candidates enter into pairwise comparison. 
#' if the number of voters who prefer a is larger than the 
#' number of voters who prefer b, then a wins b, a gets 1 
#' point, b gets 0 point. If the numbers are equal, then both 
#' of them gets 0 point.
#' Suppose there are n candidates, the one gets n-1 
#' points wins (that is, he wins in all pairwise comparison).
#' There may be no Condorcet winner. If thus, you can 
#' try other Condorcet family methods.
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
#'   \item (7) \code{winner} the winner; may be NULL.
#'   \item (8) \code{input_object} the class of \code{x}. 
#'   \item (9) \code{cdc} the Condorcet matrix which is actually used.
#'   \item (10) \code{dif} the score difference matrix. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (11) \code{binary} win and loss recorded with 1 (win), 
#' 0 (equal) and -1 (loss).
#'   \item (12) \code{summary_m} times of win (1), equal (0) 
#' and loss (-1).
#'   \item (13) \code{other_info} currently nothing.
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
#' win1 <- cdc_simple(vote) # winner is n
#' win2 <- cdc_simple(win1$cdc) # use a Condorceit matrix
#' win2 <- cdc_simple(win1) # use an existent result
cdc_simple <-
function(x, allow_dup = TRUE, min_valid = 1) {
    method <- "simple"
    if (!class(x)[1] %in% c("vote", "matrix", "condorcet")) 
        stop("x must be a vote, condorcet or matrix object.")
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")  # each vote has at least one valid value\t
    stopifnot(allow_dup %in% c(TRUE, FALSE))
    
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
    
    winner <- which(summary_m[, 3] == candidate_num - 1)
    winner_num <- length(winner)
    winner <- if (winner_num == 0) NULL else candidate[winner]
    
    message("COLLECTING RESULT")
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = winner, input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, other_info = NULL)
    class(over) <- "condorcet"
    message("DONE")
    return(over)
}
