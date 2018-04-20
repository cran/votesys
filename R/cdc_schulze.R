#' Schulze Method
#' 
#' Schulze method is essentially a widest path problem.
#' With the Condorcet matrix, we must find the so called 
#' the strongest path a > b > c > d, and the winner is a. 
#' The strength of a path is the strength of its weakest link.
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
#'   \item (5) \code{ballot_num} number of ballots in x. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (6) \code{valid_ballot_num} number of ballots that are 
#' actually used to compute the result. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (7) \code{winner} the winners, may be NULL.
#'   \item (8) \code{input_object} the class of x. 
#'   \item (9) \code{cdc} the Condorcet matrix which is actually used.
#'   \item (10) \code{dif} the score difference matrix. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (11) \code{binary} win and loss recorded with 1 (win), 
#' 0 (equal) and -1 (loss).
#'   \item (12) \code{summary_m} times of win (1), equal (0) 
#' and loss (-1).
#'   \item (13) \code{other_info} a list of 2 elements. The 1st is the strength 
#' comparison matrix. The 2nd is the strength comparison matrix in binary 
#' mode, 1 for win, 0 for else.
#' }
#'
#' @references
#' \itemize{
#'   \item Schulze, M. 2010. A new monotonic, 
#' clone-independent, reversal symmetric, 
#' and Condorcet-consistent single-winner election method. 
#' Social Choice and Welfare, 36(2), 267-303.
#' }
#'
#' @export
#' @examples
#' raw <- list2ballot(
#'     x = list(
#'         c('a', 'c', 'b', 'e', 'd'), 
#'         c('a', 'd', 'e', 'c', 'b'), 
#'         c('b', 'e', 'd', 'a', 'c'), 
#'         c('c', 'a', 'b', 'e', 'd'), 
#'         c('c', 'a', 'e', 'b', 'd'), 
#'         c('c', 'b', 'a', 'd', 'e'), 
#'         c('d', 'c', 'e', 'b', 'a'), 
#'         c('e', 'b', 'a', 'd', 'c')
#'     ), 
#'     n = c(5, 5, 8, 3, 7, 2, 7, 8)
#' )
#' vote <- create_vote(raw, xtype = 3, candidate = c('a', 'b', 'c', 'd', 'e'))
#' win1 <- cdc_simple(vote) # no winner
#' win2 <- cdc_schulze(vote) # winner is e
#' win2 <- cdc_schulze(win1)
cdc_schulze <-
function(x, allow_dup = TRUE, min_valid = 1) {
    method <- "schulze"
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    stopifnot(allow_dup %in% c(TRUE, FALSE))
    if (!class(x)[1] %in% c("vote", "matrix", "condorcet")) 
        stop("x must be a vote, condorcet or matrix object.")
    
    CORE_M <- fInd_cdc_mAtrIx(x, dup_ok = allow_dup, available = min_valid)
    
    message("EXTRACTING INFO")
    class1 <- CORE_M$input_object
    candidate <- CORE_M$candidate
    candidate_num <- CORE_M$candidate_num
    ballot_num <- CORE_M$ballot_num
    valid_ballot_num <- CORE_M$valid_ballot_num
    cdc_matrix <- CORE_M$cdc
    dif_matrix <- CORE_M$dif
    binary_m <- CORE_M$binary
    
    message("SELECTING")
    summary_m <- sUmmAry_101(x = binary_m, rname = candidate)
    
    strength_schu <- bOttlEnEck(cdc_matrix)
    nrc_str <- nrow(strength_schu)
    check_strength <- matrix(0, nrow = nrc_str, ncol = nrc_str)
    for (i in 1:nrc_str) {
        for (j in 1:nrc_str) {
            if (i != j) {
                if (strength_schu[i, j] > strength_schu[j, i]) {
                  check_strength[i, j] <- 1
                }
            }
        }
    }
    row_sumup <- rowSums(check_strength)
    winner <- which(row_sumup == candidate_num - 1)
    winner <- if (length(winner) == 0) 
        NULL else candidate[winner]
    
    message("COLLECTING RESULT")
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = winner, input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, other_info = list(strength = strength_schu, 
            binary_strength = check_strength))
    class(over) <- "condorcet"
    message("DONE")
    return(over)
}
