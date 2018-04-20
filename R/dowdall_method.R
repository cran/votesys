#' Dowdall Method
#' 
#' This is an alternative Borda method. Voters are required 
#' to assign preference scores to every candidate and one 
#' score value cannot be shared by two or more candidates. 
#' For a voter, his 1st choice gets 1, his 2nd choice gets 
#' 1/2, his 3rd choice gets 1/3... The candidate who gets 
#' the most points wins. For the function 
#' \code{dowdall_method}, ranks, rather than true 
#' values, are used. So 1, 3, 5 are ranked as 1, 2, 3, and the 
#' scores are 1/1, 1/2, 1/3.
#' 
#' @param x an object of class \code{vote}. The ballots in 
#' the object should not have duplicated values and NAs.
#' @param stop default is FALSE, when ballots do have 
#' duplicated values or NAs, error will not be raised, but 
#' the winner will be NULL. If TRUE, an error will be raised.
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
#'   \item (7) \code{winner} the winners.
#'   \item (8) \code{other_info} total scores.
#' }
#'
#' @references
#' \itemize{
#'   \item https://en.wikipedia.org/wiki/Borda_count
#' }
#'
#' @export
#' @examples
#' raw <- list2ballot(string = 
#'     c("51: a>c>b>d", "5: c>b>d>a", "23: b>c>d>a", "21: d>c>b>a")
#' )
#' vote <- create_vote(raw, xtype = 3, candidate = c("a", "b", "c", "d"))
#' y1 <- borda_method(vote) # winner is c
#' y2 <- dowdall_method(vote) # winner is a
dowdall_method <- function(x, stop = FALSE) {
    method <- "dowdall"
    if (!class(x)[1] == "vote") 
        stop("x must be a vote object.")
    stopifnot(stop %in% c(TRUE, FALSE))
    candidate <- x$candidate
    NBALLOT <- x$ballot_num
    candidate_num <- x$candidate_num
    
    do_compute <- TRUE
    if (length(x$row_with_dup) > 0 | length(x$row_with_na) > 0) {
        if (stop == TRUE) {
            stop("Some ballots in x have duplicated values or NAs.")
        } else {
            winner <- NULL
            SCORE <- NULL
            VALID_BALLOT_NUM <- NULL
            do_compute <- FALSE
            message("Method fails, winner will be NULL.")
        }
    }
    
    if (do_compute) {
        message("SELECTING")
        VALID_BALLOT_NUM <- NBALLOT
        rankdivide <- apply(x$ballot, 1, FUN = function(x) 1/rank(x))
        SCORE <- rowSums(rankdivide)
        winner <- which(SCORE == max(SCORE))
        winner <- candidate[winner]
    }
    
    message("COLLECTING RESULT")
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = NBALLOT, valid_ballot_num = VALID_BALLOT_NUM, 
        winner = winner, other_info = list(score = SCORE))
    
    message("DONE")
    return(over)
}	
