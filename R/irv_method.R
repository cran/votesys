#' Instant-Runoff Voting Method
#' 
#' Instant-runoff voting (IRV) method is also 
#' called alternative voting, 
#' transferable voting, ranked-choice voting, 
#' single-seat ranked-choice voting, or preferential voting.
#' In the 1st round, the candidate with absolute majority (that 
#' is, with more than 50 percent) wins. If no absolute winner exists, 
#' the one who gets the least votes is deleted, all other 
#' candidates enter into the 2nd round. Again, if no 
#' absolute winner exists, let the one with the least votes go and 
#' start the 3rd round... Finally, an absolute winner will 
#' appear. Ties are solved with different methods in reality; however, 
#' this function applies the following rules: (a) if more than 
#' one candidate gets the least votes, let all of them go; (b) if 
#' all the candidates get the same number of votes in a certain round, 
#' then all of them are winners. Note: the function accepts 
#' object of class \code{vote} and the method can only be 
#' used when x$approval_able is TRUE, that is, there is 
#' no duplicated values in the score matrix; otherwise, 
#' the winner will be NULL.
#' 
#' @param x an object of class \code{vote}.
#' @param min_valid default is 1. If the number of valid entries of 
#' a ballot is less than this value, the ballot will not be used.
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
#'   \item (7) \code{winner} the winners, may be NULL.
#'   \item (8) \code{absolute} whether the winner wins absolute majority in the
#' 1st round.
#'   \item (9) \code{other_info} the IRV may run for 2 or more rounds. So here 
#' the summary information of each round is recorded. The length of the list is 
#' equal to the number of rounds.
#' }
#'
#' @references
#' \itemize{
#'   \item Reilly, B. 2004. The global spread of preferential 
#' voting: Australian institutional imperialism? 
#' Australian Journal of Political Science, 39(2), 253-266.
#' }
#' 
#' @export
#' @examples
#' raw <- c(
#'    rep(c('m', 'n', 'c', 'k'), 42), rep(c('n', 'c', 'k', 'm'), 26), 
#'    rep(c('c', 'k', 'n', 'm'), 15), rep(c('k', 'c', 'n', 'm'), 17)
#' ) 
#' raw <- matrix(raw, ncol = 4, byrow = TRUE)
#' vote <- create_vote(raw, xtype = 2, candidate = c('m', 'n', 'k', 'c'))
#' y <- irv_method(vote) # winner is k
irv_method <-
function(x, min_valid = 1) {
    method <- "irv"
    if (!class(x)[1] == "vote") 
        stop("x must be a vote object.")
		
	do_ok <- if (x$approval_able == FALSE) FALSE else TRUE
   
   if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    candidate <- x$candidate
    NBALLOT <- x$ballot_num
    candidate_num <- x$candidate_num
 
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
		round_summary <- list()
		absolute <- FALSE
		winner_appear <- FALSE
		winner <- NULL
		deleted <- c()
		ROUND <- 1
		while (winner_appear == FALSE) {
			message("------ROUND: ", ROUND)
			W <- append(apply(x, 1, which.min), 1:candidate_num)
			tta <- table(W) - 1
			if (length(deleted) > 0) 
				tta <- tta[-deleted]
			ttn <- as.numeric(names(tta))
			names(tta) <- candidate[ttn]
			round_summary[[length(round_summary) + 1]] <- tta
			tta <- as.numeric(tta)
			perc <- tta/sum(tta)
			which_half <- which(perc > 0.5)
			if (length(which_half) > 0) {
				# if single wins
				winner_appear <- TRUE
				winner <- ttn[which_half]
			} else {
				whichmin <- which(tta == min(tta))
				if (length(whichmin) == length(tta)) {
					# if all have min
					winner_appear <- TRUE
					winner <- ttn[whichmin]
					message("More than one winner.")
				} else {
					for (i in ttn[whichmin]) x[, i] <- NA
					x <- x[apply(x, 1, FUN = function(x) !all(is.na(x))), ]
					deleted <- append(deleted, ttn[whichmin])
					ROUND <- ROUND + 1
				}
			}
		}
		winner <- candidate[winner]
		if (length(round_summary) == 1 & length(winner) == 1) 
			absolute <- TRUE
    }

    message("COLLECTING RESULT")
	if (do_ok == TRUE){
		over <- list(call = match.call(), method = method, candidate = candidate, 
		candidate_num = candidate_num, ballot_num = NBALLOT, valid_ballot_num = VALID_BALLOT_NUM, 
        winner = winner, absolute = absolute, other_info = round_summary)
	} else {
		over <- list(call = match.call(), method = method, candidate = candidate, 
		candidate_num = candidate_num, ballot_num = NBALLOT, valid_ballot_num = NULL, 
        winner = NULL, absolute = NULL, other_info = NULL)		
		message("The winner is NULL, for IRV can only be used when x$approval_able is TRUE.")
		message("You can ckeck x$row_with_dup to delete rows with duplicated values.")		
	}
    message("DONE")
    return(over)
}
