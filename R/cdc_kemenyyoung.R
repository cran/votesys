#' Kemeny-Young Method
#' 
#' Kemeny-Young method first lists all the permutations of 
#' candidates, that is, all possible orders, or possible ordered 
#' links. Then, it computes the sums of strength of these links.
#' The top link is the one with the highest strength score, and 
#' the winner is the first one in this link. Currently, the maximum 
#' candidate number is 8 for speed and memory reasons.  
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
#' @param margin if it is FALSE (default), the values in Condorcet 
#' matrix are used, that is: if A vs. B is 30, B vs. A is 18, then 30 and 18 are 
#' used to calculate link strength; if it is TRUE, then 30 - 18 = 12 and 
#' -12 are used.
#' @param keep_all_link if TRUE, the result will store 
#' all the links and their strength. However, it is quite memory-costing, 
#' so the default is FALSE.
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
#'   \item (7) \code{winner} the winner.
#'   \item (8) \code{input_object} the class of \code{x}. 
#'   \item (9) \code{cdc} the Condorcet matrix which is actually used.
#'   \item (10) \code{dif} the score difference matrix. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (11) \code{binary} win and loss recorded with 1 (win), 
#' 0 (equal) and -1 (loss).
#'   \item (12) \code{summary_m} times of win (1), equal (0) 
#' and loss (-1).
#'   \item (13) \code{other_info} a list with 3 elements. \code{win_link} 
#' is the link with the highest strength. Note: it is a matrix, maybe with 2
#' or more rows. \code{win_link_value} is the strength of the link. 
#' \code{all_link} is NULL when \code{keep_all_link} is FALSE. if TRUE,  
#' it stores all the links and scores sorted by scores in decreasing order (this 
#' costs much memory on your computer).
#' }
#'
#' @references
#' \itemize{
#'   \item Young, H. & Levenglick, A. 1978. 
#' A consistent extension of Condorcet's election principle. 
#' Society for Industrial and Applied Mathematics, 35(2), 285-300.
#' }
#'
#' @export
#' @examples
#' m <- matrix(c(0, 58, 58, 58, 42, 0, 32, 32, 42, 68, 0, 17, 42, 68, 83, 0), nr = 4)
#' colnames(m) <- c('m', 'n', 'c', 'k')
#' rownames(m) <- c('m', 'n', 'c', 'k')
#' y <- cdc_kemenyyoung(m, keep_all_link = TRUE) # winner is n
cdc_kemenyyoung=function(x, allow_dup = TRUE, min_valid = 1, margin=FALSE, keep_all_link=FALSE){
	method <- "kemenyyoung"
    if (!class(x)[1] %in% c("vote", "matrix", "condorcet")) 
        stop("x must be a vote, condorcet or matrix object.")
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")  # each vote has at least one valid value\t
    stopifnot(allow_dup %in% c(TRUE, FALSE))
	stopifnot(margin %in% c(TRUE, FALSE))
    
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

	if (candidate_num > 8){
		message("You have ", candidate_num, " candidates.")
		stop("In Kemeny Young method, currently the maximum candidate number is 8.")
	}
	rm(x)
    
    summary_m <- sUmmAry_101(x = binary_m, rname = candidate)

	used_matrix=cdc_matrix
	if (margin==TRUE){
		used_matrix=matrix(0, nrow=candidate_num, ncol=candidate_num)
		for (i in 1: candidate_num){
			for (j in 1: candidate_num){
				if (i > j){
					ii_jj=cdc_matrix[i, j]-cdc_matrix[j, i]
					used_matrix[i, j]=ii_jj
					used_matrix[j, i]=-ii_jj
				}
			}
		}
	}

	message("SELECTING")
	if (keep_all_link==FALSE){
		LINKS=AlllInkstrEngth(used_matrix, candname=candidate, keep=FALSE)
		message("COLLECTING RESULT")
		over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = LINKS[[1]], input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, 
		other_info = list(win_link=LINKS[[2]], win_link_value=LINKS[[3]], all_link=NULL))
	} else {
		LINKS=AlllInkstrEngth(used_matrix, candname=candidate, keep=TRUE)
		message("COLLECTING RESULT")
		over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = LINKS[[1]], input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, 
		other_info = list(win_link=LINKS[[2]], win_link_value=LINKS[[3]], all_link=LINKS[[4]]))	
	}
		
    class(over) <- "condorcet"
    message("DONE")
    return(over)
}
