#' Plurality Method to Find Absolute or Relative Majority
#' 
#' Although with plurality method each voter is required to mention 
#' only one candidate, a ballot with more than one candidate and 
#' different scores is also valid. For a score matrix, the function will 
#' check the position j which has the lowest score (in a \code{vote} object, 
#' the lower, the better) in the ith row. Duplicated values may or may 
#' not be a problem. For instance, \code{c(2, 3, 3)} is valid, for the 
#' lowest value is 2 and it is in the 1st position. However, 
#' \code{c(2, 2, 3)} is a problem, for the 1st and 2nd positions 
#' all have the lowest value 2. If this problem exists, the winner 
#' returned by this function will be NULL.
#' 
#' @param x an object of class \code{vote}.
#' @param allow_dup whether ballots with duplicated score values 
#' are taken into account. Default is TRUE.
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
#'   \item (7) \code{winner} the winners, may be one, more than one or NULL.
#'   \item (8) \code{absolute} whether the winner is of absolute majority.
#'   \item (9) \code{other_info} a list with 2 elements, the 1st is the 
#' frequencies of candidates mentioned as 1st choice; the second element is 
#' the percentage. If winner is NULL, these two are NULL.
#' }
#' 
#' @export
#' @examples
#' raw <- rbind(
#' 	c(1, 2, 5, 3, 3), c(1, 2, 5, 3, 4), c(1, 2, 5, 3, 4), 
#' 	c(NA, NA, NA, NA, NA), c(NA, 3, 5, 1, 2), 
#' 	c(NA, NA, NA, 2, 3), c(NA, NA, 1, 2, 3), 
#' 	c(NA, NA, NA, NA, 2), c(NA, NA, NA, 2, 2), 
#' 	c(NA, NA, 1, 1, 2), c(1, 1, 5, 5, NA)
#' )
#' vote <- create_vote(raw, xtype = 1)
#' y <- plurality_method(vote, allow_dup = FALSE)
#' y <- plurality_method(vote, allow_dup=FALSE, min_valid = 3)
plurality_method <-
function(x, allow_dup = TRUE, min_valid = 1) {
    method <- "plurality"
    if (!class(x)[1] == "vote") 
        stop("x must be a vote object.")
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    candidate <- x$candidate
    NBALLOT <- x$ballot_num
    candidate_num <- x$candidate_num
    stopifnot(allow_dup %in% c(TRUE, FALSE))
    
    should_del <- c()
    if (allow_dup == FALSE & length(x$row_with_dup) != 0) 
        should_del <- append(should_del, x$row_with_dup)
    if (length(x$row_with_na) > 0) {
        get_na_ok <- which(x$num_non_na < min_valid)
        if (length(get_na_ok) > 0) 
            should_del <- append(should_del, x$row_with_na[get_na_ok])
    }
    should_del <- unique(should_del)
    length_should_del <- length(should_del)
    VALID_BALLOT_NUM <- NBALLOT - length_should_del
    if (VALID_BALLOT_NUM == 0) 
        stop("No ballot is OK.")
    x <- if (length_should_del > 0) 
        x$ballot[-should_del, ] else x$ballot
    
    message("SELECTING")
    mAjtOp <- function(x) if (length(which(x == min(x, na.rm = TRUE))) == 1) 
        which.min(x) else -1
    W <- apply(x, 1, mAjtOp)
    
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = NBALLOT, valid_ballot_num = VALID_BALLOT_NUM, 
        winner = NULL, absolute = NULL, other_info = NULL)
    
    message("COLLECTING RESULT")
    if (any(W == -1)) {
        over$other_info <- list(count = NULL, percent = NULL)
    } else {
        W <- append(W, 1:candidate_num)
        tta <- table(W) - 1
        table_name <- candidate[as.numeric(names(tta))]
        each_have <- as.numeric(tta)
        whowin <- which(each_have == max(each_have))
        over$winner <- candidate[whowin]
        percent <- each_have/sum(each_have)
        over$absolute <- if (any(percent > 0.5)) TRUE else FALSE
        names(each_have) <- table_name
        names(percent) <- table_name
        over$other_info <- list(count = each_have, percent = percent)
    }
    message("DONE")
    return(over)
}
