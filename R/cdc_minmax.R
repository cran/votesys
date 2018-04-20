#' Minmax Method
#' 
#' Minmax method (also known as Simpson-Kramer method, 
#' successive reversal method) 
#' means three different methods.
#' The first is winning votes method. In pairwise comparison, 
#' if a wins b, a gets 0 point, the number of points for b is the 
#' number of voters who prefer a than b. 
#' The second method is to use margins. In pairwise comparison, 
#' a gets b - a points and b gets a - b points. 
#' The third method is pairwise opposition method. The number 
#' of points for a is the number of voters who prefer b than a; the 
#' number of points for b is the number of voters who prefer a 
#' than b. 
#' Although the point-assigning methods are different for the 
#' above three methods, they nonetheless do the same thing: 
#' to check to what extent one candidate is defeated by others. 
#' So the summarizing method is the same: for each candidate, 
#' we extract the maximum target points, and the one with the 
#' minimum points wins. 
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
#' @param variant should be 1, 2 or 3. 1 (default) for winning votes 
#' method, 2 for margins method, 3 for pairwise comparison method.
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
#'   \item (7) \code{winner} the winners.
#'   \item (8) \code{input_object} the class of x. 
#'   \item (9) \code{cdc} the Condorcet matrix which is actually used.
#'   \item (10) \code{dif} the score difference matrix. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (11) \code{binary} win and loss recorded with 1 (win), 
#' 0 (equal) and -1 (loss).
#'   \item (12) \code{summary_m} times of win (1), equal (0) 
#' and loss (-1).
#'   \item (13) \code{other_info} a list of 4 elements. The 1st is 
#' the method, which is equal to \code{variant}. The 2nd is the 
#' winning votes matrix. The 3rd is the margins matrix. The 4th 
#' is the pairwise comparison matrix.
#' }
#' 
#' @references
#' \itemize{
#'   \item https://en.wikipedia.org/wiki/Minimax_Condorcet_method
#' }
#'
#' @export
#' @examples
#' raw <- c(
#'     rep(c('m', 'n', 'c', 'k'), 42), rep(c('n', 'c', 'k', 'm'), 26), 
#'     rep(c('c', 'k', 'n', 'm'), 15), rep(c('k', 'c', 'n', 'm'), 17)
#' ) 
#' raw <- matrix(raw, ncol = 4, byrow = TRUE)
#' vote <- create_vote(raw, xtype = 2, candidate = c('m', 'n', 'k', 'c'))
#' win1 <- cdc_simple(vote)
#' win2 <- cdc_minmax(vote) # winner is n
#' win3 <- cdc_minmax(win1, variant = 2)
#' win4 <- cdc_minmax(win3$cdc, variant = 3)
cdc_minmax <-
function(x, allow_dup = TRUE, min_valid = 1, variant = 1) {
    method <- "minmax"
    if (!class(x)[1] %in% c("vote", "matrix", "condorcet")) 
        stop("x must be a vote, condorcet or matrix object.")
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    stopifnot(allow_dup %in% c(TRUE, FALSE))
    if (!variant[1] %in% c(1, 2, 3)) 
        stop("variant must be 1, 2 or 3.")
    
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
    
    nrc <- nrow(cdc_matrix)
    WV <- matrix(0, nrow = nrc, ncol = nrc)  # pairwise defeat (winning votes)
    rownames(WV) <- rownames(cdc_matrix)
    colnames(WV) <- colnames(cdc_matrix)
    MA <- WV  # worst pairwise defeat (margins)
    PO <- cdc_matrix  # worst pairwise opposition
    
    for (i in 1:nrc) {
        for (j in 1:nrc) {
            if (i > j) {
                ij <- cdc_matrix[i, j]
                ji <- cdc_matrix[j, i]
                ijdif <- ij - ji
                if (ijdif > 0) {
                  WV[i, j] <- 0
                  WV[j, i] <- cdc_matrix[i, j]
                }
                if (ijdif < 0) {
                  WV[i, j] <- cdc_matrix[j, i]
                  WV[j, i] <- 0
                }
                if (ijdif == 0) {
                  WV[i, j] <- 0
                  WV[j, i] <- 0
                }
                MA[i, j] <- -ijdif
                MA[j, i] <- ijdif
                PO[i, j] <- cdc_matrix[j, i]
                PO[j, i] <- cdc_matrix[i, j]
            }
        }
    }
    
    if (variant == 1) {
        row_max <- apply(WV, 1, max)
        winner <- which(row_max == min(row_max))
    }
    if (variant == 2) {
        row_max <- apply(MA, 1, max)
        winner <- which(row_max == min(row_max))
    }
    if (variant == 3) {
        row_max <- apply(PO, 1, max)
        winner <- which(row_max == min(row_max))
    }
    winner <- candidate[winner]
    
    message("COLLECTING RESULT")
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = winner, input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, other_info = list(variant = variant, 
            winning_votes = WV, margins = MA, pairwise_opposition = PO))
    class(over) <- "condorcet"
    message("DONE")
    return(over)
}
