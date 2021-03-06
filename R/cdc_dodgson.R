#' Dodgson Method
#' 
#' The original Dodgson method checks the number of 
#' votes each candidate has to rob from other candidates; 
#' the winner is with the smallest number. However, the 
#' function \code{cdc_dodgson} uses two alternative methods 
#' rather than the original Dodgson method. The two methods 
#' are Tideman score method and Dodgson Quick method. 
#' See Details.
#' 
#' Suppose the candidates are A, B, C and D. If A wins B in pairwise 
#' comparison or has equal votes with B, then add 0 to A. If C wins A, 
#' then add to A adv(C, A), that is, the number of voters that prefer 
#' C than A, minus the number of voters that prefer A than A.  
#' Again, if D wins A, then add to A that number. Then, we sum up 
#' the points belong to A. We do the same thing to B, C and D. The one 
#' gets the least points is the winner. This is what we do in Tideman
#' score method. In Dodgson Quick method, we first compute the number 
#' of votes, then divide it by 2 and get the ceiling, and sum 
#' all of them up.
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
#' @param dq_t the alternative Dodgson methods to be used. 
#' Default is "dq", for Dodgson Quick method; it can also be "t", 
#' Tideman score method.
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
#'   \item (13) \code{other_info} a list with four elements. The 1st 
#' indicates the method used to compute score. The 2nd is the score 
#' for pairwise comparison
#' (number of votes one has to rob). The 3rd is Tideman score 
#' summary (the smaller the better). 
#' The 4th is Dodgson Quick summary (the smaller the better).
#' }
#' 
#' @references
#' \itemize{
#'   \item McCabe-Dansted, J. & Slinko, A. 2008. Approximability of Dodgson's Rule. Social Choice and Welfare, Feb, 1-26.
#' }
#' 
#' @export
#' @examples
#' raw <- list2ballot(
#'     x = list(
#'         c('A', 'B', 'C', 'D', 'E', 'F'), 
#'         c('F', 'A', 'B', 'C', 'D', 'E'),
#'         c('E', 'D', 'C', 'B', 'F', 'A'),
#'         c('B', 'A', 'C', 'D', 'E', 'F'),
#'         c('F', 'E', 'D', 'C', 'B', 'A'),
#'         c('F', 'B', 'A', 'C', 'D', 'E'),
#'         c('E', 'D', 'C', 'A', 'F', 'B'),
#'         c('E', 'B', 'A', 'C', 'D', 'F'),
#'         c('F', 'D', 'C', 'A', 'E', 'B'),
#'         c('D', 'B', 'A', 'C', 'E', 'F'),
#'         c('F', 'E', 'C', 'A', 'D', 'B')
#'     ), 
#'     n = c(19, 12, 12, 9, 9, 10, 10 , 10 , 10, 10, 10)
#' )
#' vote <- create_vote(raw, xtype = 3, candidate = c('A', 'B', 'C', 'D', 'E', 'F'))
#' win1 <- cdc_simple(vote) # no winner
#' win2 <- cdc_dodgson(vote, dq_t = "dq") # A
#' win2 <- cdc_dodgson(win1, dq_t = "dq") # A
#' win3 <- cdc_dodgson(vote, dq_t = "t") # B
#' win3 <- cdc_dodgson(win2, dq_t = "t") # B
cdc_dodgson <-
function(x, allow_dup = TRUE, min_valid = 1, dq_t = "dq") {
    method <- "dodgson"
    if (!class(x)[1] %in% c("vote", "matrix", "condorcet")) 
        stop("x must be a vote, condorcet or matrix object.")
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    stopifnot(allow_dup %in% c(TRUE, FALSE))
    if (!dq_t[1] %in% c("dq", "t")) 
        stop("dq_t must be dq or t.")
    
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
    swap_m <- matrix(0, nrow = nrc, ncol = nrc)
    rownames(swap_m) <- rownames(cdc_matrix)
    colnames(swap_m) <- colnames(cdc_matrix)
    for (i in 1:nrc) {
        for (j in 1:nrc) {
            if (i != j) {
                ij <- cdc_matrix[i, j]
                ji <- cdc_matrix[j, i]
                ijdif <- ij - ji # this is adv(a, b)
                if (ijdif > 0) 
                  swap_m[i, j] <- 0
                if (ijdif == 0) 
                  swap_m[i, j] <- 0
                if (ijdif < 0) 
                  swap_m[i, j] <- -ijdif
            }
        }
    }
    t_row_sumup <- rowSums(swap_m)
    dq_row_sumup <- apply(swap_m, 1, FUN = function(x) sum(ceiling(x/2)))
    if (dq_t == "t") {
        winner <- which(t_row_sumup == min(t_row_sumup))
    } else if (dq_t == "dq") {
        winner <- which(dq_row_sumup == min(dq_row_sumup))
    }
    winner <- candidate[winner]
    
    message("COLLECTING RESULT")
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = winner, input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, other_info = list(dq_t = dq_t, 
            swap = swap_m, tideman = t_row_sumup, dodgson_quick = dq_row_sumup))
    class(over) <- "condorcet"
    message("DONE")
    return(over)
}
