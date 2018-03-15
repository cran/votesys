#' Ranked Pairs Method
#' 
#' It is also called Tideman method. See details.
#' 
#' The method first summarizes the result of pairwise comparison, 
#' the order used is the order of winning votes from large to small. 
#' So if pairwise comparison has ties (that is, the number of voters 
#' who prefer a than b is equal to the number of voters who prefer 
#' b than a, the method will fail, and the winner will be NULL). 
#' 
#' The second step is called tally. 
#' If a wins b with 100 votes, b wins c with 80 votes, then 
#' we put a-b-100 ahead of b-c-80. Suppose a wins b with 100 votes, 
#' a wins c with 100 votes, then we have a tie; so we have to check 
#' the relation between b and c. If b wins c, then we put a-c-100 
#' ahead of a-b-100. Suppose a wins b with 100 votes, d wins b with 
#' 100 votes, then again we have a tie and have to check the a-d 
#' relation. If d wins a, then we put d-b-100 ahead of a-b-100. Suppose 
#' a wins b with 100 votes, e wins f with 100 votes, then the ties cannot 
#' be solved, so the winner will be NULL. 
#' 
#' The third step, after the above mentioned tally, is called lock-in.
#' As the relations have been sorted according to their strength 
#' from large to small in the tally step, we now add them one 
#' by one. The rule is: if a relation is contradictory with those 
#' already locked in relations, this relation will be discarded.
#' 
#' For example, suppose we have already add relation a > b and 
#' b > c, then the two relations are locked in. As a result, we should 
#' not add b > a. Also, as a > b and b > c indicate a > c, so we should 
#' not add c > a. After this process, we will finally find the winner who 
#' defeats all others.
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
#'   \item (6) \code{valid_ballot_num} the number of ballots that are 
#' actually used to compute the result. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (7) \code{winner} the winner, may be NULL.
#'   \item (8) \code{input_object} the class of x. 
#'   \item (9) \code{cdc} the Condorcet matrix which is actually used.
#'   \item (10) \code{dif} the score difference matrix. When 
#' x is not a \code{vote} object, it may be NULL.
#'   \item (11) \code{binary} win and loss recorded with 1 (win), 
#' 0 (equal) and -1 (loss).
#'   \item (12) \code{summary_m} times of win (1), equal (0) 
#' and loss (-1).
#'   \item (13) \code{other_info} a list of 3 elements. The 1st 
#' is the reason of failure. If winner exists, it will be blank. The 2nd 
#' is the tally result (it may contain unsolved ties). 
#' The 3rd is the lock-in result; if the method fails, 
#' it will be NULL.
#' }
#' 
#' @export
#' @examples
#' raw <- c(
#' 	"98:Abby>Cora>Erin>Dave>Brad",
#' 	"64:Brad>Abby>Erin>Cora>Dave",
#' 	"12:Brad>Abby>Erin>Dave>Cora", 
#' 	"98:Brad>Erin>Abby>Cora>Dave", 
#' 	"13:Brad>Erin>Abby>Dave>Cora", 
#' 	"125:Brad>Erin>Dave>Abby>Cora", 
#' 	"124:Cora>Abby>Erin>Dave>Brad", 
#' 	"76:Cora>Erin>Abby>Dave>Brad", 
#' 	"21:Dave>Abby>Brad>Erin>Cora", 
#' 	"30:Dave>Brad>Abby>Erin>Cora", 
#' 	"98:Dave>Brad>Erin>Cora>Abby", 
#' 	"139:Dave>Cora>Abby>Brad>Erin", 
#' 	"23:Dave>Cora>Brad>Abby>Erin"
#' ) 
#' raw <- list2ballot(string = raw)
#' vote <- create_vote(raw, xtype = 3, candidate = c('Dave', 'Cora', 'Brad', 'Erin', 'Abby'))
#' win1 <- cdc_simple(vote) # no winner
#' win2 <- cdc_rankedpairs(vote) # winner is Brad
cdc_rankedpairs <-
function(x, allow_dup = TRUE, min_valid = 1) {
    method <- "rankedpairs"
    if (min_valid < 1) 
        stop("Minimux number of min_valid is 1.")
    if (!class(x)[1] %in% c("vote", "matrix", "condorcet")) 
        stop("x must be a vote, condorcet or matrix object.")
    stopifnot(allow_dup %in% c(TRUE, FALSE))
    
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
    
    result_ID <- 0  # decide the type of result
    
    message("------PAIRWISE")
    win_side <- c()
    lose_side <- c()
    win_num <- c()
    lose_num <- c()
    pair_have_tie <- 0
    for (i in 1:nrow(cdc_matrix)) {
        for (j in 1:ncol(cdc_matrix)) {
            if (i > j) {
                ij <- cdc_matrix[i, j]
                ji <- cdc_matrix[j, i]
                if (ij >= ji) {
                  win_side <- append(win_side, candidate[i])
                  lose_side <- append(lose_side, candidate[j])
                  win_num <- append(win_num, ij)
                  lose_num <- append(lose_num, ji)
                  if (ij == ji) 
                    pair_have_tie <- 1
                } else if (ij < ji) {
                  win_side <- append(win_side, candidate[j])
                  lose_side <- append(lose_side, candidate[i])
                  win_num <- append(win_num, ji)
                  lose_num <- append(lose_num, ij)
                }
            }
        }
    }
    tally <- data.frame(win_side, lose_side, win_num, lose_num, stringsAsFactors = FALSE)
    if (pair_have_tie == 0) 
        result_ID <- 1
    
    # if pairwise has no tie, then result_ID=1, then go ahead
    if (result_ID == 1) {
        message("------SORTING")
        tally_o <- order((-1) * tally[, 3], tally[, 4])
        tally <- tally[tally_o, ]
        nr_tally <- nrow(tally)
        only_num_df <- tally[, c(3, 4)]
        if (nr_tally == nrow(unique(only_num_df))) {
            # do NOT use uniqueN
            result_ID <- 2
        } else {
            BI_ZERO_ONE <- binary_m
            BI_ZERO_ONE[BI_ZERO_ONE == -1] <- 0
            re_tally <- RP_TIE_SOLVE(x = tally, zeroone = BI_ZERO_ONE)
            if (re_tally[[1]] == TRUE) result_ID <- 2
            tally <- re_tally[[2]]
        }
    }
    
    # if sort has no tie, then result_ID=2, then go ahead
    if (result_ID == 2) {
        message("------LOCKING IN")
        name_m <- as.matrix(tally[, c(1, 2)])
        the_source <- lock_winner(name_m, CAND = candidate)
        winner <- the_source[[2]]
        LOCK_IN <- the_source[[1]]
    }
    
    message("COLLECTING RESULT")
    over <- list(call = match.call(), method = method, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, 
        valid_ballot_num = valid_ballot_num, winner = NULL, input_object = class1, cdc = cdc_matrix, 
		dif = dif_matrix, binary = binary_m, summary_m = summary_m, other_info = NULL)
    
    if (result_ID == 0) {
        over$other_info <- list(failure = "Pairwise comparison has ties, i. e., the number of people who prefer i than j is equal to the number of people who prefer j than i.", 
            tally = tally, lock_in = NULL)
    }
    
    if (result_ID == 1) {
        over$other_info <- list(failure = "There are unsolved ties when sorting the tally.", tally = tally, lock_in = NULL)
    }
    
    if (result_ID == 2) {
        over$winner <- winner
        over$other_info <- list(failure = "", tally = tally, lock_in = LOCK_IN)
    }
    class(over) <- "condorcet"
    message("DONE")
    return(over)
}
