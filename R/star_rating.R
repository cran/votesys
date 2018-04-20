#' User Preference Aggregation
#' 
#' The function uses a simple method to 
#' calculate the aggregation scores of user 
#' ratings, which is described 
#' in Langville, A. and Meyer, C. (2012: 128). 
#' Input data can be stored in a sparse matrix.
#' Suppose there are 100 films and users are required to assign 
#' scores. However, each user only watched several of them. Thus, 
#' when comparing two films A and B, the method only takes account 
#' ratings from those who watched both A and B.
#' 
#' @param x a numeric matrix, or, \code{dgCMatrix} and 
#' \code{dgeMatrix} matrix created with the Matrix package.
#' 0 in the data means no score is given; and valid score values 
#' should all be larger than 0. The function will do NOTHING
#' to check the validity.
#' Besides, NA also means no score is given. If your data has NA, 
#' set \code{check_na} to TRUE so as to convert NA to 0.
#' @param show_name the default is FALSE, that is to say, the function 
#' does not store and show candidate names in the result and you 
#' cannot see them. However, you can set it to TRUE.
#' @param check_na if it is TRUE, the function will check NAs and convert 
#' them to 0s. If NAs do exist and \code{check_na} is FALSE, error will be 
#' raised.
#' 
#' @return a list object.
#' \itemize{
#'   \item (1) \code{call} the function call.
#'   \item (2) \code{method} the counting method.
#'   \item (3) \code{candidate} candidate names. If 
#' \code{show_name} is FALSE, this will be NULL.
#'   \item (4) \code{candidate_num} number of candidate.
#'   \item (5) \code{ballot_num} number of ballots in x.
#'   \item (6) \code{valid_ballot_num} number of ballots that are 
#' used to compute the result.
#'   \item (7) \code{winner} the winner. If \code{show_name} is FALSE, 
#' this only shows the number in \code{1: ncol(x)}.
#'   \item (8) \code{winner_score} the winner's score, which is the highest score.
#'   \item (9) \code{other_info} scores of all the candidates.
#' }
#'
#' @references
#' \itemize{
#'   \item Langville, A. and Meyer, C. 2012. 
#' Who's #1? The Science of Rating and Ranking. 
#' Princeton University Press, p. 128.
#' }
#'
#' @import Matrix
#' @export
#' @examples
#' # Example from Langville and Meyer, 2012: 128. 
#' # 4 films are rated by 10 users; 0 means no score.
#' raw <- c(4, 3, 1, 2, 0, 2, 0, 3, 0, 2, 2, 1, 0, 4, 3, 3, 4, 
#'     1, 3, 0, 2, 0, 2, 2, 2, 0, 1, 1, 2, 2, 0, 2, 0, 0, 5, 0, 3, 
#'     0, 5, 4
#' )
#' m <- matrix(raw, ncol = 4)
#' colnames(m) <- paste("film", 1: 4, sep = "")
#' y <- star_rating(m, show_name = TRUE) # winner is film4
star_rating=function(x, show_name=FALSE, check_na=TRUE){
	method="star"
	class1=class(x)[1]
	stopifnot(check_na %in% c(TRUE, FALSE))
	stopifnot(show_name %in% c(TRUE, FALSE))	
	if (! class1 %in% c("matrix", "dgCMatrix", "dgeMatrix")) stop('x must be of class matrix, dgeMatrix or dgCMatrix.')
	nc=ncol(x)
	nr=nrow(x)
	if (nc<2 | nr<2) stop('At least 2 rows and 2 columns.')
	candidate=colnames(x) 
	# later deal with null names
	colnames(x) = NULL
	if (check_na) x[is.na(x)] <- 0
	message("The function assumes that 0 means no score is given, and, all other valid score values should be larger than 0. The function do NOT automatically check validity, so please make sure.")
	
	# S matrix
	# num of co_raters
	message("SELECTING")
	s_matrix=Matrix::Matrix(0, nrow=nc, ncol=nc)
	
	# for every col of raw data, p, q
	for (p in 1: nc){
		for (q in 1: nc){
			if (p<q){
				rate_co=x[pmin(x[, p], x[, q]) != 0, c(p, q)]
				if (! is.vector(rate_co)){
					n_co=nrow(rate_co)
				} else {
					n_co=1
					rate_co=matrix(rate_co, nrow=1)
				}
				if (nrow(rate_co)>0){
					s_matrix[p, q] <- sum(rate_co[, 1])/n_co				
					s_matrix[q, p] <- sum(rate_co[, 2])/n_co
				}
			}
		}
	}
	
	# K matrix
	k_matrix=Matrix::Matrix(0, nrow=nc, ncol=nc)
	for (p in 1: nc){
		for (q in 1: nc){
			if (p>q){
				kpq=s_matrix[p, q]-s_matrix[q, p]
				k_matrix[p, q] <- kpq
				k_matrix[q, p] <- -kpq
			}
		}
	}
	
	# r
	r=Matrix::rowSums(k_matrix)/nc
	maxr=max(r)
	if (is.na(maxr)) stop("NAs exist in your data, please check !")
	winner=which(r==maxr)

    message("COLLECTING RESULT")
	if (show_name==FALSE){
		over <- list(call = match.call(), method = method, candidate = NULL, candidate_num = nc, ballot_num = nr, valid_ballot_num = nr, winner = winner, winner_score=maxr, other_info = list(all_score=r))
	}
	if (show_name==TRUE){
		candidate=if (!is.null(candidate)) candidate else fUll_dIgIt(nc)
		names(r)=candidate
		over <- list(call = match.call(), method = method, 
			candidate = candidate, 
			candidate_num = nc, ballot_num = nr, valid_ballot_num = nr, 
			winner = candidate[winner],
			winner_score=maxr, 
			other_info = list(all_score=r)
		)
	}
	if (length(over$winner)==0) over$winner=NULL
	message("DONE")
    return(over)
}
