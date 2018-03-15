#' Create a vote Object that can be used in counting methods
#'
#' Some counting methods in this package only accept \code{vote} object created by 
#' this function. So the first step should always be using this function. The function 
#' will return the modified ballots and some other helpful information. See Details 
#' and Values.
#' 
#' The function accepts the following input:
#' \itemize{
#'  \item (1) when \code{xtype} is 1, x must be a matrix. Column names are candidate names (if
#' column names are NULL, they will be created: x1, x2, x3...). Candidate number is the number 
#' of columns of the matrix. Entry ij is the numeric score assigned by 
#' the ith voter to the jth candidate.
#'   \item (2) when \code{xtype} is 2, x can be a matrix or data.frame. 
#' Candidate number is the length of \code{candidate}. 
#' Entries are names (character or numeric) of candidates. 
#' The i1, i2, i3... entries are the 1st, 2nd, 
#' 3rd... preferences of voter i.
#'   \item (3) when \code{xtype} is 3, x should be a list. 
#' Each element of the list is a ballot, a vector 
#' contains the names (character or numeric) of candidates. 
#' The 1st preference is in the 1st position of 
#' the vector, the 2nd preference is in the 2nd position... The number of candidates is the length 
#' of \code{candidate}; as a result, a ballot with number of names 
#' larger than candidate number is 
#' labelled as wrong.
#' }
#' 
#' The function also returns Condorcet matrix. Suppose candidates are i, j, k. 
#' The voter likes i best, so he assigns 1 to i. The 2nd choice is j, so 
#' he assigns 2 to j, leaving k as NA. Now computing the Condorcet matrix: 
#' since i's score is smaller than j' score, we add 1 to the ij cell of the matrix, 
#' and add 0 to the ji cell. Candidate k's NA is automatically set to the 
#' highest (that is, the worst) score: 3 (since there are 3 candidates); i < k, so 
#' we add 1 to the ik cell and add 0 to ki cell. Besides, there is also a score 
#' difference matrix: we add 2 - 1 = 1 to the ij cell of score difference matrix, 
#' and add 3 - 1 = 2 to the ik cell. If tie appears, both sides acquire 0.
#' 
#' Note the ways we calculate the Condorcet matrix. (1) It allow ties, that is, 
#' duplicated score values. (2) NA is deems as the worst, which means: if a 
#' voter does not mention a candidate, the candidate will be given the 
#' highest (worst) score. (3) Ballots mention only one name are assumed 
#' to express preference, since unmentioned candidates are assumed to 
#' be equally hated. (4) The Condorcet matrix returned 
#' by \code{create_vote} uses ballots that may have duplicated values 
#' and have only one valid entry. However, Condorcet family 
#' methods in this package provide possibility to recalculate the matrix. 
#' And, the simplest way to get rid of duplicated values and NAs is 
#' to delete some ballots.
#'
#' @param x a data.frame, matrix or list of raw ballots. See Details.
#' @param xtype should be 1, 2 (default) or 3, designating the 
#' type of \code{x}. See Details.
#' @param candidate if \code{xtype} is 1, this argument is ignored. 
#' If \code{xtype} is 2 or 3, candidate names
#' must be given as a character or numeric vector. If a name is not given, 
#' but is still on a ballot, then the name is ignored !
#' @param isna entries which should be taken as NAs. 
#' \code{NA} in \code{x} will always be taken as missing value, 
#' however, you can add more (e.g., you may use 99, 999 as missing values). 
#' If x contains characters, 
#' this argument should also be provided with a 
#' character vector, and if numeric, then numeric vector. Do 
#' not add \code{NA} to \code{isna}, because the default (NULL)  
#' means \code{NA} is already included.
#'
#' @return an object of class \code{vote} is returned, which 
#' is essentially a list. It has the following elements.
#' \itemize{
#'   \item (1) \code{call} the call.
#'   \item (2) \code{ballot} the returned ballot. It is always a score matrix. 
#' The column names are candidate names; entries are numeric scores 
#' assigned by voters. Missing values are all set to NA.
#'   \item (3) \code{nas} those which are taken as NA in data cleaning.
#'   \item (4) \code{candidate} candidate names.
#'   \item (5) \code{candidate_num} number of candidates.
#'   \item (6) \code{ballot_num} number of ballots.
#'   \item (7) \code{ballot_at_least_one} number of ballots that mention
#' at least one candidate.
#'   \item (8) \code{cdc} the Condorcet matrix calculated with ballots 
#' that have no NA entries.
#'   \item (9) \code{cdc_with_na} the Condorcet matrix calculated with 
#' ballots that have at least one valid entry.
#'   \item (10) \code{dif} the score difference matrix calculated with 
#' ballots that have no NA entries.
#'   \item (11) \code{dif_with_na} the score difference matrix calculated 
#' with ballots that have at least one valid entry.
#'   \item (12) \code{row_with_na} rows of \code{ballot} with NAs.
#'   \item (13) \code{row_non_na} for rows with NAs, the number of 
#' non-NA entries of them.
#'   \item (14) \code{row_with_dup} rows of \code{ballot} with 
#' duplicated score values.
#'   \item (15) \code{approval_able} if length of \code{row_non_dup} 
#' is 0, then it is TRUE, else, FALSE. It indicates whether approval 
#' method can be used. When \code{xtype} is 2 or 3, it is always TRUE.
#' }
#' 
#' @export
#' @import data.table
#' @examples
#' # xtype is 2
#' raw <- c(
#' 	rep(c('m', 'n', 'c', 'k'), 42), 
#' 	rep(c('n', 'c', 'k', 'm'), 26), 
#' 	rep(c('c', 'k', 'n', 'm'), 15), 
#' 	rep(c('k', 'c', 'n', 'm'), 17)
#' ) 
#' raw <- matrix(raw, ncol = 4, byrow = TRUE)
#' vote <- create_vote(raw, xtype = 2, candidate = c('m', 'n', 'k', 'c'))
#'
#' # xtype is 3
#' raw <- list(
#' 	c('a', 'e', 'c', 'd', 'b'), 
#' 	c('b', 'a', 'e'), 
#' 	c('c', 'd', 'b'), 
#' 	c('d', 'a', 'b'), 
#' 	c('a', 'a', 'b', 'b', 'b'), 
#' 	c(NA, NA, NA, NA), 
#' 	v7 = NULL, 
#' 	v8 = c('a', NA, NA, NA, NA, NA, NA), 
#' 	v9 = rep(" ", 3)
#' )
#' y <- check_dup_wrong(raw, xtype = 3, candidate = letters[1: 4])
#' raw2 <- raw[-y$row_with_wrong]
#' vote <- create_vote(raw2, xtype = 3, candidate = letters[1: 4])
#' 
#' # xtype is 1
#' raw <- rbind(
#' 	c(1, 2, 5, 3, 3), 	
#' 	c(2, 1, 1, 3, 5), 	
#' 	c(1, 2, 5, 3, 4), 
#' 	c(1, 2, 5, 3, 4), 
#' 	c(NA, NA, NA, NA, NA),		
#' 	c(NA, 3, 5, 1, 2), 
#' 	c(NA, 999, NA, 1, 5)
#' )
#' vote <- create_vote(raw, xtype = 1, isna = 999)
create_vote=function(x, xtype=2, candidate=NULL, isna=NULL){
	if (! xtype[1] %in% c(1, 2, 3)) stop('Sorry, currently xtype can only be 1, 2 or 3.')
	if (xtype %in% c(2, 3)){
		if (is.null(candidate)) stop('When xtype is 2 or 3, candidate must be given.')
		if (any(is.na(candidate))) stop('Argument candidate must not have NA.')
		if (length(candidate)<2) stop('Less than 2 candidate.')
	}
	if (!is.null(isna)){
		if (any(is.na(isna))) stop('Argument isna should either be NULL or a vector without NA.')
	}

	message('MATCHING NAMES AND SCORES')
	class1=class(x)[1]
	if(xtype==1){
		if (! class1 %in% c('matrix')) stop('When xtype is 1, raw data must be a matrix.')
		isna=c(isna, NA)
		if (ncol(x)<2) stop('Less than 2 candidates.')
		if (!is.null(colnames(x))){
			candidate=colnames(x)
		} else {
			candidate=fUll_dIgIt(ncol(x), "x")
		}
		if (!is.numeric(x[, 1])) stop('When xtype is 1, each column of x must be numeric.')
		x[x %in% isna]=NA
		VOTE_M=x
	} else if(xtype==3){
		if (class1 !='list') stop('When xtype is 3, x must be a list.')
		candidate=as.character(candidate)
		candidate_num=length(candidate)
		check_list_len=unlist(lapply(x, length))
		if (any(check_list_len > candidate_num)) stop("Lengths of some elements of list are larger than candidate number.")
		isna=c(as.character(isna), NA, "", " ")
		
		VOTE_M=matrix(as.integer(NA), ncol=length(x), nrow=candidate_num)
		VOTE_M=data.table::data.table(VOTE_M)
				
		for (i in 1: length(x)){
			ii=as.character(x[[i]])
			if (length(ii)==0) ii=rep(NA, candidate_num)
			lengthii=length(ii)
			ii[ii %in% isna]=NA
			if (any(!is.na(ii))){
				match_name=match(ii, candidate)
				not_na=!is.na(match_name)
				match_name=match_name[not_na]
				match_value=c(1: lengthii)[not_na]
				VOTE_M[match_name, (i) := match_value]
			}
		}
	} else if(xtype==2){
		if (! class1 %in% c('matrix', 'data.frame')) 
		    stop('When xtype is 2, x must be a matrix or data.frame.')
		if (! class1=='matrix') candidate=as.character(candidate)
		candidate_num=length(candidate)
		if (ncol(x)>candidate_num) 
		    stop("Column number is larger than candidate number.")
		if (class1 =='data.frame'){
			isna=c(as.character(isna), NA, "", " ")
			for (j in 1: ncol(x)){
				if (class(x[, j]) != 'character') x[, j] = as.character(x[, j])
			}
			
			VOTE_M=matrix(as.integer(NA), ncol=nrow(x), nrow=candidate_num)
			VOTE_M=data.table::data.table(VOTE_M)
						
			for (i in 1: nrow(x)){
				ii=as.character(x[i, ])
				ii[ii %in% isna]=NA
				if (any(!is.na(ii))){
					match_name=match(ii, candidate)
					not_na=!is.na(match_name)
					match_name=match_name[not_na]
					match_value=c(1: length(ii))[not_na]
					VOTE_M[match_name, (i) := match_value]
				}
			}				
		} else if (class1 == 'matrix'){
			if (! class(x[1, 1]) %in%  c("integer", "numeric", "character")){
				stop("When xtype is 2 and x is matrix, cells of x must be integer, numeric or character.")
			}
			isna <- if (class(x[1, 1])=="character") c(as.character(isna), NA, "", " ") else c(NA, isna)
			
			VOTE_M=matrix(as.integer(NA), ncol=nrow(x), nrow=candidate_num)
			VOTE_M=data.table::data.table(VOTE_M)
			
			for (i in 1: nrow(x)){
				ii=x[i, ]
				ii[ii %in% isna]=NA
				if (any(!is.na(ii))){
					match_name=match(ii, candidate)
					not_na=!is.na(match_name)
					match_name=match_name[not_na]
					match_value=c(1: length(ii))[not_na]
					VOTE_M[match_name, (i) := match_value]
				}
			}			
		}
	}
	rm(x)
	if (xtype != 1) VOTE_M=data.table::transpose(VOTE_M)
	colnames(VOTE_M)=candidate	

	message('COUNTING NA AND DUP VALUES')
	COUNT_NA_DUP=chEck_stAndArd_mAtrIx(VOTE_M, numeric_na=isna)
	
	message('MAKING CONDORCET TABLE')
	CDC_RESULT=rOw2cdc(VOTE_M)
	
	if (xtype %in% c(2, 3)){
		APPROVAL_OK=TRUE
	} else if (xtype==1){
		APPROVAL_OK=if (length(COUNT_NA_DUP[[3]])==0) TRUE else FALSE
	}
	
	message('COLLECTING RESULT')
	NBALLOT=nrow(VOTE_M)
	num_of_all_na=if (length(COUNT_NA_DUP[[2]]) == 0) 0 else length(which(COUNT_NA_DUP[[2]]==0))
	NBALLOT_HAVE_ONE=NBALLOT-num_of_all_na
	candidate_num=length(candidate)
	Y=list(call=match.call(), 
				ballot=if(is.matrix(VOTE_M)) VOTE_M else as.matrix(VOTE_M), 
				nas=isna, 
				candidate=candidate, 
				candidate_num=candidate_num, 
				ballot_num=NBALLOT, 
				ballot_at_least_one=NBALLOT_HAVE_ONE,
				cdc=CDC_RESULT[[1]], 
				cdc_with_na=CDC_RESULT[[2]], 
				dif=CDC_RESULT[[3]], 
				dif_with_na=CDC_RESULT[[4]], 
				row_with_na=COUNT_NA_DUP[[1]], 
				num_non_na=COUNT_NA_DUP[[2]], 
				row_with_dup=COUNT_NA_DUP[[3]], 
				approval_able=APPROVAL_OK
				)
	class(Y)="vote"
	message('DONE')
	return(Y)
}
