#' @title Voting Systems, Instant-Runoff Voting, Borda Method, Various Condorcet Methods
#'
#' @description
#' This package provides different methods to counting ballots, which 
#' can be used in election, decision making and evaluation. The basic 
#' idea is: different forms of ballots can all be transformed into a 
#' score matrix; then the score matrix can be put into different 
#' counting methods. The functions 
#' in this package provide more flexibility to deal with duplicated 
#' values (ties) and missing values. And the comparison of results 
#' of different methods is also made easy.
#' 
#' Introduction of different methods is contained in this manual. 
#' However, it is a good choice to read detailed introduction in Wikipedia. 
#' Here are the urls for some of the methods. 
#' 
#'   plurality: https://en.wikipedia.org/wiki/Plurality_voting
#' 
#'   approval: https://en.wikipedia.org/wiki/Approval_voting
#' 
#'   Borda, Dowdall: https://en.wikipedia.org/wiki/Borda_count
#' 
#'   instant-runoff: 
#' https://en.wikipedia.org/wiki/Instant-runoff_voting
#' 
#'   ordinary Condorcet: 
#' https://en.wikipedia.org/wiki/Condorcet_method
#' 
#'   Copeland: 
#' https://en.wikipedia.org/wiki/Copeland%27s_method
#' 
#'   minmax: https://en.wikipedia.org/wiki/Minimax_Condorcet
#' 
#'   Dodgson: 
#' https://en.wikipedia.org/wiki/Dodgson%27s_method
#' 
#'   Schulze: https://en.wikipedia.org/wiki/Schulze_method
#' 
#'   ranked pairs: https://en.wikipedia.org/wiki/Ranked_pairs
#'
#' @docType package
#' @name votesys-package
#' @aliases votesys
#' @rdname votesys-package
#' @author Jiang Wu
#' @family votesys_general_topics
#' @examples
#' # Suppose we have the following ballot data
#' raw <- list2ballot(
#'     x = list(
#' 	    c('m', 'n', 'c', 'k'), c('n', 'c', 'k', 'm'), 
#' 	    c('c', 'k', 'n', 'm'), c('k', 'c', 'n', 'm'), c(NA, NA, NA, NA)
#'     ) , 
#' 	n = c(42, 26, 15, 17, 3)
#' )
#' 
#' # Step 1: check validity of ballots. Delete  
#' # some of them, if needed.
#' check_validity <- check_dup_wrong(raw, 
#'     xtype = 3, 
#' 	candidate = c('m', 'n', 'k', 'c')
#' )
#' raw <- raw[- check_validity$row_all_na]
#' 
#' # Step 2: create a vote object
#' vote <- create_vote(raw, xtype = 3, candidate = c('m', 'n', 'k', 'c'))
#' 
#' # Step 3: use one or more methods
#' y <- plurality_method(vote) # winner is m
#' y <- irv_method(vote) # winner is k
#' y <- cdc_simple(vote) # winner is n
#' y <- cdc_rankedpairs(vote) # winner is n
NULL