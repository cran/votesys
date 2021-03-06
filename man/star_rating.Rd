% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_rating.R
\name{star_rating}
\alias{star_rating}
\title{User Preference Aggregation}
\usage{
star_rating(x, show_name = FALSE, check_na = TRUE)
}
\arguments{
\item{x}{a numeric matrix, or, \code{dgCMatrix} and 
\code{dgeMatrix} matrix created with the Matrix package.
0 in the data means no score is given; and valid score values 
should all be larger than 0. The function will do NOTHING
to check the validity.
Besides, NA also means no score is given. If your data has NA, 
set \code{check_na} to TRUE so as to convert NA to 0.}

\item{show_name}{the default is FALSE, that is to say, the function 
does not store and show candidate names in the result and you 
cannot see them. However, you can set it to TRUE.}

\item{check_na}{if it is TRUE, the function will check NAs and convert 
them to 0s. If NAs do exist and \code{check_na} is FALSE, error will be 
raised.}
}
\value{
a list object.
\itemize{
  \item (1) \code{call} the function call.
  \item (2) \code{method} the counting method.
  \item (3) \code{candidate} candidate names. If 
\code{show_name} is FALSE, this will be NULL.
  \item (4) \code{candidate_num} number of candidate.
  \item (5) \code{ballot_num} number of ballots in x.
  \item (6) \code{valid_ballot_num} number of ballots that are 
used to compute the result.
  \item (7) \code{winner} the winner. If \code{show_name} is FALSE, 
this only shows the number in \code{1: ncol(x)}.
  \item (8) \code{winner_score} the winner's score, which is the highest score.
  \item (9) \code{other_info} scores of all the candidates.
}
}
\description{
The function uses a simple method to 
calculate the aggregation scores of user 
ratings, which is described 
in Langville, A. and Meyer, C. (2012: 128). 
Input data can be stored in a sparse matrix.
Suppose there are 100 films and users are required to assign 
scores. However, each user only watched several of them. Thus, 
when comparing two films A and B, the method only takes account 
ratings from those who watched both A and B.
}
\examples{
# Example from Langville and Meyer, 2012: 128. 
# 4 films are rated by 10 users; 0 means no score.
raw <- c(4, 3, 1, 2, 0, 2, 0, 3, 0, 2, 2, 1, 0, 4, 3, 3, 4, 
    1, 3, 0, 2, 0, 2, 2, 2, 0, 1, 1, 2, 2, 0, 2, 0, 0, 5, 0, 3, 
    0, 5, 4
)
m <- matrix(raw, ncol = 4)
colnames(m) <- paste("film", 1: 4, sep = "")
y <- star_rating(m, show_name = TRUE) # winner is film4
}
\references{
\itemize{
  \item Langville, A. and Meyer, C. 2012. 
Who's #1? The Science of Rating and Ranking. 
Princeton University Press, p. 128.
}
}
