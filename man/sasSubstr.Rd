% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sasWhereConstructors.R
\name{sasSubstr}
\alias{sasSubstr}
\title{sasSubstr}
\usage{
sasSubstr(str.vec, target.var)
}
\arguments{
\item{str.vec}{Character vector where each element is the start of the
substring that should be searched for. Fx. c("DI05", "DI06) will search for
any recrods that start with DI05 or DI06.}

\item{target.var}{Character name of the variable in the SAS dataset that is
to be searched}
}
\value{
Returns a string that can be passed to the "where" arguments
  of importSAS, or combined with other conditions in sasWhereConstructor
}
\description{
Helper function to construct a SAS 'where' statement to search through substrings in a SAS dataset via importSAS
}
\details{
The function allows a user to search for strings matching those in
  `str.vec` in SAS datasets. Fx it can be used to find records in the LPR that match an ICD code
}
\examples{
library(heaven)
x <- c("DI05", "DI06")
sasSubstr(x, target.var="A")


}
\seealso{
importSAS
}
\author{
Matthew Phelps <mphelps@hjerteforeningen.dk>
}
