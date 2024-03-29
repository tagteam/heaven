% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simPop.R
\name{simPop}
\alias{simPop}
\title{Population simulation}
\usage{
simPop(n, min.age = 20, max.age = 90, sex = 0.5, mortality = 0.05)
}
\arguments{
\item{n}{Number of subjects}

\item{min.age}{minimal age}

\item{max.age}{maximal age}

\item{sex}{gender distribution: value between 0 and 1}

\item{mortality}{risk of death: value between 0 and 1}
}
\value{
data.table with columns: pnr, sex, birthdate, status, doddate
where doddate is the date of death for subjects with status = 1 and
the current date for subjects with status = 0.
}
\description{
Simulate population with birthdate and gender
}
\details{
Sample data for illustration, i.e., vignettes and example sections of the package.
}
\examples{
set.seed(7)
simPop(7)
}
\seealso{
simPrescriptionData, simAdmissionData
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
