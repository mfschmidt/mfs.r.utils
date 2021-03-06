% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mfs_p_string.R
\name{mfs_p_string}
\alias{mfs_p_string}
\title{Return a string representing a p-value}
\usage{
mfs_p_string(pval)
}
\arguments{
\item{pval}{A numeric p-value between 0.0 and 1.0}
}
\description{
This function just stringifies a p-value as "p=#", but if p is very
    low, it reports "p<0.0001" instead.
}
\examples{
print(mfs_p_string(0.0035))
}
\keyword{p-value}
\keyword{string}
