% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/otm_verify.R
\name{otm_verify}
\alias{otm_verify}
\title{Verify ordinal longitudinal dataset}
\usage{
otm_verify(df)
}
\arguments{
\item{df}{data.frame to check}
}
\value{
Modified data frame
}
\description{
This function:
\itemize{
\item Checks that \code{id}, \code{y}, \code{yprev}, \code{t}, and \code{tx} columns are in the dataset
\item Standardizes the data types for the columns
\item Sorts the data by \code{id} and \code{t}
}
}
