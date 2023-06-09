% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/screeplot.R
\name{screeplot}
\alias{screeplot}
\title{Scree Plot}
\usage{
screeplot(data = NULL, varexplain = 0.8, type = "cumulative", scale = FALSE)
}
\arguments{
\item{data}{dataframe with quantitative variables}

\item{varexplain}{proportion of variance explained. Default \code{varexplain = 0.80}}

\item{type}{eigenvalue or cumulative scree plot}

\item{scale}{boolean. Indicates whether data should be scaled. Default \code{scale = FALSE}}
}
\value{
plots
}
\description{
\code{screeplot()} produces a screeplot which can be customized via its arguments.
 The function allows the user to visualize the individual and cumulative variance explained by the principal components.
 The arguments within this function assist the user in determining the ideal number of principal components and the
 accumulated proportion of variance explained by the selected number of components.
}
\examples{
screeplot(data = pima_diabetes, varexplain = 0.80, type = "eigenvalue", scale = TRUE)
screeplot(data = pima_diabetes, varexplain = 0.85, type = "cumulative", scale = TRUE)
}
