% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveAllPlots.R
\name{saveAllPlots}
\alias{saveAllPlots}
\title{Saves plots}
\usage{
saveAllPlots(all_plots, quantile_vals, out_path, file_ending = "all_plots")
}
\arguments{
\item{all_plots}{list of plots, created by sevaralGgPlots}

\item{quantile_vals}{}

\item{out_path}{filepath for output}

\item{file_ending}{file ending for output pdf. Defaults to "all_plots}
}
\description{
Saves all plots created from 'severalGgPlots' in one pdf file per dataset
}
\examples{
severalGgPlots()
}
\keyword{iqd,}
\keyword{plot,}
\keyword{plots}
