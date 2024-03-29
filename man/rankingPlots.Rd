% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rankingPlots.R
\name{rankingPlots}
\alias{rankingPlots}
\title{Creates a ranking plot}
\usage{
rankingPlots(quantile_vals, plot_name, size = 3.5, phen)
}
\arguments{
\item{quantile_vals}{As created by severalGgPlots}

\item{plot_name}{Name of output plot with path}

\item{size}{Size of plots. Defaults to 3.5}

\item{phen}{Phenomenon, ex 'precipitation'}
}
\description{
Creates a ranking plot for the phenomenon 'phen' for each season by using the list
'quantile_vals' created from function 'severalGgPlots'
Also returns a list of the plots created
}
\examples{
rankingPlots()
}
\keyword{iqd,}
\keyword{plot,}
\keyword{plots}
\keyword{rankingPlots,}
