% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariatexbarchart.R
\name{univariatexbarchart}
\alias{univariatexbarchart}
\title{univariatexbarchart}
\usage{
univariatexbarchart(x)
}
\arguments{
\item{x}{the data vector to create the chart for}
}
\value{
Quality control chart as a ggplot object
}
\description{
Creates a univariate quality control chart based on the variable mean
}
\details{
UThe quality control chart shows the observations in relation to the mean across a sample
}
\examples{
\dontrun{ univariatexbarchart(x = c(3,6,7,7,9,2,4,5,5,6,7))}
}
