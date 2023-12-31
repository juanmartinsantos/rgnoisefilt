% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discNCL.R
\name{discNCL}
\alias{discNCL}
\alias{discNCL.default}
\alias{discNCL.formula}
\title{Neighborhood Cleaning Rule for Regression by Discretization}
\usage{
\method{discNCL}{default}(x, y, k = 3, ...)

\method{discNCL}{formula}(formula, data, ...)
}
\arguments{
\item{x}{a data frame of input attributes.}

\item{y}{a double vector with the output regressand of each sample.}

\item{k}{an integer with the number of nearest neighbors to be used (default: 3).}

\item{...}{other options to pass to the function.}

\item{formula}{a formula with the output regressand and, at least, one input attribute.}

\item{data}{a data frame in which to interpret the variables in the formula.}
}
\value{
The result of applying the regression filter is a reduced dataset containing the clean samples (without errors or noise), since it removes noisy samples (those with errors).
This function returns an object of class \code{rfdata}, which contains information related to the noise filtering process in the form of a list with the following elements:
\item{xclean}{a data frame with the input attributes of clean samples (without errors).}
\item{yclean}{a double vector with the output regressand of clean samples (without errors).}
\item{numclean}{an integer with the amount of clean samples.}
\item{idclean}{an integer vector with the indices of clean samples.}
\item{xnoise}{a data frame with the input attributes of noisy samples (with errors).}
\item{ynoise}{a double vector with the output regressand of noisy samples (with errors).}
\item{numnoise}{an integer with the amount of noisy samples.}
\item{idnoise}{an integer vector with the indices of noisy samples.}
\item{filter}{the full name of the noise filter used.}
\item{param}{a list of the argument values.}
\item{call}{the function call.}

Note that objects of the class \code{rfdata} support \link{print.rfdata}, \link{summary.rfdata} and \link{plot.rfdata} methods.
}
\description{
Application of the discNCL noise filtering method in a regression dataset.
}
\details{
\code{discNCL} discretizes the numerical output variable to make it compatible with \emph{Neighborhood Cleaning Rule} (NCL), typically used in classification tasks.
NCL identifies and prunes majority class instances that are predominantly surrounded by minority class counterparts, often perceived as noise or overlapping points. 
By removing these instances, decision boundaries become clearer, thereby enhancing classification performance.
}
\examples{
# load the dataset
data(rock)

# usage of the default method
set.seed(9)
out.def <- discNCL(x = rock[,-ncol(rock)], y = rock[,ncol(rock)])

# show results
summary(out.def, showid = TRUE)

# usage of the method for class formula
set.seed(9)
out.frm <- discNCL(formula = perm ~ ., data = rock)

# check the match of noisy indices
all(out.def$idnoise == out.frm$idnoise)

}
\references{
J. Laurikkala,
\strong{Improving identification of difficult small classes by balancing class distribution.}
\emph{Artificial Intelligence in Medicine}, 2101:63-66, 2001.
\doi{https://doi.org/10.1007/3-540-48229-6_9}.

A. Arnaiz-González, J. Díez-Pastor, J. Rodríguez, C. García-Osorio,
\strong{Instance selection for regression by discretization.}
\emph{Expert Systems with Applications}, 54:340-350, 2016.
\doi{https://doi.org/10.1016/j.eswa.2015.12.046}.
}
\seealso{
\code{\link{discCNN}}, \code{\link{discTL}}, \code{\link{discENN}}, \code{\link{print.rfdata}}, \code{\link{summary.rfdata}}
}
