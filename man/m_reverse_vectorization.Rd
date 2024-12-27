\name{Reverse vectorization}
\alias{Reverse vectorization}
\alias{m_vech_reverse}
\alias{m_vechr_reverse}
\title{Matrix functions in C}
\description{
Functions to reverse the transformation of a symmetric matrix to a vector.
}
\usage{
m_vech_reverse(x, diagonal);
m_vechr_reverse(x, diagonal);
}
\arguments{
\item{x}{Numeric vector}
\item{diagonal}{Logical to indicate diagonal is present, , default value is \code{TRUE}}
}
\value{
Given the output of vech(S), or vechru(S) which is identical, m_vech_reverse
reconstructs S.

Given the output of vechr(S), or vechu(S) which is identical, m_vechr_reverse
reconstructs S.
}
\details{
The length of vector x must equal \eqn{n \times (n + 1) / 2} for a certain integer \eqn{n > 0}.
}
\note{
All vectors given to these routines must be numeric, \strong{not} integer or logical!
}
\seealso{\code{\link{Vectorization}}}
\examples{
m_vech_reverse(as.double(1:15))
m_vechr_reverse(as.double(1:15))
}
