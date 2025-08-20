\name{Properties}
\alias{Properties}
\alias{m_is_diagonal}
\title{Matrix properties checking}
\description{
Check matrix properties.
}
\usage{
m_is_diagonal(A);
}
\arguments{
\item{A}{Matrix.}
}
\value{
TRUE if the matrix is diagonal, FALSE if it isn't.
}
\examples{
m_is_diagonal(matrix(c(0.1, 0.00001, 0.0, 1.), nrow = 2))
m_is_diagonal(matrix(c(0.1, 0.0, 0.0, 1.), nrow = 2))
}
