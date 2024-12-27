\name{Vectorization}
\alias{Vectorization}
\alias{m_vecr}
\alias{m_vech}
\alias{m_vechr}
\alias{m_vechu}
\alias{m_vechru}
\title{Matrix vectorization functions}
\description{
Functions to transform a matrix to a vector representation.
}
\usage{
m_vecr(A);
m_vech(S, diagonal);
m_vechr(S, diagonal);
m_vechu(S, diagonal);
m_vechru(S, diagonal);
}
\arguments{
\item{A}{Matrix.}
\item{S}{Symmetric matrix}
\item{diagonal}{logical to indicate diagonal elements should be included, default value is \code{TRUE}}
}
\value{
The vecr operator transforms a matrix into a vector by stacking the \emph{rows} of
the matrix one underneath the other.

The vech operator (for 'half vectorization') transforms a \emph{symmetric} matrix
into a vector by stacking the \emph{columns} of the matrix one underneath the
other, but eliminating all supradiagonal elements.

The vechr operator transforms a \emph{symmetric} matrix
into a vector by stacking the \emph{rows} of the matrix one after the
other, but eliminating all supradiagonal elements.

The vechu operator transforms a \emph{symmetric} matrix
into a vector by stacking the \emph{columns} of the matrix one after the
other, but eliminating all infradiagonal elements.

The vechru operator transforms a \emph{symmetric} matrix
into a vector by stacking the \emph{rows} of the matrix one after the
other, but eliminating all infradiagonal elements.
}
\note{
All matrices given to these routines must be numeric, \strong{not} integer or logical!
}

\examples{
mat1 <- matrix(1:12*1.01, ncol = 3)
print(mat1)
m_vecr(mat1)
smat1 <- matrix(c(3.3, 2.0, 1.0, 2.0, 2.2, 0.0, 1.0, 0.0, 1.1), nrow = 3)
print(smat1)
m_vech(smat1)
m_vechru(smat1, TRUE)
}
