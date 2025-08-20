\name{parse_model_string}
\alias{parse_model_string}
\title{Parser in C}
\description{
The lavaan syntax parser  in C.
Returns the flat version as a list, with attributes modifiers, constraints and warnings,
 or a numeric vector of length 2 with the error code and the position of the error.
}
\usage{
parse_model_string(model.syntax = "", debug = FALSE)
}
\arguments{
\item{model.syntax}{A description of the user-specified model. Typically, the model
    is described using the lavaan model syntax.
}
\item{debug}{A logical to indicate if debug info is to be shown.}
}
\examples{
model <- '
 visual  =~ x1 + x2 + x3
 textual =~ x4 + x5 + x6
 speed   =~ x7 + x8 + x9
 x4:x5 ~~ x7:x8
 '
parsedmodel <- parse_model_string(model)
}
