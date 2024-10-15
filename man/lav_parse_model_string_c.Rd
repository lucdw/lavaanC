\name{lav_parse_model_string_c}
\alias{lav_parse_model_string_c}
\title{Parser in C}
\description{
The lavaan syntax parser  in C.
Returns the flat version as a list, with attributes modifiers, constraints and warnings,
 or a numeric vector of length 2 with the error code and the position of the error.
}
\usage{
lav_parse_model_string_c(model.syntax = "")
}
\arguments{
\item{model.syntax}{A description of the user-specified model. Typically, the model
    is described using the lavaan model syntax.
}
}
\examples{
model <- '
 visual  =~ x1 + x2 + x3
 textual =~ x4 + x5 + x6
 speed   =~ x7 + x8 + x9 '
parsedmodel <- lav_parse_model_string_c(model)
}
