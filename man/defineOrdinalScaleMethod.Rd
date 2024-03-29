% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defineOrdinalScaleMethod.R
\name{defineOrdinalScaleMethod}
\alias{defineOrdinalScaleMethod}
\title{Ordinal scale method definition}
\usage{
defineOrdinalScaleMethod(
  name,
  description,
  subject,
  codes,
  citationString = "",
  DOI = "",
  quantifiableCodes = character(0),
  breaks = NULL,
  midPoints = NULL,
  definitions = NULL
)
}
\arguments{
\item{name}{String with the name of the method.}

\item{description}{String describing the method.}

\item{subject}{A string to identify the subject (e.g. 'plant cover').}

\item{codes}{A character vector of ordinal class codes.}

\item{citationString}{A string with the bibliographic reference for the method.}

\item{DOI}{A string with the DOI the resource related to \code{citationString}.}

\item{quantifiableCodes}{A string vector with the subset of \code{codes} that have correspondence in a quantitative scale. These codes
can be transformed to midPoints (if supplied in \code{midPoints}) and, correspondingly, codes in the quantitative scale can be transformed to ordinal codes
using class limits (if supplied in \code{breaks}).}

\item{breaks}{A vector of break points (for ordinal scales with class limits)}

\item{midPoints}{A vector of class midpoints (for ordinal scales that can be translated into quantitative values)}

\item{definitions}{A character vector of class definitions.}
}
\value{
an object of class \code{\linkS4class{VegXMethodDefinition}}
}
\description{
A general function to define a method for measurements in an ordinal scale of a given subject.
}
\examples{

#Ordinal scale with three levels
defineOrdinalScaleMethod("scale1", "Description for scale1", "subject1",
                   codes = c("first", "second","third"))

#Braun-Blanquet plant cover scale with five levels that can be translated to cover codes
defineOrdinalScaleMethod(name = "Braun-Blanquet",
                   description = "Five-level Braun-Blanquet cover scale",
                   subject = "plant cover",
                   codes = as.character(1:5),
                   quantifiableCodes = as.character(1:5),
                   breaks = c(0,5,25,50,75,100),
                   midPoints = c(2.5,17.5, 37.5, 62.5, 87.5))


#Ordinal plant cover scale with seven levels where six can be translated to a quantitative scale
defineOrdinalScaleMethod(name = "Recce cover scale",
                   description = "Recce recording method by Hurst/Allen",
                   subject = "plant cover",
                   citation = "Hurst, JM and Allen, RB. (2007) 
                        The Recce method for describing New Zealand vegetation – Field protocols. 
                        Landcare Research, Lincoln.",
                   codes = c("P","1","2","3", "4", "5", "6"),
                   quantifiableCodes = c("1","2","3", "4", "5", "6"),
                   breaks = c(0, 1, 5, 25, 50, 75, 100),
                   midPoints = c(0.05, 0.5, 15, 37.5, 62.5, 87.5),
                   definitions = c("Presence", "<1\%", "1-5\%","6-25\%", "26-50\%", 
                                   "51-75\%", "76-100\%"))

}
\seealso{
Other define measurement functions: 
\code{\link{defineQualitativeScaleMethod}()},
\code{\link{defineQuantitativeScaleMethod}()},
\code{\link{predefinedMeasurementMethod}()}
}
\concept{define measurement functions}
