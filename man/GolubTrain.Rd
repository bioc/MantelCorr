\name{GolubTrain}

\alias{GolubTrain}

\docType{data}

\title{ Golub Training Set }

\description{
  Samples were taken with Affymetrix Hgu6800 chips and expression levels measured on 7,129
  genes (probes).  The samples consist of 27 acute lymphoblastic leukemia (ALL) and 11 acute
  myeloid luekemia (AML) patients.  The data values are raw (e.g. no standardization or
  gene filtering applied).
}

\usage{data(GolubTrain)}

\format{
  A data frame of 7129 observations (genes) with the following 38 variables (samples):
  \describe{
    \item{X1}{ALL}
    \item{X2}{ALL}
    \item{X3}{ALL}
    \item{X4}{ALL}
    \item{X5}{ALL}
    \item{X6}{ALL}
    \item{X7}{ALL}
    \item{X8}{ALL}
    \item{X9}{ALL}
    \item{X10}{ALL}
    \item{X11}{ALL}
    \item{X12}{ALL}
    \item{X13}{ALL}
    \item{X14}{ALL}
    \item{X15}{ALL}
    \item{X16}{ALL}
    \item{X17}{ALL}
    \item{X18}{ALL}
    \item{X19}{ALL}
    \item{X20}{ALL}
    \item{X21}{ALL}
    \item{X22}{ALL}
    \item{X23}{ALL}
    \item{X24}{ALL}
    \item{X25}{ALL}
    \item{X26}{ALL}
    \item{X27}{ALL}
    \item{X28}{AML}
    \item{X29}{AML}
    \item{X30}{AML}
    \item{X31}{AML}
    \item{X32}{AML}
    \item{X33}{AML}
    \item{X34}{AML}
    \item{X35}{AML}
    \item{X36}{AML}
    \item{X37}{AML}
    \item{X38}{AML}
  }
}

\source{
  http://www.broad.mit.edu/cgi-bin/cancer/datasets.cgi
}

\references{
  Golub, T.R. Molecular Classification of Cancer:  Class Discovery and Class
  Prediction by Gene Expression Monitoring.  Science, vol 286, 531-537, 1999.
}

\examples{
data(GolubTrain)
}

\keyword{datasets}
