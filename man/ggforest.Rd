% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggforest.R
\name{ggforest}
\alias{ggforest}
\title{Forest Plot for Cox Proportional Hazards Model}
\usage{
ggforest(
  model,
  data = NULL,
  main = "Hazard ratio",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 0.7,
  refLabel = "reference",
  noDigits = 2
)
}
\arguments{
\item{model}{an object of class coxph.}

\item{data}{a dataset used to fit survival curves. If not supplied then data
will be extracted from 'fit' object.}

\item{main}{title of the plot.}

\item{cpositions}{relative positions of first three columns in the OX scale.}

\item{fontsize}{relative size of annotations in the plot. Default value: 0.7.}

\item{refLabel}{label for reference levels of factor variables.}

\item{noDigits}{number of digits for estimates and p-values in the plot.}
}
\value{
returns a ggplot2 object (invisibly)
}
\description{
Drawing Forest Plot for Cox proportional hazards model. In two panels the model structure is presented.
}
\examples{
require("survival")
model <- coxph( Surv(time, status) ~ sex + rx + adhere,
                data = colon )
ggforest(model)

colon <- within(colon, {
  sex <- factor(sex, labels = c("female", "male"))
  differ <- factor(differ, labels = c("well", "moderate", "poor"))
  extent <- factor(extent, labels = c("submuc.", "muscle", "serosa", "contig."))
})
bigmodel <-
  coxph(Surv(time, status) ~ sex + rx + adhere + differ + extent + node4,
    data = colon )
ggforest(bigmodel)

}
\author{
Przemyslaw Biecek (\email{przemyslaw.biecek@gmail.com}),
Fabian Scheipl (\email{fabian.scheipl@gmail.com})
}
