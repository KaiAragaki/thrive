% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrange_ggsurvplots.R
\name{arrange_ggsurvplots}
\alias{arrange_ggsurvplots}
\title{Arranging Multiple ggsurvplots}
\usage{
arrange_ggsurvplots(
  x,
  print = TRUE,
  title = NA,
  ncol = 2,
  nrow = 1,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL,
  ...
)
}
\arguments{
\item{x}{a list of ggsurvplots.}

\item{print}{logical value. If TRUE, the arranged plots are displayed.}

\item{title}{character vector specifying page title. Default is NA.}

\item{ncol, nrow}{the number of columns and rows, respectively.}

\item{surv.plot.height}{the height of the survival plot on the grid. Default
is 0.75. Ignored when risk.table = FALSE. \code{1-risk.table.height -
 ncensor.plot.height} when \code{risk.table = TRUE} and \code{ncensor.plot =
 TRUE}}

\item{risk.table.height}{the height of the risk table on the grid. Increase
the value when you have many strata. Default is 0.25. Ignored when
risk.table = FALSE.}

\item{ncensor.plot.height}{The height of the censor plot. Used when
\code{ncensor.plot = TRUE}.}

\item{...}{not used}
}
\value{
returns an invisible object of class arrangelist (see
\link[gridExtra]{marrangeGrob}), which can be saved into a pdf file using
the function \link[ggplot2]{ggsave}.
}
\description{
Arranging multiple ggsurvplots on the same page.
}
\examples{

# Fit survival curves
require("survival")
fit<- survfit(Surv(time, status) ~ sex, data = lung)

# List of ggsurvplots
require("survminer")
splots <- list()
splots[[1]] <- ggsurvplot(fit, data = lung, risk.table = TRUE, ggtheme = theme_minimal())
splots[[2]] <- ggsurvplot(fit, data = lung, risk.table = TRUE, ggtheme = theme_grey())

# Arrange multiple ggsurvplots and print the output
arrange_ggsurvplots(splots, print = TRUE,
  ncol = 2, nrow = 1, risk.table.height = 0.4)

\dontrun{
# Arrange and save into pdf file
res <- arrange_ggsurvplots(splots, print = FALSE)
ggsave("myfile.pdf", res)
}


}
\author{
Alboukadel Kassambara, \email{alboukadel.kassambara@gmail.com}
}
