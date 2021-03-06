% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectVar-print.R
\docType{methods}
\name{summary.SV}
\alias{summary,SelectVar-method}
\alias{summary.SV}
\title{\code{\link{summary}} a SelectVar object
Generic Print/Summary method for SelectVar class output visualization.}
\usage{
\S4method{summary}{SelectVar}(object)
}
\arguments{
\item{object}{SelectVar class object.}
}
\value{
\item{summary}{console output text with increasing detail of
 SelectVar object.}
}
\description{
\code{\link{summary}} a SelectVar object
Generic Print/Summary method for SelectVar class output visualization.
}
\examples{
{

 data(NCI60Selec)
 fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
 A<-SelectVar(fit,Crit="p-val(Bonf)",perc=0.95)

 summary(A)

}
}
\author{
M L Zingaretti,  J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
}
\seealso{
\code{\link{SelectVar}}
}

