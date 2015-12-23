
\docType{methods}
\name{BootPlot}
\alias{BootPlot}
\alias{BootPlot,Bootstrap-method}
\title{Plot a \code{BootPlot} of a Bootstrap object}
\usage{
\S4method{BootPlot}{Bootstrap}(x,xlabProj=NULL, ylabProj=
NULL,mainProj=NULL,legend=TRUE,colour=NULL,
xlimi=NULL,ylimi=NULL,Points=TRUE,...)
}
\arguments{
\item{x}{object of Bootstrap-Class}

\item{xlabProj}{character for the x-label title for plot}

\item{ylabProj}{character for the x-label title for plot}

\item{mainProj}{main to proj plot}

\item{legend}{Logical. It indicates whether the legend prints}

\item{colour}{colours for ellipsis}

\item{xlimi}{bounds of x-axis}

\item{ylimi}{bounds of y-axis}

\item{Points}{logical if is true, the points are plotted}

\item{...}{additional parameters for plot (generics)}
}
\value{
plotted Bootplot/s of the component/s of the given Bootstrap object.
}
\description{
Plot a \code{BootPlot} of a Bootstrap object
}
\examples{
Z2<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
 M<-Bootstrap(Z2)
if (interactive()) {
 BootPlot(M)
 }
 Colores2<-c(rep(colors()[657],5),rep(colors()[637],6),rep(colors()[537],7),
 rep(colors()[552],6),rep(colors()[57],10),rep(colors()[300],8),
 rep(colors()[461],7),rep(colors()[450],2),rep(colors()[432],7))
 Colores1<-c(rep("Breast",5),rep("CNS",6),rep("Colon",7),
 rep("Leukemia",6),rep("Melanoma",10),rep("Lung",8),rep("Ovarian",7),
 rep("Prostate",2),rep("Renal",7))
 BootPlot(M,Points=FALSE,cex.lab=0.7,cex.axis=0.7,
 las=1,xlimi=c(-0.003,0.002),ylimi=c(-0.005,0.007)
 ,legend=FALSE,col=Colores2)
 legend("topleft",unique(Colores1),col=unique(Colores2),
 bty="n",pch=16,cex=1)
}
\author{
M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
}
\seealso{
\code{\link{CompPlot}}, \code{\link{TrajPlot}},
}

