
\docType{class}
\name{Bootstrap-class}
\alias{Bootstrap-class}


\alias{Ratios.Boot<-,Bootstrap-method}
\alias{Ratios.Boot}
\alias{Ratios.Boot,Bootstrap-method}
\alias{Comparisions.Boot<-,Bootstrap-method}
\alias{Comparisions.Boot}
\alias{Comparisions.Boot,Bootstrap-method}
\alias{Elipses.Boot<-,Bootstrap-method}
\alias{Elipses.Boot}
\alias{Elipses.Boot,Bootstrap-method}
\alias{Stability.Boot<-,Bootstrap-method}
\alias{Stability.Boot}
\alias{Stability.Boot,Bootstrap-method}
\alias{QRO.Boot<-,Bootstrap-method}
\alias{QRO.Boot}
\alias{QRO.Boot,Bootstrap-method}
\alias{EigValues.Boot<-,Bootstrap-method}
\alias{EigValues.Boot}
\alias{EigValues.Boot,Bootstrap-method}
\alias{Inertia.Boot<-,Bootstrap-method}
\alias{Inertia.Boot}
\alias{Inertia.Boot,Bootstrap-method}

\title{Class \code{Bootstrap}: Bootstrap S4 class (kimod: k-tables approach to integrate multiple Omics-Data
Multiple dataset)}
\description{
Bootstrap to DiStatis object.
}
\section{Features}{

\enumerate{
  \item Bootstrap Implementation
  \item Confidence Intervals from Bootstrap implementation
  \item Plotting Bootstrap
}
}

\section{Fields}{

\itemize{
\item Ratios.Boot Ratios (with Bonferroni Correction for all observations in compromise from all dimensions)
\item Comparisions.Boot Comparision (difference between observations) from all dimensions
\item Elipses.Boot List of all projections from the elipses plot.
\item Stability.Boot Bootstrap Stability
\item QRO.Boot Rpresentation Quality of observations (Bootstrap)
\item EigValues.Boot confidence interval for eigenvalues (from SVD Compromise)
\item Inertia.Boot confidence interval for inertia(%) of all dimensions (from  SVD Compromise)
}
}

\section{Accesors}{

\itemize{

\item  \code{Ratios.Boot(x)}, \code{Ratios.Boot(x) <- value}: Ratios (with Bonferroni Correction for all observations in compromise from all dimensions)
\item  \code{Comparisions.Boot(x)}, \code{Comparisions.Boot(x) <- value}:  Comparision (difference between observations) from all dimensions.
\item \code{Elipses.Boot(x)}, \code{Elipses.Boot(x) <- value}:List of all projections from the elipses plot.
\item \code{Stability.Boot(x)}, \code{Stability.Boot(x) <- value}: Bootstrap Stability.
\item \code{QRO.Boot(x)}, \code{QRO.Boot(x) <- value}: Representation Quality of observations (Bootstrap)
\item \code{EigValues.Boot(x)}, \code{EigValues.Boot(x) <- value}: confidence interval for eigenvalues (from SVD Compromise)
\item \code{Inertia.Boot(x)}, \code{Inertia.Boot(x) <- value}: confidence interval for inertia(\%) of all dimensions (from  SVD Compromise).
}
}

\section{Bootstrap-general-functions}{

\describe{
 \item{print}{Basic output for Bootstrap class}
 \item{summary}{Basic statistics for Bootstrap class}
 \item{Bootstrap}{Getters for their respective slots.}
}
}
\author{
M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey
}

\examples{
 showClass("Bootstrap")
}

\references{
\enumerate{
 \item Efron, B.,Tibshirani, RJ. (1993). An introduction to the bootstrap. New York: Chapman and Hall. 436p.
 \item Ringrose, T.J. (1992). Bootstrapping and Correspondence Analysis in Archaeology. Journal of Archaeological. Science.19:615-629.

}
}

