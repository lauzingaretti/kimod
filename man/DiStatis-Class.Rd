\docType{class}
\name{DiStatis-class}
\alias{class:DiStatis}
\alias{DiStatis-class}

% Constructor:
\alias{DiStatis-class}


% Accessors:
\alias{Inertia.RV}
\alias{Inertia.RV,DiStatis-method}
\alias{Inertia.RV<-,DiStatis-method}
\alias{RV}
\alias{RV,DiStatis-method}
\alias{RV<-,DiStatis-method}
\alias{Euclid.Im}
\alias{Euclid.Im,DiStatis-method}
\alias{Euclid.Im<-,DiStatis-method}
\alias{Inertia.comp}
\alias{Inertia.comp,DiStatis-method}
\alias{Inertia.comp<-,DiStatis-method}
\alias{Compromise.Coords}
\alias{Compromise.Coords,DiStatis-method}
\alias{Compromise.Coords<-,DiStatis-method}
\alias{Compromise.Matrix}
\alias{Compromise.Matrix,DiStatis-method}
\alias{Compromise.Matrix<-,DiStatis-method}
\alias{RQO}
\alias{RQO,DiStatis-method}
\alias{RQO<-,DiStatis-method}
\alias{Trajectories}
\alias{Trajectories,DiStatis-method}
\alias{Trajectories<-,DiStatis-method}


\title{Class \code{DiStatis} DiStatis S4 class (kimod: k-tables approach to integrate multiple Omics-Data
Multiple dataset)}
\description{
Statis with Distance options implementation.
}
\section{Features}{

\enumerate{
  \item DiStatis (Statis with Distance options)
  \item Bootstrap Implementation
  \item Biplot Implementation with variable selection
  \item Clustering of variables
  \item Plotting compromise, bootstrap, trajectories, Biplot.
}
}

\section{Fields}{

\itemize{
 \item distances.methods: The character indicating the metrics used.
 \item Inertia.RV: Inertia (\%) explained for all tables.
 \item RV: Vectorial Correlation Matrix between studies.
 \item Euclid.Im: Euclidean Image of all studies.
 \item Inertia.Comp: Inertia (\%) explained for all dimensions of compromise matrix.
 \item Compromise.Coords: Projection of all observations in compromise (Coords).
 \item Compromise.Matrix: Compromise Matrix from statis methodology.
 \item RQO: Representation Quality of observations in compromise matrix.
 \item Trajectories: List of trajectories from Statis methodology
 }
}

\section{Accesors}{

\itemize{
 \item   \code{Inertia.RV(x)}, \code{Inertia.RV(x) <- value}:   Inertia (\%) explained for all tables.
 \item \code{RV(x)}, \code{RV(x) <- value}: Vectorial Correlation Matrix between studies.
 \item \code{Euclid.Im(x)},\code{Euclid.Im(x) <- value} : Euclidean Image of all studies.
 \item \code{Inertia.Comp(x)},\code{Inertia.Comp(x) <- value} :: Inertia (\%) explained for all dimensions of compromise matrix.
 \item \code{Compromise.Coords(x)},\code{Compromise.Coords(x) <- value}: Projection of all observations in compromise (Coords).
 \item \code{Compromise.Matrix(x)},\code{Compromise.Matrix(x) <- value}: Compromise Matrix from statis methodology.
 \item  \code{RQO(x)},\code{RQO(x) <- value}: Representation Quality of observations in compromise matrix.
 \item \code{Trajectories(x)},\code{Trajectories(x) <- value}: List of trajectories from Statis methodology.

 }
}



\section{DiStatis-general-functions}{

\describe{
 \item{print}{Basic output for DiStatis class}
 \item{summary}{Basic statistics for DiStatis class}
 \item{DiStatis}{Getters for their respective slots.}
}
}
\author{
M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey
}
\references{
\enumerate{
 \item Abdi, H., Williams, L.J.,
 Valentin, D., & Bennani-Dosse, M. (2012).
 STATIS and DISTATIS: optimum multitable principal
 component analysis and three way metric multidimensional scaling.
 WIREs Comput Stat, 4, 124-167.
 \item Escoufier, Y. (1976). Operateur associe a un tableau de donnees. Annales de laInsee, 22-23, 165-178.
 \item Escoufier, Y. (1987). The duality diagram: a means for better practical applications. En P. Legendre & L. Legendre (Eds.), Developments in Numerical Ecology, pp. 139-156, NATO Advanced Institute, Serie G. Berlin: Springer.
 \item L'Hermier des Plantes, H. (1976). Structuration des Tableaux a Trois Indices de la Statistique. [These de Troisieme Cycle]. University of Montpellier, France.


}
}
\examples{
 showClass("DiStatis")
}

\seealso{
\code{\link{CompPlot}},
 \code{\link{Biplot}}
 \code{\link{DiStatis}}, and additional related
 DiStatis class functions.
}

