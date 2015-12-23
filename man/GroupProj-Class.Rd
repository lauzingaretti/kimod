
\docType{class}
\name{GroupProj-class}
\alias{GroupProj-class}

%Accesors
\alias{SortList}
\alias{SortList,GroupProj-method}
\alias{SortList<-,GroupProj-method}
\alias{ProyGroups}
\alias{ProyGroups,GroupProj-method}
\alias{ProyGroups<-,GroupProj-method}
\alias{Groups}
\alias{Groups,GroupProj-method}
\alias{Groups<-,GroupProj-method}
\title{Class \code{GroupProj} GroupProj S4 class (kimod: k-tables approach to integrate multiple Omics-Data
of Multiple dataset)
GroupProj to SelectVar object.}
\description{
Class \code{GroupProj} GroupProj S4 class (kimod: k-tables approach to integrate multiple Omics-Data
of Multiple dataset)
GroupProj to SelectVar object.
}
\section{Features}{

\enumerate{
  \item It cluster variables for SelectVar from all tables (Studies) in STATIS Methodology.
}
}

\section{Fields}{

\itemize{
\item SortList list the clustering variables
\item ProyGroups coords (beta) for Groups representant.
\item Groups list of clusters.

}
}

\section{Accesors}{

\itemize{
\item \code{SortList(x)}, \code{SortList(x) <- value}: list the clustering variables
\item \code{ProyGroups(x)}, \code{ProyGroups(x) <- value}: coords (beta) for Groups representant.
\item \code{Groups(x)}, \code{Groups(x) <- value}:Groups list of clusters.

}
}

\section{GroupProj-general-functions}{

\describe{
 \item{print}{Generates the basic output for Bootstrap class}
 \item{summary}{Generates the basic statistics for Bootstrap class}
 \item{GroupProj}{Getters for their respective slots.}
}
}
\author{
M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey

}

\examples{
 showClass("GroupProj")
}

