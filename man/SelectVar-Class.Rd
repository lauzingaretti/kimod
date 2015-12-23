
\docType{class}
\name{SelectVar-class}
\alias{Coord.Select,SelectVar-method}
\alias{SelectVar-class}


%Accesors

\alias{Coord.Select}
\alias{Coord.Select,SelectVar-method}
\alias{Coord.Select<-,SelectVar-method}
\alias{Table.Select}
\alias{Table.Select,SelectVar-method}
\alias{Table.Select<-,SelectVar-method}
\alias{List.Selec.Var}
\alias{List.Selec.Var,SelectVar-method}
\alias{List.Selec.Var<-,SelectVar-method}
\alias{List.Selec.Est}
\alias{List.Selec.Est,SelectVar-method}
\alias{List.Selec.Est<-,SelectVar-method}
\title{Class \code{SelectVar} S4 class (kimod: k-tables approach to integrate multiple Omics-Data
Multiple dataset)
SelectVar to DiStatis object.}
\description{
Class \code{SelectVar} S4 class (kimod: k-tables approach to integrate multiple Omics-Data
Multiple dataset)
SelectVar to DiStatis object.
}
\section{Features}{

\enumerate{
  \item SelectVar (Generate a biplot making linear model of all variables over the Compromise Matrix)
  \item Plotting Biplot.
  \item Plotting Biplot-Group.
}
}

\section{Fields}{

\itemize{


\item  Coord.Select matrix are coordenates (Betas) of selected variables.
\item  Table.Select return to the data frame with all variables selected (from all studies or tables)
\item  List.Selec.Var return to the character with the names of all variables selected.
\item  List.Selec.Est list of all variables selected (with its respective table)
\item  Compromise.Coords Coords of projected observations in compromise matrix.
}
}

\section{Accesors}{

\itemize{


\item  \code{Coord.Select(x)}, \code{Coord.Select(x) <- value}:  matrix are coordenates (Betas) of selected variables.
\item  \code{ Table.Select(x)}, \code{ Table.Select(x) <- value}:return to the data frame with all variables selected (from all studies or tables)
\item  \code{ List.Selec.Var(x)}, \code{List.Selec.Var(x)) <- value}:return to the character with the names of all variables selected.
\item  \code{ List.Selec.Est(x)}, \code{List.Selec.Est(x)) <- value}:return to the character with the names of all variables selected. list of all variables selected (with its respective table)
}
}

\section{SelectVar-general-functions}{

\describe{
\item{print}{Generated basic output for SelectVar class}
\item{summary}{Generated basic statistics for SelectVar class}
\item{SelectVar}{Getters for their respective slots.}
}
}

\section{SelectVar-general-functions}{

\describe{
 \item{print}{Basic output for SelectVar class}
 \item{summary}{Basic statistics for SelectVar class}
}
}
\author{
M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
}
\references{
\enumerate{
 \item Demey, J., Vicente-Villardon, J. L., Galindo, M.P.  & Zambrano, A. (2008) Identifying Molecular Markers Associated With Classification Of Genotypes Using External Logistic Biplots. Bioinformatics, 24(24), 2832-2838.
\item Gabriel, K. (1971). The biplot graphic display of matrices with application to principal component analysis. Biometrika 58(3), 453--467.
\item Gower, J. & Hand, D. (1996). Biplots, Monographs on statistics and applied probability. 54. London: Chapman and Hall., 277 pp.

}
}
\examples{
 showClass("SelectVar")
}

\seealso{
\code{\link{Biplot}}, and additional related
 SelectVar class functions.
}

