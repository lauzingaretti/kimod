% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GroupProj.R
\docType{methods}
\name{GroupProj}
\alias{GroupProj}
\alias{GroupProj,SelectVar-method}
\alias{GroupProj-SelectVar-methods}
\title{\code{GroupProj} of SelectVar object}
\usage{
GroupProj(object, NGroups = 2, metric = c("euclidean", "manhattan"),
  method = c("ward", "single", "complete", "weighted", "flexible"), ...)

\S4method{GroupProj}{SelectVar}(object, NGroups = 2, metric = c("euclidean",
  "manhattan"), method = c("ward", "single", "complete", "weighted",
  "flexible"), ...)
}
\arguments{
\item{object}{it is an object of SelectVar Class.}

\item{NGroups}{An integer scalar or vector with the desired number of clusters.}

\item{metric}{The character string specifying the metric to
be used to calculate dissimilarities between observations.
The currently available options are "euclidean" and "manhattan".
Euclidean distances are root sum-of-squares of differences,
and manhattan distances are the sum of absolute differences.}

\item{method}{character string defining the clustering method.
The  methods implemented are "average" ([unweighted pair-]
group [arithMetic] average method, aka b
"single" (single linkage),
"complete" (complete linkage),
"ward" (Ward's method),
"weighted" (weighted average linkage, aka b
its generalization "flexible" which uses
(a constant version of) the Lance-Williams
formula and the par.method argument, and "gaverage"
a generalized "average" aka b
also using the Lance-Williams formula and par.method. (See \code{\link[cluster]{agnes}}).}

\item{...}{Additional parameters for \code{\link[cluster]{agnes}}.}
}
\value{
\item{GroupProj}{GroupProj with the
  corresponding completed slots}
}
\description{
This function calculates the relashionship between genes and samples from
SelectVar method. Also, computes agglomerative hierarchical clustering of the dataset.
}
\examples{
{
data(NCI60Selec)
Z1<-DiStatis(NCI60Selec)
M1<-SelectVar(Z1,Crit="R2-Adj")
A1<-GroupProj(M1,method="ward",metric="euclidean",NGroups=6)
data(winesassesors)
Z2<-DiStatis(winesassesors)
M2<-SelectVar(Z2,Crit="R2-Adj")
A2<-GroupProj(M2,method="ward",metric="euclidean",NGroups=6)
}
}
\author{
M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
}

