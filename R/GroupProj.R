###GroupProj from SelectVar Class Object

#' \code{GroupProj} of SelectVar object
#'
#' This function calculates the relashionship between genes and samples from
#' SelectVar method. Also, computes agglomerative hierarchical clustering of the dataset.
#' @param object it is an object of SelectVar Class.
#' @param NGroups An integer scalar or vector with the desired number of clusters.
#' @param metric The character string specifying the metric to
#'  be used to calculate dissimilarities between observations.
#'  The currently available options are "euclidean" and "manhattan".
#'  Euclidean distances are root sum-of-squares of differences,
#'  and manhattan distances are the sum of absolute differences.
#'@param method character string defining the clustering method.
#'  The  methods implemented are "average" ([unweighted pair-]
#'  group [arithMetic] average method, aka b UPGMAb ),
#'  "single" (single linkage),
#'  "complete" (complete linkage),
#'  "ward" (Ward's method),
#'  "weighted" (weighted average linkage, aka b WPGMAb ),
#'  its generalization "flexible" which uses
#'  (a constant version of) the Lance-Williams
#'  formula and the par.method argument, and "gaverage"
#'  a generalized "average" aka b flexible UPGMAb  method
#'  also using the Lance-Williams formula and par.method. (See \code{\link[cluster]{agnes}}).
#'@param ... Additional parameters for \code{\link[cluster]{agnes}}.
#' @return
#'  \item{GroupProj}{GroupProj with the
#'   corresponding completed slots}
#'
#'
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @examples
#' {
#' data(NCI60Selec)
#' Z1<-DiStatis(NCI60Selec)
#' M1<-SelectVar(Z1,Crit="R2-Adj")
#' A1<-GroupProj(M1,method="ward",metric="euclidean",NGroups=6)
#' data(winesassesors)
#' Z2<-DiStatis(winesassesors)
#' M2<-SelectVar(Z2,Crit="R2-Adj")
#' A2<-GroupProj(M2,method="ward",metric="euclidean",NGroups=6)
#' }

#' @exportMethod GroupProj
#' @docType methods
#' @name GroupProj
#' @rdname GroupProj
#' @aliases GroupProj-SelectVar-methods
#'
setGeneric(name="GroupProj",def=function(object,NGroups=2,metric=c("euclidean","manhattan"),
      method=c("ward","single","complete","weighted","flexible"),...){standardGeneric("GroupProj")})

#' @name GroupProj
#' @rdname GroupProj
#' @inheritParams GroupProj
#' @aliases GroupProj,SelectVar-method
#'
#object is an object of SelectVar S4 class
#NGroups N groups to cluster method
#metric distance measure
#method aglommerative method


setMethod(f="GroupProj", signature = signature("SelectVar"), definition =
            function(object,NGroups=2,
    metric=c("euclidean","manhattan"),
       method=c("ward","single","complete","weighted","flexible"),...){

             ##Obtain object name for future update
              nameObject<-deparse(substitute(object))

              ##Check parameters
            #stopifnot(require("cluster"))
            stopifnot(metric[1] %in% c("euclidean","manhattan"))
            stopifnot(method[1] %in% c("ward", "single","complete","weighted","flexible"))
            if(NGroups<2){
              stop("NGroups should be greater than 1")
            }
            NGroups<-round(NGroups)
            MediasP<-t(apply(object@Table.Select,1,function(y){tapply(y,colnames(object@Table.Select),mean)}))

           CoordsP<-t(apply(object@Coord.Select,1,function(y){tapply(y,colnames(object@Coord.Select),mean)}))





X<-tapply(t(object@Coord.Select)[,1],as.factor(rownames(t(object@Coord.Select))),mean)
Y<-tapply(t(object@Coord.Select)[,2],as.factor(rownames(t(object@Coord.Select))),mean)
Coordes<-cbind(X,Y)

if(NGroups>nrow(Coordes)){
message("You should choose a lower value for NGroups. (NGroups=2)")
}

Colo<-cutree(agnes(t(CoordsP),metric="euclidean",stand=FALSE,method="ward"),k=NGroups)
Grup<-t(apply(CoordsP,1,function(y){tapply(y,Colo,mean)}))
Matrix<-object@Compromise.Coords[,1:2]%*%Grup
colnames(Matrix)<-paste("G",seq(1:NGroups))


#mean of groups
Xmean<-Grup[1,]
Ymean<-Grup[2,]


Groups<-list()
for(i in 1:ncol(Grup)){
  Groups[[i]]<-rownames(Coordes[Colo==i,])
}

#ortogonal projections
#calculando proyecciones
Proyec<-list()

Ordenes<-list()
  for(i in 1:length(Xmean)){
  V<-as.matrix(rbind(Xmean[i],Ymean[i]),ncol=1)
  D<-sum(V*V)
  PM<-c(object@Compromise.Coords[,1:2]%*%V/D)
  V2<-matrix(rep(t(V),nrow(object@Compromise.Coords[,1:2])),ncol=2,byrow=TRUE)
  Proyec[[i]]<-V2*PM
  Signos<-sign(apply((Proyec[[i]]*V2),1,sum))
  names(Signos)<-rownames(object@Compromise.Coords)
  rownames(Proyec[[i]])<-rownames(object@Compromise.Coords)
  Ord1<-Proyec[[i]][,1]^2+Proyec[[i]][,2]^2
  Ord<-as.data.frame(cbind(Ord1,Signos))
  Signos[order(Ord[,1],decreasing=TRUE)]
   Ordenes[[i]]<-as.data.frame(Signos)
  colnames(Ordenes[[i]])<-c("(+1 over-exp) (-1 under-exp)")
  }


.Object<-new("GroupProj")
.Object@SortList<-Ordenes
.Object@ProyGroups<-Proyec
.Object@Groups<-Groups
validObject(.Object)
return(.Object)


})
