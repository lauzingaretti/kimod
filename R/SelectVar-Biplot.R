#' Plot a \code{\link{Biplot}} of a SelectVar class object
#'
#'
#' @param x DiStatis class object.
#' @param xlab character for the x-label title for plot
#' @param ylab character for the x-label title for plot
#' @param mainP the main Biplot
#' @param xlimi (vector) Bounds to x-axis
#' @param ylimi (vector) Bounds to y-axis
#' @param labelObs Logical. indicates whether the labels of observations are prints. Default is TRUE
#' @param labelVars Logical. indicates whether the labels of variables are prints. Default is TRUE
#' @param colVar character col for colours of the variables in the plot. Default is black.
#' @param colObs  character col for colours of the observations in the plot. Default is black.
#' @param pchPoints Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param Type type of Biplot. Options are CMP RMP SQRT or HJ.
#' @param Groups Logical. If is TRUE, the variables are grouped. See \code{\link{GroupProj}}
#' @param NGroups Only if the Groups are TRUE. Indicate the number the groups of variables.
#' @param ... additional parameters for plot
#'
#' @return plotted Biplot/s of the component/s of the given SelectVar object.
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @examples
#' {
#'data(NCI60Selec)
#'Z1<-DiStatis(NCI60Selec)
#'M1<-SelectVar(Z1,Crit="R2-Adj")
#'Colores1<-c(rep("Breast",5),rep("CNS",6),rep("Colon",7),
#'rep("Leukemia",6),rep("Melanoma",10),rep("Lung",9),
#'rep("Ovarian",7),rep("Prostate",2),rep("Renal",8))
#'Colores2<-c(rep(colors()[657],5),rep(colors()[637],6),
#'rep(colors()[537],7),rep(colors()[552],6),rep(colors()[57],10)
#',rep(colors()[300],9),rep(colors()[461],7),rep(colors()[450],2)
#',rep(colors()[432],8))
#'Biplot(M1,labelObs = FALSE,labelVars=FALSE,
#'colObs=Colores2,Type="SQRT",las=1,cex.axis=0.8,
#'cex.lab=0.8,xlimi=c(-3,3),ylimi=c(-3,3))
#'legend("topright",unique(Colores1),col=unique(Colores2),
#'bty="n",pch=16,cex=0.6)
#'Biplot(M1,labelObs = FALSE,labelVars=TRUE,colObs=Colores2,
#'Type="SQRT",las=1,cex.axis=0.8,cex.lab=0.8,xlimi=c(-3,3),
#'ylimi=c(-3,3),Groups=TRUE,NGroups=6)
#'legend("topright",unique(Colores1),col=unique(Colores2),
#'bty="n",pch=16,cex=0.6)

#'data(winesassesors)
#'Z3<-DiStatis(winesassesors)
#' M3<-SelectVar(Z3,Crit="R2-Adj")
#'Col1<-c(rep("NZ",4),rep("FR",4),rep("CA",4))
#'Col2<-c(rep(2,4),rep(3,4),rep(4,4))
#'Biplot(M3,labelObs=FALSE,labelVars=TRUE,colObs=Col2,
#'Type="SQRT",xlimi=c(-2,2),ylimi=c(-2,2),las=1,cex.axis=0.8,
#'cex.lab=0.8)
#'legend("topright",unique(Col1),col=unique(Col2),bty="n",pch=16,cex=0.8)
#'Biplot(M1,labelObs = FALSE,labelVars=FALSE,colObs=Colores2,
#'Type="CMP")
#' legend("topright",unique(Colores1),
#'col=unique(Colores2),bty="n",pch=16,cex=1)
#'
#'}


#' @exportMethod Biplot
#' @docType methods
#' @usage \S4method{Biplot}{SelectVar}(x,xlab=NULL, ylab=NULL, mainP=NULL,
#' xlimi=NULL, ylimi=NULL, labelObs=TRUE,labelVars= TRUE,
#' colVar= "black",colObs="black",
#'  pchPoints=15,Type=c("RMP","CMP","SQRT","HJ"),
#'  Groups=FALSE,NGroups=2 ,...)
#' @name Biplot
#' @rdname SelectVar-Biplot
#' @aliases Biplot,SelectVar-method
#'
#'
setGeneric("Biplot",def=function(x,xlab=NULL, ylab=NULL, mainP=NULL, xlimi=NULL, ylimi=NULL,labelObs=TRUE ,labelVars= TRUE,colVar= "black",colObs="black",
            pchPoints=15,Type=c("RMP","CMP","SQRT","HJ"),Groups=FALSE,NGroups=2,...){standardGeneric("Biplot")})



setMethod(f="Biplot", signature="SelectVar", definition=function(x,xlab=NULL, ylab=NULL, mainP=NULL, xlimi=NULL, ylimi=NULL,labelObs=TRUE,labelVars=TRUE,colVar= "black",colObs="black",
          pchPoints=15,Type=c("RMP","CMP","SQRT","HJ"),Groups=FALSE,NGroups=2 ,...){
  ##Check that is at element is available
  if (is.null(x@Coord.Select) && is.null(x@Compromise.Coords)){
    stop("Invalid Object")
  }
  ##Check parameters
  stopifnot(Type[1] %in% c("RMP","CMP","SQRT","HJ"))
  #stopifnot(require("cluster"))

if (is.null(mainP)){
  mainP=""
}


if(is.null(xlab)){
  xlab="Dim 1"
}

if(is.null(ylab)){
  ylab="Dim 2"
}

if(is.null(colVar)){
  colVar="black"
}
if(is.null(colObs)){
  colObs="black"
}

MediasP<-t(apply(x@Table.Select,1,function(y){tapply(y,colnames(x@Table.Select),mean)}))

CoordsP<-t(apply(x@Coord.Select,1,function(y){tapply(y,colnames(x@Coord.Select),mean)}))

if (Groups==TRUE){
Colo<-cutree(agnes(t(CoordsP),metric="euclidean",stand=FALSE,method="ward"),k=NGroups)
Grup<-t(apply(CoordsP,1,function(y){tapply(y,Colo,mean)}))
#Matrix<-x@Compromise.Coords[,1:2]%*%Grup
#colnames(Matrix)<-paste("G",seq(1:NGroups))
Matrix<-x@Compromise.Coords[,1:2]%*%CoordsP
}

if(Groups==FALSE){
Matrix<-x@Compromise.Coords[,1:2]%*%CoordsP
}





Svd1<-svd(Matrix)


if(Type=="RMP"){

 A<-Svd1$u%*%diag(Svd1$d)
 B<-Svd1$v
  }


if(Type=="CMP"){

  A<-Svd1$u
  B<-Svd1$v%*%diag(Svd1$d)
}
if(Type=="SQRT"){

 A<-Svd1$u%*%diag(sqrt(Svd1$d))
 B<-Svd1$v%*%diag(sqrt(Svd1$d))
}


if(Type=="HJ"){

  A<-Svd1$u%*%diag(Svd1$d)
  B<-Svd1$v%*%diag(Svd1$d)
  }

if (is.null(xlimi)){
xlimi=c(min(c(A[,1],B[,1])),max(c(A[,1],B[,1])))
}

if (is.null(ylimi)){
ylimi=c(min(c(A[,2],B[,2])),max(c(A[,2],B[,2])))
}


if(Groups==FALSE){
plot(A[,1:2], type = "n",xlab =xlab,
    ylab =ylab,xlim=xlimi,ylim=ylimi,main=mainP,asp=1,...)
  abline(h = 0, v = 0, col = "black",lty=3)
arrows(0,0,B[,1],B[,2],col=colVar,angle=20,length=0.2)
if(labelVars==TRUE){
  text(B[,1:2]+B[,1:2]*0.05, labels = colnames(Matrix),cex=0.6)
}
  points(A[,1:2],pch=pchPoints,col=colObs,...)
  if(labelObs==TRUE){
  text(A[,1:2], labels = rownames(Matrix),cex=0.75)
  }
  }
  #return(NULL)
if(Groups==TRUE){
  colVar=Colo
  plot(A[,1:2], type = "n",xlab =xlab,
       ylab =ylab,xlim=xlimi,ylim=ylimi,main=mainP,asp=1,...)
  abline(h = 0, v = 0, col = "black",lty=3)
  arrows(0,0,B[,1],B[,2],col=colVar,angle=20,length=0.2)
  if(labelVars==TRUE){
    text(B[,1:2]+B[,1:2]*0.05, labels = colnames(Matrix),cex=0.6)
  }
  points(A[,1:2],pch=pchPoints,col=colObs,...)
  if(labelObs==TRUE){
    text(A[,1:2], labels = rownames(Matrix),cex=0.75)
  }
}

})
