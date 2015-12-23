#' Plot a \code{BootPlot} of a Bootstrap object
#' @param x  object of Bootstrap-Class
#' @param xlabProj character for the x-label title for plot
#' @param ylabProj character for the x-label title for plot
#' @param mainProj main to proj plot
#' @param legend Logical. It indicates whether the legend prints
#' @param colour colours for ellipsis
#' @param xlimi bounds of x-axis
#' @param ylimi bounds of y-axis
#' @param Points logical if is true, the points are plotted
#' @param ... additional parameters for plot (generics)
#' @return plotted Bootplot/s of the component/s of the given Bootstrap object.
#'
#' @seealso \code{\link{CompPlot}}, \code{\link{TrajPlot}},
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @examples
#'  Z2<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  M<-Bootstrap(Z2)
#'  \dontrun{
#'  BootPlot(M)
#'  }
#'  Colores2<-c(rep(colors()[657],5),rep(colors()[637],6),rep(colors()[537],7),
#'  rep(colors()[552],6),rep(colors()[57],10),rep(colors()[300],8),
#'  rep(colors()[461],7),rep(colors()[450],2),rep(colors()[432],7))
#'  Colores1<-c(rep("Breast",5),rep("CNS",6),rep("Colon",7),
#'  rep("Leukemia",6),rep("Melanoma",10),rep("Lung",8),rep("Ovarian",7),
#'  rep("Prostate",2),rep("Renal",7))
#'  BootPlot(M,Points=FALSE,cex.lab=0.7,cex.axis=0.7,
#'  las=1,xlimi=c(-0.003,0.002),ylimi=c(-0.005,0.007)
#'  ,legend=FALSE,col=Colores2)
#'  legend("topleft",unique(Colores1),col=unique(Colores2),
#'  bty="n",pch=16,cex=1)

#' @exportMethod BootPlot
#' @docType methods
#' @usage \S4method{BootPlot}{Bootstrap}(x,xlabProj=NULL, ylabProj=
#' NULL,mainProj=NULL,legend=TRUE,colour=NULL,
#' xlimi=NULL,ylimi=NULL,Points=TRUE,...)
#' @name BootPlot
#' @rdname BootPlot
#' @aliases BootPlot,Bootstrap-method

setGeneric("BootPlot",def=function(x,xlabProj=NULL, ylabProj=NULL,
              mainProj=NULL,
          legend=TRUE,colour=NULL,xlimi=NULL,ylimi=NULL,Points=TRUE,...){standardGeneric("BootPlot")})



setMethod(f="BootPlot", signature="Bootstrap", definition=function(x,
  xlabProj=NULL, ylabProj=NULL,mainProj=NULL,
  legend=TRUE,colour=NULL,xlimi=NULL,ylimi=NULL,Points=TRUE ,...){
  ##Check that is at element is available
  if (is.null(x@Elipses.Boot)){
    stop("Invalid Object")
  }

if(is.null(mainProj)){
mainProj=""
}

if(is.null(xlabProj)){
  xlabProj=paste("Dim 1 (",round(x@Inertia.Boot[1,1],2),"%)")
}

if(is.null(ylabProj)){
  ylabProj=paste("Dim 2 (",round(x@Inertia.Boot[2,1],2),"%)")
}

Nom <- unique(rownames(x@Elipses.Boot))

S <- list()
Medias <- list()
Cov <- list()


for (i in 1:length(Nom)){
  S[[i]] <- x@Elipses.Boot[rownames(x@Elipses.Boot)==Nom[i],]
  Medias[[i]] <- colMeans(S[[i]])
  Cov[[i]] <- cov(S[[i]])
}

names(S) <- Nom
names(Medias) <- Nom
names(Cov) <- Nom

if(is.null(colour)){
  colour <- rgb(red=sample(seq(0:255),length(Nom)),green=sample(seq(0:255),length(Nom)),
                blue=sample(seq(0:255),length(Nom)),maxColorValue=256)
}
if(length(colour)==1){
  colour<-rep(colour,length(Nom))
}

if (is.null(xlimi)){
xlimi=c(min(x@Elipses.Boot[,1]),max(x@Elipses.Boot[,1]))
}

if (is.null(ylimi)){
  ylimi=c(min(x@Elipses.Boot[,2]),max(x@Elipses.Boot[,2]))
}






  plot(S[[i]][,1],S[[i]][,2],
  ylab=ylabProj,xlab=xlabProj,main=mainProj,type="n",xlim=xlimi,ylim=ylimi,asp=1,...)
  abline(h =0,v =0,col="black",lty=3)

  for(i in 1:length(Nom)) {
    if(Points==TRUE){
    points(S[[i]][,1],S[[i]][,2],col=colour[i],cex=0.6)
    }
    confelli(Medias[[i]],Cov[[i]],nrow(S[[i]])-2,grupo=Nom[i],col=colour[i])

  }

  if(legend==TRUE){
    for(i in 1: length(Nom)){
      text(Medias[[i]][1],Medias[[i]][2],labels=Nom[i],font=2,cex=0.75,col="black")
    }
  }
  #return(NULL)


})
