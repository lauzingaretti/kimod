#' Plot a \code{CompPlot} of a DiStatis object
#'
#'
#' @param x DiStatis class object.
#' @param xlabProj character for the x-label title for plot
#' @param ylabProj character for the x-label title for plot
#' @param xlabBar character for the x-label title for barPlot
#' @param ylabBar character for the y-label title for barPlot
#' @param mainBar main to histogram plot
#' @param mainProj main to proj plot
#' @param pchPoints pch for points in plot.
#' @param legend Logical. indicates whether the legend prints
#' @param colBar character col for bars in the BarPlot
#' @param colObs character col for observations in the plot
#' @param barPlot logical indicates whether the barPlot is prints
#' @param ... additional parameters for plot
#'
#' @return plotted CompPlot/s of the component/s of the given DiStatis object.
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @examples
#' {
#' \dontrun{
#'  data(NCI60Selec)
#'  Z2<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  Colores2<-c(rep(colors()[657],5),rep(colors()[637],6),
#'  rep(colors()[537],7),rep(colors()[552],6),rep(colors()[57],10),
#'  rep(colors()[300],9),rep(colors()[461],7),rep(colors()[450],2),
#'  rep(colors()[432],8))
#'  Colores1<-c(rep("Breast",5),rep("CNS",6),rep("Colon",7),
#'  rep("Leukemia",6),rep("Melanoma",10),rep("Lung",9),rep("Ovarian",7),
#'  rep("Prostate",2),rep("Renal",8))
#'  CompPlot(Z2,xlabBar="",colObs=Colores2,pch=15,las=1,
#'  cex=2,legend=FALSE,barPlot=FALSE,cex.main=0.6,cex.lab=0.6,
#'  cex.axis=0.6,las=1)
#'  legend("topleft",unique(Colores1),col=unique(Colores2),
#'  bty="n",pch=16,cex=1)
#' }
#' }
#' @exportMethod CompPlot
#' @docType methods
#' @usage \S4method{CompPlot}{DiStatis}(x,xlabProj=NULL, ylabProj=NULL,
#' xlabBar="Inertia(\%)",ylabBar="Values",mainBar=NULL,mainProj=NULL,
#' pchPoints=15,legend=TRUE ,colBar="red",colObs="black",barPlot=TRUE,...)
#' @name CompPlot
#' @rdname DiStatis-CompPlot
#' @aliases CompPlot,DiStatis-method

setGeneric("CompPlot",def=function(x,xlabProj=NULL, ylabProj=NULL,xlabBar="Inertia(%)",ylabBar="Values",
                                 mainBar=NULL,mainProj=NULL,pchPoints=15,
                                 legend=TRUE ,colBar="red",colObs="black",barPlot=TRUE,...){standardGeneric("CompPlot")})



setMethod(f="CompPlot", signature="DiStatis", definition=function(x,
  xlabProj=NULL, ylabProj=NULL,xlabBar="Inertia(%)",ylabBar="Values",
  mainBar=NULL,mainProj=NULL,pchPoints=15,
  legend=TRUE ,colBar="red",colObs="black",barPlot=TRUE,...){
  ##Check that is at element is available
  if (is.null(x@Inertia.comp) && is.null(x@Compromise.Coords)){
    stop("Invalid Object")
  }

if(barPlot==TRUE){
  layout(rbind(c(1,1,1,1,1,1,1,2,2,2),c(1,1,1,1,1,1,1,2,2,2)
               ,c(1,1,1,1,1,1,1,3,3,3),c(1,1,1,1,1,1,1,3,3,3)))
}

if(barPlot==FALSE){
  par(mfrow=c(1,1))
}

if (is.null(mainBar)){
  mainBar=""
}
if(is.null(mainProj)){
mainProj=""
}


if(is.null(xlabProj)){
  xlabProj=paste("Dim 1(",round(x@Inertia.comp[1,2],2),"%)")
}

if(is.null(ylabProj)){
  ylabProj=paste("Dim 2( ",round(x@Inertia.comp[2,2],2),"%)")
}



if(barPlot==TRUE){


PARACCIND<-x@Compromise.Coords


plot(PARACCIND[,1:2], type = "n",xlab =xlabProj,
    ylab=ylabProj,main=mainProj,asp=1,...)
  abline(h = 0, v = 0, col = "black",lty=3)
  points(PARACCIND[,1:2],pch=pchPoints,col=colObs)
  if(legend==TRUE){
  text(PARACCIND[,1:2], labels =rownames(PARACCIND),cex=0.75)
  }



    barplot(x@Inertia.comp[,2],main=mainBar,
            col=colBar,las=2,cex.main=0.9,cex.axis=0.6,cex.names=0.6,cex.lab=0.7,
            ylab=ylabBar,xlab=xlabBar,ylim=c(0,100))

    #return(NULL)

  }else{


      PARACCIND<-x@Compromise.Coords


      plot(PARACCIND[,1:2], type = "n",xlab =xlabProj,
           ylab =ylabProj,main=mainProj,asp=1,...)
      abline(h = 0, v = 0, col = "black",lty=3)
      points(PARACCIND[,1:2],pch=pchPoints,col=colObs)
      if(legend==TRUE){
        text(PARACCIND[,1:2], labels = rownames(PARACCIND),cex=0.75)
      }
      #return(NULL)


  }


})
