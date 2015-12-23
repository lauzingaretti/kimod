#' Plot a \code{RVPlot} of a DiStatis object
#'
#' @param x DiStatis class object.
#'
#' @param xlabProj character for the x-label title for plot
#' @param ylabProj character for the y-label title for plot
#' @param xlabBar character for the x-label title for barPlot
#' @param ylabBar character for the y-label title for barPlot
#' @param mainBar the main histogram plot
#' @param mainProj the main proj plot
#' @param colArrows character col for arrows in ProjPlot
#' @param legend Logical. indicates whether the legend prints
#' @param colBar character col for bars in the BarPlot
#' @param barPlot logical indicates whether the barPlot is prints
#' @param ... additional parameters for plot
#' @return plotted RVplot/s of the component/s of the given DiStatis object.
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#' @examples
#' {
#'  Z2<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  RVPlot(Z2)
#'  }
#'
#'

#' @exportMethod RVPlot
#' @docType methods
#' @usage \S4method{RVPlot}{DiStatis}(x,xlabProj="Dim 1", ylabProj="Dim 2",
#' xlabBar="Inertia(\%)",ylabBar="Values" ,mainBar=NULL,mainProj=NULL,
#' colArrows="black",legend=TRUE ,colBar="red",barPlot=TRUE,...)
#' @name RVPlot
#' @rdname DiStatis-RVPlot
#' @aliases RVPlot,DiStatis-method
#'
#'
setGeneric("RVPlot",def=function(x,xlabProj="Dim 1", ylabProj="Dim 2",xlabBar="Inertia(%)",ylabBar="Values",
                                 mainBar=NULL,mainProj=NULL,colArrows="black",
                                 legend=TRUE ,colBar="red",barPlot=TRUE,...){standardGeneric("RVPlot")})



setMethod(f="RVPlot", signature="DiStatis", definition=function(x,
  xlabProj="Dim 1", ylabProj="Dim 2",xlabBar="Inertia(%)",ylabBar="Values",
  mainBar=NULL,mainProj=NULL,colArrows="black",
  legend=TRUE ,colBar="red",barPlot=TRUE,...){
  ##Check that is at element is available
  if (is.null(x@Inertia.RV) && is.null(x@Euclid.Im)){
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

  if(barPlot==TRUE){

    plot(x@Euclid.Im[,1:2], type = "n",
         xlab =xlabProj,
         ylab =ylabProj,xlim=c(-1,1),ylim=c(-1,1),main=mainProj,las=1,asp=1,
         cex.axis=0.6,cex.lab=0.7,...)
    curve(sqrt(1-x^2),-1,1,xlim=c(-1,1),ylim=c(-1,1),add=TRUE,lty=2)
    curve(-sqrt(1-x^2),-1,1,add=TRUE,lty=2)
    arrows(0, 0, x@Euclid.Im[,1],x@Euclid.Im[,2],
           col =colArrows, length = .10, angle = 10)
    abline(h = 0, v = 0, col = "black",lty=3,xlim=c(-1,1),ylim=c(-1,1))
    if(legend==TRUE){
    text(x@Euclid.Im[,1:2],pos=4,labels =rownames(x@Euclid.Im),cex=0.8)
    }

    barplot(x@Inertia.RV[,2],main=mainBar,
            col=colBar,las=2,cex.main=0.9,cex.axis=0.6,cex.names=0.6,cex.lab=0.7,
            ylab=ylabBar,xlab=xlabBar,ylim=c(0,100))
    #return(NULL)

  }else{


      plot(x@Euclid.Im[,1:2], type = "n",
           xlab =xlabProj,
           ylab =ylabProj,xlim=c(-1,1),ylim=c(-1,1),main=mainProj,las=1,asp=1,
           cex.axis=0.6,cex.lab=0.7,...)
      curve(sqrt(1-x^2),-1,1,xlim=c(-1,1),ylim=c(-1,1),add=TRUE,lty=2)
      curve(-sqrt(1-x^2),-1,1,add=TRUE,lty=2)
      arrows(0, 0, x@Euclid.Im[,1],x@Euclid.Im[,2],
             col =colArrows, length = .10, angle = 10)
      abline(h = 0, v = 0, col = "black",lty=3,xlim=c(-1,1),ylim=c(-1,1))
      if(legend==TRUE){
        text(x@Euclid.Im[,1:2],pos=4,labels = rownames(x@Euclid.Im),cex=0.8)
      }

      #return(NULL)


  }


})
