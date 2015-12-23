#' Plot a \code{TrajPlot} of a DiStatis object
#'
#'
#' @param x DiStatis class object.
#'
#' @param xlabT character for the x-label title for plot
#' @param ylabT character for the y-label title for plot
#' @param mainTraj the main proj plot
#' @param legend Logical. Indicates whether the legend prints
#' @param xlimi vector bounds to x-axes.
#' @param ylimi vector bounds to y-axes.
#' @param panel logical if is true, the trajectories are plotted in panel-plot
#' @param colours for plot
#' @param ... additional parameters for plot
#' @return plotted Trajectories Plot of the given DiStatis object.
#'
#' @author M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @examples
#' {
#' Z2<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#' Colores2<-c(rep(colors()[657],5),rep(colors()[637],6),
#' rep(colors()[537],7),rep(colors()[552],6),rep(colors()[57],10)
#' ,rep(colors()[300],8),rep(colors()[461],7),rep(colors()[450],2)
#' ,rep(colors()[432],7))
#' Colores1<-c(rep("Breast",5),rep("CNS",6),rep("Colon",7)
#' ,rep("Leukemia",6),rep("Melanoma",10),rep("Lung",8),rep("Ovarian",7)
#' ,rep("Prostate",2),rep("Renal",7))
#' TrajPlot(Z2,xlabT = "",ylabT="",colours=Colores2,legend=FALSE)
#' legend("topleft",unique(Colores1),col=unique(Colores2),bty="n",pch=16,cex=1)
#'
#' }

#' @exportMethod TrajPlot
#' @docType methods
#' @usage \S4method{TrajPlot}{DiStatis}(x,xlabT="Dim 1", ylabT="Dim 2",
#' mainTraj=NULL,legend=TRUE ,xlimi=NULL,ylimi=NULL,panel=TRUE,
#' colours=NULL,...)
#' @name TrajPlot
#' @rdname DiStatis-TrajPlot
#' @aliases TrajPlot,DiStatis-method
#'

setGeneric("TrajPlot",def=function(x,xlabT="Dim 1", ylabT="Dim 2",mainTraj=NULL,legend=TRUE
                                   ,xlimi=NULL,ylimi=NULL,panel=TRUE,colours=NULL,...){standardGeneric("TrajPlot")})



setMethod(f="TrajPlot", signature="DiStatis", definition=function(x,xlabT="Dim 1", ylabT="Dim 2",mainTraj=NULL,legend=TRUE
                                                                  ,xlimi=NULL,ylimi=NULL,panel=TRUE,colours=NULL,...){
  ##Check that is at element is available
  if (is.null(x@Trajectories)){
    return("The Traj argument on DiStatis function is FALSE and Trajectories is not calculates")

  }

  ListasTrayectorias<-x@Trajectories
if(panel==TRUE){

P<-length(x@Trajectories)
Cuad<-ceiling(sqrt(P))
if(Cuad==sqrt(P)){
  par(mfrow=c(Cuad,Cuad))
}
if(Cuad>sqrt(P)){
if((Cuad*(Cuad-1))>=P){
  par(mfrow=c(Cuad,Cuad-1))
}else
  par(mfrow=c(Cuad,Cuad))
}

if(Cuad>4){
  message("figure margins too large, It will be held one plot")
  par(mfrow=c(1,1))
  panel=FALSE
}
}
if(panel==FALSE){
  par(mfrow=c(1,1))
}


if (is.null(mainTraj)){
  mainTraj=""
}

minimox<-min(unlist(lapply(ListasTrayectorias,function(x){min(as.matrix(x[,1]))})))
maximox<-max(unlist(lapply(ListasTrayectorias,function(x){max(as.matrix(x[,1]))})))
minimoy<-min(unlist(lapply(ListasTrayectorias,function(x){min(as.matrix(x[,2]))})))
maximoy<-max(unlist(lapply(ListasTrayectorias,function(x){max(as.matrix(x[,2]))})))


if(is.null(xlimi)){
 xlimi=c(minimox-abs(0.2*minimox),maximox+abs(0.2*maximox))
 }

if(is.null(ylimi)){
  ylimi=c(minimoy-abs(0.2*minimoy),maximoy+abs(0.2*maximoy))
}

if(is.null(colours)){
  colores <- rgb(red=sample(seq(0:255),length(ListasTrayectorias)),green=sample(seq(0:255),length(ListasTrayectorias)),blue=sample(seq(0:255),length(ListasTrayectorias)),maxColorValue=256)
}else {
  colores<-colours
}

if( panel==FALSE){

  A <- c()
  plot(ListasTrayectorias[[1]][,1],ListasTrayectorias[[1]][,1],
    xlim=xlimi,ylim=ylimi ,main =mainTraj,xlab=xlabT,
    ylab=ylabT,type="n",las=1,xaxt="n",yaxt="n",...)
  abline(h=0,lty=3,col="black")
  abline(v=0,lty=3,col="black")
  for (j in 1:length(ListasTrayectorias)){
    for (i in 1:(nrow(ListasTrayectorias[[j]])-1)){
      segments(ListasTrayectorias[[j]][,1][i],ListasTrayectorias[[j]][,2][i],ListasTrayectorias[[j]][,1][i+1],ListasTrayectorias[[j]][,2][i+1],col=colores[j],lwd=2)
    }
    A <- c(A,colores[j])
  }
  if(legend==TRUE){
    legend("topright",legend=names(ListasTrayectorias),pch=15,col=A,cex=0.7,bty="n")
  }
  par(mfrow=c(1,1))
  #return(NULL)
}

if( panel==TRUE){

    A <- c()

    for (j in 1:length(ListasTrayectorias)){
      plot(ListasTrayectorias[[1]][,1],ListasTrayectorias[[1]][,1],xlim=xlimi,ylim=ylimi ,main =mainTraj,xlab=xlabT,ylab=ylabT,type="n",las=1,xaxt="n",yaxt="n",...)
      abline(h=0,lty=3,col="black")
      abline(v=0,lty=3,col="black")
      for (i in 1:(nrow(ListasTrayectorias[[j]])-1)){
        segments(ListasTrayectorias[[j]][,1][i],ListasTrayectorias[[j]][,2][i],ListasTrayectorias[[j]][,1][i+1],ListasTrayectorias[[j]][,2][i+1],col=colores[j],lwd=2)
      }
      A <- c(A,colores[j])
    }
    par(mfrow=c(1,1))
    #return(NULL)
}


})

