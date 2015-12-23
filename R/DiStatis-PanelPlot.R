#' Plot a \code{PanelPlot} of a DiStatis object
#'
#'
#' @param x DiStatis class object.
#'
#'
#' @return Panel plot  of the given DiStatis object.

#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @examples
#' {
#' data(NCI60Selec)
#' Z2<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#' PanelPlot(Z2)
#' data(winesassesors)
#' Z3<-DiStatis(winesassesors)
#' PanelPlot(Z3)
#'
#' }

#' @exportMethod PanelPlot
#' @docType methods
#' @usage \S4method{PanelPlot}{DiStatis}(x)
#' @name PanelPlot
#' @rdname DiStatis-PanelPlot
#' @aliases PanelPlot,DiStatis-method
#'

setGeneric("PanelPlot",def=function(x){standardGeneric("PanelPlot")})



setMethod(f="PanelPlot", signature="DiStatis", definition=function(x){
##Check that is at element is available




par(mfrow=c(2,2))
plot(x@Euclid.Im[,1:2], type = "n",
 xlab ="Dim 1",
 ylab ="Dim 2",xlim=c(-1,1),ylim=c(-1,1),
 main="",las=1, cex.axis=0.6,cex.lab=0.7,cex.axis=0.6,cex.lab=0.6)
curve(sqrt(1-x^2),-1,1,xlim=c(-1,1),ylim=c(-1,1),add=TRUE,lty=2)
curve(-sqrt(1-x^2),-1,1,add=TRUE,lty=2)
arrows(0, 0, x@Euclid.Im[,1],x@Euclid.Im[,2],
  col ="black", length = .10, angle = 10)
abline(h = 0, v = 0, col = "black",lty=3,xlim=c(-1,1),ylim=c(-1,1))

text(x@Euclid.Im,pos=4,labels = rownames(x@Euclid.Im),cex=0.8)

barplot(x@Inertia.RV[,2],main="",
col="red",las=2,cex.main=0.9,cex.names=0.6,
            ylab="",xlab="",ylim=c(0,100),cex.axis=0.6,cex.lab=0.6)

 PARACCIND<-x@Compromise.Coords


plot(PARACCIND[,1:2], type = "n",xlab = paste("Dim 1(",round(x@Inertia.comp[1,2],2),"%)"),
ylab =paste("Dim 2( ",round(x@Inertia.comp[2,2],2),"%)"),main="",cex.axis=0.6,cex.lab=0.6,asp=1)
    abline(h = 0, v = 0, col = "black",lty=3)
    points(PARACCIND[,1:2])
    text(PARACCIND[,1:2], labels = rownames(PARACCIND),cex=0.75)

    barplot(x@Inertia.comp[,2],main="",
    col="red",las=2,cex.main=0.9,cex.axis=0.6,cex.names=0.6,
    ylab="",xlab="",ylim=c(0,100),cex.lab=0.6)
    #return(NULL)



})
