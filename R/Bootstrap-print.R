#'  \code{\link{print}} or \code{\link{summary}} a Bootstrap object
#'
#' Generic Print/Summary method for Bootstrap class output visualization.
#'
#' @param x Bootstrap class object.
#'
#
#' @return according to the call
#'  \item{print}{console output text with increasing detail of
#'  Bootstrap object.}
#'
#'
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  A<-Bootstrap(fit)
#'
#'  print(A)
#'
#'
#'}
#' @exportMethod print
#' @docType methods
#' @name print.Bootstrap
#' @rdname Bootstrap-print.Boot
#' @usage \S4method{print}{Bootstrap}(x)
#' @aliases print,Bootstrap-method

 setMethod(f="print", signature="Bootstrap", definition=function(x){
   ##If term is available the data of the corresponding term

    cat("Ratios Boot:\n")
    print(x@Ratios.Boot)

    cat("Comparision-Boot:\n")
    print(head( x@Comparisions.Boot))

    cat("Stability-boot:\n")
    print( x@Stability.Boot)
    cat("Representation Quality boot:\n")
    print(head(x@QRO.Boot))
    cat("Inertia-Boot:\n")
    print(head(x@Inertia.Boot))

    }

    ##All the terms of the model

)

#'  \code{\link{print.Bootstrap}} or \code{\link{summary.Bootstrap}} a Bootstrap object
#'
#' Generic Print/Summary method for Bootstrap class output visualization.
#'
#' @param object Bootstrap class object.
#'
#
#' @return according to the call
#'  \item{ summary}{console output text with increasing detail of
#'  Bootstrap object.}
#'
#'
#'
#' @author M L Zingaretti, J A Demey, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  A<-Bootstrap(fit)
#'
#'
#'  summary(A)
#'
#'}


#' @exportMethod summary
#' @name summary.Bootstrap
#' @rdname Bootstrap-summary
#' @inheritParams summary
#' @usage \S4method{summary}{Bootstrap}(object)
#' @aliases summary,Bootstrap-method


 setMethod(f="summary",signature="Bootstrap", definition=function(object){
  return(print(object))
})
