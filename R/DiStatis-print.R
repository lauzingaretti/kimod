#'  \code{\link{print.DiStatis}}  a DiStatis object
#' Generic Print/Summary method for DiStatis class output visualization.
#' @param x DiStatis class object.
#'
#' @return according to the call
#'  \item{print}{console output text with increasing detail of
#'  DiStatis object.}
#'
#' @seealso \code{\link{DiStatis}}
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'
#'  print(fit)
#'
#'}
#' @exportMethod print
#' @docType methods
#' @name print.DiStatis
#' @rdname DiStatis-print
#' @usage \S4method{print}{DiStatis}(x)
#' @aliases print,DiStatis-method
#'
#'
#setGeneric(name="print.DiStatis",function(x){standardGeneric("print.DiStatis")})

   setMethod(f="print", signature="DiStatis", definition=function(x){
   ##If term is available the data of the corresponding term

    cat("Distance Methods (head):\n")
    print(x@distances.methods)
    cat("Inertia(RV):\n")
    print(head(x@Inertia.RV))
    cat("Vectorial-Correlation:\n")
    print(head(x@RV))
    cat("Inertia Compromise:\n")
    print(head(x@Inertia.comp))
    cat("Compromise-Coords:\n")
    print(head(x@Compromise.Coords))
    cat("Compromise-Matrix:\n")
    print(head(x@Compromise.Matrix))
    cat("Quality of Representation of the observations:\n")
    print(head(x@RQO))


    ##All the terms of the model

})

#' \code{\link{summary.DiStatis}}  a DiStatis object
#' @param object DiStatis class object.
#' @return
#'  \item{summary}{console output text with increasing detail of
#'  DiStatis object.}
#'
#' @seealso \code{\link{DiStatis}}
#'
#' @author M L Zingaretti, J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'
#'
#'  summary(fit)
#'
#'}
#'
#' @exportMethod summary
#' @name summary.DiStatis
#' @rdname DiStatis-summary
#' @usage \S4method{summary}{DiStatis}(object)
#' @aliases summary,DiStatis-method
#'
#'
#setGeneric(name="summary.DiStatis",function(object){standardGeneric("summary.DiStatis")})


setMethod(f="summary",signature="DiStatis",definition=function(object){
  return(show(object))
})
