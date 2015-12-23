#' \code{\link{print.GP}} a GroupProj object
#' Generic print.GP method for GroupProj class output visualization.
#'
#' @param x GroupProj class object.
#'
#' @return according to the call
#'  \item{print}{console generates output text with increasing detail of
#'  GroupProj object.}
#'
#' @seealso \code{\link{GroupProj}}
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  A<-SelectVar(fit,Crit="p-val(Bonf)",perc=0.95)
#'  M3<-GroupProj(A,NGroups=4,method="ward",metric="euclidean")
#'
#'  print(M3)
#'  summary(M3)
#'
#'}
#' @exportMethod print
#' @docType methods
#' @name print.GP
#' @rdname GroupProj-print
#' @usage \S4method{print}{GroupProj}(x)
#' @aliases print,GroupProj-method


 # setGeneric(name="print",function(x){standardGeneric("print.GP")})

   setMethod(f="print", signature="GroupProj", definition=function(x){
   ##If term is available the data of the corresponding term

    cat("SortList-Groups:\n")

    print(head( x@SortList))

    cat("Groups-Var:\n")
    print(head(x@Groups))




    }

    ##All the terms of the model

)

#' \code{\link{summary.GP}} a GroupProj object
#' @param object GroupProj class object.
#'
#'
#' @return according to the call
#'  \item{summary}{console output text with increasing detail of
#'  GroupProj object.}
#'
#' @seealso \code{\link{GroupProj}}
#'
#' @author M L Zingaretti, J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  A<-SelectVar(fit,Crit="p-val(Bonf)",perc=0.95)
#'  M3<-GroupProj(A,NGroups=4,method="ward",metric="euclidean")
#'
#'
#'  summary(M3)
#'
#'}


#' @exportMethod summary
#' @name summary.GP
#' @rdname GroupProj-summary
#' @inheritParams summary
#' @usage \S4method{summary}{GroupProj}(object)
#' @aliases summary,GroupProj-method

# setGeneric(name="summary",function(object){standardGeneric("summary.GP")})
 setMethod(f="summary",signature="GroupProj", definition=function(object){
  return(print(object))
})
