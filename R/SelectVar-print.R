#' \code{\link{print.SV}} a SelectVar object
#' Generic Print/Summary method for SelectVar class output visualization.
#' @param x SelectVar class object.
#'
#
#' @return according to the call
#'  \item{print}{the console output the text with increasing detail of
#'  SelectVar object.}
#'
#' @seealso \code{\link{SelectVar}}
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  A<-SelectVar(fit,Crit="p-val(Bonf)",perc=0.95)
#'
#'  print(A)
#'  summary(A)
#'
#'}
#' @exportMethod print
#' @docType methods
#' @name print.SV
#' @rdname SelectVar-print
#' @usage \S4method{print}{SelectVar}(x)
#' @aliases print,SelectVar-method

#setGeneric(name="print.SV",function(x){standardGeneric("print.SV")})
setMethod(f="print", signature="SelectVar", definition=function(x){
   ##If term is available the data of the corresponding term

    cat("Coord.Select:\n")
    print(head(x@Coord.Select))

    cat("Table Select:\n")
    print(head( x@Table.Select))

    cat("List Select Var:\n")
    print(head(x@List.Selec.Var))



    }

    ##All the terms of the model

)


#' \code{\link{summary}} a SelectVar object
#' Generic Print/Summary method for SelectVar class output visualization.
#'
#' @param object SelectVar class object.
#'
#
#' @return
#'  \item{summary}{console output text with increasing detail of
#'  SelectVar object.}
#'
#' @seealso \code{\link{SelectVar}}
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente-Villardon, J R Demey
#'
#' @examples
#' {
#'
#'  data(NCI60Selec)
#'  fit<-DiStatis(NCI60Selec,Scale=TRUE,Center=TRUE)
#'  A<-SelectVar(fit,Crit="p-val(Bonf)",perc=0.95)
#'
#'  summary(A)
#'
#'}



#' @exportMethod summary
#' @docType methods
#' @name summary.SV
#' @rdname SelectVar-summary
#' @inheritParams summary
#' @usage \S4method{summary}{SelectVar}(object)
#' @aliases summary,SelectVar-method

#setGeneric(name="summary.SV",function(object){standardGeneric("summary.SV")})
setMethod(f="summary",signature="SelectVar", definition=function(object){
  return(print(object))
})
