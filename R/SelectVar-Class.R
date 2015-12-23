#' Class \code{SelectVar} S4 class (kimod: k-tables approach to integrate multiple Omics-Data
#' Multiple dataset)
#' SelectVar to DiStatis object.

#' @section Features:
#' \enumerate{
#'   \item SelectVar (Generate a biplot making linear model of all variables over the Compromise Matrix)
#'   \item Plotting Biplot.
#'   \item Plotting Biplot-Group.
#' }
#'
#' @section Fields:
#' \itemize{
#'
#'
#'\item  Coord.Select matrix are coordenates (Betas) of selected variables.
#'\item  Table.Select return to the data frame with all variables selected (from all studies or tables)
#'\item  List.Selec.Var return to the character with the names of all variables selected.
#'\item  List.Selec.Est list of all variables selected (with its respective table)
#'\item  Compromise.Coords Coords of projected observations in compromise matrix.
#' }
#' @section Accesors:
#' \itemize{
#'
#'
#'\item  Coord.Select matrix are coordenates (Betas) of selected variables.
#'\item  Table.Select return to the data frame with all variables selected (from all studies or tables)
#'\item  List.Selec.Var return to the character with the names of all variables selected.
#'\item  List.Selec.Est list of all variables selected (with its respective table)
#' }
#' @section SelectVar-general-functions:
#'\describe{
#'\item{print}{Generated basic output for SelectVar class}
#'\item{summary}{Generated basic statistics for SelectVar class}
#'\item{SelectVar}{Getters for their respective slots.}
#' }
#'
#'  @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @seealso \code{\link{Biplot}}, and additional related
#'  SelectVar class functions.
#'
#' @references
#' \enumerate{
#'  \item Demey, J., Vicente-Villardon, J. L., Galindo, M.P.  & Zambrano, A. (2008) Identifying Molecular Markers Associated With Classification Of Genotypes Using External Logistic Biplots. Bioinformatics, 24(24), 2832-2838.

#' \item Gabriel, K. (1971). The biplot graphic display of matrices with application to principal component analysis. Biometrika 58(3), 453--467.

#' \item Gower, J. & Hand, D. (1996). Biplots, Monographs on statistics and applied probability. 54. London: Chapman and Hall., 277 pp.

#'
#' }
#'
#'#' @examples
#' {
#' showClass("SelectVar")
#' }
#' @name SelectVar-class
#' @rdname SelectVar-Class
#' @exportClass SelectVar
#' @aliases Coord.Select,SelectVar-method
#' @aliases Table.Select,SelectVar-method
#' @aliases List.Selec.Var,SelectVar-method
#' @aliases List.Selec.Est,SelectVar-method




setClass(Class="SelectVar",
         representation=representation(
           Coord.Select="matrix",
           Table.Select="matrix",
           List.Selec.Var="character",
           List.Selec.Est="list",
           Compromise.Coords="matrix"),
         prototype=prototype(
           Coord.Select=matrix(),
           Table.Select=matrix(),
           List.Selec.Var=character(),
           List.Selec.Est=list(),
           Compromise.Coords=matrix())
)

setGeneric("Coord.Select<-", function(x, value) standardGeneric("Coord.Select<-"))
setReplaceMethod("Coord.Select", c("SelectVar", "matrix"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Coord.Select") <- value
                   x
                 })


setGeneric(name="Coord.Select", def = function(x){
  standardGeneric("Coord.Select")})
setMethod("Coord.Select", "SelectVar", function(x) x@Coord.Select)


setGeneric("Table.Select<-", function(x, value) standardGeneric("Table.Select<-"))
setReplaceMethod("Table.Select", c("SelectVar", "matrix"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Table.Select") <- value
                   x
                 })
setGeneric(name="Table.Select", def = function(x){
  standardGeneric("Table.Select")})
setMethod("Table.Select", "SelectVar", function(x) x@Table.Select)

setGeneric("List.Selec.Var<-", function(x, value) standardGeneric("List.Selec.Var<-"))
setReplaceMethod("List.Selec.Var", c("SelectVar", "character"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "List.Selec.Var") <- value
                   x
                 })

setGeneric(name="List.Selec.Var", def = function(x){
  standardGeneric("List.Selec.Var")})
setMethod("List.Selec.Var", "SelectVar", function(x) x@List.Selec.Var)


setGeneric("List.Selec.Est<-", function(x, value) standardGeneric("List.Selec.Est<-"))
setReplaceMethod("List.Selec.Est", c("SelectVar", "list"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "List.Selec.Est") <- value
                   x
                 })

setGeneric(name="List.Selec.Est", def = function(x){
  standardGeneric("List.Selec.Est")})
setMethod("List.Selec.Est", "SelectVar", function(x) x@List.Selec.Est)
