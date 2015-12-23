#' Class \code{Bootstrap}: Bootstrap S4 class (kimod: k-tables approach to integrate multiple Omics-Data
#' Multiple dataset)
#'
#' Bootstrap to DiStatis object.

#' @section Features:
#' \enumerate{
#'   \item Bootstrap Implementation
#'   \item Confidence Intervals from Bootstrap implementation
#'   \item Plotting Bootstrap
#' }
#'
#' @section Fields:
#' \itemize{
#' \item Ratios.Boot Ratios (with Bonferroni Correction for all observations in compromise from all dimensions)
#' \item Comparisions.Boot Comparision (difference between observations) from all dimensions
#' \item Elipses.Boot List of all projections from the elipses plot.
#' \item Stability.Boot Bootstrap Stability
#' \item QRO.Boot Rpresentation Quality of observations (Bootstrap)
#' \item EigValues.Boot confidence interval for eigenvalues (from SVD Compromise)
#' \item Inertia.Boot confidence interval for inertia(%) of all dimensions (from  SVD Compromise)
#' }
#' @section Accesors:
#' \itemize{
#' \item Ratios.Boot Ratios (with Bonferroni Correction for all observations in compromise from all dimensions)
#' \item Comparisions.Boot Comparision (difference between observations) from all dimensions
#' \item Elipses.Boot List of all projections from the elipses plot.
#' \item Stability.Boot Bootstrap Stability
#' \item QRO.Boot Rpresentation Quality of observations (Bootstrap)
#' \item EigValues.Boot confidence interval for eigenvalues (from SVD Compromise)
#' \item Inertia.Boot confidence interval for inertia(%) of all dimensions (from  SVD Compromise)
#' }
#' @section Bootstrap-general-functions:
#' \describe{
#'  \item{print}{Basic output for Bootstrap class}
#'  \item{summary}{Basic statistics for Bootstrap class}
#'  \item{Bootstrap}{Getters for their respective slots.}
#' }
#'
#'
#'@author M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#'
#'
#' @references
#' \enumerate{
#'  \item Efron, B.,Tibshirani, RJ. (1993). An introduction to the bootstrap. New York: Chapman and Hall. 436p.
#'  \item Ringrose, T.J. (1992). Bootstrapping and Correspondence Analysis in Archaeology. Journal of Archaeological. Science.19:615-629.
#'
#' }
#' @examples
#' {
#' showClass("Bootstrap")
#' }
#'
#' @name Bootstrap-class
#' @rdname Bootstrap-Class
#' @exportClass Bootstrap
#' @aliases Ratios.Boot,Bootstrap-methods
#' @aliases Comparisions.Boot,Bootstrap-methods
#' @aliases Elipses.Boot,Bootstrap-methods
#' @aliases Stability.Boot,Bootstrap-methods
#' @aliases QRO.Boot,Bootstrap-methods
#' @aliases EigValues.Boot,Bootstrap-methods
#' @aliases Inertia.Boot,Bootstrap-methods

setClass(Class="Bootstrap",
         representation=representation(
           Ratios.Boot="data.frame",
           Comparisions.Boot="list",
           Elipses.Boot="matrix",
           Stability.Boot="data.frame",
           QRO.Boot="data.frame",
           EigValues.Boot="data.frame",
           Inertia.Boot="data.frame"),
           prototype=prototype(
           Ratios.Boot=data.frame(),
           Comparisions.Boot=list(),
           Elipses.Boot=matrix(),
           Stability.Boot=data.frame(),
           QRO.Boot=data.frame(),
           EigValues.Boot=data.frame(),
           Inertia.Boot=data.frame())
)



setGeneric("Ratios.Boot<-", function(x, value) standardGeneric("Ratios.Boot<-"))
setReplaceMethod("Ratios.Boot", c("Bootstrap", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Ratios.Boot") <- value
                   x
                 })

setGeneric(name="Ratios.Boot", def = function(x){
  standardGeneric("Ratios.Boot")})
setMethod("Ratios.Boot", "Bootstrap", function(x) x@Ratios.Boot)

setGeneric("Comparisions.Boot<-", function(x, value) standardGeneric("Comparisions.Boot<-"))
setReplaceMethod("Comparisions.Boot", c("Bootstrap", "list"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Comparisions.Boot") <- value
                   x
                 })
setGeneric(name="Comparisions.Boot", def = function(x){
  standardGeneric("Comparisions.Boot")})
setMethod("Comparisions.Boot", "Bootstrap", function(x) x@Comparisions.Boot)

setGeneric("Elipses.Boot<-", function(x, value) standardGeneric("Elipses.Boot<-"))
setReplaceMethod("Elipses.Boot", c("Bootstrap", "matrix"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Elipses.Boot") <- value
                   x
                 })
setGeneric(name="Elipses.Boot", def = function(x){
  standardGeneric("Elipses.Boot")})
setMethod("Elipses.Boot", "Bootstrap", function(x) x@Elipses.Boot)

setGeneric("Stability.Boot<-", function(x, value) standardGeneric("Stability.Boot<-"))
setReplaceMethod("Stability.Boot", c("Bootstrap", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Stability.Boot") <- value
                   x
                 })
setGeneric(name="Stability.Boot", def = function(x){
  standardGeneric("Stability.Boot")})
setMethod("Stability.Boot", "Bootstrap", function(x) x@Stability.Boot)

setGeneric("QRO.Boot<-", function(x, value) standardGeneric("QRO.Boot<-"))
setReplaceMethod("QRO.Boot", c("Bootstrap", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "QRO.Boot") <- value
                   x
                 })
setGeneric(name="QRO.Boot", def = function(x){
  standardGeneric("QRO.Boot")})
setMethod("QRO.Boot", "Bootstrap", function(x) x@QRO.Boot)

setGeneric("EigValues.Boot<-", function(x, value) standardGeneric("EigValues.Boot<-"))
setReplaceMethod("EigValues.Boot", c("Bootstrap", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "EigValues.Boot") <- value
                   x
                 })
setGeneric(name="EigValues.Boot", def = function(x){
  standardGeneric("EigValues.Boot")})
setMethod("EigValues.Boot", "Bootstrap", function(x) x@EigValues.Boot)

setGeneric("Inertia.Boot<-", function(x, value) standardGeneric("Inertia.Boot<-"))
setReplaceMethod("Inertia.Boot", c("Bootstrap", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Inertia.Boot") <- value
                   x
                 })
setGeneric(name="Inertia.Boot", def = function(x){
  standardGeneric("Inertia.Boot")})
setMethod("Inertia.Boot", "Bootstrap", function(x) x@Inertia.Boot)
