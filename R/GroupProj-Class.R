#' Class \code{GroupProj} GroupProj S4 class (kimod: k-tables approach to integrate multiple Omics-Data
#' of Multiple dataset)
#' GroupProj to SelectVar object.
#'

#' @section Features:
#' \enumerate{
#'   \item It cluster variables for SelectVar from all tables (Studies) in STATIS Methodology.
#' }
#'
#' @section Fields:
#' \itemize{
#' \item SortList list the clustering variables
#' \item ProyGroups coords (beta) for Groups representant.
#' \item Groups list of clusters.
#'
#' }
#'@section Accesors:
#' \itemize{
#' \item SortList list the clustering variables
#' \item ProyGroups coords (beta) for Groups representant.
#' \item Groups list of clusters.
#'
#' }
#'
#' @section GroupProj-general-functions:
#' \describe{
#'  \item{print}{Generates the basic output for Bootstrap class}
#'  \item{summary}{Generates the basic statistics for Bootstrap class}
#'  \item{GroupProj}{Getters for their respective slots.}
#' }
#'
#'
#'  @author M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#'  #' @examples
#' {
#' showClass("GroupProj")
#' }
#'
#' @name GroupProj-class
#' @rdname GroupProj-Class
#' @exportClass GroupProj
#' @aliases SortList,GroupProj-methods
#' @aliases ProyGroups,GroupProj-methods
#' @aliases Groups,GroupProj-methods

#'

setClass(Class="GroupProj",
         representation=representation(
           SortList="list",
           ProyGroups="list",
           Groups="list"),
         prototype=prototype(
           SortList=list(),
           ProyGroups=list(),
           Groups=list())
)

setGeneric("SortList<-", function(x, value) standardGeneric("SortList<-"))
setReplaceMethod("SortList", c("GroupProj", "list"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "SortList") <- value
                   x
                 })
setGeneric(name="SortList", def = function(x){
  standardGeneric("SortList")})
setMethod("SortList", "GroupProj", function(x) x@SortList)

setGeneric("ProyGroups<-", function(x, value) standardGeneric("ProyGroups<-"))
setReplaceMethod("ProyGroups", c("GroupProj", "list"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "ProyGroups") <- value
                   x
                 })

setGeneric(name="ProyGroups", def = function(x){
  standardGeneric("ProyGroups")})
setMethod("ProyGroups", "GroupProj", function(x) x@ProyGroups)

setGeneric("Groups<-", function(x, value) standardGeneric("Groups<-"))
setReplaceMethod("Groups", c("GroupProj", "list"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Groups") <- value
                   x
                 })

setGeneric(name="Groups", def = function(x){
  standardGeneric("Groups")})
setMethod("Groups", "GroupProj", function(x) x@Groups)
