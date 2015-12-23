#' Class \code{DiStatis} DiStatis S4 class (kimod: k-tables approach to integrate multiple Omics-Data
#' Multiple dataset)
#'
#' Statis with Distance options implementation.

#' @section Features:
#' \enumerate{
#'   \item DiStatis (Statis with Distance options)
#'   \item Bootstrap Implementation
#'   \item Biplot Implementation with variable selection
#'   \item Clustering of variables
#'   \item Plotting compromise, bootstrap, trajectories, Biplot.
#' }
#'
#' @section Fields:
#' \itemize{
#'  \item distances.methods: The character indicating the metrics used.
#'  \item Inertia.RV: Inertia (\%) explained for all tables.
#'  \item RV: Vectorial Correlation Matrix between studies.
#'  \item Euclid.Im: Euclidean Image of all studies.
#'  \item Inertia.Comp: Inertia (\%) explained for all dimensions of compromise matrix.
#'  \item Compromise.Coords: Projection of all observations in compromise (Coords).
#'  \item Compromise.Matrix: Compromise Matrix from statis methodology.
#'  \item RQO: Representation Quality of observations in compromise matrix.
#'  \item Trajectories: List of trajectories from Statis methodology
#'  \item RowMar: Row-marquer (if observations are less than 100) to do classic biplot
#'  \item ColMar: Col-marquer (if observations are less than 100) to do classic biplot
#' }
#' @section Accesors:
#' \itemize{
#'  \item Inertia.RV: Inertia (\%) explained for all tables.
#'  \item RV: Vectorial Correlation Matrix between studies.
#'  \item Euclid.Im: Euclidean Image of all studies.
#'  \item Inertia.Comp: Inertia (\%) explained for all dimensions of compromise matrix.
#'  \item Compromise.Coords: Projection of all observations in compromise (Coords).
#'  \item Compromise.Matrix: Compromise Matrix from statis methodology.
#'  \item RQO: Representation Quality of observations in compromise matrix.
#'  \item Trajectories: List of trajectories from Statis methodology
#'  \item RowMar: Row-marquer (if observations are less than 100) to do classic biplot
#'  \item ColMar: Col-marquer (if observations are less than 100) to do classic biplot
#' }
#' @section DiStatis-general-functions:
#' \describe{
#'  \item{print}{Basic output for DiStatis class}
#'  \item{summary}{Basic statistics for DiStatis class}
#'  \item{DiStatis}{Getters for their respective slots.}
#' }
#'
#'  @author M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @seealso  \code{\link{CompPlot}},
#'  \code{\link{Biplot}}, and additional related
#'  DiStatis class functions.
#'
#' @references
#' \enumerate{
#'  \item Abdi, H., Williams, L.J.,
#'  Valentin, D., & Bennani-Dosse, M. (2012).
#'  STATIS and DISTATIS: optimum multitable principal
#'  component analysis and three way metric multidimensional scaling.
#'  WIREs Comput Stat, 4, 124-167.
#'  \item Escoufier, Y. (1976). Operateur associe a un tableau de donnees. Annales de laInsee, 22-23, 165-178.
#'  \item Escoufier, Y. (1987). The duality diagram: a means for better practical applications. En P. Legendre & L. Legendre (Eds.), Developments in Numerical Ecology, pp. 139-156, NATO Advanced Institute, Serie G. Berlin: Springer.
#'  \item L'Hermier des Plantes, H. (1976). Structuration des Tableaux a Trois Indices de la Statistique. [These de Troisieme Cycle]. University of Montpellier, France.
#'
#'
#' }
#' @examples
#' {
#' showClass("DiStatis")
#' }

#' @name DiStatis-class
#' @rdname DiStatis-Class
#' @exportClass DiStatis
#' @aliases Inertia.RV,DiStatis-methods
#' @aliases RV,DiStatis-methods
#' @aliases Euclid.Im,DiStatis-methods
#' @aliases Inertia.comp,DiStatis-methods
#' @aliases Compromise.Coords,DiStatis-methods
#' @aliases Compromise.Matrix,DiStatis-methods
#' @aliases RQO,DiStatis-methods
#' @aliases Trajectories,DiStatis-methods
#' @aliases RowMar,DiStatis-methods
#' @aliases ColMar,DiStatis-methods
#'
#'
#'
setClass(Class="DiStatis",
  representation=representation(
    distances.methods="character",
    Inertia.RV="data.frame",
    RV="matrix",
    Euclid.Im="data.frame",
    Inertia.comp="matrix",
    Compromise.Coords="data.frame",
    Compromise.Matrix="data.frame",
    RQO="matrix",
    Trajectories="list",
    RowMar="matrix",
    ColMar="data.frame",
    Data="list"),
  prototype=prototype(
    distances.methods=character(),
    Inertia.RV=data.frame(),
    RV=matrix(),
    Euclid.Im=data.frame(),
    Inertia.comp=matrix(),
    Compromise.Coords=data.frame(),
    Compromise.Matrix=data.frame(),
    RQO=matrix(),
    Trajectories=list(),
    RowMar=matrix(),
    ColMar=data.frame(),
    Data=list())
)

##Accesors

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters.
###

setGeneric("Inertia.RV<-", function(x, value) standardGeneric("Inertia.RV<-"))
setReplaceMethod("Inertia.RV", c("DiStatis", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Inertia.RV") <- value
                   x
                })


setGeneric("RV<-", function(x, value) standardGeneric("RV<-"))
setReplaceMethod("RV", c("DiStatis", "matrix"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "RV") <- value
                   x
                 })


setGeneric("Euclid.Im<-", function(x, value) standardGeneric("Euclid.Im<-"))
setReplaceMethod("Euclid.Im", c("DiStatis", "data.frame"),
                 function(x,value) {
                   ## could perform checks here
                   slot(x, "Euclid.Im") <- value
                   x
                 })


setGeneric("Inertia.comp<-", function(x, value) standardGeneric("Inertia.comp<-"))
setReplaceMethod("Inertia.comp", c("DiStatis", "matrix"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Inertia.comp") <- value
                   x
                 })



setGeneric("Compromise.Coords<-", function(x, value) standardGeneric("Compromise.Coords<-"))
setReplaceMethod("Compromise.Coords", c("DiStatis", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Compromise.Coords") <- value
                   x
                 })



setGeneric("Compromise.Matrix<-", function(x, value) standardGeneric("Compromise.Matrix<-"))
setReplaceMethod("Compromise.Matrix", c("DiStatis", "data.frame"),
                 function(x, value) {
                   ## could perform checks here
                   slot(x, "Compromise.Matrix") <- value
                   x
                 })

setGeneric(name="RQO", def = function(x){
  standardGeneric("RQO")
})

setGeneric("RQO<-", function(x, value) standardGeneric("RQO<-"))
setReplaceMethod("RQO", c("DiStatis", "matrix"),
                 function(x,value) {
                   ## could perform checks here
                   slot(x, "RQO") <- value
                   x
                 })

setGeneric("Trajectories<-", function(x, value) standardGeneric("Trajectories<-"))
setReplaceMethod("Trajectories", c("DiStatis", "list"),
                 function(x,  value) {
                   ## could perform checks here
                   slot(x, "Trajectories") <- value
                   x
                 })


setGeneric(name="Inertia.RV", def = function(x){
  standardGeneric("Inertia.RV")
})
setGeneric(name="RV", def = function(x){
  standardGeneric("RV")
})
setGeneric(name="Euclid.Im", def = function(x){
  standardGeneric("Euclid.Im")
})
setGeneric(name="Inertia.comp", def = function(x){
  standardGeneric("Inertia.comp")
})
setGeneric(name="Compromise.Coords", def = function(x){
  standardGeneric("Compromise.Coords")
})
setGeneric(name="Compromise.Matrix", def = function(x){
  standardGeneric("Compromise.Matrix")
})
setGeneric(name="RQO", def = function(x){
  standardGeneric("RQO")
})
setGeneric(name="Trajectories", def = function(x){
  standardGeneric("Trajectories")
})
setMethod("Inertia.RV", "DiStatis", function(x) (x@Inertia.RV))
setMethod("RV", "DiStatis", function(x) (x@RV))
setMethod("Euclid.Im", "DiStatis", function(x) (x@Euclid.Im))
setMethod("Inertia.comp", "DiStatis", function(x) (x@Inertia.comp))
setMethod("Compromise.Coords", "DiStatis", function(x) (x@Compromise.Coords))
setMethod("Compromise.Matrix", "DiStatis", function(x) (x@Compromise.Matrix))
setMethod("RQO", "DiStatis", function(x) (x@RQO))
setMethod("Trajectories", "DiStatis", function(x) (x@Trajectories))


