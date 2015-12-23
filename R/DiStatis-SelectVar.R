#'Function \code{SelectVar} of DiStatis object
#'
#' This function calculates the biplot method through the compromise matrix to select genes
#' SelectVar from DiStatis Class Object
#' High level constructor of SelectVar class object
#'
#' This function allows to build the  biplot for continuous response, using an external procedure
#' to obtained the regresors in the linear model (the response being an continuous variable).
#' This function allows the selection of genes using the goodness of fit of the Models Biplot.
#' object,ord=FALSE,
#'
#' @param object Object is an object of DiStatis Class.
#' @param ord Logical. If TRUE, the models with intercept are computed, else the intercept is zero.
#' @param Crit c("R2-Adj","p-val(Bonf)","AIC","BIC").Criterious of selection. "R2-Adj","p-val (Bonf)","AIC","BIC". Choose "R2-Adj" or "p-val (Bonf)" (Bonferroni correction),"AIC" or "BIC".
#' @param perc The value of percentil that indicate how much data than are selected.
#' @param Dims Numeric that indicates the number of dimensions to use for do the model. Default is 2.
#' @return
#'  \item{SelectVar}{SelectVar class object with the
#'   corresponding completed slots
#'   according to the given model}
#'
#'
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @references
#' \enumerate{
#'  \item Demey, J., Vicente-Villardon, J. L., Galindo, M.P.  & Zambrano, A. (2008) Identifying Molecular Markers Associated With Classification Of Genotypes Using External Logistic Biplots. Bioinformatics, 24(24), 2832-2838.

#' \item Gabriel, K. (1971). The biplot graphic display of matrices with application to principal component analysis. Biometrika 58(3), 453--467.

#' \item Gower, J. & Hand, D. (1996). Biplots, Monographs on statistics and applied probability. 54. London: Chapman and Hall., 277 pp.
#' }
#'
#' @examples
#' {
#' data(NCI60Selec)
#' Z1<-DiStatis(NCI60Selec)
#' M1<-SelectVar(Z1,Crit="R2-Adj",perc=0.95)
#' M2<-SelectVar(Z1,Crit="p-val(Bonf)",perc=0.95)
#' }
#'
#' @exportMethod SelectVar
#' @docType methods
#' @name SelectVar
#' @rdname DiStatis-SelectVar
#' @aliases SelectVar-methods


setGeneric(name="SelectVar", def = function(object,ord=FALSE,
 Crit=c("R2-Adj","p-val(Bonf)","AIC","BIC"),perc=0.9,Dims=2){standardGeneric("SelectVar")})

#' @name SelectVar
#' @rdname DiStatis-SelectVar
#' @inheritParams SelectVar
#' @aliases SelectVar,DiStatis-method
#'
#object is an object of DiStatis S4 class
#ord (model with or without intercept)
#crit to select R2-Adj, p-value (Bond), "AIC","BIC"
#perc percentil to use for selection
#Dims N Dims to do model

setMethod(f="SelectVar", signature = signature("DiStatis"), definition =
            function(object,ord=FALSE,
    Crit=c("R2-Adj","p-val(Bonf)","AIC","BIC"),perc=0.9,Dims=2){
              ##Obtain object name for future update
              nameObject<-deparse(substitute(object))

              WWW<-object@Compromise.Matrix

              ##Check parameters
              stopifnot(Crit[1] %in% c("R2-Adj","p-val(Bonf)","AIC","BIC"))
              if(is.null(perc)==TRUE){
              perc=0.9
              }
              if(perc<0 || perc>1){
              stop("Error in perc: Invalid percentil choice")
              }
              if (Dims>ncol(WWW)){
              Dims<-2
              message("Arg Dims must be less than dims to compromise matrix")
              }
              if(Dims<=1){
              Dims<-2
              message("Arg Dims must be upper than 1")
              }

              Dims<-round(Dims)

              SvdComp <- svd(as.matrix(WWW))

              AP <- as.matrix(WWW)%*%SvdComp$u%*%diag((1/sqrt(SvdComp$d)))
              #AP for plot
              ProjObs<-as.data.frame(AP[,1:Dims])
              rownames(ProjObs)<-rownames(WWW)
              colnames(ProjObs)<-paste("Dim",1:Dims)


  Mat<-as.matrix(ProjObs)
  Datos<-object@Data

  S<-lapply(Datos,function(m){apply(m,2, function(x) Continous(Mat,x,ord=ord))})
  S1<-c()
  if(length(S)>1){
  for (i in 1:length(S)){
  S1<-cbind(S1,S[[i]])
  }
  S1[2,]<-S1[2,]/nrow(Datos[[1]])
  rownames(S1)<-c("R2-Adj","p-value (Bonf)","AIC","BIC","XCoord","YCoord")
  }

  #if(Crit=="R2-Adj" && any(S1[1,]<0)){
  #Crit<-"p-value (Bonf)"
  #message("The p- value criterion will be used ")
  #}

    if(Crit=="R2-Adj"){
    R2<-sort(S1[1,])
    Des<-floor(perc*length(R2))
    Selec<-R2[Des:length(R2)]
    Seleccionados<-c(unique(names(Selec)))
    }

    if(Crit=="p-val(Bonf)"){
    p.adjust(S1[2,], method = "bonferroni")
    pval<-sort(S1[2,])
    Des<-floor((1-perc)*length(pval))
    Selec<-pval[1:Des]
    Seleccionados<-c(unique(names(Selec)))
    }

   if(Crit=="AIC"){
   AIC<-sort(S1[3,])
   Des<-floor((1-perc)*length(AIC))
   Selec<-AIC[1:Des]
   Seleccionados<-unique(names(Selec))
  }

   if(Crit=="BIC"){
   BIC<-sort(S1[4,])
   Des<-floor((1-perc)*length(BIC))
   Selec<-BIC[1:Des]
   Seleccionados<-unique(names(Selec))
   }

  TableSelec<-c()
  ListSelec<-c()
 for (j in 1:length(Datos)){
   A<-which(lapply(colnames(Datos[[j]]),function(x){any(x==Seleccionados)})==TRUE)
   if (length(A)==0){
    j=j+1
   }
   if(length(A)!=0){
   TableSelec<-cbind(TableSelec,as.matrix(Datos[[j]][,A]))
   ListSelec[[j]]<-Datos[[j]][,A]
   names(ListSelec)[[j]]<-names(object@Data)[[j]]
   }
 }


  Coordenadas<-c()
  A2<-which(lapply(colnames(S1),function(x){any(x==Seleccionados)})==TRUE)
  Coordenadas<-S1[5:6,A2]

  .Object<-new("SelectVar")
  .Object@Coord.Select<-Coordenadas
  .Object@Table.Select<-TableSelec
  .Object@List.Selec.Var<-Seleccionados
  .Object@List.Selec.Est<-ListSelec
  .Object@Compromise.Coords<-Mat
  validObject(.Object)
  return(.Object)

})
