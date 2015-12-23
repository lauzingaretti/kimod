#'Funtion \code{Bootstrap} of a DiStatis object
#'
#'This function is use to make Bootstrap from DiStatis object. Bootstrap
#'resampling techniques are applied on the residuals
#'matrices obtained from SVD of the Compromise and
#'the RV matrix to do multiple comparisions between
#'studies and  confidence elipses for the projections
#'of observations in the compromise.
#'
#'
#'@param object It is the object of DiStatis Class.
#'@param NRep Number of repetitions to do the bootstraping. Default is 100.
#'@param Dims Number of dimensions used to do the bootstraping. Default is 2.

#'@return
#'  \item{Bootstrap}{Bootstrap class object with the
#'   corresponding completed slots
#'   according to the given model}
#'
#'
#'
#' @author M L Zingaretti, J A Demey-Zambrano, J L Vicente Villardon, J R Demey
#'
#' @references
#' \enumerate{
#'  \item Efron, B.,Tibshirani, RJ. (1993). An introduction to the bootstrap. New York: Chapman and Hall. 436p.
#'  \item Ringrose, T.J. (1992). Bootstrapping and Correspondence Analysis in Archaeology. Journal of Archaeological. Science.19:615-629.
#'
#'}
#'
#' @examples
#' {
#' data(NCI60Selec)
#' Z1<-DiStatis(NCI60Selec)
#' B<-Bootstrap(Z1)
#' Z2<-DiStatis(NCI60Selec,Center=FALSE,Scale=FALSE)
#' B2<-Bootstrap(Z2,NRep=200)
#'
#'}

#'@exportMethod Bootstrap
#'@docType methods
#'@name Bootstrap
#'@rdname DiStatis-Bootstrap
#'@aliases Bootstrap-methods

 setGeneric(name="Bootstrap", def = function(object,NRep=100,Dims=2){
  standardGeneric("Bootstrap")
})



#'@name Bootstrap
#'@rdname DiStatis-Bootstrap
#'@inheritParams Bootstrap
#'@aliases Bootstrap,DiStatis-method

setMethod(f="Bootstrap", signature = signature("DiStatis"), definition =
            function(object,NRep=100,Dims=2){
  ##Obtain object name for future update
  nameObject<-deparse(substitute(object))

  WWW<-object@Compromise.Matrix


  if(NRep<=1){
    NRep<-100
    message("The Bootstrap was computed with 100 replicates")
  }

  if(Dims>ncol(WWW)){
    Dims<-2
    message("Arg Dims must be less than dims to compromise matrix")
  }

  Dims<-round(Dims)
  NRep<-round(NRep)

  if(Dims<=2){
    Dims<-2
  }

  if(nrow(WWW)>=3){
    SvdWWW <- svd(WWW)
    PO <- as.matrix(WWW)%*%SvdWWW$u%*%diag((1/sqrt(SvdWWW$d)))
    PO <- PO[,1:Dims]
    u<-SvdWWW$u[,1:Dims]
    d<-SvdWWW$d[1:Dims]
    v<-SvdWWW$v[,1:Dims]

    if(any(d<0)){
      saca<-which(d<0)
      u<-u[,-saca]
      d<-d[-saca]
      v<-v[,-saca]
      if(length(d)>1){
        Dhat <- u%*%diag(d)%*%t(v)
        Er <- WWW-Dhat
      }
      if(length(d)==1){
        Dhat <- u%*%t(v)*d
        Er <- WWW-Dhat
      }
    }

    if(all(d>=0)){
      if(length(d)>1){
        Dhat <- u%*%diag(d)%*%t(v)
        Er <- WWW-Dhat
      }
      if(length(d)==1){
        Dhat <- u%*%t(v)*d
        Er <- WWW-Dhat
      }
    }

  }

  #esto seria en el caso que nuestra matriz orginal sea de dimension 2
  if(nrow(WWW)<=2){
    SvdWWW <- svd(WWW)
    PO <- as.matrix(WWW)%*%SvdWWW$u%*%diag((1/sqrt(SvdWWW$d)))
    PO <- PO[,1]
    u<-SvdWWW$u[,1]
    d<-SvdWWW$d[1]
    v<-SvdWWW$v[,1]
    if(d<0){
      stop("The matrix  isn't positive semi-definite")
    }
    Dhat <- u[,1]%*%t(v[,1])*d[1]
    Er <- WWW-Dhat

  }

  Er<-as.matrix(Er)
  Diagonales<-diag(Er)

  Boot <- list()

  for (i in 1: NRep){
    Muestreos <- (sample(Er[lower.tri(Er)],length(Er[lower.tri(Er)]),replace = TRUE))
    ErEstr1<-diag(Diagonales)
    ErEstr1[lower.tri(Er)]<- Muestreos
    ErEstr1[upper.tri(Er)]<- Muestreos
    Boot[[i]] <- as.matrix(ErEstr1)+Dhat
  }

  #if diag is <0

     SvdBoot <- function(ErEstr){
      WWEstr <- ErEstr
     SvdWWWEstr <- svd(WWEstr)
      if(any(SvdWWWEstr$d<0)){
      saca<-which(SvdWWWEstr$d<0)
      SvdWWWEstr$u<-SvdWWWEstr$u[,-saca]
      SvdWWWEstr$d<-SvdWWWEstr$d[-saca]
      SvdWWWEstr$v<-SvdWWWEstr$v[,-saca]
    }

    VProp <- diag(SvdWWWEstr$d)
    InerComp <- c((SvdWWWEstr$d/sum(diag(SvdWWWEstr$d))))*100
    Lamdas <- c(SvdWWWEstr$d)
    names(Lamdas) <- paste("Dim",rep(1:nrow(WWW)))
    names(InerComp)<- paste("Dim",rep(1:nrow(WWW)))
    POEstr <- as.matrix(WWEstr)%*%SvdWWWEstr$u%*%diag((1/sqrt(SvdWWWEstr$d)))
    POEstr <- as.data.frame(POEstr[,1:Dims])
    colnames(POEstr) <- paste0("Dim",seq(1:Dims))
    rownames(POEstr) <- rownames(WWW)
    Tod=SvdWWWEstr$u%*%diag((SvdWWWEstr$d))
    Cuadrados<- function(x){x*x}
    Tod2<- apply(Tod,1,Cuadrados)
    Comp2<- apply(Tod[,1:2],1,Cuadrados)
    SumTod2<- apply(Tod2,2,sum)
    SumComp2<- apply(Comp2,2,sum)
    CalidadRepr1<- (SumComp2/SumTod2)*100
    CalidadRepr1<- as.data.frame(CalidadRepr1,ncol=1)
    rownames(CalidadRepr1) <- rownames(WWW)
    colnames(CalidadRepr1)<- ("RQO (%)")
    A=list("RQO(%)"=CalidadRepr1,"Projections"=POEstr,"InerC"=InerComp,"Lamdas"=Lamdas,"WWEstr"= WWEstr)
    return(A)
  }


  result <- lapply(Boot,SvdBoot)


  ####calidad de representacion media de cada individuo#######
  CalidadReprBoot=c()
  for(i in 1: NRep){
    CalidadReprBoot<- rbind(CalidadReprBoot,result[[i]][[1]])
  }


  rownames(CalidadReprBoot) <- NULL
  CalidadReprBoot <- cbind(rep(rownames(WWW),NRep),CalidadReprBoot)
  colnames(CalidadReprBoot) <- c("Obs", "RQO(%)")
  SortCalidadRepr <-list(tapply(CalidadReprBoot[,2],CalidadReprBoot[,1],sort))

  LiLs<-function(M){
    LI<-M[floor(0.05*length(M))]
    LS<-M[floor(0.95*length(M))]
    structure(list("LI"=LI,"LS"=LS))
  }


  CIQRO <- lapply(SortCalidadRepr[[1]],LiLs)
  CIQRO <- data.frame(matrix(unlist(CIQRO),ncol=2,byrow=TRUE))
  colnames(CIQRO) <- c("LI","LS")
  rownames(CIQRO) <- rownames(WWW)


  ##########inertia table############

  InertiaBoot=c()
  for(i in 1: NRep){
    InertiaBoot<- rbind(InertiaBoot,as.matrix(result[[i]][[3]],ncol=1))
  }


  MeanInertia <- as.matrix(tapply(InertiaBoot,rownames(InertiaBoot),mean),ncol=1)
  SdInertia <- as.matrix(tapply(InertiaBoot,rownames(InertiaBoot),sd),ncol=1)
  LI <- MeanInertia-qt(1-0.05/2, 200-1)*SdInertia
  LS <- MeanInertia+qt(1-0.05/2, 200-1)*SdInertia
  options(Digits=3)
  InertiaTable <- as.data.frame(cbind(MeanInertia,SdInertia,LI,LS))
  rownames(InertiaTable) <- rownames(SdInertia)
  colnames(InertiaTable) <- c("Inertia-Mean","Inertia-Sd","LI","LS")
  InertiaTable <- InertiaTable[order(InertiaTable[,1],decreasing=TRUE),]

  ##########lamdas table############
  LamdasBoot=c()
  for(i in 1: NRep){
    LamdasBoot<- rbind(LamdasBoot,as.matrix(result[[i]][[4]],ncol=1))
  }

  MeanLamdas <- matrix(tapply( LamdasBoot,rownames( LamdasBoot),mean),ncol=1)
  SdLamdas <- matrix(tapply( LamdasBoot,rownames( LamdasBoot),sd),ncol=1)
  LLI <- MeanLamdas-qt(1-0.05/2, 200-1)*SdLamdas
  LLS <- MeanLamdas+qt(1-0.05/2, 200-1)*SdLamdas
  LamdasTable <- as.data.frame(cbind(MeanLamdas,SdLamdas,LLI,LLS))
  colnames(LamdasTable) <- c("Lamda-Mean","Lamda-Sd","LI","LS")
  LamdasTable <- LamdasTable[order(LamdasTable[,1],decreasing=TRUE),]
  rownames(LamdasTable) <- c(paste("Dim",seq(1:nrow(LamdasTable))))



  Elipses <- c()
  for(i in 1: NRep){
    Elipses <- rbind(Elipses,as.matrix(result[[i]][[2]]))
  }

  #construir bonferroni test
  DataOrd<-Elipses[order(rownames(Elipses)),]
  NameD<-unique(rownames(DataOrd))
  Ratio<-as.data.frame(matrix(0,nrow=length(NameD),ncol=Dims))
  rownames(Ratio)<-NameD
  colnames(Ratio)<-paste0("Dim",seq(1:Dims))

    for (d in 1:length(NameD)){
    Media<-colMeans(DataOrd[rownames(DataOrd)==NameD[d],])
    Sd<-apply(DataOrd[rownames(DataOrd)==NameD[d],],2,sd)
    Ratio[d,]<-Media/Sd
    }
 Comparisions<-list()

 for(g in 1:Dims ){
   Pos<-rownames(Ratio[which(Ratio[,1]>3),])
   Neg<-rownames(Ratio[which(Ratio[,1]< (-3)),])
   Comparisions[[g]]<-list("Pos"=Pos,"Neg"=Neg)

 }



  #######################Estability##############################

  SumNum <- c()
  SumDenom <- c()
  for (i in 1:NRep){
    Difer <-(result[[i]][[5]]-WWW)^2
    Denom <- result[[i]][[5]]^2
    SumNum <- c(SumNum,sum(Difer))
    SumDenom <- c(SumDenom,sum(Denom))
  }

  Est=as.data.frame(1-sum(SumNum)/sum(SumDenom))



  #################################################################################
  ##Return of different slots
  .Object<-new("Bootstrap")
  .Object@Ratios.Boot<-Ratio
  .Object@Comparisions.Boot<-Comparisions
  .Object@Elipses.Boot<-Elipses
  .Object@Stability.Boot<-Est
  .Object@QRO.Boot<-CIQRO
  .Object@EigValues.Boot<-LamdasTable
  .Object@Inertia.Boot<-InertiaTable
  validObject(.Object)
  return(.Object)

 })
