
Xfunction<-function(S,SvdComp,Studies,Observations){
  As<-S%*%SvdComp$u%*%diag(1/sqrt(SvdComp$d))
  As<-cbind(as.data.frame(As),rep(Studies,nrow(As)),Observations)
  colnames(As)<-c(paste0("CP",seq(1:(ncol(As)-2))),"Studies","Observations")
  return(As)
}

XCSfunction<-function(S,AP,Studies,Observations){
  As<-S%*%AP
  As<-cbind(as.data.frame(As),rep(Studies,nrow(As)),Observations)
  colnames(As)<-c(paste0("CP",seq(1:(ncol(As)-2))),"Studies","Observations")
  return(As)
}

Lfunction<-function(XC,Studies){
  As<-(svd(XC)$v)*(1/ncol(XC))
  As<-cbind(as.data.frame(As),colnames(XC),Studies)
  colnames(As)<-c(paste0("L",seq(1:ncol(XC))),"Variables","Studies")
  return(As)
}

ProjAllVar<-function(alphas,X,K,Individuos){
  alphas<-sqrt(alphas)
  D_alphas<-diag(unlist(lapply(seq(1:K),function(k){rep(alphas[k],ncol(X[[k]]))})))
  Estud_var<-unlist(lapply(seq(1:K),function(k){rep(paste0("Estudio-",k),ncol(X[[k]]))}))

  Xt<-c()
  for(i in 1:K){
    if(class(X[[i]])!="matrix"){
    X[[i]]<-as.matrix(X[[i]])
    }
    Xt<-cbind(Xt,X[[i]])
  }

  Mt<-Xt%*%D_alphas
  Decomp<-svd(Mt)
  MFil<-Decomp$u%*%diag(sqrt(Decomp$d))
  MCol<-t(Decomp$v)%*%solve(D_alphas)
  #MCol<-t(Decomp$v)
  rownames(MFil)<-Individuos
  MCol<-cbind(as.data.frame(t(MCol)),colnames(Xt),Estud_var)
  MColT<-MCol[order(MCol[,ncol(MCol)-1]),]
  MColT<-cbind(MColT[,1:2],MColT[,(ncol(MCol)-1):ncol(MCol)])
  colnames(MColT)<-c("D1","D2","Variables","Studies")

  M<-list()
  M$MColT<-MColT
  M$MFil<-MFil
  return(M)
}

ProjAllVarCS<-function(alphas,X,K,Individuos,Sk,D){
  alphas<-sqrt(alphas)
  D_alphas<-diag(unlist(lapply(seq(1:K),function(k){rep(alphas[k],ncol(X[[k]]))})))
  Estud_var<-unlist(lapply(seq(1:K),function(k){rep(paste0("Estudio-",k),ncol(X[[k]]))}))

  Xt<-c()
  for(i in 1:K){
    if(class(X[[i]])!="matrix"){
      X[[i]]<-as.matrix(X[[i]])
    }
    A1<-svd(solve(Sk[[i]]))
    X1<-X[[i]]%*%A1$u%*%diag((A1$d)^(1/2))
    colnames(X1)<- colnames(X[[i]])
    Xt<-cbind(Xt,X1)
  }

  Mt<-Xt%*%D_alphas
  Mt<-D^(1/2)%*%Mt
  Decomp<-svd(Mt)
  MFil<-(solve(D))^(1/2)%*%Decomp$u%*%diag(Decomp$d)
  MCol<-t(Decomp$v)%*%solve(D_alphas)
  ####Calidad
  ST=apply(MFil^2,1,sum)
  RQF=((diag(1/ST)) %*% MFil^2)*100
  rownames(RQF)<-Individuos

  #MCol<-t(Decomp$v)
  rownames(MFil)<-Individuos
  MCol<-cbind(as.data.frame(t(MCol)),colnames(Xt),Estud_var)
  MColT<-MCol[order(MCol[,ncol(MCol)-1]),]
  MColT<-cbind(MColT[,1:2],MColT[,(ncol(MCol)-1):ncol(MCol)])
  colnames(MColT)<-c("D1","D2","Variables","Studies")
  M<-list()
  M$MColT<-MColT
  M$MFil<-MFil
  M$RQF<-RQF
  return(M)
}

ProjAllObsDS<-function(alphas,X,K,Variables){
  ###Corregir esta
  alphas<-sqrt(alphas)
  D_alphas<-diag(unlist(lapply(seq(1:K),function(k){rep(alphas[k],nrow(X[[k]]))})))
  Estud_var<-unlist(lapply(seq(1:K),function(k){rep(paste0("Estudio-",k),nrow(X[[k]]))}))

  Xt<-c()
  for(i in 1:K){
    if(class(X[[i]])!="matrix"){
      X[[i]]<-as.matrix(X[[i]])
    }
    Xt<-rbind(Xt,X[[i]])
    colnames(Xt)<-Variables
  }

  Mt<-D_alphas%*%Xt
  Decomp<-svd(Mt)
  MFil<-solve(D_alphas)%*%Decomp$u%*%diag(Decomp$d)
  MCol<-t(Decomp$v)
  #MCol<-t(Decomp$v)
  rownames(MCol)<-Variables
if(is.null(rownames(Xt))){
  rownames(Xt)<-paste0("I",seq(1:nrow(Xt)))
}
  MFil<-cbind(as.data.frame(MFil),rownames(Xt),Estud_var)
  MFilT<-MFil[order(MFil[,ncol(MFil)-1]),]
  MFilT<-cbind(MFilT[,1:2],MFilT[,(ncol(MFil)-1):ncol(MFil)])
  colnames(MFilT)<-c("D1","D2","Individuos","Studies")
  M<-list()
  M$MFilT<-MFilT
  M$MCol<-MCol
  return(M)
}


calculosen <-
  function(V){
    Resultado=asin(V[2]/(sqrt((V[1])^2+(V[2])^2)))
    return(Resultado)
  }
#lmeDGC <-
#  function(MDE,Q,showplot=TRUE,Title="")
#  {
#    D <- MDE$distancias_estandarizadas
#    medias <-  MDE$medias
#    covar <- MDE$covarmedias
#     n=round(mean(MDE$n))
    #  q=calcularQ(nrow(D),n,"average")
    # Q=q[2]
#     D <- as.dist(D)
#     H <- hclust(D,method = "average")
#     Q <- min(Q,H$height[length(H$height)])
#     if (showplot==TRUE) {plot(H,main=Title,sub="",ylim=c(0,max((Q+1),H$height[length(H$height)])), xlab="",ylab="Q",cex=0.8);abline(h=Q)}
#     indices=cutree(H,h=(Q-0.000001))
#     result<-as.data.frame(cbind(medias,sqrt(diag(covar)),indices))
#     rownames(result)=rownames(covar)
#     colnames(result)=c('medias','ee','indices')
#    result
#   }

Normalize <-function(X,scale=TRUE,center=TRUE){


  if(class(X)!="list"){
    stop("Error:Object of type 'list' expected")
  }

  f2 <- function(v){
    if(class(v)!="numeric"){
      stop("Error:Object of type 'numeric' expected")
    }
    row.w <- rep(1, length(v))/length(v)
    sqrt(sum(v * v * row.w)/sum(row.w))
  }

  # list data
  XC=list()
  # list Aux
  M=list()

  for (i in 1: length(X)){
    if (center==TRUE && scale==TRUE){
      XC[[i]] <- matrix(scale(X[[i]],center=TRUE,scale=FALSE),nrow=nrow(X[[i]]))
      M[[i]] <- apply(XC[[i]],2,f2)
      M[[i]][M[[i]]< 1e-08] <- 1
      XC[[i]]<- sweep(XC[[i]], 2, M[[i]], "/")
      XC[[i]] <- XC[[i]]*(as.numeric(1/sqrt(sum((XC[[i]]%*%t(XC[[i]]))^2))))
    }

    if (center==FALSE && scale==FALSE){
      XC[[i]] <- X[[i]]
    }
    if (center==TRUE && scale==FALSE){
      XC[[i]] =matrix(scale(X[[i]],scale=FALSE),nrow=nrow(X[[i]]))
    }
    if (center==FALSE && scale==TRUE){
      XC[[i]] <- apply(X[[i]],2,f2)
      M[[i]][M[[i]]< 1e-08] <- 1
      XC[[i]]<- sweep(XC[[i]], 2, M[[i]], "/")
      XC[[i]]=XC[[i]]*(as.numeric(1/sqrt(sum((XC[[i]]%*%t(XC[[i]]))^2))))
    }
  }

  return(XC)
}

NameTables<-function(X,rownam,colnam){
rownames(X)<-rownam
colnames(X)<-colnam
return(X)
}

ScalarProduct <-
  function(X,Row=TRUE){
    if(class(X)!="matrix")
    {
      X <- as.matrix(X)
    }
    clases<-apply(X,2,class)
    if(any(clases!="numeric") && any(clases!="integer")){
      stop("The Scalar Product only should be used with numerical data")
    }
    if (Row==TRUE){
      Y <- X%*%t(X)
    }
    else{
      Y <- t(X)%*%X
    }
    return(Y)
  }
cia<-function(df){
  df <- as.data.frame(df)
  if (!is.data.frame(df))
    stop("data.frame expected")
  if (any(df < 0))
    stop("negative entries in table")
  if ((N <- sum(df)) == 0)
    stop("all frequencies are zero")
  df <- df/N

  row.w <- apply(df, 1, sum)
  col.w <- apply(df, 2, sum)
  df <- df/row.w
  df <- sweep(df, 2, col.w, "/") - 1
  if (any(is.na(df))) {
    fun1 <- function(x) {
      if (is.na(x))
        return(0)
      else return(x)
    }
    df <- apply(df, c(1, 2), fun1)
    df <- data.frame(df)
  }
  df
  return(df)
}

#####Auxiliares ACT#####

XACTfunction<-function(X,SvdComp,Dp,Studies){
  X <- as.matrix(X)
  As <-X%*%Dp%*%SvdComp$v
  As<-cbind(as.data.frame(As),rep(Studies,nrow(As)),rownames(X))
  colnames(As)<-c(paste0("CP",seq(1:(ncol(As)-2))),"Studies","Observations")
  return(As)
}

LACTfunction<-function(S,Dn,SvdComp,Dp,WW,Studies){
  S <- as.matrix(S)
  As<-t(S)%*%Dn%*%as.matrix(WW)%*%Dp%*%SvdComp$v%*%diag(1/sqrt(SvdComp$d))
  As<-cbind(as.data.frame(As),colnames(S),Studies)
  colnames(As)<-c(paste0("L",seq(1:ncol(S))),"Variables","Studies")
  return(As)
}

Continous<-function(Mat=NULL,X=NULL,ord=FALSE){

  X1<-matrix(as.numeric(X),ncol=1)
   L<-as.matrix(Mat)

    if(ord==TRUE){
       Model<-lm(X1~L)
       }
     if(ord==FALSE){
       Model<-lm(X1~-1+L)
       }
      Res<-summary(Model)

        R2<-Res$adj.r.squared
        pval<-1-pf(Res$fstatistic[1],Res$fstatistic[2],Res$fstatistic[3])
        AICM<-AIC(Model)
        BICM<-BIC(Model)
        if(ord==TRUE){
        XCoord<-Res$coefficients[2,1]
        YCoord<-Res$coefficients[3,1]
        }
        if(ord==FALSE){
        XCoord<-Res$coefficients[1,1]
        YCoord<-Res$coefficients[2,1]
       }
      M2<-cbind(R2,pval,AICM,BICM,XCoord,YCoord)
      rownames(M2)<-rownames(X)

      colnames(M2)<-c("R2-Adj","p-value","AIC","BIC","XCoord","YCoord")
       return(M2)
       }


###BootPlot aux
confelli <- function(b=NULL, C=NULL, df=NULL, level = 0.95,grupo=" ",xlab = "",
                     ylab = "", add=TRUE, prec=51,col=col) {
  d <- sqrt(diag(C))
  dfvec <- c(2, df)
  phase <- acos(C[1, 2]/(d[1] * d[2]))
  angles <- seq( - (pi), pi, len = prec)
  mult <- sqrt(dfvec[1] * qf(level, dfvec[1], dfvec[2]))
  xpts <- b[1] + d[1] * mult * cos(angles)
  ypts <- b[2] + d[2] * mult * cos(angles + phase)

  if(add) {lines(xpts, ypts,col="white",lwd=1.5)
    polygon(xpts, ypts,col=col,border=col,lwd=1.5)}
  else plot(xpts, ypts, type = "l", xlab = xlab, ylab = ylab,col=col,lwd=1.5)
  a<-round(runif(1,1,1))
  #text(x=b[1],y=b[2]+2*d[2],labels=paste(grupo),font=2,cex=0.7,col="black",pos=4)

}
