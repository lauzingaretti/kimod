#' \code{DiStatis} of a DiStatis object
#' High level constructor of DiStatis class object
#'
#'This is the function that makes DiStatis Methodology: Statis is part of the PCA
#'family and therefore the main analytical tool for STATIS
#'is the singular value decomposition (SVD) and the
#'generalized singular value decomposition (GSVD) of a matrix.
#'The goal of Statis is to analyze several data sets of
#'variables that were collected on the same set of observations.
#'Originally, the comparisons were drawn from the compute
#'of the scalar product between the different tables.
#'In this approach, the condition is made more flexible,
#' allowing  the incorporation of different distance measurements
#'(including the scalar product)  to compare the tables.
#'
#'
#' @param Data The data frame or of k-tables type. The Observations should be in rows (common elements in DAnisostatis), the variables and Studies must be in columns. After the name of the variable an underscore (_) must be written to indicate the Study (eg. Var1_Est1 , eg. Var1_EstK, for more information see the data object). The name of a variable can include any symbol except an underscore (_). REMEMBER the underscore (_) should be reserved to indicate the study.
#' Also, the Data can be a list of k components.
#'  Each element of the list is one of the tables
#'  with observations in rows and variables in columns.
#'  The elements of list must be data.frame or ExpressionSet data.
#' @param Distance Vector is the length equal to the number of studies
#'  that indicates the kind of distance (or scalar product)
#'  that is calculated in each study. If not specify
#'  (or is wrong  specify) the scalar product is used.
#'  The options can be ScalarProduct, euclidean,
#'  manhattan, canberra, pearson, pearsonabs, spearman,
#'  spearmanabs, mahalanobis. In the binary data the distance can be: jaccard,
#'  simple matching, sokal&Sneath, Roger&Tanimoto, Dice,
#'  Hamman,#'  Ochiai, Sokal&Sneath, Phi-Pearson,
#'  Gower&Legendre.
#' @param Center A logical value. If TRUE, the data frame
#'  is centered  by the mean. By default is TRUE.
#' @param Scale A logical value indicating whether the column vectors (of the data.frame) should be
#' standardized by the rows weight, by default is TRUE.
#' @param CorrelVector a logical value. If TRUE (default), Vectorial correlation coefficient  is computed for the  RV matrix.
#' If FALSE the Hilbert-Smith distance is used in the RV matrix.
#' @param Frec  Logical. Should the data be treated data as frequencies? By default is FALSE.
#' @param Traj  Logical. Should the trajectories analysis be done?  By default is TRUE.
#' @return
#'  \item{DiStatis}{DiStatis class object with the
#'   corresponding completed slots
#'  according to the given model}
#'
#' @section Note: use \bold{\code{\link{DiStatis-class}}} high level
#' constructor for the
#'  creation of the class instead of directly
#'  calling its constructor by new
#'  means.
#'
#'
#' @author M L Zingaretti,  J A Demey-Zambrano, J L Vicente Villardon, J R Demey
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
#'
#' @examples
#' {
#' data(NCI60Selec_ESet)
#' Z1<-DiStatis(NCI60Selec_ESet)
#' data(winesassesors)
#' Z3<-DiStatis(winesassesors)
#'
#' }
#'
#' @exportMethod DiStatis
#' @docType methods
#' @name DiStatis
#' @rdname DiStatis-DiStatis
#' @aliases DiStatis-methods
 setGeneric(name="DiStatis",def = function(Data, Distance=c(),Center=TRUE,
                                          Scale=TRUE,CorrelVector=TRUE,Frec=FALSE,Traj=TRUE){standardGeneric("DiStatis")})
#'
#' @name DiStatis
#' @rdname DiStatis-DiStatis
#' @inheritParams DiStatis
#' @aliases DiStatis
#' @title DiStatis
#' @description function to do Statis (K-tables methodology)with distance options
#'@details STATIS methods: to more information, see references.

DiStatis<-function (Data=NULL, Distance = c(), Center = TRUE, Scale = TRUE,
                    CorrelVector = TRUE, Frec = FALSE, Traj = TRUE){
  ## Auxiliary functions
  ## Function for implementation the Statis Method  with Distance options.
  ##The Data can be an data.frame or an list
  ##check if elements of list are ESet data.
if(class(Data)=="list"){
  if(unique(lapply(Data,class))=="ExpressionSet"){
    Data<-lapply(Data,function(x){as(x,"data.frame")})
    Data<-lapply(Data,function(x){t(x)})
    Data<-lapply(Data,as.data.frame)
  }
}


            Auxil<-ReadData(Data)
            X<-Auxil$X
            NameStudies<-Auxil$NameStudies
            Individuos<-Auxil$Individuos
            K<-Auxil$K
            NamesVar<-Auxil$NamesVar

            Observations <- as.matrix(unique(rownames(X[[1]])))

            # Normalice Data
            if(Scale==FALSE && Center==FALSE && Frec==TRUE){
              X<-lapply(X,cia)
              names(X) <- names(X)
            } else{
              X <- Normalize(X,scale=Scale,center=Center)
              names(X) <- names(X)
            }

            In <- nrow(X[[1]])
            D <- diag(rep(1,In))/In

            # Z is  the  centering matrix to calculate distances
            # see Abdi 2007

            M1 <- matrix(rep(1,In)/In,ncol=In,nrow=1)
            Ones <- matrix(rep(1,In),ncol=1,nrow=In)
            Id <- diag(1,nrow=In,ncol=In)
            Z <- Id-Ones%*%M1

            #name of data
            X<-lapply(seq(1:length(X)),function(i){NameTables(X[[i]],Individuos,NamesVar[[i]])})

            # Calculate Distance for each table
            # If not specify Distance or length(Distance)!=K, the scalar product is used
            # This list name (S) follows Abdi (2007)
            S=list()

            # if(class(Distance)!="NULL" && class(Distance)!="character"){
            #  stop("Invalid class object Distance")
            #}

            if (length(Distance)!=K){
              S<-lapply(X,ScalarProduct)
              message( "The calculations were performed using the scalar product between the tables")
             Distance<-"scalar-product"
                }

            if (length(Distance)==K) {
              S<-lapply(1:length(Distance),function(i){CalculateDist(X[[i]],Distance[i],Z)})
            }

            names(S)<-NameStudies

            ##########################################################################
            #############################calculating SW###############################
            ##########################################################################
            SW<-list()
            for (k in 1:K) {
              wk <- as.matrix(S[[k]])%*%D
              wk <- t(t(wk)%*%D)
              wk <- wk %*% t(wk)
              SW[[k]] <- wk
            }

            if(CorrelVector==TRUE){
              sep <- matrix(unlist(SW), In * In, K)
              RV <- t(sep) %*% sep
              ak <- sqrt(diag(RV))
              RV <- sweep(RV, 1, ak, "/")
              RV <- sweep(RV, 2, ak, "/")
              dimnames(RV) <- list(NameStudies, NameStudies)
            }
            ##############Si no quieres las correlaciones############################

            if(CorrelVector==FALSE){
            sep <- matrix(unlist(SW), In * In, K)
            RV <- t(sep) %*% sep
            dimnames(RV) <- list(NameStudies, NameStudies)
            }

            #########################################################################
            ##########################INTER-STRUCTURE################################
            #########################################################################

            SvdRV <- svd(RV)
            # percentage of inertia explained by the dimensions
            InertiaExpRV <- c(((SvdRV $d)/sum(diag(SvdRV $d)))*100)
            InertiaExpRV <- data.frame(InertiaExpRV)
            # inertia accumulated
            CumInertiaExpRV <- cumsum(InertiaExpRV)
            InertiaExpRV <- as.data.frame(cbind(SvdRV$d,InertiaExpRV, CumInertiaExpRV))
            colnames(InertiaExpRV) <- c("Value","Inertia(%)","Cumulative Inertia (%)")
            Dim <- c(paste("Dim",1:K))
            rownames(InertiaExpRV) <- Dim

            ###the following code is for plot the Euclidean image of the studies###

            cc<-SvdRV$u%*%sqrt(diag(SvdRV$d))
            if (any(cc[, 1] < 0))
              cc[, 1] <- -cc[, 1]
            ImSt<-as.data.frame(cc[,1:2])
            rownames(ImSt) <-NameStudies
            colnames(ImSt)<-c("Dim1","Dim2")

            ######Calculate the cosine between studies#########
            A <- cc[,1:2]%*%t(cc[,1:2])
            M <- diag(1/sqrt(diag(A)))
            Cosin<-M%*%A%*%M
            Angulos <- acos(as.dist(Cosin))
            SqCos <- Cosin^2
            SqCos <- as.data.frame(SqCos)
            colnames(SqCos) <- NameStudies
            rownames(SqCos) <- NameStudies

            ######################################################
            ##############INTER-STRUCTURE:Compromise #############
            ######################################################

            # The first eigenvector give the optimal weights to compute the compromise matrix.
            # Practically, the optimal weights can be obtained by re-scaling these values such
            # that their sum is equal to one. So the weights are obtained by dividing each element
            # of p1 by the sum of the elements of p1.
            # if bootstrap=TRUE, use mean

            sc <- sum(sqrt(diag(RV)))
            if (any(SvdRV$u[, 1] < 0))
              SvdRV$u[, 1] <- -SvdRV$u[, 1]
            pit<-c()
            for(i in 1:nrow(RV)){
              pit[i] <-(sc/sqrt(SvdRV$d[1]))*SvdRV$u[i,1]
            }
            alphas<- pit/sum(pit);

            # WW compromise matrix
            WW<-matrix(0,nrow=nrow(S[[1]]),ncol=ncol(S[[1]]))

            for (i in 1: K){
              WW <- WW+alphas[i]*S[[i]]
            }

            ###www data.frame compromise
            WWW <- as.data.frame(WW)
            rownames(WWW)<- as.matrix(Observations)
            colnames(WWW)<- as.matrix(Observations)


            ################################################
            ##############svd del compromiso################
            ################################################

            SvdComp <- svd(WW)
            InerComp <- c((SvdComp$d/sum(diag(SvdComp$d)))*100)
            #InerComp porcentaje de inercia explicado por las dimensiones
            InerComp <- cbind(SvdComp$d,InerComp,cumsum(InerComp))
            Dim <- c(paste("Dim",1:(nrow(WW))))
            rownames(InerComp) <- Dim
            colnames(InerComp) <- c("Values","Inertia", "Cumulative Inertia")
            ###########Projection of observations###########

            AP <- WW%*%SvdComp$u%*%diag((1/sqrt(SvdComp$d)))
            #AP for plot
            ProjObs<-as.data.frame(AP[,1:min(4,ncol(AP))])
            rownames(ProjObs)<-Observations
            A<-paste("Dim",1:min(4,ncol(AP)))
            colnames(ProjObs)<-A

            #####################################################
            ###Rendering Quality of the Observations (RQO,RQI)###
            #####################################################
            SumCuadTot=apply(AP^2,1,sum)
            RQI=((diag(1/SumCuadTot)) %*% AP^2)*100
            #Cambiamos linea anterior para hacer calidad por todas las dimensiones
            RQIcum=t(apply(RQI,1,cumsum))
            RQI<- cbind(RQI,RQIcum[,2])
            rownames(RQI)<-Observations
            colnames(RQI)<-c(paste0("RQI",seq(1:ncol(AP)),"%"),"RQIT")
            RQITot<-as.matrix(RQI[,colnames(RQI)=="RQIT"],ncol=1)
            rownames(RQITot) <- Observations
            colnames(RQITot) <- c("RQI (%)")


            ####################INTRA-STRUCTURE: trajectories###########
            # The intrastructure step is a projection of the rows
            # of each table of the series into the multidimensional
            # space of the compromise analysis.
            if(Traj==TRUE){
            Studies <- unique(NameStudies)
            AA <- lapply(seq(1:K),function(i){Xfunction(S[[i]],SvdComp,Studies[i],Observations)})
            #Load <- lapply(seq(1:K),function(i){Lfunction(X[[i]],Studies[i])})

            ###############Table for trajectories####################
            ###############Variables projections of all tables#########
            DatosPP<-c()
            #LoadingsPP<-c()
            for(i in 1:K){
              DatosPP<-rbind(DatosPP,cbind(as.data.frame(AA[[i]][,1:2]),AA[[i]][,(ncol(AA[[i]])-1):ncol(AA[[i]])]))
              #LoadingsPP<-rbind(LoadingsPP,cbind(as.data.frame(Load[[i]][,1:2]),Load[[i]][,(ncol(Load[[i]])-1):ncol(Load[[i]])]))
            }

            TrajectoriesList<-list()
            for(i in 1:length(Observations)){
              TrajectoriesList[[i]]<-DatosPP[DatosPP[,4]==Observations[i],]
              names(TrajectoriesList)[[i]]<-Observations[i]
            }
            }
            if(Traj==FALSE){
              TrajectoriesList<-list()
            }

            #Variables Projection

            if(sum(unlist(lapply(X,ncol)))<50){
            M<-ProjAllVar(alphas,X,K,Individuos)
            LoadingsPP<-M$MColT
            RowM<-M$MFil
            }
            if(sum(unlist(lapply(X,ncol)))>=50){
              LoadingsPP<-data.frame()
              RowM<-matrix()
              message("The Projection of all variables will be made when the number of variables is less than 50")
            }

            ##Return of different slots
            .Object<-new("DiStatis")
            .Object@distances.methods<-Distance
            .Object@Inertia.RV<-InertiaExpRV
            .Object@RV<-RV
            .Object@Euclid.Im<-ImSt
            .Object@Inertia.comp<-InerComp
            .Object@Compromise.Coords<-ProjObs
            .Object@Compromise.Matrix<-WWW
            .Object@RQO<-RQI
            .Object@Trajectories<-TrajectoriesList
            .Object@RowMar<-RowM
            .Object@ColMar<-LoadingsPP
            .Object@Data<-Auxil$X

            validObject(.Object)
            return(.Object)
          }








