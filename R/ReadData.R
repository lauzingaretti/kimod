#Data sera un data frame donde se indiquen con un guion bajo (despues del nombre de la variable), el nombre de los estudios
#o bien sera una lista que contenga los estudios como sus componentes
#Data puede ser de dos clases list o Data.frame
ReadData<-function(Data){


#################################if Data is data frame#####################################
#  Individuos should be in rows (common elements in statis)
#  Variables and Studies in columns
#  After the name of  Variable, you  must write an underscore (_), to indicate the Study
#  eg. Var1_Est1
#  eg. Var1_EstK
#  The name of a variable can include any symbol except an underscore (_) , that should be reserved to indicate the study
###############################if data is un list##########################################
#Observations in rows, variables in columns
#The Studies are elements of the list
###########################################################################################

if( is.null(Data)){
stop("You have not provided data.")
}

if( is.matrix(Data)==FALSE && is.data.frame(Data)==FALSE && is.list(Data)==FALSE){
stop("incorrect class of object")
}

#if(class(Data)!="data.frame" & class(Data)!="list" & class(Data)!="matrix"){
#  stop("Incorrect class of Data")
#}

if(is(Data,"data.frame") || is(Data,"matrix")){
##################################################

if((all(grepl("_",rownames(Data))))==TRUE && !all(grepl("_",colnames(Data)))==TRUE){
  #studies indicados en filas

  NamesAndStudies <- unlist(strsplit(rownames(Data), "_", fixed = TRUE))
  if (length(NamesAndStudies)!=2*nrow(Data)){
    stop("Error: you incorrectly indicated the names of the studies")
  }

  Individuos<- c(rep(0,nrow(Data)))
  Studies <- c(rep(0,nrow(Data)))

  for (i in 1:length(Individuos)){
    Individuos[i]<- NamesAndStudies[2*i-1]
    Studies[i]<- NamesAndStudies[2*i]
  }
  Individuos <- gsub(" ", "",  Individuos)
  Studies <- gsub(" ", "",  Studies)
  #number of studies
  K <- length(levels(as.factor(Studies)))

  NameStudies <- unique(Studies)
  #  NameStudies is a vector that holds the names of the studies

  Variables <- as.matrix(unique(colnames(Data)))
  #  Observations (saves names of the individuals)

  DataAux=cbind(Individuos,Studies)
  # DataAux stores the names of studies and variables

  X <- as.list(rep(0,length(NameStudies)))
  names(X) <- NameStudies


  # X (list storing all tables)
   for ( i in 1: length(NameStudies)){
    X[[i]] <- (Data[DataAux[,2]==NameStudies[i],])
    rownames(X[[i]]) <- DataAux[DataAux[,2]==NameStudies[i],1]
    colnames(X[[i]]) <- Variables
    }

  NamesVar<-colnames(X[[1]])

}


##################################################
if(!(all(grepl("_",rownames(Data))))==TRUE && all(grepl("_",colnames(Data)))==TRUE){
if (is.null(rownames(Data))){
  rownames(Data) <- 1:nrow(Data)
}
if((any(grepl("_",rownames(Data))))==TRUE){
  stop("You can't use '_' to indicate the name of the observations")
}
if((all(grepl("_",colnames(Data))))==FALSE){
  stop("You should use '_' to separate the names of the variables and studies in the columns (see example)" )
}

NamesAndStudies <- unlist(strsplit(colnames(Data), "_", fixed = TRUE))

if (length(NamesAndStudies)!=2*ncol(Data)){
  stop("Error: you incorrectly indicated the names of the studies")
}

Variables <- c(rep(0,ncol(Data)))
Studies <- c(rep(0,ncol(Data)))

for (i in 1:length(Variables)){
  Variables[i]<- NamesAndStudies[2*i-1]
  Studies[i]<- NamesAndStudies[2*i]
}
Variables <- gsub(" ", "",  Variables)
Studies <- gsub(" ", "",  Studies)

#  number of studies
K <- length(levels(as.factor(Studies)))


NameStudies <- unique(Studies)
#  NameStudies is a vector that holds the names of the studies

Individuos <- as.matrix(unique(rownames(Data)))

#  Individuos (saves names of the individuals)

DataAux=rbind(Studies,Variables)
# DataAux stores the names of studies and variables
X <- as.list(rep(0,length(NameStudies)))
names(X) <- NameStudies

NamesVar= list()
# X (list storing all tables)

for ( i in 1: length(NameStudies)){
  X[[i]]<- (Data[,DataAux[1,]==NameStudies[i]])
  NamesVar[[i]] <- names((Data[2,DataAux[1,]==NameStudies[i]]))
}


for (i in 1:length(NamesVar)){
  for (j in 1:length(NamesVar[[i]])){
    NamesVar[[i]][j] <- unlist(strsplit(NamesVar[[i]][j], "_", fixed = TRUE))[1]
  }
}

for (i in 1: length(NameStudies)){
  colnames(X[[i]]) <- NamesVar[[i]]
}



Individuos <- rownames(Data)

}

}

if(is(Data,"list")){

if(unique(lapply(Data,class))!="data.frame"){
  stop("The elements of the list must be data.frame")
}

if(is.null(names(Data))){
  NameStudies<-paste0("E",seq(1:length(Data)))
  names(Data)<-NameStudies
}

if(is.null(names(Data))==FALSE){
  NameStudies<-names(Data)
}

if(length(unique(lapply(Data,nrow)))!=1){
  stop("All data must have the same number of Individuos")
}

Individuos<-rownames(Data[[1]])
X<-Data
K<-length(Data)

NamesVar= list()
# X (list storing all tables)

for ( i in 1: length(NameStudies)){
  if(is.null(colnames(Data[[i]]))==FALSE){
  NamesVar[[i]]<-colnames(Data[[i]])
  }
  if(is.null(colnames(Data[[i]]))){
    stop("you must include the name of the variables")
  }
}
}

return(structure(list("X"=X,"NameStudies"=NameStudies,"Individuos"=Individuos,"K"=K,"NamesVar"=NamesVar)))

}


