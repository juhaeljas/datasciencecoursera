complete <- function(directory, id = 1:332) {
  iidee<-NULL
  for(int in id){
    if(int<10) {
      iidee<-c(iidee,paste("00",int,sep=""))
    }
    if(9<int&int<100){
      iidee<-c(iidee,paste("0",int,sep=""))
    }
    if(99<int){
      iidee<-c(iidee,int)
    }
  }
  d<-NULL
  for(i in 1:length(iidee)){
    d<-rbind(d,read.csv(paste(directory,"/",iidee[i],".csv",sep=""),head=TRUE))
  }
  all_comp<-NULL
  all_comp<-d[complete.cases(d),]
  frecqs<-NULL
  for(x in 1:length(id)){
    
    frecqs<-rbind(frecqs,data.frame(id=id[x],nobs=sum(all_comp$ID==id[x])))  
  }
  return(frecqs)
}