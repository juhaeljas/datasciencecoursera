pollutantmean <- function(directory, pollutan, id = 1:332) {
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
  mean(d[(!is.na(d[,pollutan])&is.element(d$ID,id)),pollutan])
}