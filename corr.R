corr <- function(directory, threshold = 0) {
  d<-NULL
  for(i in 1:length(list.files(directory))){
    d<-rbind(d,read.csv(paste(directory,"/",list.files(directory)[i],sep=""),head=TRUE))
  }
  all_comp<-NULL
  all_comp<-d[complete.cases(d),]
  frecqs<-NULL
  for(x in 1:length(unique(d$ID))){
    frecqs<-rbind(frecqs,data.frame(id=unique(d$ID)[x],nobs=sum(all_comp$ID==unique(d$ID)[x])))  
  }
  all_comp_filtered<-all_comp[is.element(all_comp$ID,(frecqs[frecqs$nobs>=threshold,]$id)),]
    corr_res<-NULL
    for(x in 1:length(unique(d$ID))){
    corr_res<-c(corr_res,cor(all_comp_filtered[all_comp_filtered$ID==unique(all_comp_filtered$ID)[x],"sulfate"],
        all_comp_filtered[all_comp_filtered$ID==unique(all_comp_filtered$ID)[x],"nitrate"]))
    corr_res<-corr_res[!is.na(corr_res)]
    }
  return(corr_res)
}