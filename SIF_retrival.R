#This is the time series retrieval function for SIF product
#alpha 1.0
#developed by Yao Zhang on Feb. 12th, 2016

library(rgdal)
library(raster)

get_time_series<-function(coor){
  px<-ceiling(coor/0.5)
  timeseries<-as.data.frame(array(NA,dim=c(108,26)))
  for(i in 1:25){
    ts<-SIF_stack[181-px[1]+ax[i%%5+1],px[2]+360+ax[i%%5+1]]
    SIF<-as.vector(ts)
    timeseries[,i+1]<-data.frame(SIF)
  }
  layer<-unlist(attributes(ts)$dimnames[2])
  time<-substr(layer,9,14)
  timeseries[,1]<-time
  return(timeseries)
}

site<-read.csv("/data/ifs/users/yzhang/PROJECT/NA_GPP/list.txt")
#coor lat,long


SIF_f<-list.files('V:/SIF/GOME-2/V026_monthly_0.5deg/tiff/SIF/',pattern = '*.tif',full.names = T)
SIF_stack<-stack(SIF_f)

ax<-c(2,-2,-1,0,1)
av_mon<-rowMeans(timeseries[,2:26])
dim(av_mon)<-c(12,9)
av_ann<-rowMeans(av_mon)
sd_ann<-apply(av_mon,1,sd)
plot(1:12,av_ann,
         ylim=c(0.4,1.4),
         pch=15,xlab='month',ylab='SIF',
         main='k34 SIF in 5*5 window')
lines(1:12,av_ann)
arrows(1:12, av_ann-sd_ann, 1:12, av_ann+sd_ann, length=0.05, angle=90, code=3)

write.csv(av_ann,)


for (i in 1:length(site$SID)){
  coor<-matrix(c(site$long[i],site$lat[i]),ncol=2)
  ts<-get_time_series(coor)
  id<-rep(i,length(ts$time))
  temp<-data.frame(id,ts)
  if (i==1){
    data<-temp
  }else
    data<-rbind(data,temp)
}
write.csv(data,"/data/ifs/users/yzhang/PROJECT/NA_GPP/VPM_SG.txt")

#####

