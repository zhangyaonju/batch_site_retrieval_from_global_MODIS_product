#This is the time series retrieval function for VPM dataset
#alpha 1.0
#developed by Yao Zhang on MAR. 11th, 2015

library(rgdal)
library(raster)

get_time_series<-function(coor){
  proj<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  years<-2000:2015
  #dir<-"/data/ifs/users/yzhang/VPM/NARR_VPM/VPM_product_NARR_SG/"
  #dir<-"/data/ifs/modis/products/GPP_VPM/v0_NA/"
  dir<-"/data/ifs/VPM/product/v04/gpp/"
  #dir<-"/data/ifs/modis/products/mod09a1/geotiff/lswi/"
  sin.coor<-project(coor,proj)
  sin.cr<-sin.coor/463.3127
  h<-floor(sin.cr[1]/2400)+18
  v<-8-floor(sin.cr[2]/2400)
  px<-c(ceiling(sin.cr[1]%%2400),ceiling(2400-sin.cr[2]%%2400)) #calculate the tile and its location
  pattern<-paste('GPP.*h',
                 formatC(h,format='d',width=2,flag='0'),
                 'v',formatC(v,format='d',width=2,flag='0'),
                 '.tif$',sep="")
  vpm.file<-list.files(dir, pattern=pattern,
                       full.names=TRUE,recursive=TRUE)
  if (length(vpm.file)==0){
    return(data.frame(time=0,VPM=0))
  }else{
    vpm.stack<-stack(vpm.file)
    ts<-vpm.stack[px[2],px[1]]
    layer<-unlist(attributes(ts)$dimnames[2])
    time<-substr(layer,5,11)
    VPM<-as.vector(ts)/1000
    timeseries<-data.frame(time,VPM)
    return(timeseries)
  }
}

#site<-read.csv("/data/ifs/users/yzhang/PROJECT/NA_GPP/list.txt")

site<-read.csv("/data/ifs/users/yzhang/VPM/TS/djw_grassland/site_jw_v2.csv")
for (i in 26:length(site$SITE_ID)){
  coor<-matrix(c(site$LOCATION_LONG[i],site$LOCATION_LAT[i]),ncol=2)
  ts<-get_time_series(coor)
  id<-rep(i,length(ts$time))
  temp<-data.frame(id,ts)
  if (i==1){
    data<-temp
  }else
    data<-rbind(data,temp)
}
#write.csv(data,"/data/ifs/users/yzhang/PROJECT/NA_GPP/apar_SG.csv")
write.csv(data,"/data/ifs/users/yzhang/VPM/TS/djw_grassland/VPM_GPP_v3.csv")

