#This is the time series retrieval function for MOD09A1 product
#alpha 1.0
#developed by Yao Zhang on MAR.29th, 2016

library(rgdal)
library(raster)

get_time_series<-function(coor){
  prod<-c("aerosolmask","cloudmask","snow","evi","ndvi")
  timeseries<-as.data.frame(array(NA,dim=c(740,6)))
  names(timeseries)<-c("Date",prod)
  
  proj<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  years<-2000:2015

  prodir<-'/data/ifs/modis/scratch/mod09a1/geotiff/'
  
  sin.coor<-project(coor,proj)
  sin.cr<-sin.coor/463.3127
  h<-floor(sin.cr[1]/2400)+18
  v<-8-floor(sin.cr[2]/2400)
  px<-c(ceiling(sin.cr[1]%%2400),ceiling(2400-sin.cr[2]%%2400)) #calculate the tile and its location
  
  for (i in 1:5){
    dirs=paste(prodir,prod[i],'/',years,'/h',
                  formatC(h,format='d',width=2,flag='0'),
                  'v',formatC(v,format='d',width=2,flag='0'),sep="")
    files=list.files(dirs,pattern=".tif$",full.names = T)
    numfiles<-length(files)
    if (numfiles==0){
      return(data.frame(time=0,evi=0))
    }else{
      prodStack<-stack(files)
      ts<-prodStack[px[2],px[1]]
      layer<-unlist(attributes(ts)$dimnames[2])
      time<-substr(layer,10,16)
      prodTS<-as.vector(ts)
      timeseries[1:numfiles,1+i]<-data.frame(prodTS)
    }
  }
  timeseries$Date[1:numfiles]<-time
  timeseries<-timeseries[1:numfiles,]
  return(timeseries)
}

site<-read.csv("/data/ifs/users/yzhang/PROJECT/fpar/flux_data/site.csv",header=TRUE)
for (i in 1:length(site$SITE_ID)){
  coor<-matrix(c(site$LOCATION_LONG[i],site$LOCATION_LAT[i]),ncol=2)
  ts<-get_time_series(coor)
  write.csv(ts,paste("/data/ifs/users/yzhang/PROJECT/fpar/flux_data/VI_timeseries/VI",
                     formatC(i,format='d',width=3,flag='0'),"_",site$SITE_ID[i],".csv",sep=''))
}

