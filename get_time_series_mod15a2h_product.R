#This is the time series retrieval function for MOD15A2H product
#alpha 1.0
#developed by Yao Zhang on April. 2nd, 2016

library(rgdal)
library(raster)

get_time_series<-function(coor){
  prod<-c("fpar","LAI","qc")
  timeseries<-as.data.frame(array(NA,dim=c(740,4)))
  names(timeseries)<-c("Date",prod)
  
  proj<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  years<-2012
  doy <-(1:46)*8-7
  
  prodir<-'/data/ifs/modis/products/MOD15A2H/'
  #prodir<-'V:/modis/products/MOD15A2H/'
  
  sin.coor<-project(coor,proj)
  sin.cr<-sin.coor/463.3127
  h<-floor(sin.cr[1]/2400)+18
  v<-8-floor(sin.cr[2]/2400)
  px<-c(ceiling(sin.cr[1]%%2400),ceiling(2400-sin.cr[2]%%2400)) #calculate the tile and its location
  
  for (p in 1:length(prod)){
    dirs=paste(prodir,prod[p],'/',years,'/',sep="")
    doydirs =""
    for (i in 1:length(years)){
      doydir<-paste(dirs[i],formatC(doy,format='d',width=3,flag='0'),sep="")
      doydirs=c(doydirs,doydir)
    }
    files = paste(doydirs,'/MOD15A2H.A',substr(doydirs,41-p,44-p),substr(doydirs,46-p,48-p),
                  '.h',formatC(h,format='d',width=2,flag='0'),
                  'v',formatC(v,format='d',width=2,flag='0'), 
                  '.',prod[p],'.tif',sep="")
    
    file_exist<-files[file.exists(files)]
    numfiles<-length(file_exist)
    if (numfiles==0){
      return(data.frame(time=0,evi=0))
    }else{
      prodStack<-stack(file_exist)
      ts<-prodStack[px[2],px[1]]
      layer<-unlist(attributes(ts)$dimnames[2])
      time<-substr(layer,11,17)
      prodTS<-as.vector(ts)
      timeseries[1:numfiles,1+p]<-data.frame(prodTS)
    }
  }
  timeseries$Date[1:numfiles]<-time
  timeseries<-timeseries[1:numfiles,]
  return(timeseries)
}

#site<-read.csv("/data/ifs/users/yzhang/PROJECT/fpar/flux_data/site.csv",header=TRUE)
#filein<-"/data/ifs/users/yzhang/DATA/FLUXNET2015/FLUXNET_subset_20161103/site_information.csv"

filein<-"/data/ifs/users/yzhang/DATA/SHA/HEIHE/site_info.csv"
site<-read.csv(filein,header=TRUE)
dir.create(paste(dirname(filein),"/LAI/",sep=""))
for (i in 5){#1:length(site$SITE_ID)){
  coor<-matrix(c(site$LOCATION_LONG[i],site$LOCATION_LAT[i]),ncol=2)
  ts<-get_time_series(coor)
  write.csv(ts,paste(dirname(filein),"/LAI/LAI",
                     formatC(i,format='d',width=3,flag='0'),"_",site$SITE_ID[i],".csv",sep=''))
}

