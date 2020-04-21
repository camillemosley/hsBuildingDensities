##### Script for generating shoreline building density for lakes in hyperstability survey DNR dataset
##### SEJ
##### 4-13-2020
rm(list=ls())

# load spatial packages
library(rgdal)
library(rgeos)
library(raster)
library(geosphere)
library(sf)

####################
# Load WDNR HYDRO 24K
####################
setwd("~/Documents/Research/People/Students/current/Mosley_Camille/codeHelp/hyperstabilitySurveyBuildingDensities/WDNR_HYDRO_24K/")
gdb='WDNR_HYDRO_24K.gdb'
ogrListLayers(gdb)

# load a layer from a geodatabase
WDNRwbic=readOGR(gdb,layer="WD_HYDRO_WATERBODY_WBIC_AR_24K")
wbic=as.data.frame(WDNRwbic,stringsAsFactors=FALSE)

# project WDNR 24K in NAD83 (epsg:4269)
# epsg:4326 is WGS84
WDNRwbic_NAD83=spTransform(WDNRwbic,CRS("+init=epsg:4269"))

# get list of wbics for each county
setwd("~/Documents/Research/Fishscapes/hyperstability/hsSurvey/")
HSwbics=read.csv("HSwbic.csv",stringsAsFactors=FALSE)
# remove spaces from county names
HSwbics$county=gsub(" ","",HSwbics$county)

# get list of county geospatial files
setwd("~/Documents/Research/People/Students/current/Mosley_Camille/codeHelp/hyperstabilitySurveyBuildingDensities/HsSurvey_WI_buildings/")
countyFiles=list.files()
countyFiles=countyFiles[!grepl("x$",countyFiles)]

# removing counties without spatial data from HSwbics
HSwbics=HSwbics[HSwbics$county%in%toupper(gsub("_.*","",countyFiles)),]

# data frame to store output
HSbuildingsSummary=data.frame(matrix(NA,nrow(HSwbics),10))
colnames(HSbuildingsSummary)=c('wbic','county','lakePerimeter_m','lakeArea_m2','buildingCount50m','buildingCount100m','buildingCount200m','buildingDensity50m','buildingDensity100m','buildingDensity200m')

# loop through each county
counter=1
for(i in 1:length(countyFiles)){
  setwd("~/Documents/Research/People/Students/current/Mosley_Camille/codeHelp/hyperstabilitySurveyBuildingDensities/HsSurvey_WI_buildings/") 
  # if data is in a geodatabase
  if(grepl("gdb",countyFiles[i])){
    gdb=countyFiles[i]
    curCounty=readOGR(gdb,layer=gsub(".gdb","",countyFiles[i],fixed=TRUE))
    curCountyDF=as.data.frame(curCounty)
    
    # works for polygon or point data convert to building centroids
      centBuild=gCentroid(curCounty,byid=TRUE)
      # project building centroids in NAD83 (epsg:4269)
      centBuild_NAD83=spTransform(centBuild,CRS("+init=epsg:4269"))
 
  # other counties use a shapefile  
  }else{
    setwd(countyFiles[i])
    curCounty=st_read(paste(countyFiles[i],".shp",sep=""))
    
    # works for multipolygon shapefile...
    centBuild=as_Spatial(st_centroid(curCounty)$geometry)
    centBuild_NAD83=spTransform(centBuild,CRS("+init=epsg:4269"))
  }

  # subset focal county's lakes from HSwbics and WDNRwbic_NAD83 based on wbics
  curWBICs=HSwbics[grepl(gsub("_.*","",countyFiles[i]),HSwbics$county,ignore.case=TRUE),]
  curLakes=WDNRwbic_NAD83[wbic$WATERBODY_WBIC%in%curWBICs$wbic,]
  
  # count building densities for each lake and store in table
  for(j in 1:length(curLakes)){
    HSbuildingsSummary[counter,c(1,3:4)]=unlist(curLakes[j,]@data)
    HSbuildingsSummary[counter,2]=gsub("_.*","",countyFiles[i])
      
    p=curLakes[j,]
    p=spTransform(p,CRS=CRS("+proj=utm"))
    b50=gBuffer(p,width=50,quadsegs=1000)
    b50=spTransform(b50,CRS="+init=epsg:4269")
    b100=gBuffer(p,width=100,quadsegs=1000)
    b100=spTransform(b100,CRS="+init=epsg:4269")
    b200=gBuffer(p,width=200,quadsegs=1000)
    b200=spTransform(b200,CRS="+init=epsg:4269")
    
    # if you want to visualize one of these...
    plot(b200)
    plot(curLakes[j,],add=TRUE,col='lightblue')
    plot(centBuild_NAD83,add=TRUE)
    
    HSbuildingsSummary$buildingCount50m[counter]=sum(!is.na(over(centBuild_NAD83,b50)))
    HSbuildingsSummary$buildingCount100m[counter]=sum(!is.na(over(centBuild_NAD83,b100)))
    HSbuildingsSummary$buildingCount200m[counter]=sum(!is.na(over(centBuild_NAD83,b200)))
    counter=counter+1
  }
}

HSbuildingsSummary$buildingDensity50m=HSbuildingsSummary$buildingCount50m/(HSbuildingsSummary$lakePerimeter_m/1000)
HSbuildingsSummary$buildingDensity100m=HSbuildingsSummary$buildingCount100m/(HSbuildingsSummary$lakePerimeter_m/1000)
HSbuildingsSummary$buildingDensity200m=HSbuildingsSummary$buildingCount200m/(HSbuildingsSummary$lakePerimeter_m/1000)

#write.csv(HSbuildingsSummary,"HSbuildingsSummary.csv",row.names=FALSE)

