#RUN extract results from an input GDX file name 
#SAVE result as rds file with same name
#BMc. Edited this so it calls the answer model extractor. removed cmd line references

library(reshape2)
library(gdxrrw)
library(dplyr)
library(rpivotTable)
library(data.table)
library(shiny)

#the GDX files location
gdxLocation = 'C:/EMOD/GDXout/'
saverdspath = 'C:/EMOD/RDSfiles/'

fileDetails = file.info(list.files(gdxLocation,pattern= '*.gdx',full.names = T))
fileDetails = fileDetails[with(fileDetails,order(as.POSIXct(mtime),decreasing = T)),]

gdxfile = rownames(fileDetails)[1]
gdxPath = gdxfile
gdxname = gsub('.*/','',gdxfile)#extract the name of the gdx file without the extension
gdxname = gsub('.gdx','',gdxname)

#file paths used in extractResults

# location of your GAMS main directory. 
GAMS_lib_dir = 'C:/GAMS/win64/24.7' 
# connect to the GAMS library.
igdx(GAMS_lib_dir) 

#main working directory for this file/script and colourcoding files etc. 
workdir = paste(getwd(),'satimviz/processing/',sep = '/') # this is also needed for the extractResults.R script


#LOAD FUNCTIONS
source(paste(workdir,'extractResults.R',sep =''))

print(paste('Running results extraction on: ',gdxfile,sep = ''))

#PROCESS GDX FILE:
tmplist = list()
#setwd(gdxLocation)

listname = gdxname
tmplist[[listname]] = processGDX(gdxPath,gdxname)

#SAVE RDS FILE
rdsname = gdxname
saveRDS(tmplist,paste(saverdspath,paste(rdsname,'.rds',sep = ''),sep = ''))
print('results extraction saved')
