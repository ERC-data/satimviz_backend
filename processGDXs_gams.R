#RUN extract results from an input GDX file name 
#SAVE result as rds file with same name

library(reshape2)
library(gdxrrw)
library(dplyr)
library(rpivotTable)
library(data.table)
library(shiny)


#INPUT FROM THE TERMINAL
  cmd_gdxfile <- commandArgs(trailingOnly = TRUE) #this must be the file name eg: 01REF (without the .gdx extension)
  gdxfile = cmd_gdxfile[1]

#the GDX files location
gdxLocation = 'C:/EMOD/GDXout/'
saverdspath = 'C:/EMOD/RDSfiles/'

#file paths used in extractResults
  
  # location of your GAMS main directory. 
  GAMS_lib_dir = 'C:/GAMS/win64/24.7' 
  # connect to the GAMS library.
  igdx(GAMS_lib_dir) 
  
  #main working directory for this file/script and colourcoding files etc. 
  workdir = 'C:/EMOD/satimviz/processing/' # this is also needed for the extractResults.R script
  

#LOAD FUNCTIONS
source(paste(workdir,'extractResults.R',sep =''))

print(paste('Running results extraction on: ',gdxfile,sep = ''))

#PROCESS GDX FILE:
  tmplist = list()
  setwd(gdxLocation)
  
  gdxPath = paste(gdxLocation,gdxfile,sep = '')
  
  gdxname = gdxfile
  listname = gdxname
  tmplist[[listname]] = processGDX(gdxPath,gdxname)

#SAVE RDS FILE
rdsname = gdxname
saveRDS(tmplist,paste(saverdspath,paste(rdsname,'.rds',sep = ''),sep = ''))
print('results extraction saved')
