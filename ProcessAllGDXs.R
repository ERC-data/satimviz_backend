#RUN results extraction on all GDX's in the gdxlocation
#SAVE results as rds file

library(reshape2)
library(gdxrrw)
library(dplyr)
library(rpivotTable)
library(data.table)
library(shiny)

# location of your GAMS main directory. 
GAMS_lib_dir = 'C:/GAMS/win64/24.7' 

#main working directory for this file/script and colourcoding files etc. 
workdir = 'C:/SATIMGE_02/satimviz/processing/'

#the GDX files location
gdxLocation = 'C:/SATIMGE_02/GDXout/'
saverdspath = 'C:/SATIMGE_02/RDSfiles/'

# connect to the GAMS library.
igdx(GAMS_lib_dir) 


#LOAD FUNCTIONS

source(paste(workdir,'extractResults.R',sep ='/'),local = FALSE)

setwd(gdxLocation)

gdxlist=list.files(pattern=".gdx")#get list of all gdx's in the location

#go over each gdx name on the list, and compute the gdx summaries for each, appending to a main list
N = length(gdxlist) 


for (i in (1:N)){
  tmplist = list()
  gdxPath = paste(gdxLocation,gdxlist[i],sep = '')
  listname = gsub('.{4}$', '', gdxlist[i])
  gdxname = substr(gdxlist[i],1,nchar(gdxlist[i])-4)
  
  tmplist[[listname]] = processGDX(gdxPath,gdxname)
  rdsname = paste(listname,'.rds',sep ='')
  
  saveRDS(tmplist,paste(saverdspath,rdsname,sep = ''))
  print('saving complete')
    
  }
