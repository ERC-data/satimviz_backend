
#This script contains the functions for reading in GDX files and extracting results and processing other results
#it returns a list of the results for the GDX file name. 
#this is an adapted version of extractResults. It extracts from Answer models. 

mapPRC = read.csv(paste(workdir,'mapPRC.csv',sep =''))
mapCOM = read.csv(paste(workdir,'mapCOM.csv',sep =''))
convToNeg = function(val,id){
  #for making the energy balance
  if(id == 'Imports'){
    return(val)
  }else{
    val = val*-1 #convert to neg. 
  }
  
}
addPRCmap <- function(db){
  #this function gets the mapPRC dataset from the csv file which should be in your workdir
  #and merges it with db
  print('Adding process mapping')
  
  db = merge(db,mapPRC,all = FALSE)
  return(db)
}

addCOMmap <- function(db){
  #this function gets the mapPRC dataset from the csv file which should be in your workdir
  #and merges it with db
  print('Adding mapping for commodities')
  
  db = merge(db,mapCOM,all = FALSE)
  return(db)
}


addEmisSource = function(mychar){
  y = mychar
  x = substr(y,5,7)
  return(x)
}
addEmisNames = function(comin){
  emis_types = c('CH4S','CMOX','CO2S','N2OS','NMVS','NOXS','SOXS','CO2EQS','CH4S')
  n = length(emis_types)
  for (i in seq(1,n)){
    myemis = emis_types[i]
    if(grepl(myemis,comin)){
      emisname = myemis
    }
    
    
  }
  return(emisname)
  
}
getSectorNameFromComs = function(comin,sectorsDetail){
  n = length(sectorsDetail$sectorsNames)

  for (i in seq(1,n)){
    
    abrv = as.character(sectorsDetail[i,1])
    
    
    sectorname = ''
    if(grepl(abrv,comin)){
      sectorname = as.character(sectorsDetail[i,2])
      break #found it, so stop loop. 
    }
  }
  return(sectorname)
}

processGDX <- function(gdxPath,gdxname){
  
  print(paste('Reading in parameters from GDX'))
  
  
  #Read in Parameters-------------------------------
  
  #Case name
  #mruncase =rgdx.set(gdxPath,'MRUNCASE')
  myCase= gdxname #as.character(mruncase[mruncase$RUN == gdxname,2])
  print(paste('.......',paste(myCase,'.......',sep = ''),sep =''))
  
  #Capacity
  CAPL = rgdx.param(gdxPath,'PAR_CAPL')
  NCAPL = rgdx.param(gdxPath,'PAR_NCAPL')
  RESID = rgdx.param(gdxPath,'PRC_RESID')
  
  #Activity
  VARACT = rgdx.param(gdxPath,'VARACT')
  
  #flows
  F_IN = rgdx.param(gdxPath,'F_IN')
  F_OUT = rgdx.param(gdxPath,'F_OUT')
  
  #Costs
  CST_INVC = rgdx.param(gdxPath,'CST_INVC')
  CST_FIXC = rgdx.param(gdxPath,'CST_FIXC')
  CST_ACTC =rgdx.param(gdxPath,'CST_ACTC')
  
  LEVCOST = rgdx.param(gdxPath,'PAR_NCAPR')
  LEVCOST= LEVCOST[,-4] #Drop the 'LEVCOST' descriptor column
  
  FuelCOMBAL = rgdx.param(gdxPath,'PAR_COMBALEM')
  names(FuelCOMBAL) =c('Region','Year','Commodity','Timeslice','fuelCombal')
  FuelCOMBAL= FuelCOMBAL[FuelCOMBAL$Timeslice =='ANNUAL',]
  FuelCOMBAL= FuelCOMBAL[FuelCOMBAL$Timeslice =='ANNUAL',]
  FuelCOMBAL= FuelCOMBAL[,-4]
  
  #add names and remove redundant columns
  
  names(CAPL) = c('Region','Year','Process','CAPL')
  names(NCAPL) = c('Region','Year','Process','NCAPL')  
  names(RESID) = c('Region','Year','Process','RESID')
  names(VARACT) = c('Region','Year','Process','VAR_ACT')
  
  names(F_IN) = c('Region','V_Year','Year','Process','Commodity','Timeslice','F_IN')
  names(F_OUT) = c('Region','V_Year','Year','Process','Commodity','Timeslice','F_OUT')
  
  names(CST_ACTC) = c('Region','V_Year','Year','Process','CST_ACTC')
  names(CST_FIXC) = c('Region','V_Year','Year','Process','CST_FIXC')
  CST_INVC = CST_INVC[,c(-5)] # remove 'inv' column which appears to have no data
  names(CST_INVC) = c('Region','V_Year','Year','Process','CST_INVC')
  
  names(LEVCOST) = c('Region','Year','Process','LEVCOST')
  
  RESID = RESID[RESID$Year %in% unique(CAPL$Year),] #RESID has every year, not just milestone years, so extract all years that CAPL has.
  
  CST_INVC <- CST_INVC %>%
    group_by(Region,Process,Year)%>%
    summarise(CST_INVC = sum(CST_INVC)) 
  CST_ACTC <- CST_ACTC %>%
    group_by(Region,Process,Year)%>%
    summarise(CST_ACTC = sum(CST_ACTC))
  CST_FIXC <- CST_FIXC %>%
    group_by(Region,Process,Year)%>%
    summarise(CST_FIXC = sum(CST_FIXC))
  
  #sum over vintages:
  F_IN = F_IN%>%
    group_by(Region,Year,Process,Commodity,Timeslice)%>%
    summarise(F_IN = sum(F_IN))
  
  F_OUT = F_OUT%>%
    group_by(Region,Year,Process,Commodity,Timeslice)%>%
    summarise(F_OUT = sum(F_OUT))
  
  #convert commodity to upper case for mapping to work properly
  F_IN$Commodity = toupper(F_IN$Commodity)
  F_OUT$Commodity = toupper(F_OUT$Commodity)
  
  #add mapping 
  F_IN = addPRCmap(F_IN)
  F_IN = addCOMmap(F_IN)
  F_IN = F_IN[F_IN$Commodity_Name != 'Water',]#REMOVING WATER FOR NOW
  F_IN = droplevels(F_IN)
  
  F_OUT = addPRCmap(F_OUT)
  F_OUT = addCOMmap(F_OUT)
  F_OUT = droplevels(F_OUT)
  
  VARACT = addPRCmap(VARACT)
  VARACT = droplevels(VARACT)
  
  CST_INVC = addPRCmap(CST_INVC)
  CST_FIXC = addPRCmap(CST_FIXC)
  CST_ACTC = addPRCmap(CST_ACTC)
  
  ###################################
  ###################################
  
  #MARGINALS
  comsMargs = rgdx.param(gdxPath,'PAR_COMBALEM')
  names(comsMargs) =c('Region','Year','Commodity','Timeslice','comsMargs_value')
  
  #calculate avg. marginal for non annual commodities (like electricity)
    #get all non 'ANNUAL' entries
      tmpNotAnnual = comsMargs[comsMargs$Timeslice != 'ANNUAL',]
      tmp2 = as.data.frame(comsMargs[comsMargs$Timeslice == 'ANNUAL',]) #original annual marginals. 
      
    #get all commodities of interest. this excludes enduse commodities. 
      wantedComs = c('ODS','OGS','ELC','ELCC','COA','OHF','OKE','OLP','GAS','GIC')
      
      tmp = tmpNotAnnual[(tmpNotAnnual$Commodity %in% wantedComs),]
    
    #get volumes of these commodities flows
      tmpflows = F_OUT[F_OUT$Commodity %in% wantedComs,]
      
      tmpflows = tmpflows %>% group_by(Year,Region,Commodity,Timeslice)%>% summarise(totalVol = sum(F_OUT))#sum over all processes
      
      tmpflowsAn = tmpflows %>% group_by(Year,Region,Commodity)%>% summarise(totalAnVol = sum(totalVol))#sum over all timeslices to get annual volumes
      tmpflows = merge(tmpflows,tmpflowsAn) # this adds annual volume for each commodity
      
      tmpTScosts = merge(tmp,tmpflows) 
      tmpTScosts = tmpTScosts %>% mutate(totalVolCost = comsMargs_value*totalVol)%>% group_by(Year,Region,Commodity,totalAnVol)%>%
        summarise(totalVolCost = sum(totalVolCost))%>% mutate(comsMargs_value = totalVolCost/totalAnVol)
      tmpTScosts$Timeslice = as.factor('ANNUAL')
      
      tmp = as.data.frame(tmpTScosts[,names(comsMargs)])# for rbind to work. 
      
    #merge these results with original marginals
      comsMargs = rbind(tmp2,tmp)
  
  #finalise commodity marginals
    comsMargs = comsMargs[,names(comsMargs) != 'Timeslice'] #remove 'annual' timeslice column
    
    #add commodity names KEEPING ALL commodities
    comsMargs = merge(comsMargs,mapCOM,all.x = T)
    comsMargs$Case = myCase
  
  print('...done reading in parameters')
  
  #--------------------------------------------------------------
  
  print('...Refining the parameters and process results')
  
  
  #Total capacity = RESID + CAPL
  CAP_T = merge(CAPL,RESID,all = TRUE) 
  CAP_T[is.na(CAP_T)] = 0
  
  #Capacity total
  CAP_T = CAP_T %>%
    group_by(Region,Year,Process) %>%
    summarise(Capacity = sum(CAPL,RESID))
  CAP_T = addPRCmap(CAP_T)
  
  #ACTIVITY SHARES
  tmp = as.data.table(VARACT)
  vardts = tmp[,.(Vsector_sum = sum(VAR_ACT)),by = .(Year,Sector)] #Sum by sector
  vardf = merge(VARACT,as.data.frame(vardts))%>% mutate(VAR_ACTshare = VAR_ACT/Vsector_sum)
  vardf = vardf[,!(names(vardf)%in% c('Vsector_sum'))]
  
  #ENERGY SHARES
  #fuels = readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='Fuels')
  
  #GET processing sets from csv.
  tmp = read.csv(paste(workdir,'processingsets.csv',sep =''))
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],1]) 
  names(tmp2) = tmp[1,1]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  passengerModes =tmp2
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],2]) 
  names(tmp2) = tmp[1,2]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  fuelpcpwr =tmp2
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],3]) 
  names(tmp2) = tmp[1,3]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  fuelpcpwr_a =tmp2
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],4]) 
  names(tmp2) = tmp[1,4]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  fuelpcpwr_cb = tmp2 
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],5]) 
  names(tmp2) = tmp[1,5]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  freightModes = tmp2
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],6]) 
  names(tmp2) = tmp[1,6]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  mincldual = tmp2 
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],7]) 
  names(tmp2) = tmp[1,7]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  mincldual_a = tmp2
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],c(8,9)]) 
  names(tmp2) =c(as.character(tmp[1,8]),as.character(tmp[1,9]))
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  mfuelpwr = tmp2 
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],10]) 
  names(tmp2) = tmp[1,10]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  fuels = tmp2 
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],11]) 
  names(tmp2) = 
    tmp2= tmp2[tmp2[,1]!='',,drop = F]
  Fuels2 =tmp2
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],12]) 
  names(tmp2) = tmp[1,3]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  minclpwr = tmp2
  
  
  #TRANSPORT PROCESSING
  
  print('calculating transport results')
  
  #SUM OVER TIMESLICES TO AGGREGATE FOR THE YEAR
    F_INa <- F_IN %>% group_by(Region,Year,Process,Sector,Subsector,Subsubsector,Commodity,Commodity_Name,TechDescription)%>%
      summarise(F_IN = sum(F_IN))
    F_OUTa <- F_OUT %>%  group_by(Region,Year,Process,Sector,Subsector,Subsubsector,Commodity,Commodity_Name,TechDescription)%>%
      summarise(F_OUT = sum(F_OUT))
    
  #passenger
  passengerkmAll = F_OUT #bm
  passengerkmAll = passengerkmAll[,!(names(passengerkmAll) %in% c('Sector','Timeslice','Commodity_Name'))]#drop redundant columns
  names(passengerkmAll)[1] = 'TRA_commodity'
  
  #add F_IN 
  passengerkmAll = merge(passengerkmAll,F_INa,by.x = c('Process','Year','Region','Subsector','Subsubsector'),by.y = c('Process','Year','Region','Subsector','Subsubsector'))
  passengerkmAll = passengerkmAll[,!(names(passengerkmAll) %in% c('Sector','Timeslice'))]#drop redundant columns
  
  #add mapping
  passengerkmAll = addPRCmap(passengerkmAll)
  passengerkmAll = droplevels(passengerkmAll)
  passengerkmAll = passengerkmAll[,!(names(passengerkmAll) %in% c('Sector','Timeslice'))]#drop redundant columns
  
  #FREIGHT
  tonkmAll = F_OUT #bm
  tonkmAll = tonkmAll[,!(names(tonkmAll) %in% c('Timeslice','Sector','Commodity_Name'))]#drop the timeslice column
  #bm  tonkmAll$btkm_fout = tonkmAll$load*tonkmAll$F_OUT
  names(tonkmAll)[1] = 'TRA_commodity' # for distinguishing later in the pivottables
  #add FIN
  tonkmAll = merge(tonkmAll,F_INa)
  
  #add mapping
  tonkmAll = addPRCmap(tonkmAll)
  tonkmAll = droplevels(tonkmAll)
  tonkmAll = tonkmAll[,!(names(tonkmAll) %in% c('Timeslice','Sector'))]#drop the timeslice column
  
  
  
  #-----------------------------------
  #Combine relavant dataframes and lists
  print('Combining dataframes into relavent sections...')
  
  #POWER SECTOR - total capacity
  print('...power')
  pwr_cap = CAP_T[CAP_T$Sector == 'Power',] 
  pwr_cap$Case = myCase
  pwr_cap = pwr_cap[,!(names(pwr_cap)%in% c('Subsubsector'))]
  pwr_cap = droplevels(pwr_cap)
  
  #POWER SECTOR - NEW capacity
  pwr_ncap = addPRCmap(NCAPL)
  pwr_ncap = pwr_ncap[pwr_ncap$Sector == 'Power',]
  pwr_ncap$Case = myCase
  pwr_ncap = droplevels(pwr_ncap)
  
  #POWER SECTOR - flows
  pwr_flows = merge(vardf,F_INa,all.x = TRUE)
  pwr_flows = pwr_flows[pwr_flows$Sector =='Power',!(names(pwr_flows) %in% c('Timeslice'))] #drop timeslices
  pwr_flows$Case = myCase
  pwr_flows[is.na(pwr_flows$F_IN),'F_IN'] = 0
  pwr_flows = droplevels(pwr_flows)
  
  #POWER SECTOR - costs
  pwr_costs = merge(merge(CST_INVC,CST_ACTC,all = T),CST_FIXC,all = T)
  pwr_costs = pwr_costs[pwr_costs$Sector == 'Power',]
  pwr_costs[is.na(pwr_costs)] = 0
  pwr_costs$Case = myCase
  pwr_costs = droplevels(pwr_costs)
  
  tmp = merge(CAP_T,NCAPL,all = TRUE)#add capacity
  tmp = merge(tmp,CST_INVC,all.x = TRUE)#add investments
  #bm    tmp = merge(tmp,ERPRICE)#add elec price
  tmp = addPRCmap(tmp)#add mapping
  tmp = tmp[tmp$Sector == 'Power',]
  tmp$Case = myCase
  pwrdf = droplevels(tmp)
  
  
  #TRANSPORT
  print('...transport')
  tmp = merge(tonkmAll,passengerkmAll,all = TRUE)
  tmp$Case = myCase
  tmp = tmp[!(names(tmp) == 'Sector'),] #drop sector column - dont need
  tradf = droplevels(tmp)
  tradf = merge(VARACT[VARACT$Sector == 'Transport',],F_INa[F_INa$Sector == 'Transport',])
  tradf$Case = myCase
  tradf = tradf[,!(names(tradf) %in% c('Sector','Timeslice'))] #drop sector column - dont need
  tradf = droplevels(tradf)
  
  tra_flows = merge(vardf[vardf$Sector =='Transport',],F_INa)
  tra_flows[is.na(tra_flows)] = 0
  tra_flows = tra_flows[,!(names(tra_flows)%in% c('Sector','Timeslice'))]
  tra_flows$Case = myCase
  tra_flows = droplevels(tra_flows)
  
  tra_costs = CST_INVC[CST_INVC$Sector =='Transport',]
  tra_costs = tra_costs[,!(names(tra_costs)%in% c('Sector','Timeslice'))]
  tra_costs$Case = myCase
  tra_costs = droplevels(tra_costs)
  
  tra_cap = CAP_T[CAP_T$Sector =='Transport',]
  tra_cap$Case = myCase
  tra_cap = tra_cap[,!(names(tra_cap)%in% c('Sector'))]
  tra_cap = droplevels(tra_cap)
  
  NCAPL = addPRCmap(NCAPL)
  tra_ncap = NCAPL[NCAPL$Sector =='Transport',]
  tra_ncap$Case = myCase
  tra_ncap = tra_ncap[,!(names(tra_ncap)%in% c('Sector'))]
  tra_ncap = droplevels(tra_ncap)
  
  #REFINERIES
  print('...refineries')
  refsfout = F_OUTa[F_OUTa$Sector == 'Refineries'&F_OUTa$Commodity_Name != '',]
  refsin = F_INa[F_INa$Sector == 'Refineries'&F_INa$Commodity_Name != '',]
  refsvaract =VARACT[VARACT$Sector == 'Refineries',]
  refs_flows = merge(refsvaract,merge(refsin,refsfout,all = TRUE),all = TRUE)
  refs_flows$Case= myCase
  refs_flows[duplicated(paste(paste(refs_flows$VAR_ACT,refs_flows$Process),refs_flows$Year)),'VAR_ACT'] = 0# we get duplicate var_act for each F_IN commodity, make the duplicates 0 (except one of them)
  refs_flows = droplevels(refs_flows)
  
  refs_costs = merge(CST_INVC,merge(CST_ACTC,CST_FIXC,all = T),all = T)
  refs_costs = refs_costs[refs_costs$Sector =='Refineries',]
  refs_costs = refs_costs[,!(names(refs_costs)%in% c('Sector','Timeslice'))]
  refs_costs[is.na(refs_costs)] = 0
  refs_costs = refs_costs %>% mutate(Allcosts = CST_INVC+CST_ACTC+CST_FIXC)
  refs_costs$Case = myCase
  refs_costs = droplevels(refs_costs)
  
  refs_cap = CAP_T[CAP_T$Sector =='Refineries',]
  refs_cap$Case = myCase
  refs_cap = refs_cap[,!(names(refs_cap)%in% c('Sector'))]
  refs_cap = droplevels(refs_cap)
  
  NCAPL = addPRCmap(NCAPL)
  refs_ncap = NCAPL[NCAPL$Sector =='Refineries',]
  if(dim(refs_ncap)[1]!=0){#no ncaps, so the rest wont compute
    refs_ncap$Case = myCase
    refs_ncap = refs_ncap[,!(names(refs_ncap)%in% c('Sector'))]
    refs_ncap = droplevels(refs_ncap)
  }

  #INDUSTRY
  print('...industry')
  ind_flows = merge(vardf[vardf$Sector == 'Industry',],F_INa[F_INa$Sector =='Industry',])
  ind_flows = ind_flows[,!(names(ind_flows)%in% c('Sector','Timeslice'))]
  ind_flows$Case = myCase
  ind_flows = droplevels(ind_flows)
  
  if('Industries'%in% unique(CST_INVC$Sector)){
    
  ind_costs = merge(merge(CST_INVC,CST_FIXC,all.x = TRUE),CST_ACTC,all.x = TRUE)
  ind_costs = ind_costs[ind_costs$Sector =="Industry",!(names(ind_costs) %in% c('Sector','Timeslice'))]
  ind_costs[is.na(ind_costs)] = 0
  ind_costs = ind_costs %>% mutate(Allcosts = CST_INVC+CST_ACTC+CST_FIXC)
  ind_costs$Case = myCase
  ind_costs = droplevels(ind_costs)
  }else{
    print('no data for industry costs...')
    ind_costs = refs_costs # there are no costs
  }
  
  inddf = merge(VARACT[VARACT$Sector == 'Industry',],F_INa[F_INa$Sector == 'Industry',])
  inddf$Case = myCase
  inddf = inddf[,!(names(inddf) %in% c('Sector','Timeslice'))] #drop sector column - dont need
  inddf = droplevels(inddf)
  
  #RESIDENTIAL
  print('...residential')
  resdf = merge(VARACT[VARACT$Sector == 'Residential',],F_INa[F_INa$Sector == 'Residential',])
  resdf$Case = myCase
  resdf = resdf[,!(names(resdf) %in% c('Sector','Timeslice'))] #drop sector column - dont need
  resdf = droplevels(resdf)
  
  res_flows = merge(F_INa,vardf)
  res_flows = res_flows[res_flows$Sector =='Residential',!(names(res_flows) %in%c('Sector','Timeslice'))] #drop sector and timeslices
  res_flows$Case = myCase
  res_flows = droplevels(res_flows)
  
  res_cost = CST_INVC#merge(CST_INVC,CST_FIXC,all =T) #only using investment costs now since the res update, only investment costs are included. 
  #res_cost[is.na(res_cost)] = 0 
  res_cost = res_cost[res_cost$Sector =='Residential',]
  res_cost = res_cost[,names(res_cost)[!(names(res_cost) %in% 'Sector')]] #drop sector column
  #res_cost = res_cost %>% mutate(Allcosts = CST_INVC+CST_FIXC)
  
  res_cost$Case = myCase
  res_cost = droplevels(res_cost)

  #COMMERCIAL
  print('...commercial')
  comdf = merge(VARACT[VARACT$Sector == 'Commerce',],F_INa[F_INa$Sector == 'Commerce',])
  comdf$Case = myCase
  comdf = comdf[,!(names(comdf) %in% c('Sector','Timeslice'))]#drop sector column - dont need
  comdf = droplevels(comdf)
  
  com_flows = merge(vardf[vardf$Sector == 'Commerce',],F_INa[F_INa$Sector == 'Commerce',])
  com_flows = com_flows[,!(names(com_flows) %in%c('Sector','Timeslice'))]
  com_flows$Case = myCase
  com_flows = droplevels(com_flows)
  
  com_costs = merge(merge(CST_INVC[CST_INVC$Sector =='Commerce',],CST_FIXC,all.x = TRUE),CST_ACTC,all.x = TRUE)
  com_costs = com_costs[,!(names(com_costs) %in%c('Sector','Timeslice'))]
  com_costs[is.na(com_costs)] = 0
  com_costs = com_costs %>% mutate(Allcosts = CST_INVC+CST_FIXC+CST_ACTC)
  com_costs$Case = myCase
  com_costs = droplevels(com_costs)
  
  #CAP
  #VARACT
  VARACT$Case = myCase
  VARACT = droplevels(addPRCmap(VARACT))
  
  # GET SECTOR EMISSIONS BY FULL SECTOR DETAILS
  
  # get emissions factors for each process/commodity
  print('Getting emissions results from X-techs...')
  
    #things we want to include/exclude:
    myEmisTypes = c('CH4S','CMOX','CO2S','N2OS','NMVS','NOXS','SOXS','CO2EQS','CH4S')
    myEmisTypes_code = paste(myEmisTypes,collapse = '|')
    mycols = c('Sector','Subsector','Subsubsector','Commodity_Name')
   
    sectorsID = c('TRA','IND','AGR','PWR','RES','COM','UPS')
    sectorsNames = c('Transport','Industry','Agriculture','Power','Residential','Commerce','Supply')
    sectorsDetail = data.frame(sectorsID,sectorsNames)
    emissionsFactors = read.csv(paste(workdir,'emissionsFactors.csv',sep ='/'))#get emissions factors. 
    
    newemiss = F_OUT[grepl(myEmisTypes_code,F_OUT$Commodity),] #get all flows of the emissions commodities. 
    newemiss = newemiss %>% group_by(Commodity,Process,Year,TechDescription,Sector,Subsector,Subsector) %>% summarise(total_kt = sum(F_OUT)) #annualise emissions
    
    #remove the preffix of the commodity name to give us the emissions name
      remove = paste(sectorsID,collapse = '|')
      newemiss$Emissions = gsub(remove,'',newemiss$Commodity)
      
      newemiss$tmp = gsub('X','',newemiss$Process)
    
    #add sector details using sector ID's
      newemiss$tmp2 = sapply(newemiss$tmp,getSectorNameFromComs,sectorsDetail = sectorsDetail)
      newemiss$Sector = paste(newemiss$Sector,newemiss$tmp2,sep = '')
      newemiss$Emissions_source = 'Fuel'
        
      #remove temporary columns
        newemiss = newemiss[,!grepl('tmp',names(newemiss))]
      
      #add Fugitive emisssions source
        newemiss$Emissions_source = as.character(newemiss$Emissions_source)
        newemiss[grepl('F$',newemiss$Emissions),'Emissions_source'] = 'Fugitive'   
        
      #add GWP
        tmp_emis_facs = emissionsFactors %>% group_by(Emissions,GWP)%>%summarise(GWPx = mean(GWP))
        tmp = tmp_emis_facs
        tmp$Emissions = paste(tmp$Emissions,'F',sep = '')#need to add for Fugitive
        tmp_emis_facs$Emissions = as.character(tmp_emis_facs$Emissions)#so R stops complaining about coercing.
        tmp_emis_facs = rbind(tmp_emis_facs,tmp)
        
        newemiss = merge(newemiss,tmp_emis_facs)
        newemiss$CO2eq_kt = newemiss$total_kt*newemiss$GWP
        newemiss = newemiss[,!(grepl('GWP',names(newemiss)))]
        newemiss$Case = myCase
        all_emissions = newemiss
    #Alternate attempt (simpler)
     
  
    print('Calculating emissions from bottom up energy consumption...')
    myexclusions = "^U|^PEX|^X" #exclude refineries, transmissions, and exports
    
      Emissions_flows = F_IN[!(grepl('ELC',F_IN$Process))&!(grepl(myexclusions,F_IN$Process)),]# get all flow in's except electricity and exports
      Emissions_flows = merge(Emissions_flows,emissionsFactors)#add emissions factors column from csv
      Emissions_flows = Emissions_flows %>% mutate(emissions_kt = ktPJ*F_IN)#calculate ghg kt 
      Emissions_flows = Emissions_flows %>% mutate(CO2eq_kt = emissions_kt*GWP)
      Emissions_flows = Emissions_flows %>% group_by(Region,Process,Year,Sector,Subsector,Subsubsector,Commodity_Name,TechDescription,
                                                     Emissions) %>% summarise(emissions_kt = sum(emissions_kt),CO2eq_kt = sum(CO2eq_kt))#sum over timeslices
      names(Emissions_flows)[names(Emissions_flows) =='Commodity_Name'] = 'Emissions_source'
      Emissions_flows = ungroup(Emissions_flows)
      Emissions_flows = droplevels(Emissions_flows)
      #Emissions_flows$Emissions = sapply(Emissions_flows$Emissions,addEmisNames) #add emissions names, some of them are xyzCH4 etc.
    
    #some have processt emissions (catch these on the flow_out) - no need to convert from PJ 
      femissions = paste(unique(emissionsFactors[,'Emissions']),'F',sep = '')
    
    #get all processes that produce femissions
      Emissions_flows_prc = F_OUT[(grepl(paste(femissions,collapse = '|'),F_OUT$Commodity)),!(names(F_OUT) %in% mycols)] 
      names(Emissions_flows_prc)[names(Emissions_flows_prc) == 'F_OUT'] = 'emissions_kt' #change name of FOUT to kt. since this is not PJ.
      names(Emissions_flows_prc)[names(Emissions_flows_prc) == 'Commodity'] = 'Emissions' # change name of commodity to Emission
    
      tmp = emissionsFactors[seq(1,length(unique(emissionsFactors[,'Emissions']))),-1] # to add GWP for process emissions
      tmp$Emissions = paste(tmp$Emissions,'F',sep ='')
    
      Emissions_flows_prc = merge(Emissions_flows_prc,tmp)
      Emissions_flows_prc = addPRCmap(Emissions_flows_prc)
      
      Emissions_flows_prc = Emissions_flows_prc %>% mutate(CO2eq_kt = emissions_kt*GWP) %>%
        group_by(Region,Process,Year,Sector,Subsector,Subsubsector,TechDescription,Emissions)%>%
        summarise(emissions_kt = sum(emissions_kt),CO2eq_kt = sum(CO2eq_kt))# sum over timeslices
    
      Emissions_flows_prc$Emissions_source = 'Process'
      Emissions_flows_prc$Emissions = paste(Emissions_flows_prc$Emissions,'_prc',sep = '')#add a suffix to denote process emissions
      Emissions_flows_prc = ungroup(Emissions_flows_prc)
      #Emissions_flows_prc$Emissions= sapply(Emissions_flows_prc$Emissions,addEmisNames)
      Emissions_flows_prc = droplevels(Emissions_flows_prc)
      
    #COmbine the Emissions and process emissions together
      
      All_emissions2 = rbind(Emissions_flows,Emissions_flows_prc)
      All_emissions2$Case = myCase
    print('...done calculating')
  #subselect sectors:
  
  com_emis = all_emissions[all_emissions$Sector == 'Commerce',]
  refs_emis = all_emissions[all_emissions$Sector == 'Refineries',]
  pwr_emis = all_emissions[all_emissions$Sector == 'Power',]
  res_emis = all_emissions[all_emissions$Sector == 'Residential',]
  tra_emis= all_emissions[all_emissions$Sector == 'Transport',]
  ind_emis = all_emissions[all_emissions$Sector == 'Industry',]
  sup_emis = all_emissions[all_emissions$Sector == 'Supply',]
  
  print('...done getting emissions results')
  
  print('Generating energy balances...')
  #Assembling energy balance  STILL NEEDS FIXING - things are not balancing
  myPrc_exclude = paste(c('^X','ETRANS','ZZ','EPP','EPD','UXL'),collapse = '|') #transmissions
  tmp = append(myEmisTypes,c('WAT','PWRENV'))
  myCom_exclude = "WAT|PWRENV" #remove emissions from the selection
  
  #exclude transmissions techs, ETRANS,ZZ, and exclude all emissions and other non fuel commodities
  x = !(grepl(myPrc_exclude,F_INa$Process))&!(F_INa$Commodity_Name %in% fuels)#&(F_INa$Sector != '')
  fin = droplevels(F_INa[x,])
  fin[fin$Subsector !='Imports','F_IN'] = fin[fin$Sector !='Imports','F_IN']*-1#convert to negative
  #fin$F_IN = sapply(val = fin$F_IN,id = fin$Subsector,convToNeg)
  
  names(fin)[names(fin)=='F_IN'] = 'flow_PJ'
  
  x =!(grepl(myPrc_exclude,F_OUTa$Process))&!(grepl(myCom_exclude,F_OUTa$Commodity))&!(F_OUTa$Commodity_Name %in% fuels)#&!(F_OUTa$Sector == ' ')
  fout = droplevels(F_OUTa[x,])
  names(fout)[names(fout) == 'F_OUT'] = 'flow_PJ'
  
  EB = merge(fin,fout,all = TRUE) #merge the flows together
  EB[is.na(EB)] = 0
  
  print('Energy Balance sectors: ')
  print(unique(EB$Sector))
  
  EB$Case = myCase
  EB = droplevels(EB[EB$Commodity_Name != '',])
  print('...done generating energy balances')
  
  print('calcualting power sector indicators...')
  pwr_indicators = merge(CAP_T[CAP_T$Process == 'ETRANS',],F_OUTa,all.x = TRUE)
  pwr_indicators = pwr_indicators[,names(pwr_indicators)%in% c('Capacity','Region','Year','F_OUT')]
  names(pwr_indicators)[names(pwr_indicators)== 'Capacity'] = 'Peak_dispatch_GW'
  names(pwr_indicators)[names(pwr_indicators)== 'F_OUT'] = 'Elec_dispatch_PJ'
  
    #getting total electricity consumed (including autogenerated)
    tmp = F_IN[grepl('.*ELC$',F_IN$Commodity)&F_IN$Sector != 'Power',]
    #drop all Xtechs, and pumped storage, and transmission
      tmp = tmp[!(grepl('^ELC$|INDELC',tmp$Commodity)),]
      tmp = tmp%>%group_by(Region,Year,Sector,Subsector)%>%summarise(elec_consumed = sum(F_IN))
  
  pwr_indicators$Peak_dispatch_GW = pwr_indicators$Peak_dispatch_GW*(1/0.96) #adjust for transmision efficiency
  pwr_indicators$Elec_dispatch_TWh = pwr_indicators$Elec_dispatch_PJ/3.6
  pwr_indicators$LoadFactor = (pwr_indicators$Elec_dispatch_TWh*1000)/(pwr_indicators$Peak_dispatch_GW*8760)
  pwr_indicators$Case = myCase
  print('...done calculating indicators.')
  
  coalPrices = data.frame() #need to fix shiny part to exclude this. This is a temp fix. we are not reading coal prices 
                            #externally anymore
  print('getting electricity price results...')
  
  elcprice =  rgdx.param(gdxPath,'ERPRICE')
  names(elcprice) = c('Year','Case','RpkWh')
  print('getting investment costs for processes')
  
  invcost = rgdx.param(gdxPath,'INVCOST')
  names(invcost) = c('Case','Year','base','Process','invcost')
  invcost = merge(invcost,mapPRC,all.x = T) # this is addPRC but wanted to make sure all items are kept. 
  
  #Combine into list:
  masterlist = list(pwr_indicators,pwr_cap,pwr_ncap,pwr_flows,pwr_costs,tradf,coalPrices,VARACT,inddf,ind_flows,
                    ind_costs,resdf,res_flows,res_cost,comdf,com_costs,com_flows,tra_flows,tra_costs,tra_cap,
                    tra_ncap,refs_flows,refs_costs,refs_cap,refs_ncap,pwr_emis,ind_emis,res_emis,com_emis,tra_emis,
                    sup_emis,refs_emis,all_emissions,EB,F_IN,F_OUT,comsMargs,elcprice,invcost,All_emissions2)
 
  print('....DONE PROCESSING RESULTS!....')
  return(masterlist)
}