
#This script contains the functions for reading in GDX files and extracting results and processing other results
#it returns a list of the results for the GDX file name. 
mapPRC = read.csv(paste(workdir,'mapPRC.csv',sep =''))
mapCOM = read.csv(paste(workdir,'mapCOM.csv',sep =''))

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
  
  db = merge(db,mapCOM,all.x = T)
  db[is.na(db)] = ''# make all commodities without a name blank
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
  
  comsMargs = rgdx.param(gdxPath,'PAR_COMBALEM')
  names(comsMargs) =c('Region','Year','Commodity','Timeslice','comsMargs_value')
  comsMargs= comsMargs[comsMargs$Timeslice =='ANNUAL',]
  comsMargs= comsMargs[,-4] #remove 'annual' timeslice column
  
  #add commodity names KEEPING ALL commodities
  comsMargs = merge(comsMargs,mapCOM,all.x = T)
  comsMargs$Case = myCase
  
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
  
  simDEMX = rgdx.param(gdxPath,'SIM_DEMX')# demand extracted from excel
  names(simDEMX) = c('Commodity','Year','Demand')
  
  
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
  names(tmp2) = tmp[1,11]
    tmp2= tmp2[tmp2[,1]!='',,drop = F]
  Fuels2 =tmp2
  
  tmp2 = data.frame(tmp[2:dim(tmp)[1],12]) 
  names(tmp2) = tmp[1,3]
  tmp2= tmp2[tmp2[,1]!='',,drop = F]
  minclpwr = tmp2
  
  
  #totalFinal = sum of all fuels over all sectors. one value for each year
  CINPL1F <- F_IN[F_IN$Sector != ''&F_IN$Commodity_Name != ''&F_IN$Commodity_Name %in% fuels$Fuels,]%>%
    group_by(Region,Year,Sector,Commodity_Name)%>%
    summarise(total_in = sum(F_IN))
  
  totalFinal <- CINPL1F %>%
    group_by(Region,Year)%>%
    summarise(final = sum(total_in))
  
  #coalfinal. Note that this excludes exports (the supply sector)
  coalFinal = F_IN[F_IN$Commodity_Name == 'Coal'& !(F_IN$Sector %in% c('','Supply')),] %>%
    group_by(Region,Year)%>%
    summarise(CoalTotal = sum(F_IN))
  
  #coalsharefinal 
  coalShareFinal = merge(coalFinal,totalFinal)
  coalShareFinal$CoalShare = coalShareFinal$CoalTotal/coalShareFinal$final
  coalShareFinal = coalShareFinal[,-c(3,4)]
  
  #gasfinal. Note that this excludes exports (the supply sector)
  gasFinal = F_IN[F_IN$Commodity_Name %in% c('GAS','Gas')&F_IN$Sector !='',] %>%
    group_by(Region,Year)%>%
    summarise(GasTotal = sum(F_IN))
  
  gasShareFinal = merge(gasFinal,totalFinal)
  gasShareFinal$GasShare = gasShareFinal$GasTotal/gasShareFinal$final
  gasShareFinal = gasShareFinal[,-c(3,4)]
  
  oilFinal = F_OUT[F_OUT$Commodity_Name == 'Crude Oil'&F_OUT$Sector == 'Supply',]%>% 
    group_by(Region,Year)%>%
    summarise(oilTotal = sum(F_OUT))
  
  oilShareFinal = merge(totalFinal,oilFinal) %>% mutate(oilshare = oilTotal/final)
  oilShareFinal = oilShareFinal[,-c(3,4)]
  
  #Fossil totals
  fossilTotals = merge(merge(coalFinal,gasFinal),oilFinal)
  
  #Fossil Share 
  fossilShareFinal = merge(merge(gasShareFinal,coalShareFinal),oilShareFinal)
  fossilShareFinal = fossilShareFinal%>% mutate(totalFossilShare = GasShare+CoalShare+oilshare)
  #fossilShareFinal = fossilShareFinal[,-c(3,4)]
  
  
  #Average coal prices. NEEDS SIM inputs  
  print('Calculating Average coal prices')
  sim_fuelpx = rgdx.param(gdxPath,'SIM_FUELPX')#fuel price (input) for central basin
  names(sim_fuelpx) =c('Process','Year','FUELPX')
  
  #fuelpcpwr_cb = readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='fuelpcpwr_cb')
  #fuelpcpwr_a =  readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='fuelpcpwr_a')
  #fuelpcpwr=  readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='fuelpcpwr')
  
  sim_FuelPcPwrCB = sim_fuelpx[sim_fuelpx$Process %in% fuelpcpwr_cb$Process,]
  sim_FuelPcPwrA = sim_fuelpx[sim_fuelpx$Process %in%fuelpcpwr_a$Process,]
  sim_FuelPcPwr = sim_fuelpx[sim_fuelpx$Process %in%fuelpcpwr$Process,]
  
  avgcoalpriceCB = merge(VARACT,sim_FuelPcPwrCB) %>% 
    mutate(t_cost = VAR_ACT*FUELPX)
  avgcoalpriceCB = avgcoalpriceCB%>%
    group_by(Region,Year)  %>%
    summarise(t_act = sum(VAR_ACT),
              t_cost = sum(t_cost))
  avgcoalpriceCB = avgcoalpriceCB %>% mutate(avgCLpriceCB = t_cost/t_act)           
  avgcoalpriceCB = avgcoalpriceCB[,c(1,2,5)]
  
  avgcoalpriceA = merge(VARACT,sim_FuelPcPwrA) %>% 
    mutate(t_cost = VAR_ACT*FUELPX)
  avgcoalpriceA = avgcoalpriceA%>%
    group_by(Region,Year)  %>%
    summarise(t_act = sum(VAR_ACT),
              t_cost = sum(t_cost))
  avgcoalpriceA = avgcoalpriceA %>% mutate(avgCLpriceA = t_cost/t_act)   
  avgcoalpriceA = avgcoalpriceA[,c(1,2,5)]
  
  #all 
  avgcoalprice = merge(VARACT,sim_FuelPcPwr) %>% 
    mutate(t_cost = VAR_ACT*FUELPX)
  avgcoalprice = avgcoalprice%>%
    group_by(Region,Year)  %>%
    summarise(t_act = sum(VAR_ACT),
              t_cost = sum(t_cost))
  avgcoalprice = avgcoalprice %>% mutate(avgCLpriceAll = t_cost/t_act)   
  avgcoalprice = avgcoalprice[,c(1,2,5)]
  
  #Electricity price calculations 
  print('calculating electricity price')
  TCST_ELE = merge(CST_INVC,CST_ACTC,all = TRUE)
  TCST_ELE = merge(TCST_ELE,CST_FIXC,all = TRUE)
  TCST_ELE[is.na(TCST_ELE)] = 0
  TCST_ELE = TCST_ELE[TCST_ELE$Sector == 'Power',]
  TCST_ELE = TCST_ELE %>% mutate(AllCosts = CST_ACTC+CST_FIXC+CST_INVC)
  TCST_ELE = TCST_ELE %>%
    group_by(Region,Year)%>%
    summarise(tcst_ele = sum(AllCosts))
  
  
  #dedicated mines
  #minclpwr =readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='minclpwr')
  
  TCST_PWRCL = merge(CST_ACTC,merge(CST_FIXC,CST_INVC,all = TRUE),all = TRUE) 
  TCST_PWRCL = TCST_PWRCL[TCST_PWRCL$Process %in% minclpwr$Process,]
  TCST_PWRCL[is.na(TCST_PWRCL)] = 0
  TCST_PWRCL = TCST_PWRCL %>% 
    group_by(Region,Year) %>%
    summarise(tcst_pwrcl = sum(CST_ACTC,CST_FIXC,CST_INVC))
  
  
  #non dedicated mines
  #mincldual = readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='mincledual')
  
  mincldual_i = merge(mincldual,CST_INVC,all.x = TRUE)
  mincldual_f = merge(mincldual,CST_FIXC,all.x = TRUE)
  mincldual_a = merge(mincldual,CST_ACTC,all.x = TRUE)
  mincldual_all = merge(mincldual_i,mincldual_a,all = TRUE)
  TCST_PWRDUAL = merge(mincldual_all,mincldual_f,all = TRUE)
  TCST_PWRDUAL = TCST_PWRDUAL[!(is.na(TCST_PWRDUAL$Year)),]#remove NA rows
  
  tmp = VARACT[VARACT$Process == 'XPWRCLE',names(VARACT) %in%c('Year','VAR_ACT','Region')]
  names(tmp)[names(tmp) == 'VAR_ACT'] = 'xpwr_varact' #change name because we need to add VAR_ACT separately for mincldual techs too 
  TCST_PWRDUAL = merge(TCST_PWRDUAL,tmp,all = TRUE)#add var act for these techs
  
  TCST_PWRDUAL = merge(TCST_PWRDUAL,VARACT,all.x = TRUE)#add var act for these techs
  
  TCST_PWRDUAL[is.na(TCST_PWRDUAL)] = 0
  TCST_PWRDUAL = TCST_PWRDUAL %>% mutate(tcost = xpwr_varact*(CST_ACTC+CST_FIXC+CST_INVC))
  TCST_PWRDUAL = TCST_PWRDUAL %>%
    group_by(Region,Year) %>%
    summarise(vartotal = sum(VAR_ACT),tcost = sum(tcost))
  TCST_PWRDUAL = TCST_PWRDUAL%>% mutate(tcst_pwrdual = tcost/vartotal)
  
  
  #non dedicated mines waterberg
  #mincldual_a = readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='mincldual_a')
  TCST_PWRDUAL_A =  merge(CST_ACTC,merge(CST_FIXC,CST_INVC,all = TRUE),all = TRUE) 
  TCST_PWRDUAL_A = merge(TCST_PWRDUAL_A,VARACT,all.x = TRUE)
  TCST_PWRDUAL_A = TCST_PWRDUAL_A[TCST_PWRDUAL_A$Process %in%  mincldual_a$Process,]
  TCST_PWRDUAL_A[is.na(TCST_PWRDUAL_A)] = 0
  
  tmp = merge(TCST_PWRDUAL_A[,c(2,3)],VARACT[VARACT$Process == 'XPWRCLE-A',],all.x = TRUE)# used for getting varact of XPWRCLE-A for next calculation 
  if(any( is.na(unique(tmp$Process)) ) ) {
    #no XPWRCLE-A in the model.
    #this catches the case where there is no XPRWCLE-A
    print('no XPWRCLE-A for PWRDUAL-A')
    tmp[is.na(tmp)] = 0
  }else{
    tmp$Process = unique(tmp$Process)[!(is.na(unique(tmp$Process)))]
    tmp[is.na(tmp)] = 0
  }
  
  TCST_PWRDUAL_A$Var_XPWR = tmp$VAR_ACT
  TCST_PWRDUAL_A = TCST_PWRDUAL_A %>% mutate(tcost = Var_XPWR*(CST_ACTC+CST_FIXC+CST_INVC))%>%
    group_by(Region,Year,tcost) %>%
    summarise(vartotal = sum(VAR_ACT))%>%
    mutate(tv = tcost/vartotal)%>%
    group_by(Region,Year)%>%
    summarise(tcst_pwrdual_a = sum(tv))
  
  #total coal costs
  TCST_PWRCL_T = TCST_PWRDUAL_A 
  TCST_PWRCL_T$tcst_pwrcl_t = TCST_PWRCL_T$tcst_pwrdual_a +TCST_PWRDUAL$tcst_pwrdual + TCST_PWRCL$tcst_pwrcl
  TCST_PWRCL_T = TCST_PWRCL_T[,-3]
  
  #TCST_PWROTH
  #take process activity from mfuelpwr and multiply with marginal in comsMargs that corresponds with that process
  
  #GAMS CODE: 
  #TCST_PWROTH(T) = SUM((P,C)$MFUELPWR(P,C), VAR_ACT(T,P)*comsMargs(T,C,RUN));
  
  TCST_PWROTH = merge(merge(mfuelpwr,VARACT),comsMargs)
  TCST_PWROTH = TCST_PWROTH %>% mutate(other_pwr_costs = VAR_ACT*comsMargs_value)%>%
    group_by(Region,Year)%>%
    summarise(other_pwr_costs =sum(other_pwr_costs))
  
  #regulated elctricity price
  #the gams code: 
  #ERPRICE(T,RUN) = (TCST_PWRCLT(T)+TCST_PWROTH(T)+TCST_ELE(T))/VARACTPL1(T,'Power',RUN)*3.6/1000
  
  varact_pwr = addPRCmap(VARACT)
  varact_pwr = varact_pwr[varact_pwr$Sector == 'Power',]
  varact_pwr_an = varact_pwr%>% group_by(Region,Year)%>%
    summarise(t_pwract = sum(VAR_ACT))
  
  ERPRICE = merge(merge(TCST_PWROTH,merge(TCST_PWRCL_T,TCST_ELE)),varact_pwr_an)
  ERPRICE = ERPRICE%>% mutate(t_pwrcost = (3.6/1000)*(tcst_pwrcl_t+tcst_ele+other_pwr_costs))%>%
    mutate(Elec_price_RpkWh = round(t_pwrcost/t_pwract,4))
  ERPRICE = ERPRICE[,names(ERPRICE)%in% c('Region','Year','Elec_price_RpkWh')] # keep only the data we want. 
  ERPRICE$Case = myCase #add scenario name. 
  
  #TRANSPORT PROCESSING
  
  print('calculating transport results')
  #passenger
  
  #SUM OVER TIMESLICES TO AGGREGATE FOR THE YEAR
  F_INa <- F_IN %>% group_by(Region,Year,Process,Sector,Subsector,Subsubsector,Commodity,Commodity_Name)%>%
    summarise(F_IN = sum(F_IN))
  F_OUTa <- F_OUT %>%  group_by(Region,Year,Process,Sector,Subsector,Subsubsector,Commodity,Commodity_Name)%>%
    summarise(F_OUT = sum(F_OUT))
  
  #passengerModes = readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='PassengerModes')
  passengerkm = rgdx.param(gdxPath,'Passengerkm')#get the passenger km
  names(passengerkm) = c('Commodity','Year','bpkm') 
  
  simDEMX_pass = simDEMX[simDEMX$Commodity %in% passengerModes$Commodity,]
  
  passengerOccupancy = merge(passengerkm,simDEMX[simDEMX$Commodity %in% passengerModes$Commodity,])
  names(passengerOccupancy)[4] = 'bvkm'
  #names(passengerOccupancy)[1] = 'Transport_Commodity'
  passengerOccupancy$Occupancy = passengerOccupancy$bpkm/passengerOccupancy$bvkm
  
  #add FOUT
  passengerkmAll = merge(passengerOccupancy,F_OUT)#this will cut out all non-milestone years
  passengerkmAll = passengerkmAll[,!(names(passengerkmAll) %in% c('Sector','Timeslice','Commodity_Name'))]#drop redundant columns
  passengerkmAll$bpkm_fout = passengerkmAll$Occupancy*passengerkmAll$F_OUT
  names(passengerkmAll)[1] = 'TRA_commodity'
  
  #add F_IN 
  passengerkmAll = merge(passengerkmAll,F_INa,by.x = c('Process','Year','Region','Subsector','Subsubsector'),by.y = c('Process','Year','Region','Subsector','Subsubsector'))
  passengerkmAll = passengerkmAll[,!(names(passengerkmAll) %in% c('Sector','Timeslice'))]#drop redundant columns

  #add mapping
  passengerkmAll = addPRCmap(passengerkmAll)
  passengerkmAll = droplevels(passengerkmAll)
  passengerkmAll = passengerkmAll[,!(names(passengerkmAll) %in% c('Sector','Timeslice'))]#drop redundant columns
  
  #FREIGHT
  #freightModes = readWorksheetFromFile(paste(workdir,'ProcessingSets.xlsx',sep =''), sheet ='FreightModes')
  tonkm = rgdx.param(gdxPath,'Tonkm')
  
  names(tonkm) =c('Commodity','Year','btkm')
  
  freightLoad = merge(tonkm,simDEMX[simDEMX$Commodity %in% freightModes$Commodity,])
  names(freightLoad)[4] = 'bvkm'
  
  freightLoad = freightLoad%>% mutate(load = btkm/bvkm)
  
  tonkmAll = merge(freightLoad,F_OUT)
  tonkmAll = tonkmAll[,!(names(tonkmAll) %in% c('Timeslice','Sector','Commodity_Name'))]#drop the timeslice column
  tonkmAll$btkm_fout = tonkmAll$load*tonkmAll$F_OUT
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
  pwr_costs = merge(merge(CST_INVC,CST_ACTC,all.x = T),CST_FIXC,all.x = T)
  pwr_costs = pwr_costs[pwr_costs$Sector == 'Power',]
  pwr_costs$Case = myCase
  pwr_costs[is.na(pwr_costs)]= 0
  pwr_costs = droplevels(pwr_costs)
  
  tmp = merge(CAP_T,NCAPL,all = TRUE)#add capacity
  tmp = merge(tmp,CST_INVC,all.x = TRUE)#add investments
  tmp = merge(tmp,ERPRICE)#add elec price
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
  
  tra_flows = merge(vardf[vardf$Sector =='Transport',],F_INa)
  tra_flows = merge(tra_flows,passengerkmAll[,names(passengerkmAll)%in%c('Year','Process','bpkm_fout')],all.x = T)
  tra_flows =merge(tra_flows,tonkmAll[,names(tonkmAll)%in% c('Year','Process','btkm_fout')],all.x = T)
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
  
  refs_costs = merge(CST_INVC,merge(CST_ACTC,CST_FIXC,all.x = T),all.x = T)
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
  refs_ncap$Case = myCase
  refs_ncap = refs_ncap[,!(names(refs_ncap)%in% c('Sector'))]
  refs_ncap = droplevels(refs_ncap)
  
  #COAL PRICES
  tmp = merge(avgcoalprice,avgcoalpriceA,all = TRUE)
  tmp = merge(tmp,avgcoalpriceCB,all = TRUE)
  
  tmp$Case = myCase
  coalPrices = droplevels(tmp)
  
  #INDUSTRY
  print('...industry')
  ind_flows = merge(vardf[vardf$Sector == 'Industry',],F_INa[F_INa$Sector =='Industry',])
  ind_flows = ind_flows[,!(names(ind_flows)%in% c('Sector','Timeslice'))]
  ind_flows$Case = myCase
  ind_flows = droplevels(ind_flows)
  
  ind_costs = merge(merge(CST_INVC,CST_FIXC,all.x = TRUE),CST_ACTC,all.x = TRUE)
  ind_costs = ind_costs[ind_costs$Sector =="Industry",!(names(ind_costs) %in% c('Sector','Timeslice'))]
  ind_costs[is.na(ind_costs)] = 0
  ind_costs = ind_costs %>% mutate(Allcosts = CST_INVC+CST_ACTC+CST_FIXC)
  ind_costs$Case = myCase
  ind_costs = droplevels(ind_costs)
  
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
  
  res_cost = CST_INVC
  res_cost = res_cost[res_cost$Sector =='Residential',]
  res_cost = res_cost[,-4] #drop sector column
  res_cost = res_cost %>% mutate(Allcosts = CST_INVC+CST_FIXC)
  res_cost = res_cost[,-c(6,7)]
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
  
  #things we want to include/exclude:
  myEmisTypes = c('CH4S','CMOX','CO2S','N2OS','NMVS','NOXS','SOXS','CO2EQS','CH4S')
  myEmisTypes_code = paste(myEmisTypes,collapse = '|')
  mycols = c('Sector','Subsector','Subsubsector','Commodity_Name')
  myexclusions = paste('^',paste(c('U','PEX','X'),collapse = '|'),sep = '') #exclude refineries, transmissions, and exports
  
  #Alternate attempt (simpler)
  emissionsFactors = read.csv(paste(workdir,'emissionsFactors.csv',sep ='/'))#get emissions factors
  
  
  #get total flow in to each sector+subsector for each commodity
  
  Emissions_flows = F_INa[!(grepl('ELC',F_INa$Process))&!(grepl(myexclusions,F_INa$Process)),]# get all flow in's except electricity and exports
  Emissions_flows = merge(Emissions_flows,emissionsFactors)#add emissions factors column from csv
  Emissions_flows = Emissions_flows %>% mutate(GHG_kt = ktPJ*F_IN)#calculate ghg kt 
  Emissions_flows = Emissions_flows %>% group_by(Region,Process,Year,Sector,Subsector,Subsubsector,Commodity_Name,
                                                 Emissions) %>% summarise(GHG_kt = sum(GHG_kt))#sum over timeslices
  names(Emissions_flows)[names(Emissions_flows) =='Commodity_Name'] = 'Emissions_source'
  Emissions_flows = ungroup(Emissions_flows)
  Emissions_flows = droplevels(Emissions_flows)
  Emissions_flows$Emissions = sapply(Emissions_flows$Emissions,addEmisNames) #add emissions names, some of them are xyzCH4 etc.
  
  #some have processt emissions (catch these on the flow_out) - no need to convert from PJ 
  Emissions_flows_prc = F_OUT[(grepl(myEmisTypes_code,F_OUT$Commodity))&!(grepl('^X',F_OUT$Process)),!(names(F_OUT) %in% mycols)] #get all not X processes that DO have myEmisTypes
  names(Emissions_flows_prc)[names(Emissions_flows_prc) == 'F_OUT'] = 'GHG_kt' #change name of FOUT to kt
  names(Emissions_flows_prc)[names(Emissions_flows_prc) == 'Commodity'] = 'Emissions' # change name of commodity to Emission
  Emissions_flows_prc = addPRCmap(Emissions_flows_prc)
  Emissions_flows_prc = Emissions_flows_prc %>% group_by(Region,Process,Year,Sector,Subsector,Subsubsector,
                                                         Emissions)%>%summarise(GHG_kt = sum(GHG_kt))# sum over timeslices
  Emissions_flows_prc$Emissions_source = 'Process'
  Emissions_flows_prc$Emissions = paste(Emissions_flows_prc$Emissions,'_prc',sep = '')#add a suffix to denote process emissions
  Emissions_flows_prc = ungroup(Emissions_flows_prc)
  Emissions_flows_prc$Emissions= sapply(Emissions_flows_prc$Emissions,addEmisNames)
  Emissions_flows_prc = droplevels(Emissions_flows_prc)
  
  #COmbine the Emissions and process emissions together
  
  All_emissions = rbind(Emissions_flows,Emissions_flows_prc)
  All_emissions$Case = myCase
  
  #subselect sectors:
  com_emis = All_emissions[All_emissions$Sector == 'Commerce',]
  refs_emis = All_emissions[All_emissions$Sector == 'Refineries',]
  pwr_emis = All_emissions[All_emissions$Sector == 'Power',]
  res_emis = All_emissions[All_emissions$Sector == 'Residential',]
  tra_emis= All_emissions[All_emissions$Sector == 'Transport',]
  ind_emis = All_emissions[All_emissions$Sector == 'Industry',]
  sup_emis = All_emissions[All_emissions$Sector == 'Supply',]
  
  
  #Assembling energy balance
  myPrc_exclude = paste(c('^X','ETRANS','ZZ','EPP','EPD','UXL'),collapse = '|') #transmissions
  tmp = append(myEmisTypes,c('WAT','PWRENV'))
  myCom_exclude = paste(tmp,collapse = '|') #remove emissions and water from the selection
  
  #exclude transmissions techs, ETRANS,ZZ, and exclude all emissions and other non fuel commodities
  x = !(grepl(myPrc_exclude,F_INa$Process))&!(F_INa$Commodity_Name %in% fuels)#&(F_INa$Sector != '')
  fin = droplevels(F_INa[x,])
  fin$F_IN = fin$F_IN*-1#convert to negative
  names(fin)[names(fin)=='F_IN'] = 'flow_PJ'
  x =!(grepl(myPrc_exclude,F_OUTa$Process))&!(grepl(myCom_exclude,F_OUTa$Commodity))&!(F_OUTa$Commodity_Name %in% fuels)#&!(F_OUTa$Sector == ' ')
  fout = droplevels(F_OUTa[x,])
  names(fout)[names(fout) == 'F_OUT'] = 'flow_PJ'
  
  EB = merge(fin,fout,all = TRUE) #merge the flows together
  EB[is.na(EB)] = 0
  
  #EB = EB[EB$Sector != '',]
  EB$Case = myCase
  EB = droplevels(EB[EB$Commodity_Name != '',])
  
  pwr_indicators = merge(CAP_T[CAP_T$Process == 'ETRANS',],F_OUTa,all.x = TRUE)
  pwr_indicators = pwr_indicators[,names(pwr_indicators)%in% c('Capacity','Region','Year','F_OUT')]
  names(pwr_indicators)[names(pwr_indicators)== 'Capacity'] = 'Peak_dispatch_GW'
  names(pwr_indicators)[names(pwr_indicators)== 'F_OUT'] = 'Elec_dispatch_PJ'
  pwr_indicators$Peak_dispatch_GW = pwr_indicators$Peak_dispatch_GW*(1/0.96) #adjust for transmision efficiency
  pwr_indicators$Elec_dispatch_TWh = pwr_indicators$Elec_dispatch_PJ/3.6
  pwr_indicators$LoadFactor = (pwr_indicators$Elec_dispatch_TWh*1000)/(pwr_indicators$Peak_dispatch_GW*8760)
  pwr_indicators$Case = myCase
  pwr_indicators = droplevels(merge(pwr_indicators,ERPRICE))
  
  #Combine into list:
  masterlist = list(pwr_indicators,pwr_cap,pwr_ncap,pwr_flows,pwr_costs,tradf,coalPrices,VARACT,inddf,ind_flows,ind_costs,
                    resdf,res_flows,res_cost,comdf,com_costs,com_flows,tra_flows,tra_costs,tra_cap,tra_ncap,
                    refs_flows,refs_costs,refs_cap,refs_ncap,pwr_emis,ind_emis,res_emis,com_emis,tra_emis,sup_emis,refs_emis,All_emissions,EB,comsMargs)
  
  print('....DONE PROCESSING RESULTS!....')
  return(masterlist)
}