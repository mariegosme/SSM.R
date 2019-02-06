#function names
#fXXXX: function (e.g. fPhotosynthesis)
#rXXXX: pRocedure (e.g. rUpdateLAI)
###

### definition of functions (which return calculated variables)


fComputePAR<-function(globalradiation, CoefPAR=ALLPARAMETERS$pCoefPAR) {
  return(CoefPAR*globalradiation)
  
}

fComputeTemp<-function(tasmax=ALLDAYDATA$iTASMax,tasmin=ALLDAYDATA$iTASMin) {
  return(cTemp = (tasmax+tasmin)/2)

}

fDelta_thermal_unit<-function(pTbasdev,pTopt1dev,cCoefTemp=1,cWaterStressFactorDevelopment=1) {
  return(cDeltaThermalUnit = (pTopt1dev - pTbasdev)*cCoefTemp*cWaterStressFactorDevelopment)

}

fBiologicalDay<-function(cCoefTemp=1,cCoefPhotoPeriod=1,cWaterStressFactorDevelopment=1,cCoefVernalization=1){
  
  return(cBiologicalDay=cCoefTemp*cCoefPhotoPeriod*cWaterStressFactorDevelopment*cCoefVernalization)
}

fFunctionstep<-function(x, x1,x2, x3, x4, y1, y2, y3) {
#fonction en _/~\_ 
#attention: marche SOIT avec x=vecteur et les params=1 chiffre chacun 
#                  SOIT avec x=1 chiffre et les params=vecteurs (ou un chiffre)
#                  SOIT avec x et les éventuels params multiples de même longueur
  toto<-y1 + (x-x1)*(y1-y2)/(x1-x2)
  toto[x<x1]<-y1
  toto[x>x2 & x<=x3]<-y2
  toto[x>x3 & x<x4]<-y2 + (x-x3)*(y3-y2)/(x4-x3)
  toto[x>=x4]<-y3
  #autrepossibilite avec ifelse
  #toto<-ifelse(x<=x1, y1, 
  #  ifelse(x>x1 & x<=x2), y1 + (x-x1)*(y1-y2)/(x1-x2), 
  #    ifelse(x>x2 & x<x3, y2, 
  #      ifelse(x>x3 & x>=x4, y2 + (x-x3)*(y3-y2)/(x4-x3),
  #        y3)))
    return(toto)
}

fComputeCoefTemp<-function(cTemp,pTbasdev,pTopt1dev,pTop2dev,pTlethaldev) {
  return(fFunctionstep(x=cTemp, x1=pTbasdev, x2=pTopt1dev, x3=pTop2dev, x4=pTlethaldev, y1=0, y2=1, y3=0))
}
fComputeDailyVernalization<-function(cTemp,pTbasdev,pTopt1dev,pTop2dev,pTlethaldev) {
  return(fFunctionstep(x=cCrownTemp, x1=TbaseVernalization, x2=Topt1Vernalization, x3=Top2Vernalization, x4=TlethalVernalization, y1=0, y2=1, y3=0))
}

fDegreeDays<-function(cTemp, Tbase) return(max(Tbase, cTemp))

fThermoCardinal<-function(cTemp, Tbase, Topt1, Topt2, Tlethal) { #icicici : truc debile juste pour tester
 return(fFunctionstep(x=cTemp, x1=Tbase, x2=Topt1, x3=Topt2, x4=Tlethal, y1=0, y2=1, y3=0))
}

fFindNextStage<-function(crop, currentstage) { #we assume all cultivars of a crop have the same stages
  df<-data.frame(crop, currentstage)
  df$nextstage<-NA
  for (i in 1:nrow(df)) {
    cropstages<-names(ALLCROPS[[df$crop[i]]][[1]][["phenology"]])
    df$nextstage[i]<-cropstages[which(cropstages==df$currentstage[i])[1]+1] 
  }
  return(df$nextstage)
}

rUdtatePhenology<-function() {
  vectfonct<-character(0)
  vectincrement<-numeric(0)
  vectthreshold<-numeric(0)
  for (i in 1:nrow(ALLDAYDATA)) {#we have to do it line by line because we pick the crop parameters at different levels in the list
    #find phenological function to apply and its parameters
    paramspheno<-ALLCROPS[[ ALLDAYDATA$sCrop[i] ]][[ ALLDAYDATA$sCultivar[i] ]][["phenology"]][[ALLDAYDATA$sGrowthStage[i]]]
    functiontoapply<-paramspheno$phenofunction
    paramsfunction<-paramspheno$paramsFunctPheno[intersect(names(paramspheno$paramsFunctPheno),names(formals(functiontoapply)))]
    #other arguments from the data at the current time step
    otherarguments<-as.list(ALLDAYDATA[i,intersect(names(ALLDAYDATA),names(formals(functiontoapply))), drop=FALSE])
    #apply the function
    increment<-do.call(functiontoapply, args=c(paramsfunction, otherarguments))
    #format the vectors of extracted information
    vectfonct<-c(vectfonct, functiontoapply)
    vectincrement<-c(vectincrement, increment)
    vectthreshold<-c(vectthreshold, paramspheno$threshold)
  }
  #increment the count, see if stage changes, and update stage, and counter
  newcounts<-ALLDAYDATA$sCumulatedPhenoCounts+vectincrement
  changestage<-newcounts>vectthreshold
  ALLDAYDATA[!changestage,"sCumulatedPhenoCounts"]<<-newcounts[!changestage]
  #ALLDAYDATA[changestage,"sCumulatedPhenoCounts"]<<-newcounts[changestage]-vectthreshold[changestage] #if we changed stages, we start not from 0 but from the "extra units accuulated during the timestep
  ALLDAYDATA[changestage,"sCumulatedPhenoCounts"]<<- 0 #if we changed stages, we start from 0 
  ALLDAYDATA[changestage,"sGrowthStage"]<<-fFindNextStage(crop=ALLDAYDATA[changestage,"sCrop"], currentstage=ALLDAYDATA[changestage,"sGrowthStage"])
}


fComputeSnowMelt<-function(iTASMax, iPr, sSnow) {
  return(ifelse(iTASMax<=1, 0, min(sSnow, iTASMax + iPr * 0.4))) # icicici: unit problem: we expect mm, but we have degrees... this equation has no meaning!
  
}

fComputeCorrectedPr<-function(iTASMax, iPr, cSnowMelt){
  return(ifelse(iTASMax<=1, 0, iPr + cSnowMelt )) #in cases where it s cold, it snows instead of raining so correctedPr=0, where it s hot, the snow melt is added to the rain
}

fComputeCrownTemperature<-function(sSnow=ALLDAYDATA$sSnow,iTASMax=ALLDAYDATA$iTASMax,iTASMin=ALLDAYDATA$iTASMin){

  if(sSnow > 15){
    sSnow = 15
  } 
  if(iTASMin < 0 & sSnow > 0){
    iTASMin = 2 + iTASMin * (0.4 + 0.0018 * (sSnow - 15) ^ 2)  
  }
  if(iTASMax < 0 & sSnow > 0){
    iTASMax = 2 + iTASMax * (0.4 + 0.0018 * (sSnow - 15) ^ 2)  
  }
  return((iTASMax + iTASMin) / 2)
}




fComputeCoefVernalization<-function(VernalizationSensitivity=ALLCROPS$pVernalizationSensitivity,VDSAT=ALLPARAMETERS$pVDSAT,sVernalization=ALLDAYDATA$sVernalization){
  CoefVernalization = 1 - VernalizationSensitivity * (VDSAT - sVernalization)
  if(CoefVernalization > 1 ) CoefVernalization = 1
  if(CoefVernalization < 0 ) CoefVernalization = 0
  retunr(CoefVernalization)
}

#### definition of procedures (which update state variables)

rUpdatePAR<-function(){
  print("updating PAR")
  
  daybefore<-length(ALLSIMULATEDDATA)-1 #-1 because there is an element for day 0
  PAR<-fComputePAR(globalradiation=ALLDAYDATA$iRSDS, CoefPAR=ALLPARAMETERS$pCoefPAR)
  ALLDAYDATA$cPAR<<-PAR
}

rUpdateThermalUnite<-function(){
  print("updating ThermalUnite")
  
  daybefore<-length(ALLSIMULATEDDATA)-1 #-1 because there is an element for day 0
  sThermalUnite<-ALLSIMULATEDDATA[[daybefore]]$sThermalUnite + fDelta_thermal_unit(pTbasdev,pTopt1dev,Tempovar$cCoefTemp)
  ALLDAYDATA$sThermalUnit<<-sThermalUnite
}

#####Weather module
rWeatherDay<-function(){
  print("Updating weather intput") 
  
  daybefore<-length(ALLSIMULATEDDATA)-1 #-1 because there is an element for day 0
  date<-ALLDAYDATA[1,"iDate"]
  dfclimate<-fGetClimateDay(date=date)
  climatevariables<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="weather" & VARIABLEDEFINITIONS$typeinthemodel=="input","name"]
  ALLDAYDATA[,climatevariables]<<-dfclimate[PARAMSIM$cases$climatename, climatevariables]
  ###To be full in
  #Calculate sSnow evolution, cSnowMelt, CorrectedPr
  
  ALLDAYDATA$cSnowMelt<<-fComputeSnowMelt(sSnow=ALLSIMULATEDDATA[[daybefore]]$sSnow,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr) 
  ALLDAYDATA$cPrCorrected<<-fComputeCorrectedPr(cSnowMelt=ALLDAYDATA$cSnowMelt,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  ALLDAYDATA$sSnow<<-ALLSIMULATEDDATA[[daybefore]]$sSnow + ALLDAYDATA$iPr -  ALLDAYDATA$cPrCorrected #if temp<=1, prcorrected = 0 and all rain is snow ; if temp>1, cPrCorrected = iPr-snowmelt so snowmelt=ipr - cPr
  #warning: this equation for sSnow is true only if the temperature threshold for snowmelt is the same as the temperature threshold for snowing
}


#####Vernalization module
rVernalizationDay<-function(){
  print("Daily vernalization coefficient")
  
  daybefore<-length(ALLSIMULATEDDATA)-1 #-1 because there is an element for day 0
  
  
  ALLDAYDATA$cCrownTemp <<- fComputeCrownTemperature(sSnow=ALLDAYDATA$sSnow,iTASMax=ALLDAYDATA$iTASMax,iTASMin=ALLDAYDATA$iTASMin)
  ALLDAYDATA$cDailyVernalization <<- fComputeDailyVernalization(CrownTemp=ALLDAYDATA$cCrownTemp,
                                                                TbaseVernalization=ALLPARAMETERS$pTbaseVernalization,
                                                                Topt1Vernalization=ALLPARAMETERS$pTopt1Vernalization,
                                                                Top2Vernalization=ALLPARAMETERS$pTop2Vernalization,
                                                                TlethalVernalization=ALLPARAMETERS$pTlethalVernalization,
                                                                pVDSAT=ALLPARAMETERS$pVDSAT)
  ALLDAYDATA$sVernalization <<- ALLSIMULATEDDATA[[daybefore]]$sVernalization + ALLDAYDATA$cDailyVernalization
  If (ALLDAYDATA$sVernalization < 10 & ALLDAYDATA$iTASMax > 30){
    ALLDAYDATA$sVernalization = ALLDAYDATA$sVernalization - 0.5 * (ALLDAYDATA$iTASMax - 30)
  }  
  If (ALLDAYDATA$sVernalization < 0){
    ALLDAYDATA$sVernalization = 0
  }
  cCoefVernalization = 1
  if(CBD >= bdBRV & CBD <= bdTRV){   ###A MODIFIER
    cCoefVernalization = fComputeCoefVernalization(VernalizationSensitivity=ALLCROPS$pVernalizationSensitivity,VDSAT=ALLPARAMETERS$pVDSAT,sVernalization=ALLDAYDATA$sVernalization)
  }
  ALLDAYDATA$cCoefVernalization = cCoefVernalization
}
