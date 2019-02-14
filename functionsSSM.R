#function names
#fXXXX: function (e.g. fPhotosynthesis)
#rXXXX: pRocedure (e.g. rUpdateLAI)
###

### definition of functions (which return calculated variables)


fComputePAR<-function(globalradiation, CoefPAR) {
  return(CoefPAR*globalradiation)

}

fComputeTemp<-function(tasmax,tasmin) {
  return(cTemp = (tasmax+tasmin)/2)

}

fDeltaThermalUnit<-function(pTbasdev,pTopt1dev,cCoefTemp,cWaterStressFactorDevelopment) {
  return((pTopt1dev - pTbasdev)*cCoefTemp*cWaterStressFactorDevelopment)

}

fBiologicalDay<-function(cCoefTemp,cCoefPhotoPeriod,cWaterStressFactorDevelopment,cCoefVernalization){

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

fComputeDailyVernalization<-function(cTemp,pTbasdev,pTopt1dev,pTop2dev,pTlethaldev) {
  return(fFunctionstep(x=cCrownTemp, x1=TbaseVernalization, x2=Topt1Vernalization, x3=Top2Vernalization, x4=TlethalVernalization, y1=0, y2=1, y3=0))
}

fDegreeDays<-function(cTemp, Tbase) return(max(Tbase, cTemp))

fComputeCoefTemp<-function(cTemp, Tbase, Topt1, Topt2, Tlethal) { #icicici : truc debile juste pour tester
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




fComputeSnowMelt<-function(iTASMax, iPr, sSnow) {
  return(ifelse(iTASMax<=1, 0, min(sSnow, iTASMax + iPr * 0.4))) # icicici: unit problem: we expect mm, but we have degrees... this equation has no meaning!

}

fComputeCorrectedPr<-function(iTASMax, iPr, cSnowMelt){
  return(ifelse(iTASMax<=1, 0, iPr + cSnowMelt )) #in cases where it s cold, it snows instead of raining so correctedPr=0, where it s hot, the snow melt is added to the rain
}

fComputeCrownTemperature<-function(sSnow,iTASMax,iTASMin){

  sSnow=ifelse(sSnow > 15,15,sSnow)

  iTASMin=ifelse(iTASMin < 0 & sSnow > 0,2 + iTASMin * (0.4 + 0.0018 * (sSnow - 15) ^ 2),iTASMin)
  iTASMax=ifelse(iTASMax < 0 & sSnow > 0,2 + iTASMax * (0.4 + 0.0018 * (sSnow - 15) ^ 2),iTASMax)
  return((iTASMax + iTASMin) / 2)
}

fPhotoperiodDuration<-function(iDate,latitude){
  doy=as.numeric(strftime(iDate, format = "%j"))
  Pi = 3.141592654
  RDN = Pi / 180
  DEC = sin(23.45 * RDN) * cos(2 * Pi * (doy + 10) / 365)
  DEC = atan(DEC / sqrt(1 - DEC ^ 2)) * -1
  DECL = DEC * 57.29578
  SINLD = sin(RDN * latitude) * sin(DEC)
  COSLD = cos(RDN * latitude) * cos(DEC)
  AOB = SINLD / COSLD
  AOB2 = atan(AOB / sqrt(1 - AOB ^ 2))
  DAYL = 12 * (1 + 2 * AOB2 / Pi)
  return(DAYL + 0.9)
}

fComputeCoefPhotoperiodCrops<-function(photoDuration,CriticalPhotoPerdiod,PhotoPeriodSensitivity){
  ppfun = ifelse(photoDuration < CriticalPhotoPerdiod,1 - PhotoPeriodSensitivity * (cpp - photoDuration) ^ 2,1)
  return(ifelse(ppfun< 0,0,ppfun)
}

fComputeCoefPhotoperiodMaize<-function(){
}

fComputeCoefPhotoperiodLegume<-function(){
}

fComputeCoefVernalization<-function(VernalizationSensitivity,VDSAT,sVernalization){
  CoefVernalization = 1 - VernalizationSensitivity * (VDSAT - sVernalization)
  ifelse(CoefVernalization > 1,1, CoefVernalization)
  ifelse(CoefVernalization < 0,0,CoefVernalization)
  return(CoefVernalization)
}

fComputeCoefWaterstress<-function(){        ####icicici à modifier pour prendre en compte le stress hydrique
  return(1)
}

fComputeMatrixvalidity<-function(){         ####icicici à modifier : fonction qui créer une matrix True et False pour corriger les coef Verna, PP, Temp et stress en fonction des phases
  return(1)
}
#### definition of procedures (which update state variables)

rUpdatePAR<-function(){
  print("updating PAR")

  daybefore<-length(ALLSIMULATEDDATA)-1 #-1 because there is an element for day 0
  PAR<-fComputePAR(globalradiation=ALLDAYDATA$iRSDS, CoefPAR=ALLPARAMETERS$pCoefPAR)
  ALLDAYDATA$cPAR<<-PAR
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

  cSnowMelt<-fComputeSnowMelt(sSnow=ALLSIMULATEDDATA[[daybefore]]$sSnow,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  cPrCorrected<-fComputeCorrectedPr(cSnowMelt=ALLDAYDATA$cSnowMelt,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  sSnow<-ALLSIMULATEDDATA[[daybefore]]$sSnow + ALLDAYDATA$iPr -  ALLDAYDATA$cPrCorrected #if temp<=1, prcorrected = 0 and all rain is snow ; if temp>1, cPrCorrected = iPr-snowmelt so snowmelt=ipr - cPr
  #warning: this equation for sSnow is true only if the temperature threshold for snowmelt is the same as the temperature threshold for snowing
  ALLDAYDATA[,c("cSnowMelt","cPrCorrected","sSnow")]<<-data.frame(cSnowMelt,cPrCorrected,sSnow)
}

#####phenology module
rUdtatePhenology2<-function() {         ####A SUPPRIMER
  daybefore<-length(ALLSIMULATEDDATA)-1 #-1 because there is an element for day 0



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

rUdtatePhenology<-function(){
  print("Daily vernalization coefficient")

  daybefore<-length(ALLSIMULATEDDATA)-1 #-1 because there is an element for day 0
  paramspheno<-ALLCROPS[[ ALLDAYDATA$sCrop ]][[ ALLDAYDATA$sCultivar ]][["phenology"]][[ALLDAYDATA$sGrowthStage]]

###Vernalization
  cCrownTemp <- fComputeCrownTemperature(sSnow=ALLDAYDATA$sSnow,iTASMax=ALLDAYDATA$iTASMax,iTASMin=ALLDAYDATA$iTASMin)
  cDailyVernalization <- fComputeDailyVernalization(CrownTemp=cCrownTemp,TbaseVernalization=ALLCROPS$pTbaseVernalization,
                                                                Topt1Vernalization=ALLCROPS$pTopt1Vernalization,
                                                                Top2Vernalization=ALLCROPS$pTop2Vernalization,
                                                                TlethalVernalization=ALLCROPS$pTlethalVernalization,
                                                                pVDSAT=ALLCROPS$pVDSAT)
  sVernalization <- ALLSIMULATEDDATA[[daybefore]]$sVernalization + cDailyVernalization
  sVernalization <- ifelse(sVernalization < 10 & ALLDAYDATA$iTASMax > 30,sVernalization - 0.5 * (ALLDAYDATA$iTASMax - 30),sVernalization)
  sVernalization <- ifelse(sVernalization < 0,0,sVernalization)
  cCoefVernalization <- fComputeCoefVernalization(VernalizationSensitivity=ALLCROPS$pVernalizationSensitivity,VDSAT=ALLCROPS$pVDSAT,sVernalization=sVernalization)
  cCoefVernalization <- fComputeMatrixvalidity()*cCoefVernalization          #To take account the period with the vernalization have an impact

###Waterstress
  cCoefWaterstress<-fComputeCoefWaterstress()
  cCoefWaterstress<-fComputeMatrixvalidity()*cCoefWaterstress                #To take account the period with the vernalization have an impact

###temperature
  cTemp<-fComputeTemp(tmax=ALLDAYDATA$iTASMax,tmin=ALLDAYDATA$iTASMin)
  cCoefTemp<-fComputeCoefTemp(cTemp=cTemp,Tbase=ALLCROPS$pTbasedev,Topt1=ALLCROPS$pTopt1dev,Topt2=ALLCROPS$pTop2dev,Tlethal=ALLCROPS$pTlethaldev)

###PhotoPeriod
  cPhotoDuration<-fPhotoperiodDuration(iDate=ALLDAYDATA[1,"iDate"],latitude=PARAMSIM$cases$lat)
  cCoefPhotoPeriod<-fComputeCoefPhotoperiodCrops(photoDuration=cPhotoDuration,CriticalPhotoPerdiod=ALLCROPS$pCriticalPhotoPerdiod,PhotoPeriodSensitivity=ALLCROPS$pPhotoPeriodSensitivity)
  cCoefPhotoPeriod<-fComputeMatrixvalidity()*cCoefPhotoPeriod

###Phenology rUpdate
  cDeltaThermalUnit=fDeltaThermalUnit(pTbasdev=ALLCROPS$pTbasedev,pTopt1dev=ALLCROPS$pTopt1dev,cCoefTemp,cCoefWaterstress)
  #icicici Ajouter la condition If FTSW(1) <= 0 Then bd = 0 (avant émergence) (c'est pour blé et légume)
  sThermalUnite<-ALLSIMULATEDDATA[[daybefore]]$sThermalUnite + cDeltaThermalUnit
  cBiologicalDay<-fBiologicalDay(cCoefTemp,cCoefPhotoPeriod,cWaterStressFactorDevelopment,cCoefVernalization)
  #icicici Ajouter la condition If FTSW(1) <= 0 Then bd = 0 (avant émergence)
  sBiologicalDay<-ALLSIMULATEDDATA[[daybefore]]$sBiologicalDay+cBiologicalDay

####stage changes
  changestage<-sBiologicalDay>paramspheno$threshold                                      #icicici Modifier vectthreshold

  ALLDAYDATA[!changestage,"sBiologicalDay"]<<-sBiologicalDay[!changestage]
  #ALLDAYDATA[changestage,"sCumulatedPhenoCounts"]<<-newcounts[changestage]-vectthreshold[changestage] #if we changed stages, we start not from 0 but from the "extra units accuulated during the timestep
  ALLDAYDATA[changestage,"sBiologicalDay"]<<- 0 #if we changed stages, we start from 0
  ALLDAYDATA[changestage,"sGrowthStage"]<<-fFindNextStage(crop=ALLDAYDATA[changestage,"sCrop"], currentstage=ALLDAYDATA[changestage,"sGrowthStage"])

####Update ALLDAYDATA
  ALLDAYDATA[,c("cCrownTemp","cDailyVernalization","sVernalization","CoefVernalization","cCoefWaterstress","cTemp","cCoefTemp","cPhotoDuration","cCoefPhotoPeriod","cDeltaThermalUnit","sThermalUnite","cBiologicalDay")]<<-data.frame(cCrownTemp,cDailyVernalization,sVernalization,CoefVernalization,cCoefWaterstress,cTemp,cCoefTemp,cPhotoDuration,cCoefPhotoPeriod,cDeltaThermalUnit,sThermalUnite,cBiologicalDay,sBiologicalDay)

}
