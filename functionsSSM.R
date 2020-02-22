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

fComputeDailyVernalization<-function(cCrownTemp,TbaseVernalization,Topt1Vernalization,Top2Vernalization,TlethalVernalization) {
  return(fFunctionstep(x=cCrownTemp, x1=TbaseVernalization, x2=Topt1Vernalization, x3=Top2Vernalization, x4=TlethalVernalization, y1=0, y2=1, y3=0))
}

fDegreeDays<-function(cTemp, Tbase) return(max(Tbase, cTemp))

fComputeCoefTemp<-function(cTemp, Tbase, Topt1, Topt2, Tlethal) { #icicici : truc debile juste pour tester
 return(fFunctionstep(x=cTemp, x1=Tbase, x2=Topt1, x3=Topt2, x4=Tlethal, y1=0, y2=1, y3=0))
}

fComputeSnowMelt<-function(iTASMax, iPr, sSnow) {
  return(ifelse(iTASMax<=1, 0, pmin(sSnow, iTASMax + iPr * 0.4))) # icicici: unit problem: we expect mm, but we have degrees... this equation has no meaning!

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
  ppfun <- ifelse(photoDuration < CriticalPhotoPerdiod, pmax(0, 1 - PhotoPeriodSensitivity * (CriticalPhotoPerdiod - photoDuration) ^ 2),1)
  return(ppfun)
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

fComputeCoefWaterstressDevelopment<-function(watercontent){        ####icicici à modifier pour prendre en compte le stress hydrique
  return(rep(1, length(watercontent)))
}

fComputeDecreaseLAIwithN<-function(DailyRateNfromLeave,SpecLeafNGreenLeaf,SpecLeafNSenescenceLeaf){
    return(DailyRateNfromLeave / (SpecLeafNGreenLeaf - SpecLeafNSenescenceLeaf))
}

fComputeDecreaseLAIwithoutN<-function(){

}
fFrostEffect<-function(LAI,tasmin,FreezeThresholdTemp,FreezeFracLeafDestruction){
  frstf = abs(tasmin - FreezeThresholdTemp) * FreezeFracLeafDestruction
  frstf= max(min(frstf,1),0)
  return(LAI * frstf)
}

fHeatEffect<-function(cDecreaseLAI,tasmax,HeatThresholdTemp,HeatFracLeafDestruction){
  heatf = 1 + (tasmax - HeatThresholdTemp) * HeatFracLeafDestruction              #Semenov-Sirius
  heatf = min(1,heatf)
  return(cDecreaseLAI * heatf)
}


#####Weather module
rWeatherDay<-function(){
  print("Updating weather intput")

  daybefore<-length(ALLSIMULATEDDATA)
  Dateoftheday<-ALLDAYDATA[1,"iDate"]
  dfclimate<-fGetClimateDay(date=Dateoftheday)
  climatevariables<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="weather" & VARIABLEDEFINITIONS$typeinthemodel=="input","name"]
  ALLDAYDATA[,climatevariables]<<-dfclimate[PARAMSIM$cases$climatename, climatevariables]
  ###To be full in
  #Calculate sSnow evolution, cSnowMelt, CorrectedPr

  cSnowMelt<-fComputeSnowMelt(sSnow=ALLSIMULATEDDATA[[daybefore]]$sSnow,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  cPrCorrected<-fComputeCorrectedPr(cSnowMelt=cSnowMelt,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  sSnow<-ALLSIMULATEDDATA[[daybefore]]$sSnow + ALLDAYDATA$iPr -  cPrCorrected #if temp<=1, prcorrected = 0 and all rain is snow ; if temp>1, cPrCorrected = iPr-snowmelt so snowmelt=ipr - cPr
  #warning: this equation for sSnow is true only if the temperature threshold for snowmelt is the same as the temperature threshold for snowing
  ALLDAYDATA[,c("cSnowMelt","cPrCorrected","sSnow")]<<-data.frame(cSnowMelt,cPrCorrected,sSnow)
  return()
}


#### Management module (for now, just keeps the crop, cultivar and crop parameters at their previous values)
rUpdateManagement<-function(){
  print("Updating crops according to crop management")
  daybefore<-length(ALLSIMULATEDDATA)
  #whosows
  #whoharvests
  #whofertilizes
  #whoirrigates
  rSetParamsFromCrops() #in HousekeepingFunctions
  
  return()
}


#####phenology module

rUpdatePhenology<-function(){
  print("Updating phenology")

  daybefore<-length(ALLSIMULATEDDATA)
  #paramspheno<-ALLCROPS[[ ALLDAYDATA$sCrop ]][[ ALLDAYDATA$sCultivar ]][["phenology"]][[ALLDAYDATA$sGrowthStage]]

###Vernalization
  cCrownTemp <- fComputeCrownTemperature(sSnow=ALLDAYDATA$sSnow,iTASMax=ALLDAYDATA$iTASMax,iTASMin=ALLDAYDATA$iTASMin)
  cDailyVernalization <- fComputeDailyVernalization(cCrownTemp=cCrownTemp,TbaseVernalization=ALLDAYDATA$pTbaseVernalization,
                                                                Topt1Vernalization=ALLDAYDATA$pTopt1Vernalization,
                                                                Top2Vernalization=ALLDAYDATA$pTopt2Vernalization,
                                                                TlethalVernalization=ALLDAYDATA$pTlethalVernalization
                                                              )
  sVernalization <- ALLSIMULATEDDATA[[daybefore]]$sVernalization + cDailyVernalization
  sVernalization[sVernalization < 10 & ALLDAYDATA$iTASMax > 30] <- sVernalization - 0.5 * (ALLDAYDATA$iTASMax - 30)
  sVernalization[sVernalization < 0] <- 0
  cCoefVernalization <- rep(1, nrow(ALLDAYDATA))
  resultfilter<-applyfilters("vernalisation") #on cree le filtre des TRUE FALSE de l application de la vernalisation
  cCoefVernalization[resultfilter] <- fComputeCoefVernalization(VernalizationSensitivity=ALLDAYDATA$pVernalizationSensitivity,VDSAT=ALLDAYDATA$pVDSAT,sVernalization=sVernalization)[resultfilter]

###Waterstress
  cCoefWaterstressDevelopment <- rep(1, nrow(ALLDAYDATA))
  resultfilter<-applyfilters("waterstress") #on cree le filtre des TRUE FALSE de l application de la waterstress
  #icicicic : fComputeCoefWaterstressDevelopment is not coded, watercontent should not be only at the surface.... to be changed when water module is coded
  cCoefWaterstressDevelopment[resultfilter]<-fComputeCoefWaterstressDevelopment(watercontent=ALLDAYDATA$sWater.1)[resultfilter] #To take account the period when waterstress has an impact

###temperature
  cTemp<-fComputeTemp(tasmax=ALLDAYDATA$iTASMax,tasmin=ALLDAYDATA$iTASMin)
  cCoefTemp<-fComputeCoefTemp(cTemp=cTemp,Tbase=ALLDAYDATA$pTbasedev,Topt1=ALLDAYDATA$pTopt1dev,Topt2=ALLDAYDATA$pTopt2dev,Tlethal=ALLDAYDATA$pTlethaldev)

###PhotoPeriod
  cPhotoDuration<-fPhotoperiodDuration(iDate=ALLDAYDATA[1,"iDate"],latitude=PARAMSIM$cases$lat)
  cCoefPhotoPeriod <- rep(1, nrow(ALLDAYDATA))
  resultfilter<-applyfilters("photoperiod") #on cree le filtre des TRUE FALSE de l application de la photoperiode
  #icicici voir comment gérer plusieurs fonctions de photopériodisme en fonction des espèces
  cCoefPhotoPeriod[resultfilter]<-fComputeCoefPhotoperiodCrops(photoDuration=cPhotoDuration,CriticalPhotoPerdiod=ALLDAYDATA$pCriticalPhotoPerdiod,PhotoPeriodSensitivity=ALLDAYDATA$pPhotoPeriodSensitivity)[resultfilter]  

###Phenology rUpdate
  cDeltaThermalUnit<-fDeltaThermalUnit(pTbasdev=ALLDAYDATA$pTbasedev,pTopt1dev=ALLDAYDATA$pTopt1dev,cCoefTemp,cCoefWaterstressDevelopment)
  #icicici Ajouter la condition If FTSW(1) <= 0 Then bd = 0 (avant émergence) (c'est pour blé et légume)
  sThermalUnite<-ALLSIMULATEDDATA[[daybefore]]$sThermalUnite + cDeltaThermalUnit
  cBiologicalDay<-fBiologicalDay(cCoefTemp,cCoefPhotoPeriod,cCoefWaterstressDevelopment,cCoefVernalization)
  #icicici Ajouter la condition If FTSW(1) <= 0 Then bd = 0 (avant émergence)
  sBiologicalDay<-ALLSIMULATEDDATA[[daybefore]]$sBiologicalDay+cBiologicalDay

####stage changes
  cultivars<-paste(ALLSIMULATEDDATA[[daybefore]]$sCrop,ALLSIMULATEDDATA[[daybefore]]$sCultivar, sep=".")
  thresholds<-mapply(function(cropname, stage) return(ALLCROPS[[cropname]]$thresholds[[stage]]), cultivars, as.character(ALLSIMULATEDDATA[[daybefore]]$sGrowthStage), SIMPLIFY = TRUE, USE.NAMES=FALSE) #on choope les
  changestage<-sBiologicalDay>thresholds
  sGrowthStage<-ALLSIMULATEDDATA[[daybefore]]$sGrowthStage
  sGrowthStageNumber<-ALLSIMULATEDDATA[[daybefore]]$sGrowthStageNumber
  sGrowthStageNumber[changestage]<-sGrowthStageNumber[changestage]+1
  #find the corresponding stage name
  stagenames<-unlist(unname(lapply(ALLCROPS, function(x) {toto<-names(x$thresholds) ; names(toto)<-paste(x$name, 1:length(x$thresholds)) ; return(toto)})))
  sGrowthStage[changestage]<-stagenames[paste(paste(ALLSIMULATEDDATA[[daybefore]]$sCrop[changestage], ALLSIMULATEDDATA[[daybefore]]$sCultivar[changestage], sep="."),
                                              sGrowthStageNumber[changestage])]
  # when the stage changed, we start the counter with the remaining of increment-threshold
  sBiologicalDay[changestage]<-sBiologicalDay[changestage]-thresholds[changestage] #if we changed stages, we start not from 0 but from the "extra units accuulated during the timestep

####Update ALLDAYDATA
  ALLDAYDATA[,c("cCrownTemp","cDailyVernalization","sVernalization","cCoefVernalization","cCoefWaterstressDevelopment",
                "cTemp","cCoefTemp","cPhotoDuration","cCoefPhotoPeriod","cDeltaThermalUnit","sThermalUnite",
                "cBiologicalDay","sBiologicalDay","sGrowthStage","sGrowthStageNumber")]<<-data.frame(
                  cCrownTemp,cDailyVernalization,sVernalization,cCoefVernalization,cCoefWaterstressDevelopment,
                  cTemp,cCoefTemp,cPhotoDuration,cCoefPhotoPeriod,cDeltaThermalUnit,sThermalUnite,
                  cBiologicalDay,sBiologicalDay,sGrowthStage,sGrowthStageNumber)

  return()
}

#####LAI module
rUpdateLAI<-function(){
  print("Updating LAI")
  daybefore<-length(ALLSIMULATEDDATA)

  ###LAI Growing (similar with and without N contribution)
  #LAIMainstem
  a <- ALLDAYDATA$sThermalUnite / ALLDAYDATA$pPhyllochron 
  sMainstemNodeNumber <- ALLSIMULATEDDATA[[daybefore]]$sMainstemNodeNumber  + a
  cPlantLeafArea <- ALLDAYDATA$pcoefPlantLeafNumberNode * sMainstemNodeNumber ^ ALLDAYDATA$pExpPlantLeafNumberNode
  #icicicic in the following line of code, we dont follow our rules about computed/state variables: we use computed values (cPlantLeafArea and cCoefWaterstressLeaf) from last time step : normally only state variables are carried on from one day to the next... check if it's possible to do differently
  #so it poses a problem at initialization because computed values are initialized as NA
  LAIMainstem <- ((
    (cPlantLeafArea - ALLSIMULATEDDATA[[daybefore]]$cPlantLeafArea) * ALLDAYDATA$sPlantdensity / 10000) 
    * ALLSIMULATEDDATA[[daybefore]]$cCoefWaterstressLeaf
  )
  #LAISecondary
  LAISecondary <- ALLSIMULATEDDATA[[daybefore]]$cDailyLeafWeight * ALLDAYDATA$pSpecificLeafArea

  #LAI Total Growing
  cGrowthLAI <- rep(0, nrow(ALLDAYDATA))
  alaimainstem<-applyfilters("LAI_Mainstem")
  alaisecondary<-applyfilters("LAI_Secondary")
  cGrowthLAI<-cGrowthLAI+alaimainstem * LAIMainstem + alaisecondary * LAISecondary

  ###LAI Decrease
  if(PARAMSIM$Neffect==T){
    cDecreaseLAI<-fComputeDecreaseLAIwithN(DailyRateNfromLeave=ALLSIMULATEDDATA[[daybefore]]$cDailyRateNfromLeave,
                                           SpecLeafNGreenLeaf=ALLDAYDATA$pSpecLeafNGreenLeaf,
                                           SpecLeafNSenescenceLeaf=ALLDAYDATA$pSpecLeafNSenescenceLeaf)
  }else{
    cDecreaseLAI<-fComputeDecreaseLAIwithoutN()

  #icicici écrire la procédure et la fonction de décroissance sans azote
  #  If CBD < bdBLS Then
  #     DLAI = 0
  #     BLSLAI = LAI             'Saving LAI at BLS
  #  ElseIf CBD >= bdBLS Then
  #     DLAI = bd / (bdMAT - bdBLS) * BLSLAI
  #  End If

  }

  frost<-fFrostEffect(LAI=ALLSIMULATEDDATA[[daybefore]]$sLAI,tasmin=ALLDAYDATA$iTASMin,
                      FreezeThresholdTemp=ALLDAYDATA$pFreezeThresholdTemp,
                      FreezeFracLeafDestruction=ALLDAYDATA$pFreezeFracLeafDestruction)
  heat<-fHeatEffect(cDecreaseLAI,tasmax=ALLDAYDATA$iTASMax,
                    HeatThresholdTemp=ALLDAYDATA$pHeatThresholdTemp,
                    HeatFracLeafDestruction=ALLDAYDATA$pHeatFracLeafDestruction)
  cDecreaseLAI<-max(frost,heat)   ####heat LAI corresponds to cDecreaseLAI if no heat effect and cDreaseLAI corresponds to heat if hot effet. Take effect of frozen if it is more important that cDea

  sLAI <- ALLSIMULATEDDATA[[daybefore]]$sLAI+cGrowthLAI-cDecreaseLAI         #Update LAI (sLAI) by the end of the module (in SSM excel is in the beginning)

  ####Mortality test with low LAI CONDITION
  alaicond<-applyfilters("DMDistribution_SeedGrowing")
  cEndCropCycle<-ifelse((sLAI< 0.05 & alaicond==TRUE),"pre-mature due to low LAI",NA)
  sLAI<-ifelse(sLAI<0,0,sLAI)

  ALLDAYDATA[,c("sMainstemNodeNumber","cPlantLeafArea","cGrowthLAI","cDecreaseLAI","sLAI","cEndCropCycle")]<<-data.frame(sMainstemNodeNumber,cPlantLeafArea,cGrowthLAI,cDecreaseLAI,sLAI,cEndCropCycle)
                                                  #Delete negative value and to limit to 0
}

rUpdateDMProduction<-function(){
  print("Updating DMProduction")
  cCoefRadiationEfficiency <- rep(0, nrow(ALLDAYDATA))                             #Radiation effiency is null when the plant doens't produce leaf
  resultfilter<-applyfilters("DMProduction") #on cree le filtre des TRUE FALSE de l application de la vernalisation
  cCoefRadiationEfficiency[resultfilter]=fComputeCoefTemp(cTemp=ALLDAYDATA$cTemp,Tbase=ALLDAYDATA$pTbasRUE,
                                                        Topt1=ALLDAYDATA$DMProduction$pTopt1RUE,
                                                        Topt2=ALLDAYDATA$pTopt2RUE,
                                                        Tlethal=ALLDAYDATA$plethalRUE)[resultfilter]
  cRadiationUseEffiency = ALLDAYDATA$pRadEffiencyOptimal * cCoefRadiationEfficiency * ALLDAYDATA$cCoefWaterstressDryMatter
  cPAR<-fComputePAR(globalradiation=ALLDAYDATA$iRSDS, CoefPAR=GENERALPARAMETERS["pCoefPAR", "defaultInitialvalue"])
  aFINT= 1 - exp(-ALLDAYDATA$pKPAR * ALLDAYDATA$sLAI)
  cDryMatterProduction = ALLDAYDATA$cPAR * aFINT * cRadiationUseEffiency

  ALLDAYDATA[,c("cCoefRadiationEfficiency","cRadiationUseEffiency","cPAR","cDryMatterProduction")]<<-data.frame(cCoefRadiationEfficiency,cRadiationUseEffiency,cPAR,cDryMatterProduction)

}
