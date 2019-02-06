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

fComputeCoefTemp<-function(cTemp,pTbasdev,pTopt1dev,pTop2dev,pTlethaldev) {
  if(cTemp <= pTbasdev |cTemp >= pTlethaldev){
    cCoefTemp = 0 
  }else if(cTemp > pTbasdev & cTemp < pTopt1dev){
    cCoefTemp = (cTemp - pTbasdev) / (pTopt1dev - pTbasdev)
  }else if(cTemp > pTopt2dev & cTemp < pTlethaldev){
    cCoefTemp = ((pTlethaldev - cTemp) / (pTlethaldev - pTopt2dev))
  }else{
    cCoefTemp = 1
  }

  return(cCoefTemp)
}



fComputeSnowMelt<-function(sSnow=ALLSIMULATEDDATA[[step]]$sSnow,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr) {
  cSnowMelt=0
  if(sSnow>0 & iTASMax<=1){
    cSnowMelt=iTASMax + iPr * 0.4
    if(SnowMelt>sSnow){
      cSnowMelt = sSnow
    }
  } 
  return(cSnowMelt)
  
}

fComputeCorrectedPr<-function(cSnowMelt=ALLDAYDATA$cSnowMelt,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr){
  if(iTASMax<=1){
    cPrCorrected=0            #all precipitation is transform in snow  
  }else{
    cPrCorrected=iPr + cSnowMelt 
  }
  return(cPrCorrected)
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


fComputeDailyVernalization<-function(CrownTemp,TbaseVernalization,Topt1Vernalization,Top2Vernalization,TlethalVernalization,pVDSAT){
  if(CrownTemp <= TbaseVernalization | CrownTemp > TlethalVernalization){
    cDailyVernalization=0
  }else if(CrownTemp > TbaseVernalization & CrownTemp < Topt1Vernalization){
    cDailyVernalization = (CrownTemp - TbaseVernalization) / (Topt1Vernalization - TbaseVernalization)  
  }else if(CrownTemp > Topt2Vernalization & CrownTemp < TlethalVernalization){
    cDailyVernalization = (TlethalVernalization - CrownTemp) / (TlethalVernalization - Topt2Vernalization) 
  }else{
    cDailyVernalization = 1
  }
  retunr(cDailyVernalization)
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
  
  step<-length(ALLSIMULATEDDATA)   
  PAR<-fComputePAR(globalradiation=ALLDAYDATA$iRSDS, CoefPAR=ALLPARAMETERS$pCoefPAR)
  ALLDAYDATA$cPAR<<-PAR
}

rUpdateThermalUnite<-function(){
  print("updating ThermalUnite")
  
  step<-length(ALLSIMULATEDDATA)  
  sThermalUnite<-ALLSIMULATEDDATA[[step]]$sThermalUnite + fDelta_thermal_unit(pTbasdev,pTopt1dev,Tempovar$cCoefTemp)
  ALLDAYDATA$sThermalUnit<<-sThermalUnite
}

#####Weather module
rWeatherDay<-function(){
  print("Weather Day intput") 
  
  step<-length(ALLSIMULATEDDATA)  
  #Read and write iPr, iTASmin, iTASmax, iRSDS
  ###To be full in
  #Calculate sSnow evolution, cSnowMelt, CorrectedPr
  
  ALLDAYDATA$cSnowMelt<<-fComputeSnowMelt(sSnow=ALLSIMULATEDDATA[[step]]$sSnow,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  ALLDAYDATA$cPrCorrected<<-fComputeCorrectedPr(cSnowMelt=ALLDAYDATA$cSnowMelt,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  ALLDAYDATA$sSnow<<-ALLSIMULATEDDATA[[step]]$sSnow + ALLDAYDATA$iPr -  ALLDAYDATA$cPrCorrected
    
}

#####Vernalization module
rVernalizationDay<-function(){
  print("Daily vernalization coefficient")
  
  step<-length(ALLSIMULATEDDATA)  
  
  ALLDAYDATA$cCrownTemp <<- fComputeCrownTemperature(sSnow=ALLDAYDATA$sSnow,iTASMax=ALLDAYDATA$iTASMax,iTASMin=ALLDAYDATA$iTASMin)
  ALLDAYDATA$cDailyVernalization <<- fComputeDailyVernalization(CrownTemp=ALLDAYDATA$cCrownTemp,
                                                                TbaseVernalization=ALLPARAMETERS$pTbaseVernalization,
                                                                Topt1Vernalization=ALLPARAMETERS$pTopt1Vernalization,
                                                                Top2Vernalization=ALLPARAMETERS$pTop2Vernalization,
                                                                TlethalVernalization=ALLPARAMETERS$pTlethalVernalization,
                                                                pVDSAT=ALLPARAMETERS$pVDSAT)
  ALLDAYDATA$sVernalization <<- ALLSIMULATEDDATA[[step]]$sVernalization + ALLDAYDATA$cDailyVernalization
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
