#function names
#fXXXX: function (e.g. fPhotosynthesis)
#rXXXX: pRocedure (e.g. rUpdateLAI)
###

### definition of functions (which return calculated variables)


fComputePAR<-function(globalradiation, CoefPAR=ALLPARAMETERS$pCoefPAR) {
  return(CoefPAR*globalradiation)
  
}

fComputeTemp<-function(tasmax,tasmin) {
  return(cTemp = (tasmax+tasmin)/2)

}

fDelta_thermal_unit<-function(pTbasdev,pTopt1dev,cCoefTemp=1,cWaterStressFactorDevelopment=1) {
  return(cDeltaThermalUnit = (pTopt1dev - pTbasdev)*cCoefTemp)

}

fBiologicalDay<-function(cCoefTemp=1,cCoefPhotoPeriod=1,cWaterStressFactorDevelopment=1){
  
  return(cBiologicalDay=cCoefTemp*cCoefPhotoPeriod*cWaterStressFactorDevelopment*cCoefVernalization)
}

fComptuteCoefTemp=function(cTemp,pTbasdev,pTopt1dev,pTop2dev,pTlethaldev) {
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

#### definition of procedures (which update state variables)

rUpdatePAR<-function(){
  print("updating PAR")
  
  step<-length(ALLSIMULATEDDATA)   
  PAR<-fComputePAR(globalradiation=ALLSIMULATEDDATA[[step]]$iRSDS, CoefPAR=ALLPARAMETERS$pCoefPAR)
  ALLSIMULATEDDATA[[step]]$cPAR<<-PAR
}

rUpdateThermalUnite<-function(){
  print("updating ThermalUnite")
  
  step<-length(ALLSIMULATEDDATA)  
  ThermalUnite<-ALLSIMULATEDDATA[[step-1]]$sThermalUnite + fDelta_thermal_unit(pTbasdev,pTopt1dev,Tempovar$cCoefTemp)
  ALLSIMULATEDDATA[[step]]$sThermalUnit<<-ThermalUnite
}



