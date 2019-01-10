#function names
#fXXXX: function (e.g. fPhotosynthesis)
#rXXXX: pRocedure (e.g. rUpdateLAI)


### definition of functions (which return calculated variables)


fComputePAR<-function(globalradiation, CoefPAR=ALLPARAMETERS$pCoefPAR) {
  return(CoefPAR*globalradiation)
  
}

#### definition of procedures (which update state variables)

rUpdatePAR<-function(){
  print("updating PAR")
  
  step<-length(ALLSIMULATEDDATA)
  PAR<-fComputePAR(globalradiation=ALLSIMULATEDDATA[[step]]$iRSDS, CoefPAR=ALLPARAMETERS$pCoefPAR)
  ALLSIMULATEDDATA[[step]]$cPAR<<-PAR
}



