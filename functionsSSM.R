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

fFunctionstep<-function(x, x1=NA,x2=NA, x3=NA, x4=NA, y1=NA, y2=NA, y3=NA) {
#fonction en _/~\_
#attention: marche SOIT avec x=vecteur et les params=1 chiffre chacun
#                  SOIT avec x=1 chiffre et les params=vecteurs (ou un chiffre)
#                  SOIT avec x et les éventuels params multiples de même longueur
  df<-data.frame(x, x1, x2, x3, x4, y1, y2, y3) #so that they ar all the same length
  df$toto<-df$y1 + (df$x-df$x1)*(df$y1-df$y2)/(df$x1-df$x2)
  df[!is.na(df$x) & df$x<df$x1, "toto"]<-df$y1[!is.na(df$x) & df$x<df$x1]
  df[!is.na(df$x) & df$x>df$x2 & df$x<=df$x3, "toto"]<-df$y2[!is.na(df$x) & df$x>df$x2 & df$x<=df$x3]
  df[!is.na(df$x) & df$x>df$x3 & df$x<df$x4, "toto"]<- (df$y2 + (df$x-df$x3)*(df$y3-df$y2)/(df$x4-df$x3))[!is.na(df$x) & df$x>df$x3 & df$x<df$x4]
  df[!is.na(df$x) & df$x>=df$x4, "toto"]<-y3[!is.na(df$x) & df$x>=df$x4]
  return(df$toto)
}

fComputeDailyVernalization<-function(cCrownTemp,TbaseVernalization,Topt1Vernalization,Topt2Vernalization,TstopVernalization) {
  return(fFunctionstep(x=cCrownTemp, x1=TbaseVernalization, x2=Topt1Vernalization, x3=Topt2Vernalization, x4=TstopVernalization, y1=0, y2=1, y3=0))
}

fDegreeDays<-function(cTemp, Tbase) return(pmax(Tbase, cTemp))

fComputeCoefTemp<-function(cTemp, Tbase, Topt1, Topt2, Tstop) { 
 return(fFunctionstep(x=cTemp, x1=Tbase, x2=Topt1, x3=Topt2, x4=Tstop, y1=0, y2=1, y3=0))
}

fComputeSnowMelt<-function(iTASMax, iPr, sSnow) {
  return(ifelse(iTASMax<=1, 0, pmin(sSnow, iTASMax + iPr * 0.4))) # icicici: unit problem: we expect mm, but we have degrees... this equation has no meaning!
  #it comes from SNOMLT = TMAX + RAIN * 0.4
}

fComputeCorrectedPr<-function(iTASMax, iPr, cSnowMelt){
  return(ifelse(iTASMax<=1, 0, iPr + cSnowMelt )) #in cases where it s cold, it snows instead of raining so correctedPr=0, where it s hot, the snow melt is added to the rain
}

fComputeCrownTemperature<-function(sSnow,iTASMax,iTASMin){
  sSnow<-pmin(15, sSnow)
  TcrownMin<-iTASMin
  TcrownMax<-iTASMax
  TcrownMin[iTASMin < 0 & sSnow > 0]<- 2 + iTASMin * (0.4 + 0.0018 * (sSnow - 15) ^ 2)
  TcrownMax[iTASMax < 0 & sSnow > 0]<- 2 + iTASMax * (0.4 + 0.0018 * (sSnow - 15) ^ 2)
  return((TcrownMin + TcrownMax) / 2)
}

fPhotoperiodDuration<-function(iDate,latitude){
  doy=as.numeric(strftime(iDate, format = "%j"))
  RDN = pi / 180
  DEC = sin(23.45 * RDN) * cos(2 * pi * (doy + 10) / 365)
  DEC = atan(DEC / sqrt(1 - DEC ^ 2)) * -1
  DECL = DEC * 57.29578
  SINLD = sin(RDN * latitude) * sin(DEC)
  COSLD = cos(RDN * latitude) * cos(DEC)
  AOB = SINLD / COSLD
  AOB2 = atan(AOB / sqrt(1 - AOB ^ 2))
  DAYL = 12 * (1 + 2 * AOB2 / pi)
  return(DAYL + 0.9)
}

fComputeCoefPhotoperiodCrops<-function(pPhotoperiodFunction, photoDuration,CriticalPhotoPeriod,PhotoPeriodSensitivity){
  ppfun<-rep(1, length(pPhotoperiodFunction))
  for (funct in unique(pPhotoperiodFunction)) {
    whichcases<- (pPhotoperiodFunction==funct)
    ppfun[whichcases]<-do.call(funct, args=list(
      photoDuration=photoDuration[whichcases],
      CriticalPhotoPeriod=CriticalPhotoPeriod[whichcases],
      PhotoPeriodSensitivity=PhotoPeriodSensitivity[whichcases]
    ))
    
  }
 return(ppfun)
}

fComputeCoefPhotoperiodWheat<-function(photoDuration,CriticalPhotoPeriod,PhotoPeriodSensitivity){
  ppfun <- ifelse(photoDuration < CriticalPhotoPeriod, 
                  pmax(0, 1 - PhotoPeriodSensitivity * (CriticalPhotoPeriod - photoDuration) ^ 2),
                  1)
  return(ppfun)
}

rComputeCoefPhotoperiodMaize<-function(photoDuration,CriticalPhotoPeriod,PhotoPeriodSensitivity){
  #we cheat : this is supposed to be a function, i.e. use only arguments that are passed to it, 
  #but it is a procedure because it accsses parameters from ALLDAYDATA (cultivars) and from ALLCROPS (threshold EJU) 
  #instead of only the parameters that are passed to all the other photoperiod functions (pp, cpp, ppsen)
  cultivars<-paste(ALLDAYDATA$sCrop,ALLDAYDATA$sCultivar, sep=".")
  bdEJUTSI<-sapply(cultivars, function(cv) sapply(ALLCROPS[cv, "thresholds"], function(th) return(th["EJU"])))
  ppfun <- ifelse(CriticalPhotoPeriod < photoDuration , 
                  pmax(0, bdEJUTSI / (bdEJUTSI + ppsen * (pp - cpp))),
                  1)
  return(ppfun)
}

fComputeCoefPhotoperiodLegume<-function(photoDuration,CriticalPhotoPeriod,PhotoPeriodSensitivity){
  ppfun<-pmax(0, pmin(1, 1-PhotoPeriodSensitivity*(CriticalPhotoPeriod-photoDuration)))
  return(ppfun)
  # If ppsen >= 0 Then
  # ppfun = 1 - ppsen * (cpp - pp)
  # ElseIf ppsen < 0 Then
  # ppfun = 1 - (-ppsen) * (pp - cpp) #this is mathematically the same as the above
  # End If
  # If ppfun > 1 Then ppfun = 1
  # If ppfun < 0 Then ppfun = 0
  # 
}

fComputeCoefVernalization<-function(VernalizationSensitivity,VDSAT,sVernalization){
  CoefVernalization = pmax(0, pmin(1, 1 - VernalizationSensitivity * (VDSAT - sVernalization)))
  return(CoefVernalization)
}

fComputeDurationTSIMaize<-function(cTUbetweenEMRandTSI, pPhyllochron, pT1optDev, pTbaseDev){
  TLNO <- cTUbetweenEMRandTSI / (pPhyllochron * 0.5) + 5
  return((((TLNO + 0.5) * pPhyllochron) - cTUbetweenEMRandTSI) / (pT1optDev - pTbaseDev))
}

fComputeDecreaseLAIwithN<-function(DailyRateNfromLeave,SpecLeafNGreenLeaf,SpecLeafNSenescenceLeaf){
    return(DailyRateNfromLeave / (SpecLeafNGreenLeaf - SpecLeafNSenescenceLeaf))
}


fFrostEffect<-function(LAI,tasmin,FreezeThresholdTemp,FreezeFracLeafDestruction){
  DLAIF<-rep(0, length(LAI))
  frstf<-pmax(pmin((FreezeThresholdTemp-tasmin) * FreezeFracLeafDestruction,1),0)
  FreezeThresholdTemp[is.na(FreezeThresholdTemp)]<- -Inf
  DLAIF[tasmin<FreezeThresholdTemp] <- (frstf*LAI)[tasmin<FreezeThresholdTemp]
  return(DLAIF)
}

fHeatEffectSemenovSirius<-function(cDecreaseLAI,tasmax,HeatThresholdTemp,HeatFracLeafDestruction){
  DLAIH<-rep(0, length(cDecreaseLAI))
  heatf <- pmax(1 + (tasmax - HeatThresholdTemp) * HeatFracLeafDestruction, 1)              #Semenov-Sirius
  HeatThresholdTemp[is.na(HeatThresholdTemp)]<- Inf
  DLAIH[tasmax>HeatThresholdTemp] <- (heatf*cDecreaseLAI)[tasmax>HeatThresholdTemp]
  return(DLAIH)
}

#just in case we want to add a parameter in the crop file to select the type of LAI decrease response to heat
fHeatEffectAssengAPSIM<-function(cDecreaseLAI,tasmax,HeatThresholdTemp,HeatFracLeafDestruction){
  DLAIH<-rep(0, length(LAI))
  heatf = 4-(1 - (tasmax - 34) /2)              #Asseng-APSIM
  HeatThresholdTemp[is.na(HeatThresholdTemp)]<- Inf
  DLAIH[tasmax>HeatThresholdTemp] <- (heatf*cDecreaseLAI)[tasmax>HeatThresholdTemp]
  return(DLAIH)
}

#' Find the thickness of the rooted part of each layer
#' this is not really a function because it accesses global soil parameters
#' @param layers vector of target soil layer(s) (single value, or one layer per case)
#' @param df data.frame of state variables (ALLDAYDATA or ALLSIMULATEDDATA[[timestep]])
#'
#' @return if layers is finite, a vector (one element per layer), if layers is Inf, a matrix (cases in rows, layers in columns), of rooted thickness : when the roots go deeper, it is the depth of the layer, and in the last rooted layer, it is the length of roots inside this layer
#' @examples
#' #' \dontrun{
#' fFindRLYER(Inf, ALLDAYDATA)
#' fFindRLYER(c(1,1,2), ALLDAYDATA) #works only if there are 3 cases
#' }
#' 
fFindRLYER<-function(layers, df) {#rootFrontDepth, LayerThicknesses){
  rootFrontDepth<-df$sRootFrontDepth
  LayerThicknesses<-fExtractSoilParameter(paramname="pLayerThickness", layers=Inf)
  Layerceilings<-t(apply(LayerThicknesses, 1, cumsum))-LayerThicknesses
  rootedlength<-pmin(pmax(rootFrontDepth-Layerceilings,0), LayerThicknesses)
  if(any(!is.finite(layers))) { #all layers
    return(rootedlength)
  } else {
    return(rootedlength[cbind(1:nrow(df), layers)])
  }
}

#' Computes the WL (water content) or ATSW (available transpirable water) or TTSW (total transpirable soil water) or FTSW (fraction transpirable soil water) in a given layer (one for each case, or in all soil), weighted or not by the fraction of the layer that is colonized by roots
#' this is not really a function because it accesses global soil parameters
#' @param layers vector of target soil layer(s) (single value, or one layer per case)
#' @param df data.frame of state variables (ALLDAYDATA or ALLSIMULATEDDATA[[timestep]])
#' @param what 
#' 
#' @return vector of ATSW in all cases, in the target layers
#' @examples
#'\dontrun{
#' fFindWater(layers=1, df=ALLDAYDATA, what="ATSW") #ATSW(1)
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="ATSW", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #ATSWRZ
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #FTSWRZ
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="WL", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) # WRZ
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=FALSE),1,sum, na.rm=TRUE) #FTSWSL
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="WL", weightedbyroots=FALSE),1,sum, na.rm=TRUE) #FTSWSL
#'}
fFindWater<-function(layers, df, what=c("ATSW", "TTSW", "FTSW", "WL"), weightedbyroots=FALSE){
  what<-what[1]
  #get water and make it into a matrix
  water<-as.matrix(df[,paste("sWater", 1:10, sep='.')])
  if (any(!is.finite(layers)))  WL<-water else WL<-as.matrix(water[cbind(1:nrow(water), layers)]) #water amount in the target layers
  #compute weigths (proportion of layer colonized by roots)
  if (weightedbyroots) {
    RLYER<-fFindRLYER(layers, df) #length of roots in each layer
    DLYER<-fExtractSoilParameter(paramname="pLayerThickness", layers=layers) #layer thickness
    weights<-RLYER/DLYER
  } else weights<-1 
  if (what=="WL") return(WL*weights)
  WLLL<-fExtractSoilParameter(paramname="pWiltingPoint", layers=layers)*fExtractSoilParameter(paramname="pLayerThickness", layers=layers) #amount of water at wilting point
  WLUL<-fExtractSoilParameter(paramname="pFieldCapacity", layers=layers)*fExtractSoilParameter(paramname="pLayerThickness", layers=layers) #amount of water at field capacity
  if (what=="ATSW") {
    return(drop((WL-WLLL)*weights))
  } else if (what=="TTSW") {
    return(drop((WLUL-WLLL)*weights))
  } else if (what=="FTSW") return(drop(((WL-WLLL)/(WLUL-WLLL))*weights)) else stop("what in fFindWater should be either ATSW, TTSW or FTSW")
}


#' Computes runoff
#'
#' @param vectorRain (corrected) rain
#' @param vectorCurveNumber Curve number  describing the soil infiltration capacity 
#' @param vectorSAT water content at full saturation in top layer
#' @param vectorLayerThickness thickness of top soil layer 
#' @param vectorWaterContent water content in soil layer 1
#' @param vectorWLL water content at wilting point in soil layer 1
#'
#' @return vector (one element per case) of runoff water
#' @examples
#' \dontrun{
#' fComputeRunoff(vectorrain=rain,
#' vectorCurveNumber=fExtractSoilParameter("pSoilCurveNumber"), 
#' vectorSAT=fExtractSoilParameter("pSaturation", layer=1),
#' vectorlayerthickness=fExtractSoilParameter("pLayerThickness", layer=1),
#' vectorwatercontent=ALLSIMULATEDDATA[[daybefore]][,"sWater.1"],
#' vectorWLL=fExtractSoilParameter("pWiltingPoint", layer=1)
#' }
fComputeRunoff<-function(vectorRain,
                         vectorCurveNumber, 
                         vectorSAT,
                         vectorLayerThickness,
                         vectorWaterContent,
                         vectorWLL){
  S <- 254 * (100 / vectorCurveNumber - 1)
  SWER <- pmax(0, 0.15 * ((vectorSAT*vectorLayerThickness - vectorWaterContent) / (vectorSAT*vectorLayerThickness - vectorWLL)))
  vectorrunof<-ifelse(vectorRain-SWER*S<0, 0, (vectorRain - SWER * S) ^ 2 / (vectorRain + (1 - SWER) * S))
  vectorrunof[rain<=0.01]<-0
  return(vectorrunof)
}

#' Potential evapotranspiration following simplified Penman equation
#' icicici this function contains a lot of hard-coded parameters... they should be moved to sheet generalPhysicalParameters of allvariables
#' @param tmax daily max temperature
#' @param tmin daily min temperature
#' @param srad total daily solar radiation
#' @param calb crop albedo
#' @param ket canopy extinction coefficient
#' @param etlai LAI used for PET calculation (takes into account senesced leaves still attached to the plant and/or on the soil)
#' @param salb #soil albedo
#' @return vector of PET (one element per case)
#' @examples
fComputePETsimplifiedPenman<-function(tmax, tmin, srad, calb, ket, etlai, salb) {
  td = 0.6 * tmax + 0.4 * tmin
  albedo = calb * (1 - exp(-ket * etlai)) + salb * exp(-ket * etlai)
  eeq = srad * (0.004876 - 0.004374 * albedo) * (td + 29)
  pet = eeq * 1.1
  pet[tmax > 34]<- eeq * ((tmax - 34) * 0.05 + 1.1)
  pet[tmax < 5]<- eeq * 0.01 * exp(0.18 * (tmax + 20))
  return(pet)
}

fComputeSoilEvaporationTwoStages<-function(vectorPet, vectorCanopyExtinctionCoefficient, vectorEtlai, atomicMinimalSoilEvaporation, 
                                           vectorStubbleWeight, vectorRain, vectorIrrigation, atomicpSoilWettingWaterQuantity) {
  
}


#####Weather module
rWeatherDay<-function(){
  #print("Updating weather intput")

  Dateoftheday<-ALLDAYDATA[1,"iDate"]
  dfclimate<-fGetClimateDay(date=Dateoftheday)
  climatevariables<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="weather" & VARIABLEDEFINITIONS$typeinthemodel=="input","name"]
  ALLDAYDATA[,climatevariables]<<-dfclimate[PARAMSIM$cases$climatename, climatevariables]
  ###To be full in
  #Calculate sSnow evolution, cSnowMelt, CorrectedPr

  cSnowMelt<-fComputeSnowMelt(sSnow=ALLDAYDATA$sSnow,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  cPrCorrected<-fComputeCorrectedPr(cSnowMelt=cSnowMelt,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  sSnow<-ALLDAYDATA$sSnow + ALLDAYDATA$iPr -  cPrCorrected #if temp<=1, prcorrected = 0 and all rain is snow ; if temp>1, cPrCorrected = iPr-snowmelt so snowmelt=ipr - cPr
  #warning: this equation for sSnow is true only if the temperature threshold for snowmelt is the same as the temperature threshold for snowing
  ALLDAYDATA[,c("cSnowMelt","cPrCorrected","sSnow")]<<-data.frame(cSnowMelt,cPrCorrected,sSnow)
  return()
}


#### Management module (for now, just keeps the crop, cultivar and crop parameters at their previous values)
rUpdateManagement<-function(){
  #print("Updating crops according to crop management")
  #whosows
  rSetParamsFromCrops() #in HousekeepingFunctions
  #icicici also initialize sRootFrontDepth
  
  #whoharvests
  #whofertilizes
  #whoirrigates
  
  
  return()
}

#### computation of water stresses and N stresses, from water level and nitrogen of the previous day
#do it now because they are used by several modules
rUpdateStresses<-function(){
  cFTSWweightedByRoots<-apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #FTSWRZ
  ###icicicici this is computed twice, once here and once in waterbudget, because we decided not to save layer-related soil variables, except water content
  #### and we need it now just to compute AROOT, because in the code, AROOT from last time step is used to compute WUUR before updating aroot, don't know why
  rootLength_L<-fFindRLYER(Inf, ALLDAYDATA) #RLYER
  FTSW_L<-fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=FALSE) #FTSW(l)
  waterStressTranspiration_L<-pmax(pmin(FTSW_L/ALLDAYDATA$pThresholdWaterStressGrowth, 1),0 ) #RT(L)
  cEfficientRootLength<-apply(rootLength_L*waterStressTranspiration_L, 1, sum, na.rm=TRUE) #AROOT
  
  cCoefWaterstressGrowth<-pmin(1, cFTSWweightedByRoots/ALLDAYDATA$pThresholdWaterStressGrowth) #WSFG 
  cCoefWaterstressLeafArea<-pmin(1, cFTSWweightedByRoots/ALLDAYDATA$pThresholdWaterStressLeafArea) #WSFL
  cCoefWaterstressDevelopment<-(1-cCoefWaterstressGrowth)*ALLDAYDATA$pCoefWaterStressGrowthToDevelopment+1 #WSFD 
  
  #not coded yet:
    # If WRZ <= WRZUL Then WSXF = 1 Else WSXF = ((WRZST - WRZ) / (WRZST - WRZUL))
    # If WSXF < 0 Then WSXF = 0
    # If FTSWRZ > 1 Then
    # WSFG = WSXF:   WSFL = WSXF:   'WSFN = WSXF:
    # End If
    # If WSXF <= 0.02 Then FLDUR = (FLDUR + 1) Else FLDUR = 0
    # If FLDUR > FLDKIL Then CBD = bdTSG: MATYP = 5
    
    
  ALLDAYDATA[,c("cEfficientRootLength", "cFTSWweightedByRoots", "cCoefWaterstressGrowth", 
                "cCoefWaterstressLeafArea", "cCoefWaterstressDevelopment")]<<-data.frame(
                  cEfficientRootLength, cFTSWweightedByRoots, cCoefWaterstressGrowth, 
                  cCoefWaterstressLeafArea, cCoefWaterstressDevelopment
                )
  
}

#####phenology module

#special procedure that is run only at stage change from EJU to SIL for Maize, to compute the duration of TSI stage
#this procedure has arguments (the cases where it has to be run) and returns the values, but it can access ALLDAYDATA and ALLSIMULATEDDATA
rComputeTSISILdurationMaize<-function(whichcases){
  #find deltaTU between EMR (i.e. the TU at the last occurence of SOW from ALLSIMULATEDDATA) and now (from ALLDAYDATA)
  TUmatrix<-as.data.frame(lapply(ALLSIMULATEDDATA, function(x) return(x[whichcases,"sThermalUnit"])))
  GSmatrix<-as.data.frame(lapply(ALLSIMULATEDDATA, function(x) return(x[whichcases,"sGrowthStage"])))
  daylastSOW<-apply(GSmatrix, 1, function(x) max(which(x=="SOW")))
  #get the TU by extracting, for each line, the column corresponding to the day of the last SOW stage
  TUlastSOW<-TUmatrix[cbind(
    seq_len(nrow(TUmatrix)),
    daylastSOW
  )]
  TuEMRTIL<-ALLDAYDATA$sThermalUnit[whichcases]-TUlastSOW
  pPhyllochron<-ALLDAYDATA$pPhyllochron[whichcases]
  pTopt1dev<-ALLDAYDATA$pTopt1dev[whichcases]
  pTbasedev<-ALLDAYDATA$pTbasedev[whichcases]
  duration<-((TuEMRTIL/(0.5*pPhyllochron)+5)*pPhyllochron-TuEMRTIL)/(pTopt1dev-pTbasedev)
  return(duration) 
}

rUpdatePhenology<-function(){
  #print("Updating phenology")
  sThermalUnit<-ALLDAYDATA$sThermalUnit
  sBiologicalDay<-ALLDAYDATA$sBiologicalDay
  sBiologicalDaysSinceSowing<-ALLDAYDATA$sBiologicalDaysSinceSowing
  sGrowthStage<-ALLDAYDATA$sGrowthStage
  sGrowthStageNumber<-ALLDAYDATA$sGrowthStageNumber
  sDurationStage<-ALLDAYDATA$sDurationStage
  cDeltaThermalUnit<-(ALLDAYDATA$pTopt1dev - ALLDAYDATA$pTbasedev)
  cDeltaBiologicalDay<-ALLDAYDATA$cDeltaBiologicalDay
  
  cDailyVernalization<-ALLDAYDATA$cDailyVernalization
  sVernalization <- ALLDAYDATA$sVernalization
  cCoefVernalization <- ALLDAYDATA$cCoefVernalization
  cCrownTemp <- ALLDAYDATA$cCrownTemp
  cCoefWaterstressDevelopment <- ALLDAYDATA$cCoefWaterstressDevelopment
  cTemp<-ALLDAYDATA$cTemp
  cCoefTemp <-ALLDAYDATA$cCoefTemp
  cCoefPhotoPeriod <- ALLDAYDATA$cCoefPhotoPeriod
  cPhotoDuration<-ALLDAYDATA$cPhotoDuration
  cCoefDrySoilSurface <- ALLDAYDATA$cCoefDrySoilSurface
  
  ###Vernalization
  resultfilterBD<-applyfilters("vernalisation_onBD")
  resultfilterTU<-applyfilters("vernalisation_onTU")
  resultfilter<-resultfilterBD | resultfilterTU
  if(any(resultfilter)) {
    cCrownTemp[resultfilter] <- fComputeCrownTemperature(sSnow=ALLDAYDATA$sSnow[resultfilter],
                                           iTASMax=ALLDAYDATA$iTASMax[resultfilter],
                                           iTASMin=ALLDAYDATA$iTASMin[resultfilter])
    cDailyVernalization[resultfilter] <- fComputeDailyVernalization(cCrownTemp=cCrownTemp[resultfilter],
                                                      TbaseVernalization=ALLDAYDATA$pTbaseVernalization[resultfilter],
                                                      Topt1Vernalization=ALLDAYDATA$pTopt1Vernalization[resultfilter],
                                                      Topt2Vernalization=ALLDAYDATA$pTopt2Vernalization[resultfilter],
                                                      TstopVernalization=ALLDAYDATA$pTstopVernalization[resultfilter]
    )
    sVernalization[resultfilter] <- sVernalization[resultfilter] + cDailyVernalization[resultfilter]
    autumnheatwave<- (!is.na(sVernalization) & sVernalization < 10 & ALLDAYDATA$iTASMax > 30)
    sVernalization[autumnheatwave] <- (
      sVernalization[autumnheatwave] - 0.5 * (ALLDAYDATA$iTASMax[autumnheatwave] - 30)
    )
    sVernalization[sVernalization < 0] <- 0
    cCoefVernalization[resultfilter] <- fComputeCoefVernalization(VernalizationSensitivity=ALLDAYDATA$pVernalizationSensitivity[resultfilter],
                                                    VDSAT=ALLDAYDATA$pVDSAT[resultfilter],
                                                    sVernalization=sVernalization[resultfilter])
    #modify DTU and bd accordingly
    cDeltaThermalUnit[resultfilterTU]<-cDeltaThermalUnit[resultfilterTU]*cCoefVernalization[resultfilterTU]
    cDeltaBiologicalDay[resultfilterBD]<-cDeltaBiologicalDay[resultfilterBD]*cCoefVernalization[resultfilterBD]
  }
  
###Waterstress
  resultfilterBD<-applyfilters("waterstress_onBD")
  resultfilterTU<-applyfilters("waterstress_onTU")
  resultfilter<-resultfilterBD | resultfilterTU
  if(any(resultfilter)) {
    #modify DTU and bd accordingly
    cDeltaThermalUnit[resultfilterTU]<-cDeltaThermalUnit[resultfilterTU]*cCoefWaterstressDevelopment[resultfilterTU]
    cDeltaBiologicalDay[resultfilterBD]<-cDeltaBiologicalDay[resultfilterBD]*cCoefWaterstressDevelopment[resultfilterBD]
  }

###temperature
  resultfilterBD<-applyfilters("temperature_onBD")
  resultfilterTU<-applyfilters("temperature_onTU")
  resultfilter<-resultfilterBD | resultfilterTU
  if(any(resultfilter)){
    cTemp[resultfilter]<-fComputeTemp(tasmax=ALLDAYDATA$iTASMax[resultfilter],
                        tasmin=ALLDAYDATA$iTASMin[resultfilter])
    cCoefTemp[resultfilter]<-fComputeCoefTemp(cTemp=cTemp[resultfilter],
                                              Tbase=ALLDAYDATA$pTbasedev[resultfilter],
                                              Topt1=ALLDAYDATA$pTopt1dev[resultfilter],
                                              Topt2=ALLDAYDATA$pTopt2dev[resultfilter],
                                              Tstop=ALLDAYDATA$pTstopdev[resultfilter])
    #modify DTU and bd accordingly
    cDeltaThermalUnit[resultfilterTU]<-cDeltaThermalUnit[resultfilterTU]*cCoefTemp[resultfilterTU]
    cDeltaBiologicalDay[resultfilterBD]<-cDeltaBiologicalDay[resultfilterBD]*cCoefTemp[resultfilterBD]
  }
  
###PhotoPeriod
  resultfilterBD<-applyfilters("photoperiod_onBD")
  resultfilterTU<-applyfilters("photoperiod_onTU")
  resultfilter<-resultfilterBD | resultfilterTU
  if(any(resultfilter)){
    cPhotoDuration[resultfilter]<-fPhotoperiodDuration(iDate=ALLDAYDATA[1,"iDate"],
                                                       latitude=PARAMSIM$cases[resultfilter, "lat"])
    cCoefPhotoPeriod[resultfilter]<-fComputeCoefPhotoperiodCrops(pPhotoperiodFunction=ALLDAYDATA$pPhotoperiodFunction,
                                                                 photoDuration=cPhotoDuration,
                                                                 CriticalPhotoPeriod=ALLDAYDATA$pCriticalPhotoPeriod,
                                                                 PhotoPeriodSensitivity=ALLDAYDATA$pPhotoPeriodSensitivity
    )[resultfilter] #warning: we have to compute this for all cases, even for cases that don't need the photoperiod,
    #because in case pPhotoperiodFunction is rComputeCoefPhotoperiodMaize, it accesses ALLDAYDATA, 
    #without knowing which cases it applies to
    #modify DTU and bd accordingly
    cDeltaThermalUnit[resultfilterTU]<-cDeltaThermalUnit[resultfilterTU]*cCoefPhotoPeriod[resultfilterTU]
    cDeltaBiologicalDay[resultfilterBD]<-cDeltaBiologicalDay[resultfilterBD]*cCoefPhotoPeriod[resultfilterBD]
  }
  
###dry Soil Surface
  resultfilterBD<-applyfilters("drySoilSurface_onBD")
  resultfilterTU<-applyfilters("drySoilSurface_onTU")
  resultfilter<-resultfilterBD | resultfilterTU
  if(any(resultfilter)) {
    FTSW.1<-fFindWater(layers=1, df=ALLDAYDATA, what="FTSW")
    cCoefDrySoilSurface[resultfilter & FTSW.1<=0]<-0
    #modify DTU and bd accordingly
    cDeltaThermalUnit[resultfilterTU]<-cDeltaThermalUnit[resultfilterTU]*cCoefDrySoilSurface[resultfilterTU]
    cDeltaBiologicalDay[resultfilterBD]<-cDeltaBiologicalDay[resultfilterBD]*cCoefDrySoilSurface[resultfilterBD]
  }
  
###Phenology Update
  sThermalUnit<-sThermalUnit + cDeltaThermalUnit
  sBiologicalDay<-sBiologicalDay + cDeltaBiologicalDay
  sBiologicalDaysSinceSowing<-sBiologicalDaysSinceSowing + cDeltaBiologicalDay
  
####stage changes
  cultivars<-paste(ALLDAYDATA$sCrop,ALLDAYDATA$sCultivar, sep=".")
  changestage<-sBiologicalDay>sDurationStage
  if(any(changestage)) {
    sGrowthStageNumber[changestage]<-sGrowthStageNumber[changestage]+1
    #find the corresponding stage name
    sGrowthStage[changestage]<-mapply(function(thresh, num) return(names(thresh)[num]),
                                      ALLCROPS[cultivars[changestage], "thresholds"], 
                                      sGrowthStageNumber[changestage]
    )
    #find the corresponding stage duration
    DurationNextStage<-mapply(function(cropname, stage) return(ALLCROPS[cropname,"thresholds"][[1]][stage]),
                        cultivars[changestage], 
                        as.character(sGrowthStage)[changestage], SIMPLIFY = TRUE, USE.NAMES=FALSE
                        )
    # when the stage changes, we start the counter with the remaining of increment-threshold (but not for TU)
    sBiologicalDay[changestage]<-sBiologicalDay[changestage]-sDurationStage[changestage] #if we change stages, we start not from 0 but from the "extra units accuulated during the timestep
    #and we update the stage duration with the duration of nextstage
    sDurationStage[changestage]<-DurationNextStage
    #and we update it again by parsing the actionsAtStageChange that correspond to the cultivars that changed and the corresponding stages
    actionstodo<-mapply(function(cropname, stage) return(ALLCROPS[cropname,"actionsAtStageChange"][[1]][stage]),
                               cultivars, 
                               as.character(sGrowthStage), SIMPLIFY = TRUE, USE.NAMES=FALSE
    )
    for (atd in unique(actionstodo[!is.na(actionstodo)])) {
      whichcases<-(!is.na(actionstodo) &  actionstodo==atd) 
      sDurationStage[whichcases]<-do.call(atd, list(whichcases=whichcases))
    }
  }
  
####Update ALLDAYDATA
  ALLDAYDATA[,c("sThermalUnit", "sBiologicalDay", "sBiologicalDaysSinceSowing", "sGrowthStage", "sGrowthStageNumber", "sDurationStage", "sVernalization",
                "cDeltaThermalUnit", "cDeltaBiologicalDay", "cDailyVernalization", "cCoefVernalization", "cCrownTemp", 
                "cTemp", "cCoefTemp", "cCoefPhotoPeriod", "cPhotoDuration", "cCoefDrySoilSurface")]<<-data.frame(
                  sThermalUnit, sBiologicalDay, sBiologicalDaysSinceSowing, sGrowthStage, sGrowthStageNumber, sDurationStage, sVernalization,
                  cDeltaThermalUnit, cDeltaBiologicalDay, cDailyVernalization, cCoefVernalization, cCrownTemp, 
                  cTemp, cCoefTemp, cCoefPhotoPeriod, cPhotoDuration, cCoefDrySoilSurface  )
  return()
}

#####LAI module
rUpdateLAI<-function(){
  #print("Updating LAI")
  ###LAI Growing (similar with and without N contribution)
  #LAIMainstem (i.e.between bdBLG and bdTLM)
  cCoefWaterstressLeafArea<-ALLDAYDATA$cCoefWaterstressLeafArea
  daily_increase_node_number <- ALLDAYDATA$sThermalUnit / ALLDAYDATA$pPhyllochron 
  sMainstemNodeNumber <- ALLDAYDATA$sMainstemNodeNumber  + daily_increase_node_number
  leaf_area_yesterday<-ALLDAYDATA$sPlantLeafArea
  LAI_yesterday<-ALLDAYDATA$sLAI
  sPlantLeafArea <- ALLDAYDATA$pcoefPlantLeafNumberNode * sMainstemNodeNumber ^ ALLDAYDATA$pExpPlantLeafNumberNode
  
  #Mainstem
  increase_LAIMainstem <- rep(0, nrow(ALLDAYDATA))
  resultfilter<-applyfilters("LAI_Mainstem")
  if(any(resultfilter)) {
    increase_LAIMainstem[resultfilter] <- ((
      (sPlantLeafArea - leaf_area_yesterday) * ALLDAYDATA$sPlantdensity / 10000) 
      * ALLDAYDATA$cCoefWaterstressLeafArea
    )[resultfilter]
  }
  
  #LAISecondary (between booting and beginning of seed growth for wheat, between bdTLM and bdTLP for legumes)
  increase_LAISecondary <- rep(0, nrow(ALLDAYDATA))
  applyfilters("LAI_Secondary")
  if(any(resultfilter)) {
    increase_LAISecondary[resultfilter] <- (ALLDAYDATA$sDailyLeafWeightIncrease * ALLDAYDATA$pSpecificLeafArea)[resultfilter] #sDailyLeafWeightIncrease = GLF from yesterday, from module DM_Distribution
  }
  #LAI Total Growing
  cGrowthLAI<- increase_LAIMainstem + increase_LAISecondary 
  
  ###LAI Decrease
  cDecreaseLAI<-rep(0, nrow(ALLDAYDATA))
  sDecreaseLAIperBD<-ALLDAYDATA$sDecreaseLAIperBD #simple decrease rate that remains the same throughout leaf senescence, to arrive at 0 LAI at MAT
  sLAIforEvapotranspiration<-ALLDAYDATA$sLAIforEvapotranspiration #we need to keep this information because it is used for evapotranspiration
  if(PARAMSIM$Neffect==T){
    #icicici not done yet, need to do it after Plant N module is coded
    cDecreaseLAI<-fComputeDecreaseLAIwithN(DailyRateNfromLeave=ALLDAYDATA$sDailyRateNfromLeave, #XNLF from yesterday, from module PlantN
                                           SpecLeafNGreenLeaf=ALLDAYDATA$pSpecLeafNGreenLeaf,
                                           SpecLeafNSenescenceLeaf=ALLDAYDATA$pSpecLeafNSenescenceLeaf)
  }else{ #LAI decrease following a straight line from LAI at beginning of senescence to 0 at MAT (=last stage in all crops)
    #the computation is more complicated than in the excel version, because we want to have nothing hard-coded (except that MAT is the last before last stage in the vector of stages)
    #and we didn't want to asking the user to add an extra parameter for senescence duration
    resultfilter<-applyfilters("LAI_Senescence")
    sLAIforEvapotranspiration[!resultfilter]<-LAI_yesterday[!resultfilter] #before senescence, we use the LAI (after, it will allways remain the same)
    if(any(resultfilter)) {
      startsenescence<-resultfilter & sDecreaseLAIperBD==0 #cases where we just started leaf senescence (sDecreaseLAIperBD was still at its initial value)
      if (any(startsenescence)) {
        BLSLAI<-rep(NA, nrow(ALLDAYDATA))
        BLSLAI[startsenescence]<-LAI_yesterday[startsenescence] #LAI at start of senescence
        cultivars_startsenescence<-paste(ALLDAYDATA$sCrop,ALLDAYDATA$sCultivar, sep=".")[startsenescence]
        durationOtherstages<-numeric()
        for (cr in unique(cultivars_startsenescence)) { #for each cultivar, we will find the duration of remaining stages after the current one
          currentstageNumber<-ALLDAYDATA$sGrowthStageNumber[startsenescence & paste(ALLDAYDATA$sCrop,ALLDAYDATA$sCultivar, sep=".")==cr][1] #they are all the same, because the filter for senescence is defined at the crop-cultivar level
          durationstages<-ALLCROPS[cr, "thresholds"][[1]]
          if(is.finite(durationstages[length(durationstages)])) warning(paste("leaf senescence (in the case where PARAMSIM$Neffect is FALSE) expects that the vector of thresholds for the different stages of the crops ends with a 'fake' last stage with length Inf. It is not the case for crop", cr,"so the last stage is not included in senescence" ))
          durationremainingstages<-sum(durationstages[(currentstageNumber+1):(length(durationstages)-1)]) #duration of all the other stages NOT including the current one, and of course not including the "fake" last stage with length Inf
          names(durationremainingstages)<-cr
          durationOtherstages<-c(durationOtherstages, durationremainingstages)
          if(is.na(durationremainingstages)) stop(paste("the duration of leaf senescence for", cr, " cannot be computed due to some stages having variable duration, this is not allowed in this version of the model"))
        }
        bdToMAT<-rep(NA, nrow(ALLDAYDATA)) #number of biological days from start of senescence to end of last stage
        bdToMAT[startsenescence]<-(ALLDAYDATA$sDurationStage[startsenescence] - ALLDAYDATA$sBiologicalDay[startsenescence]
        ) + ( #remaining duration of the current stage
          durationOtherstages[cultivars_startsenescence]
        )
        sDecreaseLAIperBD[startsenescence]<-BLSLAI[startsenescence]/bdToMAT[startsenescence]
      }
      cDecreaseLAI[resultfilter]<-sDecreaseLAIperBD[resultfilter]*ALLDAYDATA$cDeltaBiologicalDay[resultfilter]
    }
  }

   cFrost<-fFrostEffect(LAI=LAI_yesterday,tasmin=ALLDAYDATA$iTASMin,
                       FreezeThresholdTemp=ALLDAYDATA$pFreezeThresholdTemp,
                       FreezeFracLeafDestruction=ALLDAYDATA$pFreezeFracLeafDestruction)
   cHeat<-fHeatEffectSemenovSirius(
        cDecreaseLAI,tasmax=ALLDAYDATA$iTASMax, HeatThresholdTemp=ALLDAYDATA$pHeatThresholdTemp,
        HeatFracLeafDestruction=ALLDAYDATA$pHeatFracLeafDestruction)
   #just in case we want to add a parameter in the crop file to select the type of LAI decrease response to heat
   # cHeat<-rep(0, nrow(ALLDAYDATA))
   # cHeat[ALLDAYDATA$pHeateffectfunction=="SemenovSirius"]<- fHeatEffectSemenovSirius(
   #   cDecreaseLAI,tasmax=ALLDAYDATA$iTASMax, HeatThresholdTemp=ALLDAYDATA$pHeatThresholdTemp,
   #   HeatFracLeafDestruction=ALLDAYDATA$pHeatFracLeafDestruction)[ALLDAYDATA$pHeateffectfunction=="SemenovSirius"]
   # cHeat[ALLDAYDATA$pHeateffectfunction=="AssengAPSIM"]<- fHeatEffectAssengAPSIM(
   #   cDecreaseLAI,tasmax=ALLDAYDATA$iTASMax, HeatThresholdTemp=ALLDAYDATA$pHeatThresholdTemp,
   #   HeatFracLeafDestruction=ALLDAYDATA$pHeatFracLeafDestruction)[ALLDAYDATA$pHeateffectfunction=="SemenovSirius"]
   cDecreaseLAI<-pmax(cFrost,cHeat)   
   ####heat LAI corresponds to cDecreaseLAI if no heat effect 
   # and cDreaseLAI corresponds to heat if hot effet. 
   #Take effect of frost if it is more important that cDecreaseLAI

  sLAI <- pmax(0, ALLDAYDATA$sLAI+cGrowthLAI-cDecreaseLAI)         #Update LAI (sLAI) by the end of the module (in SSM excel is in the beginning)

  ####Mortality test with low LAI CONDITION
  #alaicond<-applyfilters("DMDistribution_SeedGrowing")
  #cEndCropCycle<-ifelse((sLAI< 0.05 & alaicond==TRUE),"pre-mature due to low LAI",NA)

  ALLDAYDATA[,c("sMainstemNodeNumber",
                "sPlantLeafArea",
                "cGrowthLAI",
                "sDecreaseLAIperBD",
                "sLAIforEvapotranspiration",
                "cDecreaseLAI",
                "cFrost",
                "cHeat",
                "sLAI")]<<-data.frame(
                  sMainstemNodeNumber,
                  sPlantLeafArea,
                  cGrowthLAI,
                  sDecreaseLAIperBD,
                  sLAIforEvapotranspiration,
                  cDecreaseLAI,
                  cFrost,
                  cHeat,
                  sLAI)
}


##### DM Production module
rUpdateDMProduction<-function(){
  #print("Updating DMProduction")
  cRUE <- rep(0, nrow(ALLDAYDATA))   #Radiation efficiency is null when the plant doens't produce leaf
  cCoefTemperatureRUE <- ALLDAYDATA$cCoefTemperatureRUE
  cCoefWaterstressGrowth<-ALLDAYDATA$cCoefWaterstressGrowth
  cPAR<-ALLDAYDATA$cPAR
  cDryMatterProduction<-ALLDAYDATA$cDryMatterProduction
  aFINT<-rep(0, nrow(ALLDAYDATA)) 
  resultfilter<-applyfilters("DMProduction") 
  if(any(resultfilter)) {
    cCoefTemperatureRUE[resultfilter]<-fComputeCoefTemp(cTemp=ALLDAYDATA$cTemp,Tbase=ALLDAYDATA$pTbaseRUE,
                                                             Topt1=ALLDAYDATA$pTopt1RUE,
                                                             Topt2=ALLDAYDATA$pTopt2RUE,
                                                             Tstop=ALLDAYDATA$pTstopRUE)[resultfilter]
    cRUE[resultfilter] <- (ALLDAYDATA$pRadEffiencyOptimal * cCoefTemperatureRUE * cCoefWaterstressGrowth)[resultfilter]
    cPAR[resultfilter]<-fComputePAR(globalradiation=ALLDAYDATA$iRSDS, 
                                    CoefPAR=GENERALPARAMETERS["pCoefPAR", "defaultInitialvalue"])[resultfilter]
    aFINT[resultfilter]<- (1 - exp(-ALLDAYDATA$pKPAR * ALLDAYDATA$sLAI))[resultfilter]
    cDryMatterProduction[resultfilter] <- (cPAR * aFINT * cRUE)[resultfilter]
  }
  
  ALLDAYDATA[,c("cCoefTemperatureRUE","cRUE","cPAR","cDryMatterProduction")]<<-data.frame(
    cCoefTemperatureRUE,cRUE,cPAR,cDryMatterProduction)

}


#### DM DIstribution module
rUpdateDMDistribution<-function(){
  #icicici warning: this procedure is a simple translation of the VBA code, which does not correspond to what 
  #is described in the book, for example it does not take into acount tuBSG and tuTSG
  
  #local copy of all variables that are needed
  #crop parameters (not to be saved)
  pFractionCropMassTranslocatable<-ALLDAYDATA$pFractionCropMassTranslocatable #FRTRL
  pGrainConversionCoefficient<-ALLDAYDATA$pGrainConversionCoefficient #GCC
  pFractionDMtoLeavesPhase1A<-ALLDAYDATA$pFractionDMtoLeavesPhase1A #FLF1A
  pFractionDMtoLeavesPhase1B<-ALLDAYDATA$pFractionDMtoLeavesPhase1B #FLF1B
  pFractionDMtoLeavesPhase2<-ALLDAYDATA$pFractionDMtoLeavesPhase2 #FLF2
  pPotentialDailyRateHIincrease<-ALLDAYDATA$pPotentialDailyRateHIincrease #PDHI
  pDailyRateHIincreaseCriticalPoint1<-ALLDAYDATA$pDailyRateHIincreaseCriticalPoint1 #WDHI1
  pDailyRateHIincreaseCriticalPoint2<-ALLDAYDATA$pDailyRateHIincreaseCriticalPoint2 #WDHI2
  pDailyRateHIincreaseCriticalPoint3<-ALLDAYDATA$pDailyRateHIincreaseCriticalPoint3 #WDHI3
  pDailyRateHIincreaseCriticalPoint4<-ALLDAYDATA$pDailyRateHIincreaseCriticalPoint4 #WDHI4
  pInflexionPointLeafAllocationAB<-ALLDAYDATA$pInflexionPointLeafAllocationAB #WTOPL
  pStemMinimumNconcentration<-ALLDAYDATA$pStemMinimumNconcentration #SNCS
  #state and computed variables from this module (to be saved)
  cDailyDryMatterforLeavesAndStems<-ALLDAYDATA$cDailyDryMatterforLeavesAndStems #DDMP2
  sDailyLeafWeightIncrease<-ALLDAYDATA$sDailyLeafWeightIncrease #GLF
  cDailyPortionDMtranslocated<-ALLDAYDATA$cDailyPortionDMtranslocated #TRANSL
  cDailySeedWeightIncrease<-ALLDAYDATA$cDailySeedWeightIncrease #SGR
  cDailyStemWeightIncrease<-ALLDAYDATA$cDailyStemWeightIncrease #GST
  cEffectOfHeatOnDHI<-ALLDAYDATA$cEffectOfHeatOnDHI #FrHtDHI
  cFractionDMtoLeaves<-ALLDAYDATA$cFractionDMtoLeaves #FLF
  sAccumulatedAboveGroundDryMatter<-ALLDAYDATA$sAccumulatedAboveGroundDryMatter #WTOP
  sAccumulatedGrainDryMatter<-ALLDAYDATA$sAccumulatedGrainDryMatter #WGRN
  sAccumulatedLeafDryMatter<-ALLDAYDATA$sAccumulatedLeafDryMatter #WLF
  sAccumulatedStemDryMatter<-ALLDAYDATA$sAccumulatedStemDryMatter #WST
  sAccumulatedVegetativeDryMatter<-ALLDAYDATA$sAccumulatedVegetativeDryMatter #WVEG
  sDailyRateHIincrease<-ALLDAYDATA$sDailyRateHIincrease #DHI
  sDryMatterAtBeginningSeedGrowth<-ALLDAYDATA$sDryMatterAtBeginningSeedGrowth #BSGDM
  sHarvestIndex<-ALLDAYDATA$sHarvestIndex #HI
  sTranslocatableBiomass<-ALLDAYDATA$sTranslocatableBiomass #TRLDM
  #state variables from other modules (not to be saved)
  DryMatterProduction<-ALLDAYDATA$cDryMatterProduction #DDMP of today, from DMProduction module
  LAI<-ALLDAYDATA$sLAI #LAI of today, from LAI module
  Nstem<-ALLDAYDATA$sNstem # NST of yesterday, from PlantN module (which updates it later)
  
  
  #seed growth
  phaseSeedGrowth<-applyfilters("DMDistribution_SeedGrowing")
      # things to do before seed growth so that some variables are initialized at their value at beginning of seed growth 
      # (these could be replaced by an actionsAtStageChange, but it would mean that beginning of seed growth has to be explicitely defined as a stage change, which is not the case in the initial crop files for wheat (ANT+5) and maize (SIL+170/effT))
  sDryMatterAtBeginningSeedGrowth[!phaseSeedGrowth]<-sAccumulatedAboveGroundDryMatter[!phaseSeedGrowth] #Saving WTOP at BSG because this stops being overwritten by abovegroundDM (thus remains at its value from yesterday) as soon as seed growth begins
  sDailyRateHIincrease[!phaseSeedGrowth]<-fFunctionstep(x=sDryMatterAtBeginningSeedGrowth, 
                                      x1=pDailyRateHIincreaseCriticalPoint1,
                                      x2=pDailyRateHIincreaseCriticalPoint2,
                                      x3=pDailyRateHIincreaseCriticalPoint3,
                                      x4=pDailyRateHIincreaseCriticalPoint4,
                                      y1=0,
                                      y2=pPotentialDailyRateHIincrease,
                                      y3=0)[!phaseSeedGrowth]
  sTranslocatableBiomass[!phaseSeedGrowth]<-(sDryMatterAtBeginningSeedGrowth*pFractionCropMassTranslocatable)[!phaseSeedGrowth]
  
  #compute first version of seed growth rate
  ## icicici cEffectOfHeatOnDHI (FrHtDHI) is never changed in the code, it is always 1
  ## icicici I don't understand this equation : it seems to me DryMatterProduction is added twice: 
  cDailySeedWeightIncrease[phaseSeedGrowth]<-pmax(0, ((sDailyRateHIincrease*cEffectOfHeatOnDHI) # coefficient that is a function of DryMatterAtBeginningSeedGrowth
                             *(sAccumulatedAboveGroundDryMatter + DryMatterProduction) # accumulated DM until today (included)
                             + DryMatterProduction*sHarvestIndex)[phaseSeedGrowth]) #dry matter produced today * harvest index of yesterday
  
  #if there is no N for seed filling (can happen with option Neffect=TRUE (LAI_N)), no seed growth
  cDailySeedWeightIncrease[phaseSeedGrowth & LAI==0 & Nstem <= (sAccumulatedStemDryMatter * pStemMinimumNconcentration)]<-0
  
  #translocation (remobilization of DM from stem and leaves to seed)
  cDailyPortionDMtranslocated[phaseSeedGrowth]<-pmax(0, pmin(sTranslocatableBiomass[phaseSeedGrowth], (cDailySeedWeightIncrease/pGrainConversionCoefficient-DryMatterProduction)[phaseSeedGrowth]))
  sTranslocatableBiomass[phaseSeedGrowth]<-(sTranslocatableBiomass-cDailyPortionDMtranslocated)[phaseSeedGrowth]
  cDailySeedWeightIncrease[phaseSeedGrowth]<-pmin(cDailySeedWeightIncrease, 
                                                  (DryMatterProduction+cDailyPortionDMtranslocated)*pGrainConversionCoefficient)[phaseSeedGrowth]
  #dry matter available for leaves and stems
  cDailyDryMatterforLeavesAndStems<-pmax(0, DryMatterProduction-cDailySeedWeightIncrease*pGrainConversionCoefficient)
  
  #select the cFractionDMtoLeaves corresponding to the phase
  phase1A<-applyfilters("LAI_Mainstem") & sAccumulatedAboveGroundDryMatter<pInflexionPointLeafAllocationAB
  phase1B<-applyfilters("LAI_Mainstem") & sAccumulatedAboveGroundDryMatter>=pInflexionPointLeafAllocationAB
  phase2<-applyfilters("LAI_Secondary")
  phaseVegetativeGrowth<- (phase1A | phase1B | phase2)
  cFractionDMtoLeaves[phase1A]<-pFractionDMtoLeavesPhase1A[phase1A]
  cFractionDMtoLeaves[phase1B]<-pFractionDMtoLeavesPhase1B[phase1B]
  cFractionDMtoLeaves[phase2]<-pFractionDMtoLeavesPhase2[phase2]
  
  #distribution of DM between leaves and stem
  sDailyLeafWeightIncrease[phaseVegetativeGrowth]<- (cFractionDMtoLeaves*cDailyDryMatterforLeavesAndStems)[phaseVegetativeGrowth]
  cDailyStemWeightIncrease[phaseVegetativeGrowth]<-((1-cFractionDMtoLeaves)*cDailyDryMatterforLeavesAndStems)[phaseVegetativeGrowth]
  
  #Organs accumulated mass
  sAccumulatedLeafDryMatter<-sAccumulatedLeafDryMatter + sDailyLeafWeightIncrease
  sAccumulatedStemDryMatter<-sAccumulatedStemDryMatter + cDailyStemWeightIncrease
  sAccumulatedVegetativeDryMatter<-sAccumulatedVegetativeDryMatter + cDailyDryMatterforLeavesAndStems
  sAccumulatedGrainDryMatter<-sAccumulatedGrainDryMatter + cDailySeedWeightIncrease
  sAccumulatedAboveGroundDryMatter<-sAccumulatedVegetativeDryMatter + sAccumulatedGrainDryMatter
  sHI<-sAccumulatedGrainDryMatter/sAccumulatedAboveGroundDryMatter
  
  #saving state and computed variables
  ALLDAYDATA[,c("cDailyDryMatterforLeavesAndStems", "sDailyLeafWeightIncrease", "cDailyPortionDMtranslocated",
               "cDailySeedWeightIncrease", "cDailyStemWeightIncrease", "cEffectOfHeatOnDHI", "cFractionDMtoLeaves",
               "sAccumulatedAboveGroundDryMatter", "sAccumulatedGrainDryMatter", "sAccumulatedLeafDryMatter", 
               "sAccumulatedStemDryMatter", "sAccumulatedVegetativeDryMatter", "sDailyRateHIincrease", 
               "sDryMatterAtBeginningSeedGrowth", "sHarvestIndex", "sTranslocatableBiomass")]<<-data.frame(
                 cDailyDryMatterforLeavesAndStems, sDailyLeafWeightIncrease, cDailyPortionDMtranslocated,
                 cDailySeedWeightIncrease, cDailyStemWeightIncrease, cEffectOfHeatOnDHI, cFractionDMtoLeaves, 
                 sAccumulatedAboveGroundDryMatter, sAccumulatedGrainDryMatter, sAccumulatedLeafDryMatter, 
                 sAccumulatedStemDryMatter, sAccumulatedVegetativeDryMatter, sDailyRateHIincrease,
                 sDryMatterAtBeginningSeedGrowth, sHarvestIndex, sTranslocatableBiomass
               )
    
    
}


####root depth module
rRootDepth<-function(){
  filter<-applyfilters("rRootDepth") #filter on stage (crop parameter)
  if (any(filter)){
    grtd<-ALLDAYDATA$cDeltaBiologicalDay*ALLDAYDATA$pPotentialRootGrowth #potential growth
    grtd<-grtd*filter #filter on stage (crop parameter)
    grtd[ALLDAYDATA$cDryMatterProduction==0 #in case no biomass accumulation
         | ALLDAYDATA$sRootFrontDepth>=ALLDAYDATA$pMaxDepthWaterExtraction #or root depth already at the maximum water extraction depth
         | ALLDAYDATA$sRootFrontDepth>=fExtractSoilParameter("pLayerThickness", Inf, "sum", na.rm=TRUE) #or root already at hte bottom of the soil
         ]<-0 #then no root growth
    rtln<-mapply(function(floors,rootdepths) {toto<-which.max(floors[floors<rootdepths])+1 ; if(length(toto)==0) toto<-1; return(toto)},
                 as.data.frame(t(fExtractSoilParameter("pLayerThickness", Inf, "cumsum"))), #lignes: cases, colonnes: profondeurs du plancher de chaque couche
                 ALLDAYDATA$sRootFrontDepth #vecteur (cases) de profondeur de racines
    ) #vector (one element per case) of number of the lowest layer with roots
    grtd[fFindWater(layer=rtln, what="ATSW", df=ALLDAYDATA)==0]<-0 #in case the lowest layer with roots (=the layer of root tips) is dry, no growth
    ALLDAYDATA$sRootFrontDepth<<-ALLDAYDATA$sRootFrontDepth+grtd
  }
  
  return()
}


#### Water module
rWaterBudget<-function(){
  #we transform all variables into matrices (rows = cases, columns= layers) to facilitate computations
  sWater<-as.matrix(ALLDAYDATA[,paste("sWater", 1:10, sep=".")])
  cFTSWweightedByRoots<-ALLDAYDATA$cFTSWweightedByRoots #FTSWRZ (computed in rUpdateStresses)
  FTSW_L<-fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=FALSE)
  WLUL_L<- fExtractSoilParameter(paramname="pFieldCapacity", Inf)*fExtractSoilParameter(paramname="pLayerThickness", Inf) #water amount above the field capacity
  waterStressTranspiration_L<-pmax(pmin(FTSW_L/ALLDAYDATA$pThresholdWaterStressGrowth, 1),0 ) #RT(L)
 
  ##### icicici dont forget to code irrigation in management module
  
  #input rain+melted snow
  rain<-ALLDAYDATA[,"cPrCorrected"]
  
  #drainage
  FLOUT<-pmax((sWater - WLUL_L)*fExtractSoilParameter(paramname="pDrainedFraction", Inf), 0) #flow of water downward
  #icicici: why is this done now? why is FLIN not added?
  sWater<-sWater-FLOUT
  
  #compute runoff
  cRunoff<-fComputeRunoff(vectorRain=rain,
                        vectorCurveNumber=fExtractSoilParameter("pSoilCurveNumber"), 
                        vectorSAT=fExtractSoilParameter("pSaturation", layers=1),
                        vectorLayerThickness=fExtractSoilParameter("pLayerThickness", layers=1),
                        vectorWaterContent=sWater[,1],
                        vectorWLL=fExtractSoilParameter("pWiltingPoint", layers=1)
  )
  #find LAI useful to compute soil evaporation (= real LAI until beginning of seed growth, and then LAI at stage TLP (which is not necessarily BSG))
  etlai<-ALLDAYDATA[,"sLAIforEvapotranspiration"] # icicicici BLSLAI is overwritten at different places: at beginning of leaf senescence (BLS), and at termination of secondary LAI growth (TLP).... and depending on the crop, BLS is before (legumes), after (maize) or the same (wheat) as TLP. I haven't had time to study in detail in which case BLSLAI is really at beginning of leaf senescence and in which case it isn't
  #compute PET
  cropAlbedo<-GENERALPARAMETERS["pCropAlbedo", "defaultInitialvalue"]
  canopyExtinctionCoefficient<-GENERALPARAMETERS["pCanopyExtinctionCoefficient", "defaultInitialvalue"]
  minimalSoilEvaporation<-GENERALPARAMETERS["pMinimalSoilEvaporation", "defaultInitialvalue"]
  soilWettingWaterQuantity<-GENERALPARAMETERS["pSoilWettingWaterQuantity","defaultInitialvalue"]
  cPET<-fComputePETsimplifiedPenman(tmax=ALLDAYDATA[,"iTASMax"], 
                                   tmin=ALLDAYDATA[,"iTASMin"], 
                                   srad=ALLDAYDATA[,"iRSDS"], 
                                   calb=canopyExtinctionCoefficient, 
                                   ket=canopyExtinctionCoefficient, 
                                   etlai=etlai , salb=fExtractSoilParameter("pSoilAlbedo"))
  #compute soil evaporation
  cPotentialSoilEvaporation <- cPET * exp(-canopyExtinctionCoefficient * etlai)
  cPotentialSoilEvaporation[cPET > minimalSoilEvaporation & cPotentialSoilEvaporation<minimalSoilEvaporation]<-minimalSoilEvaporation
  #modify to take into account the effect of mulch 
  #icicic this equation has hard-coded parameters
  cPotentialSoilEvaporation <- cPotentialSoilEvaporation * (1.5 - 0.2 * log((100 * ALLDAYDATA$sStubleWeight)))
  #real soil evaporation 
  cActualSoilEvaporation<-cPotentialSoilEvaporation
  sDaysSinceStage2evaporation<-ALLDAYDATA$sDaysSinceStage2evaporation
  sDaysSinceStage2evaporation[rain+ALLDAYDATA$cIrrigationWater>soilWettingWaterQuantity]<-0
  conditionstageII<- cFTSWweightedByRoots<=0.5 | fFindWater(layers=1, df=ALLDAYDATA, what="ATSW")<=1
  cActualSoilEvaporation[conditionstageII]<-(cActualSoilEvaporation*((sDaysSinceStage2evaporation+1)^0.5-sDaysSinceStage2evaporation^0.5))[conditionstageII]
  sDaysSinceStage2evaporation[conditionstageII]<-sDaysSinceStage2evaporation[conditionstageII]+1 
  
  #plant transpiration
  #icicici this equation contains hard-coded parameters
  VPTMIN<- 0.6108 * exp(17.27 * ALLDAYDATA$iTASMin / (ALLDAYDATA$iTASMin + 237.3))
  VPTMAX<- 0.6108 * exp(17.27 * ALLDAYDATA$iTASMax / (ALLDAYDATA$iTASMax + 237.3))
  VPD <- fExtractSoilParameter(paramname="pVPDcoef") * (VPTMAX - VPTMIN) #warning: in SSM.R, VPDF (and latitude) has been moved from location-specific parameters to soil parameters, to avoid having a file just for locations
  cTranspiration <- pmax(0, ALLDAYDATA$cDryMatterProduction * VPD / ALLDAYDATA$pTranspirationEfficiencyLinkedToCO2) #icicicic VPD in kPa, TEC in Pa
  
  #compute water uptake
  #icicicic I don't understand why we use cEfficientRootLength (AROOT) from the day before 
  # while we use rootLength_L and waterStressTranspiration_L from the current time step
  #if this is an error, remove computation of cEfficientRootLength fromrUpdateStresses
  WUUR<-cTranspiration/(ALLDAYDATA$cEfficientRootLength + 1e-8) 
  rootLength_L<-fFindRLYER(Inf, ALLDAYDATA) #RLYER
  waterStressTranspiration_L<-pmax(pmin(FTSW_L/ALLDAYDATA$pThresholdWaterStressGrowth, 1),0 ) #RT(L)
  waterUptake_L<-rootLength_L*waterStressTranspiration_L*WUUR
  
  #distribute soil evaporation in different soil layers
  soilEvaporation_L<-matrix(0, nrow=nrow(PARAMSIM$cases), ncol=10)
  WLAD_L<-fExtractSoilParameter("pSoilDryness", layers=Inf)*fExtractSoilParameter("pLayerThickness", layers=Inf)
  DRAINF_L<-fExtractSoilParameter("pDrainedFraction", layers=Inf)
  toBeEvaporated<-actualSoilEvaporation #TSE
  for (l in 1:10) {
    evapHere<-pmax(0, pmin(toBeEvaporated, (sWater[,l]-WLAD_L[l])*DRAINF_L[l]))
    soilEvaporation_L[,l]<-evapHere
    toBeEvaporated<-pmax(0, toBeEvaporated-evapHere)
  }
  
  
  FTSW_L<-fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=FALSE) #FTSW(l)
  
  
  #update water content in each layer of soil and everything else
  
  
  ALLDAYDATA[,c("cRunoff",
                "cPET",
                "cPotentialSoilEvaporation",
                "sDaysSinceStage2evaporation",
                "cActualSoilEvaporation",
                "cTranspiration")]<<-data.frame(
                  cRunoff,
                  cPET,
                  cPotentialSoilEvaporation,
                  sDaysSinceStage2evaporation,
                  cActualSoilEvaporation,
                  cTranspiration
                )
    
}


