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
  noNA<-apply(df,1, function(x) !any(is.na(x)))
  df$toto<-df$y1 + (df$x-df$x1)*(df$y1-df$y2)/(df$x1-df$x2)
  df[noNA & df$x<df$x1, "toto"]<-df$y1[noNA & df$x<df$x1]
  df[noNA & df$x>df$x2 & df$x<=df$x3, "toto"]<-df$y2[noNA & df$x>df$x2 & df$x<=df$x3]
  df[noNA & df$x>df$x3 & df$x<df$x4, "toto"]<- (df$y2 + (df$x-df$x3)*(df$y3-df$y2)/(df$x4-df$x3))[noNA & df$x>df$x3 & df$x<df$x4]
  df[noNA & df$x>=df$x4, "toto"]<-y3[noNA & df$x>=df$x4]
  df[!noNA, "toto"]<-NA
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
  for (funct in unique(pPhotoperiodFunction[!is.na(pPhotoperiodFunction)])) {
    whichcases<- (! is.na(pPhotoperiodFunction) & pPhotoperiodFunction==funct)
    if ("whichcases" %in% names(formals(funct))) {
      arguments<-list(photoDuration=photoDuration,
                      CriticalPhotoPeriod=CriticalPhotoPeriod,
                      PhotoPeriodSensitivity=PhotoPeriodSensitivity,
                      whichcases=whichcases)
    }else {
      arguments<-list(photoDuration=photoDuration[whichcases],
                      CriticalPhotoPeriod=CriticalPhotoPeriod[whichcases],
                      PhotoPeriodSensitivity=PhotoPeriodSensitivity[whichcases]
      )
    }
    ppfun[whichcases]<-do.call(funct, args=arguments)
  }
 return(ppfun)
}

fComputeCoefPhotoperiodWheat<-function(photoDuration,CriticalPhotoPeriod,PhotoPeriodSensitivity){
  ppfun <- ifelse(photoDuration < CriticalPhotoPeriod, 
                  pmax(0, 1 - PhotoPeriodSensitivity * (CriticalPhotoPeriod - photoDuration) ^ 2),
                  1)
  return(ppfun)
}

rComputeCoefPhotoperiodMaize<-function(photoDuration,CriticalPhotoPeriod,PhotoPeriodSensitivity, whichcases){
  #we cheat : this is supposed to be a function, i.e. use only arguments that are passed to it, 
  #but it is a procedure because it accesses parameters from ALLDAYDATA (cultivars) and from ALLCROPS (threshold EJU) 
  #instead of only the parameters that are passed to all the other photoperiod functions (pp, cpp, ppsen)
  cultivars<-paste(ALLDAYDATA$sCrop,ALLDAYDATA$sCultivar, sep=".")[whichcases]
  bdEJUTSI<-sapply(cultivars, function(cv) sapply(ALLCROPS[cv, "thresholds"], function(th) return(th["EJU"])))
  ppfun <- pmin(1, pmax(0, bdEJUTSI / (bdEJUTSI + PhotoPeriodSensitivity[whichcases] * (photoDuration[whichcases] - CriticalPhotoPeriod[whichcases]))))
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
fFindRLYER<-function(layers, df) {
  if (nrow(df) != nrow(ALLSOILS$paramlayers)) {
    whichcases <- rownames(ALLSOILS$paramlayers) %in% rownames(df) #if df contained only some of the cases, we need to tell fExtractSoilParameter which cases to take from the soil parameters array
  } else  whichcases<-TRUE
  rootFrontDepth<-df$sRootFrontDepth
  LayerThicknesses<-fExtractSoilParameter(paramname="pLayerThickness", layers=Inf, whichcases=whichcases)
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
#' @param layers vector of target soil layer(s) (single value, or one layer per case) if Inf, returns all layers (as matrix: rows=cases, columns = layers) 
#' @param df data.frame of state variables (ALLDAYDATA or ALLSIMULATEDDATA[[timestep]])
#' @param what one of "ATSW", "TTSW", "FTSW", "WL"
#' @param weightedbyroots TRUE or FALSE, depending of wether the value should be weighted by the rooted proportion of the layer
#' @return if layers is Inf, a matrix (rows are cases, columns are layers), else a vector of ATSW in all cases of df, in the target layers
#' @examples
#'\dontrun{
#' fFindWater(layers=1, df=ALLDAYDATA, what="ATSW") #ATSW(1)
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="ATSW", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #ATSWRZ
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #FTSWRZ
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="WL", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) # WRZ
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=FALSE),1,sum, na.rm=TRUE) #FTSWSL
#' apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="WL", weightedbyroots=FALSE),1,sum, na.rm=TRUE) #SWSL
#'}
fFindWater<-function(layers, df, what=c("ATSW", "TTSW", "FTSW", "WL", "WLUL", "WLST"), weightedbyroots=FALSE){
  what<-what[1]
  if (nrow(df) != nrow(ALLSOILS$paramlayers)) {
    whichcases <- rownames(ALLSOILS$paramlayers) %in% rownames(df) #if df contained only some of the cases, we need to tell fExtractSoilParameter which cases to take from the soil parameters array
  } else  whichcases<-TRUE
  #get water and make it into a matrix
  water<-as.matrix(df[,paste("sWater", 1:10, sep='.')])
  if (any(!is.finite(layers)))  WL<-water else WL<-as.matrix(water[cbind(1:nrow(water), layers)]) #water amount in the target layers
  #compute weigths (proportion of layer colonized by roots)
  if (weightedbyroots) {
    RLYER<-fFindRLYER(layers, df) #length of roots in each layer
    DLYER<-fExtractSoilParameter(paramname="pLayerThickness", layers=layers, whichcases= whichcases, keepcasesasrows = TRUE) #layer thickness
    weights<-RLYER/DLYER
  } else weights<-1 
  if (what=="WL") return(WL*weights)
  WLLL<-fExtractSoilParameter(paramname="pWiltingPoint", layers=layers, whichcases= whichcases, keepcasesasrows = TRUE)*fExtractSoilParameter(paramname="pLayerThickness", layers=layers, whichcases= whichcases, keepcasesasrows = TRUE) #amount of water at wilting point
  WLUL<-fExtractSoilParameter(paramname="pFieldCapacity", layers=layers, whichcases= whichcases, keepcasesasrows = TRUE)*fExtractSoilParameter(paramname="pLayerThickness", layers=layers, whichcases= whichcases, keepcasesasrows = TRUE) #amount of water at field capacity
  WLST<-fExtractSoilParameter(paramname="pSaturation", layers=layers, whichcases= whichcases, keepcasesasrows = TRUE)*fExtractSoilParameter(paramname="pLayerThickness", layers=layers, whichcases= whichcases, keepcasesasrows = TRUE) #amount of water at saturation
  if (what=="ATSW") {
    result<-(WL-WLLL)*weights
  } else if (what=="TTSW") {
    result<-(WLUL-WLLL)*weights
  } else if (what=="FTSW") {
    result<-((WL-WLLL)/(WLUL-WLLL))*weights
  } else if (what=="WLUL") {
    result<-WLUL*weights
  } else if (what=="WLST") {
    result<-WLST*weights
  } else stop("what in fFindWater should be either ATSW, TTSW, FTSW, WL, WLUL or WLST")
  if (any(!is.finite(layers))) return(result) else return(drop(result))
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
  vectorrunof[vectorRain<=0.01]<-0
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

#' function (but with access to global variables) to compute the variables needed to determine sowing
#'
#' @param whichcases logical vector indicating which cases to use for computing
#' @param what one of "cumuRain5days", "temp", "FTSW1", "ATSWwholeSoil"
#' @return the variable that was requested, for the requested cases
#' @examples
#' fVariablesForSowing()
fVariablesForSowing<-function(whichcases=TRUE, what=c("cumuRain5days", "temp", "FTSW1", "ATSWwholeSoil")) {
  what<-what[1]
  if (what=="cumuRain5days") {
    timestep<-length(ALLSIMULATEDDATA)
    if(timestep<5) return(NA) else
      return(rowSums(do.call(cbind, lapply(ALLSIMULATEDDATA[(timestep-5): timestep], "[", whichcases, "iPr", drop=FALSE))))
  } else if (what=="temp") {
    return((ALLDAYDATA[whichcases, "iTASMax"]+ALLDAYDATA[whichcases, "iTASMin"])/2)
  } else if (what=="FTSW1") {
    return(fFindWater(layers=1, df=ALLDAYDATA[whichcases,, drop=FALSE], what="FTSW"))
  } else if (what=="ATSWwholeSoil") {
    return(apply(fFindWater(layers=Inf, df=ALLDAYDATA[whichcases,, drop=FALSE], what="ATSW"),1,sum, na.rm=TRUE))
  } else stop("function fVariablesForSowing cannot compute", what)
}
################################ procedures

#####Weather module
rWeatherDay<-function(){
  #print("Updating weather intput")
  Dateoftheday<-ALLDAYDATA[1,"iDate"]
  dfclimate<-fGetClimateDay(date=Dateoftheday)
  climatevariables<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="rWeatherDay" & VARIABLEDEFINITIONS$typeinthemodel=="input","name"]
  ALLDAYDATA[,climatevariables]<<-dfclimate[PARAMSIM$cases$climatename, climatevariables]
  cSnowMelt<-fComputeSnowMelt(sSnow=ALLDAYDATA$sSnow,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  cPrCorrected<-fComputeCorrectedPr(cSnowMelt=cSnowMelt,iTASMax=ALLDAYDATA$iTASMax,iPr=ALLDAYDATA$iPr)
  sSnow<-ALLDAYDATA$sSnow + ALLDAYDATA$iPr -  cPrCorrected #if temp<=1, prcorrected = 0 and all rain is snow ; if temp>1, cPrCorrected = iPr-snowmelt so snowmelt=ipr - cPr
  #warning: this equation for sSnow is true only if the temperature threshold for snowmelt is the same as the temperature threshold for snowing
  ALLDAYDATA[,c("cSnowMelt","cPrCorrected","sSnow")]<<-data.frame(cSnowMelt,cPrCorrected,sSnow)
}


#### Management module 
rFindWhoSows<-function(){
  doy<-as.POSIXlt(ALLDAYDATA$iDate[1])$yday+1
  cCycleEndType<-ALLDAYDATA$cCycleEndType
  sLastSowing<-ALLDAYDATA$sLastSowing
  #find the sowing window for each case
  sowingwindowstart<-rep(Inf, nrow(ALLDAYDATA))
  sowingwindowend<-rep(-Inf, nrow(ALLDAYDATA))
  for (manag in unique(ALLDAYDATA$sManagement)) {
    sowingwindowstart[ALLDAYDATA$sManagement==manag]<-ALLMANAGEMENTS[[manag]]$dfSowing$Fpdoy
    sowingwindowend[ALLDAYDATA$sManagement==manag]<-ALLMANAGEMENTS[[manag]]$dfSowing$Lpdoy
  }
  withinsowingwindow<-( ((sowingwindowstart<=sowingwindowend & doy>=sowingwindowstart & doy<=sowingwindowend) #summer crop
                        |  (sowingwindowstart>sowingwindowend & (doy>=sowingwindowstart | doy<sowingwindowend)) ) #winter crop
  ) & ALLDAYDATA$sLastSowing<ALLDAYDATA$sLastHarvest #to avoid sowing again something that is already sowed
  #find the sowing type the thresholds (sowwat and sowtemp) for each case (type depends on crop)
  sowingtype<-factor(rep(8, nrow(ALLDAYDATA)), levels=c(
    "5 days without rain", 
    "5 days without rain  and temp>SowTmp",
    "5 days without rain  and temp<SowTmp",
    "FTSW1>SowWat",
    "FTSW1<SowWat",
    "cumulated rainfall over 5 days > SowWat",
    "ATSW whole soil > SowWat",
    "fixed doy" #last because levels are numbered from 1 to n, but this was code 0
    ))
  waterthreshold<-numeric(nrow(ALLDAYDATA))
  tempthreshold<-numeric(nrow(ALLDAYDATA))
  for (manag in unique(ALLDAYDATA$sManagement[withinsowingwindow])) {
    sowingtype[ALLDAYDATA$sManagement==manag]<-levels(sowingtype)[ALLMANAGEMENTS[[manag]]$dfSowing$FixFind]
    waterthreshold[ALLDAYDATA$sManagement==manag]<-ALLMANAGEMENTS[[manag]]$dfSowing$SowWat
    tempthreshold[ALLDAYDATA$sManagement==manag]<-ALLMANAGEMENTS[[manag]]$dfSowing$SowTmp
  }
  #compute the necessary variables for each type and compare with the thresholds for each case
  sowingday<-rep(FALSE, nrow(ALLDAYDATA))
  for(type in unique(sowingtype[withinsowingwindow])) {
    tobetested<- withinsowingwindow & sowingtype==type
    if(type == "5 days without rain") {
      test<-fVariablesForSowing(whichcases=tobetested, what="cumuRain5days")==0
    } else if(type == "5 days without rain  and temp>sowtemp") {
      test<-(
        fVariablesForSowing(whichcases=tobetested, what="cumuRain5days")==0
        & fVariablesForSowing(whichcases=tobetested, what="temp") > tempthreshold[tobetested]
      )
    } else if(type == "5 days without rain  and temp<sowtemp") {
      test<-(
        fVariablesForSowing(whichcases=tobetested, what="cumuRain5days")==0
        & fVariablesForSowing(whichcases=tobetested, what="temp") < tempthreshold[tobetested]
      )
    } else if(type == "FTSW1>SowWat") {
      test<-(
        fVariablesForSowing(whichcases=tobetested, what="FTSW1")>= waterthreshold[tobetested]
      )
    } else if(type == "FTSW1<SowWat") {
      test<-(
        fVariablesForSowing(whichcases=tobetested, what="FTSW1")<= waterthreshold[tobetested]
      )
    } else if(type == "cumulated rainfall over 5 days > SowWat") {
      test<-(
        fVariablesForSowing(whichcases=tobetested, what="cumuRain5days") > waterthreshold[tobetested]
      )
    }  else if(type == "ATSW whole soil > SowWat") {
      test<-(
        fVariablesForSowing(whichcases=tobetested, what="ATSWwholeSoil") > waterthreshold[tobetested]
      )
    }  else test<-TRUE #fixed sowing=>as soon as within the sowing window, we sow
    sowingday[tobetested]<- test
  }
  #update sLastSowing
  sLastSowing[sowingday]<-length(ALLSIMULATEDDATA) #ALLDAYDATA$iDate[1] 
  ##if the sowing window end is reached and nothing is sowed, go to next crop in the rotation, i.e. do the "harvest" operation
  cCycleEndType[sowingwindowend==doy & sLastSowing<ALLDAYDATA$sLastHarvest]<-"not sowed"
    
  ALLDAYDATA[, c("sLastSowing", "cCycleEndType")]<<-data.frame(sLastSowing, cCycleEndType)
}
rSowing<-function(){
  whosows<-ALLDAYDATA$sLastSowing==length(ALLSIMULATEDDATA) #ALLDAYDATA$iDate[1]
  if(any(whosows)) {
    df<-ALLDAYDATA[whosows,]
    splits<-strsplit(df$sCropCult, split=".", fixed=TRUE)
    df$sCrop<-sapply(splits,"[[", 1)
    df$sCultivar<-sapply(splits,"[[", 2)
    #update crop parameters according to crop and cultivar (do it now even if usually it is done just after sowing, because... I don't remember why)
    cropparameters<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel =="CropParameter", "name"]
    cultivars<-paste(df$sCrop, df$sCultivar, sep=".")
    possiblecrops<-unique(cultivars)
    missing<-setdiff(cropparameters, names(ALLCROPS))
    if(length(missing)>0) warning("the following parameters are mising from crop parameters: ", paste(missing, collapse=", "))
    paramstobechanged<-intersect(cropparameters, names(ALLCROPS))
    df[, paramstobechanged]<-ALLCROPS[cultivars, paramstobechanged]
    #initialize management
    #df$cCycleEndType<-"not yet"
    #initialize phenology
    df$sGrowthStageNumber<-1
    df$sGrowthStage<-mapply(function(thresh, num) return(names(thresh)[num]),
                            ALLCROPS[df$sCropCult, "thresholds"], 
                            df$sGrowthStageNumber
    )
    df$sDurationStage<-mapply(function(cropname, stage) return(ALLCROPS[cropname,"thresholds"][[1]][stage]),
                              df$sCropCult, 
                              as.character(df$sGrowthStage), SIMPLIFY = TRUE, USE.NAMES=FALSE
    )
    df$sBiologicalDay<-0
    #initialize other things
    df$sPlantdensity<-sapply(ALLMANAGEMENTS[df$sManagement], function(x) x$dfSowing$Pden)
    #stubleWeight is initialized by harvest, at the same time as management change, because it is necessary for soil evaporation before sowing
    df$sRootFrontDepth<-as.numeric(ALLCROPS[df$sCropCult,"iDEPORT"])
    #initialize all other crop-related variables to their default value
    variablestoinitialize<-setdiff(
      VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel == "stateVariable"
                          & VARIABLEDEFINITIONS$module %in% c(
                            "rUpdateStresses", "rUpdatePhenology", "rUpdateLAI",
                            "rUpdateDMProduction", "rUpdateDMDistribution", "rUpdateRootDepth"
                          ), "name"],
      c("sCrop", "sCultivar", "sGrowthStageNumber", "sGrowthStage", "sDurationStage", 
        "sBiologicalDay", "sPlantdensity", "sStubleWeight", "sRootFrontDepth")
    )
    numericvariables<-variablestoinitialize[VARIABLEDEFINITIONS[variablestoinitialize,"typeR"]=="numeric"]
    df[,variablestoinitialize]<-VARIABLEDEFINITIONS[variablestoinitialize, "defaultInitialvalue"]
    df[,numericvariables]<-lapply(df[,numericvariables],as.numeric)
    #update ALLDAYDATA
    ALLDAYDATA[whosows,names(df)]<<-df
  }
}

rFindWhoHarvests<-function(){
  sLastHarvest<-ALLDAYDATA$sLastHarvest
  cCycleEndType<-factor(NA, levels=c("normal", "low LAI", "not sowed", "stopDAP", "killed by flood", "not yet"))
  cCycleEndType[sLastHarvest<ALLDAYDATA$sLastSowing]<-"not yet"
  harvestday<-rep(FALSE, nrow(ALLDAYDATA))
  #last stage reached
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
  numlaststages<-sapply(ALLCROPS[cultivars,"thresholds"], length)#{toto<- x$thresholds ; return(names(toto)[length(toto)])})
  reachedMaturity<-sLastHarvest<ALLDAYDATA$sLastSowing & ALLDAYDATA$sGrowthStageNumber==numlaststages
  reachedMaturity[is.na(reachedMaturity)]<-FALSE
  #lowLAI
  duringSeedGrowth<-applyfilters("DMDistribution_SeedGrowing")
  LowLAI<-duringSeedGrowth & ALLDAYDATA$sLAI<GENERALPARAMETERS["pTresholdLowLAIdeath", "defaultInitialvalue"]
  LowLAI[is.na(LowLAI)<-FALSE]
  #stopDAP parameter reached
  doy<-as.POSIXlt(ALLDAYDATA$iDate[1])$yday+1
  stopDAP<-sapply(ALLMANAGEMENTS[ALLDAYDATA$sManagement], function(x) x$dfSowing$StopDAP)
  reachedStopDAP<-sLastHarvest<ALLDAYDATA$sLastSowing & doy==stopDAP
  reachedStopDAP[is.na(reachedStopDAP)]<-FALSE
  # flooding (or anoxia?)
  flooding<-sLastHarvest<ALLDAYDATA$sLastSowing & ALLDAYDATA$sFloodDuration>ALLDAYDATA$pFloodLengthDeath
  # icicici warning: the initial code says CBD = bdTSG: MATYP = 5, does it mean that seed growth stops, but the maturation continues normally?
  #in this case, we should not put this code in rHarvest, but in rUpdatePhenology because it is just accelerating phenology, not killing the plant.
  #For now I leave it here because the explaination of MATYP says "Flood kill"
  flooding[is.na(flooding)]<-FALSE
  
  cCycleEndType[reachedMaturity]<-"normal"
  cCycleEndType[LowLAI]<-"low LAI"
  #cCycleEndType[FALSE]<-"not sowed" #done in the rFindWhoSows procedure
  cCycleEndType[reachedStopDAP]<-"stopDAP"
  cCycleEndType[flooding]<-"killed by flood"
  harvestday[!is.na(cCycleEndType) & cCycleEndType!="not yet"]<-TRUE
  #update sLastHarvest
  sLastHarvest[harvestday]<-length(ALLSIMULATEDDATA)#ALLDAYDATA$iDate[1] 
  
  ALLDAYDATA[,c("cCycleEndType", "sLastHarvest")]<<-data.frame(cCycleEndType, sLastHarvest)
}
rHarvesting<-function(){
  whoharvests<-ALLDAYDATA$sLastHarvest==length(ALLSIMULATEDDATA) #ALLDAYDATA$iDate[1]
   if(any(whoharvests)) {
    df<-ALLDAYDATA[whoharvests,]
    #find the next crop in the rotation
    rotations<-PARAMSIM$cases$rotation[whoharvests]
    nextnum<-ALLDAYDATA$sCropNum[whoharvests]+1
    nextnum[nextnum>sapply(rotations, length)]<-1 #last crop finished=>we start again the rotation
    #reset all crop parameters and state variables to their default values
    cropparam<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel =="CropParameter", "name"]
    numericparam<-cropparam[VARIABLEDEFINITIONS[cropparam,"typeR"]=="numeric"]
    df[,cropparam]<-VARIABLEDEFINITIONS[cropparam, "defaultInitialvalue"]
    df[,numericparam]<-lapply(df[,numericparam],as.numeric)
    variablestoinitialize<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel == "stateVariable"
                          & VARIABLEDEFINITIONS$module %in% c(
                            "rUpdateManagement",  "rUpdateStresses", "rUpdatePhenology", "rUpdateLAI",
                            "rUpdateDMProduction", "rUpdateDMDistribution", "rUpdateRootDepth"
                          ), "name"]
    variablestoinitialize<-setdiff(variablestoinitialize, c("sLastHarvest", "sLastSowing", "cCycleEndType"))
    numericvariables<-variablestoinitialize[VARIABLEDEFINITIONS[variablestoinitialize,"typeR"]=="numeric"]
    df[,variablestoinitialize]<-VARIABLEDEFINITIONS[variablestoinitialize, "defaultInitialvalue"]
    df[,numericvariables]<-lapply(df[,numericvariables],as.numeric)
    #df$sCrop<-NA #done by initialization
    #df$sCultivar<-NA #done by initialization
    #prepare for sowing: indicate the next crop and management
    df$sCropNum<-nextnum
    df$sCropCult<-mapply("[", rotations, nextnum)
    df$sManagement<-mapply("[", PARAMSIM$cases$management[whoharvests], nextnum)
    #initialize sStubleWeight (necessary for soil evaporation during the fallow period)
    df$sStubleWeight<-sapply(ALLMANAGEMENTS[df$sManagement], function(x) x$dfSowing$STBLW) #because it is necessary before sowing so it cannot be initialized at sowing like the other variables
    ALLDAYDATA[whoharvests,names(df)]<<-df
  }
}

rFindWhoIrrigates<-function(){
  
}

rUpdateManagement<-function(){
  #print("Updating crops according to crop management")
  #DOY<-as.POSIXlt(ALLDAYDATA[1,"iDate"])$yday+1
  #whosows
  rFindWhoSows() #writes the current date into ALLDAYDATA$sLastSowing
  rSowing() #initialize crop variables
  rSetParamsFromCrops() #assign the crop parameters according to the crop present (i.e. do it again fro crops that are just sowed today, but we need it for the other cases as well)
  rFindWhoHarvests()
  rHarvesting()
  #whofertilizes
  #whoirrigates
}

#### computation of water stresses and N stresses, from water level and nitrogen of the previous day
#do it now because they are used by several modules
rUpdateStresses<-function(){
  sFloodDuration<-ALLDAYDATA$sFloodDuration
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

  waterWeightedByRoots<-apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="WL", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #WRZ
  waterFieldCapacityWeightedByRoots<-apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="WLUL", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #WRZUL
  waterSaturationWeightedByRoots<-apply(fFindWater(layers=Inf, df=ALLDAYDATA, what="WLST", weightedbyroots=TRUE), 1, sum, na.rm=TRUE) #WRZST
  cCoefWaterStressSaturation<-pmin(pmax(((waterSaturationWeightedByRoots - waterWeightedByRoots) / (waterSaturationWeightedByRoots - waterFieldCapacityWeightedByRoots)), 0), 1) #WSXF
  cCoefWaterstressGrowth[cFTSWweightedByRoots>1]<-cCoefWaterStressSaturation[cFTSWweightedByRoots>1]
  cCoefWaterstressLeafArea[cFTSWweightedByRoots>1]<-cCoefWaterStressSaturation[cFTSWweightedByRoots>1]
  saturated<-cCoefWaterStressSaturation<=GENERALPARAMETERS["pWaterStressSaturationFlood", "defaultInitialvalue"]
  saturated[is.na(saturated)]<-FALSE
  sFloodDuration[saturated]<-(sFloodDuration+1)[saturated]
  sFloodDuration[!saturated]<-0
    
  ALLDAYDATA[,c("cEfficientRootLength", "cFTSWweightedByRoots", "cCoefWaterstressGrowth", 
                "cCoefWaterstressLeafArea", "cCoefWaterstressDevelopment",
                "cCoefWaterStressSaturation", "sFloodDuration")]<<-data.frame(
                  cEfficientRootLength, cFTSWweightedByRoots, cCoefWaterstressGrowth, 
                  cCoefWaterstressLeafArea, cCoefWaterstressDevelopment,
                  cCoefWaterStressSaturation, sFloodDuration
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
  ALLDAYDATA$sDurationStage[whichcases]<<-duration
}

rUpdatePhenology<-function(){
  #print("Updating phenology")
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
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
    #icicici there are hard-coded parameters here
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
  sThermalUnit[cultivars!="NA.NA"]<-(sThermalUnit + cDeltaThermalUnit)[cultivars!="NA.NA"]
  sBiologicalDay[cultivars!="NA.NA"]<-(sBiologicalDay + cDeltaBiologicalDay)[cultivars!="NA.NA"]
  sBiologicalDaysSinceSowing[cultivars!="NA.NA"]<-(sBiologicalDaysSinceSowing + cDeltaBiologicalDay)[cultivars!="NA.NA"]
  
####stage changes
  changestage<-sBiologicalDay>sDurationStage
  changestage[is.na(changestage)]<-FALSE
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
    #we don't do the actions now, first we update ALLDAYDATA because actionstodo are procedures that directly modify ALLDAYDATA
  } else actionstodo<-list()
  
####Update ALLDAYDATA
  ALLDAYDATA[,c("sThermalUnit", "sBiologicalDay", "sBiologicalDaysSinceSowing", "sGrowthStage", "sGrowthStageNumber", "sDurationStage", "sVernalization",
                "cDeltaThermalUnit", "cDeltaBiologicalDay", "cDailyVernalization", "cCoefVernalization", "cCrownTemp", 
                "cTemp", "cCoefTemp", "cCoefPhotoPeriod", "cPhotoDuration", "cCoefDrySoilSurface")]<<-data.frame(
                  sThermalUnit, sBiologicalDay, sBiologicalDaysSinceSowing, sGrowthStage, sGrowthStageNumber, sDurationStage, sVernalization,
                  cDeltaThermalUnit, cDeltaBiologicalDay, cDailyVernalization, cCoefVernalization, cCrownTemp, 
                  cTemp, cCoefTemp, cCoefPhotoPeriod, cPhotoDuration, cCoefDrySoilSurface  )
  ##### do actionstodo
  for (atd in unique(actionstodo[!is.na(actionstodo) & !sapply(actionstodo, is.null)])) {
    whichcases<-(!is.na(actionstodo) &  !sapply(actionstodo, is.null) & actionstodo==atd) 
    do.call(atd, list(whichcases=whichcases))
  }
}

#####LAI module
rUpdateLAI<-function(){
  #print("Updating LAI")
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
  sMainstemNodeNumber<-ALLDAYDATA$sMainstemNodeNumber
  sPlantLeafArea<-ALLDAYDATA$sPlantLeafArea
  cGrowthLAI<-ALLDAYDATA$cGrowthLAI
  sDecreaseLAIperBD<-ALLDAYDATA$sDecreaseLAIperBD #simple decrease rate that remains the same throughout leaf senescence, to arrive at 0 LAI at MAT
  sLAIforEvapotranspiration<-ALLDAYDATA$sLAIforEvapotranspiration #we need to keep this information because it is used for evapotranspiration
  sLAI<-ALLDAYDATA$sLAI
  ###LAI Growing (similar with and without N contribution)
  #LAIMainstem (i.e.between bdBLG and bdTLM)
  cCoefWaterstressLeafArea<-ALLDAYDATA$cCoefWaterstressLeafArea
  daily_increase_node_number <- ALLDAYDATA$sThermalUnit / ALLDAYDATA$pPhyllochron 
  sMainstemNodeNumber[!is.na(daily_increase_node_number)] <- (ALLDAYDATA$sMainstemNodeNumber  + daily_increase_node_number)[!is.na(daily_increase_node_number)]
  leaf_area_yesterday<-ALLDAYDATA$sPlantLeafArea
  LAI_yesterday<-ALLDAYDATA$sLAI
  toto<-ALLDAYDATA$pcoefPlantLeafNumberNode * sMainstemNodeNumber ^ ALLDAYDATA$pExpPlantLeafNumberNode
  sPlantLeafArea[!is.na(toto)] <- toto[!is.na(toto)]
  
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
  resultfilter<-applyfilters("LAI_Secondary")
  if(any(resultfilter)) {
    increase_LAISecondary[resultfilter] <- (ALLDAYDATA$sDailyLeafWeightIncrease * ALLDAYDATA$pSpecificLeafArea)[resultfilter] #sDailyLeafWeightIncrease = GLF from yesterday, from module DM_Distribution
  }
  #LAI Total Growing
  cGrowthLAI[cultivars != "NA.NA"]<- (increase_LAIMainstem + increase_LAISecondary )[cultivars != "NA.NA"]
  
  ###LAI Decrease
  cDecreaseLAI<-rep(0, nrow(ALLDAYDATA))
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

  sLAI[cultivars != "NA.NA"] <- pmax(0, ALLDAYDATA$sLAI+cGrowthLAI-cDecreaseLAI)[cultivars != "NA.NA"]         #Update LAI (sLAI) by the end of the module (in SSM excel is in the beginning)

  ####Mortality test with low LAI CONDITION moved to DMDistribution
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
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
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
  withcrop<- (cultivars != "NA.NA")
  sAccumulatedLeafDryMatter[withcrop]<-(sAccumulatedLeafDryMatter + sDailyLeafWeightIncrease)[withcrop]
  sAccumulatedStemDryMatter[withcrop]<-(sAccumulatedStemDryMatter + cDailyStemWeightIncrease)[withcrop]
  sAccumulatedVegetativeDryMatter[withcrop]<-(sAccumulatedVegetativeDryMatter + cDailyDryMatterforLeavesAndStems)[withcrop]
  sAccumulatedGrainDryMatter[withcrop]<-(sAccumulatedGrainDryMatter + cDailySeedWeightIncrease)[withcrop]
  sAccumulatedAboveGroundDryMatter[withcrop]<-(sAccumulatedVegetativeDryMatter + sAccumulatedGrainDryMatter)[withcrop]
  sHarvestIndex[withcrop]<-(sAccumulatedGrainDryMatter/sAccumulatedAboveGroundDryMatter)[withcrop]
  
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
rUpdateRootDepth<-function(){
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
}


#### Water module
rUpdateWaterBudget<-function(){
  #we transform all variables into matrices (rows = cases, columns= layers) to facilitate computations
  sWater<-as.matrix(ALLDAYDATA[,paste("sWater", 1:10, sep=".")])
  
  #icicicic warning  : rootLength_L here is computed after today's root growth,
  # while cEfficientRootLength (AROOT) was computed with root length from the day before in rUpdateStresses
  # and waterStressTranspiration_L is also from yesterday variables (FTSW_L is computed at the beginning of the current procedure, and does not weight by root length
  #check in the excel code when each element is updated to do the same
  yesterdayFTSWweightedByRoots<- ALLDAYDATA$cFTSWweightedByRoots #FTSWRZ (computed in rUpdateStresses)
  yesterdayEfficientRootLength<- ALLDAYDATA$cEfficientRootLength #AROOT (computed in rUpdateStresses)
  FTSW_L<- fFindWater(layers=Inf, df=ALLDAYDATA, what="FTSW", weightedbyroots=FALSE) #FTSW(L)
  ATSW_1<- fFindWater(layers=1, df=ALLDAYDATA, what="ATSW")
  WLUL_L<- fExtractSoilParameter(paramname="pFieldCapacity", Inf)*fExtractSoilParameter(paramname="pLayerThickness", Inf) #water amount above the field capacity
  waterStressTranspiration_L<- pmax(pmin(FTSW_L/ALLDAYDATA$pThresholdWaterStressGrowth, 1),0 ) #RT(L)
  sDaysSinceStage2evaporation<-ALLDAYDATA$sDaysSinceStage2evaporation
  rootLength_L<-fFindRLYER(Inf, ALLDAYDATA) #RLYER with root length of today !!
  cTranspiration<-ALLDAYDATA$cTranspiration
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
  
  ##### icicici dont forget to code irrigation in management module
  cIrrigationWater<-ALLDAYDATA$cIrrigationWater
  
  #input rain+melted snow
  rain<-ALLDAYDATA[,"cPrCorrected"]
  
  #drainage has been moved to after computation of FLOUT (it is just for saving the amount of water drained, can be done at the end of the procedure)
  
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
  etlai[is.na(etlai)]<-0
  
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
  sDaysSinceStage2evaporation[rain + cIrrigationWater > soilWettingWaterQuantity]<-0
  conditionstageII<- yesterdayFTSWweightedByRoots<=0.5 | ATSW_1<=1
  cActualSoilEvaporation[conditionstageII]<-(cActualSoilEvaporation*((sDaysSinceStage2evaporation+1)^0.5-sDaysSinceStage2evaporation^0.5))[conditionstageII]
  sDaysSinceStage2evaporation[conditionstageII]<-sDaysSinceStage2evaporation[conditionstageII]+1 
  
  #plant transpiration
  #icicici this equation contains hard-coded parameters
  VPTMIN<- 0.6108 * exp(17.27 * ALLDAYDATA$iTASMin / (ALLDAYDATA$iTASMin + 237.3))
  VPTMAX<- 0.6108 * exp(17.27 * ALLDAYDATA$iTASMax / (ALLDAYDATA$iTASMax + 237.3))
  VPD <- fExtractSoilParameter(paramname="pVPDcoef") * (VPTMAX - VPTMIN) #warning: in SSM.R, VPDF (and latitude) has been moved from location-specific parameters to soil parameters, to avoid having a file just for locations
  cTranspiration[cultivars != "NA.NA"] <- pmax(0, ALLDAYDATA$cDryMatterProduction * VPD / ALLDAYDATA$pTranspirationEfficiencyLinkedToCO2)[cultivars != "NA.NA"] #icicicic VPD in kPa, TEC in Pa
  
  #compute water uptake
  WUUR<-cTranspiration/(yesterdayEfficientRootLength + 1e-8) 
  waterUptake_L<-rootLength_L*waterStressTranspiration_L*WUUR
  waterUptake_L[is.na(waterUptake_L)] <-0
  
  #distribute soil evaporation in different soil layers
  soilEvaporation_L<-matrix(0, nrow=nrow(PARAMSIM$cases), ncol=10)
  WLAD_L<-fExtractSoilParameter("pSoilDryness", layers=Inf)*fExtractSoilParameter("pLayerThickness", layers=Inf)
  DRAINF_L<-fExtractSoilParameter("pDrainedFraction", layers=Inf)
  toBeEvaporated<-cActualSoilEvaporation #TSE
  for (l in 1:10) {
    evapHere<-pmax(0, pmin(toBeEvaporated, (sWater[,l]-WLAD_L[l])*DRAINF_L[l]))
    soilEvaporation_L[,l]<-evapHere
    toBeEvaporated<-pmax(0, toBeEvaporated-evapHere)
  }
  soilEvaporation_L[is.na(soilEvaporation_L)] <-0
  
  #water budget
  FLOUT<-pmax((sWater - WLUL_L)*fExtractSoilParameter(paramname="pDrainedFraction", Inf), 0) #flow of water downward
  FLIN<-matrix(0, nrow=nrow(PARAMSIM$cases), ncol=10)
  FLIN[,1]<-rain + cIrrigationWater - cRunoff
  FLIN[,2:10]<-FLOUT[,1:9]
  sWater<-sWater + FLIN - FLOUT - waterUptake_L - soilEvaporation_L
  
  #drainage
  drainLayer<-fExtractSoilParameter("pDrainLayer")
  cDrain<-FLOUT[cbind(1:nrow(FLOUT), drainLayer)]
  #update water content in each layer of soil and everything else
  ALLDAYDATA[,c(paste("sWater", 1:10, sep="."), 
                "cRunoff",
                "cPET",
                "cPotentialSoilEvaporation",
                "sDaysSinceStage2evaporation",
                "cActualSoilEvaporation",
                "cTranspiration", "cDrain")]<<-cbind(as.data.frame(sWater), data.frame(
                  cRunoff,
                  cPET,
                  cPotentialSoilEvaporation,
                  sDaysSinceStage2evaporation,
                  cActualSoilEvaporation,
                  cTranspiration, cDrain
                ))
}


