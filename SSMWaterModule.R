
#' Extraction of soil Parameters
#'
#' @param paramname name of the parameter to extract
#' @param layer layer from which the parameter should be extracted (single value), or on which the aggregation should be done (=> vector of layers), or Inf to aggregate over all layers, or NULL (default) for general parameters of the soil
#' @param aggregationfunction name of function to aggregate over given layers, or over all layers (if layer=Inf), must be a valid function name (e.g. sum, mean, min, max, cumsum etc...)
#' @param ... additional parameters to pass to aggregationfunction (e.g. na.rm=TRUE)
#' @return vector (one element per case) of parameters, except if aggregationfunction returns a vector, in which case, it returns a data.frame with cases as columns and the dimension of the return value of the aggregation function as rows
#' @examples
#' \dontrun{ 
#' fExtractSoilParameter("pSoilAlbedo") #extracts a single parameter from all cases (parameter of the whole soil)
#' fExtractSoilParameter("pLayerThickness", 2) #extracts a parameter from a single layer (layer-specific parameter)
#' fExtractSoilParameter("pLayerThickness", 1:2, "sum", na.rm=TRUE) #computes an aggregated value over n layers
#' fExtractSoilParameter("pLayerThickness", Inf, "cumsum") # computes an aggregated value over all layers, returning cases as columns and layers as rows
#' fExtractSoilParameter("pLayerThickness", Inf, "range") # computes an aggregated value over all layers, returning a data.frame with cases as columns, and 2 rows (min and max)
#' fExtractSoilParameter("pLayerThickness", Inf, "I") #returns the values "as.is" but with cases in colums and layers in rows
#' }
fExtractSoilParameter<-function(paramname, layer=NULL, aggregationfunction=NULL, ...){
  if(is.null(layer)) {
    return(PARAMSSOILS[[paramname]])
    } else { #layer not null
    if (is.null (aggregationfunction)) {
      if (length(layer)>1) stop("only one layer should be extracted with fExtractSoilParameter when no aggregation function is given")
      toto<-PARAMSSOILS[["paramlayers"]]
      return(unlist(lapply(toto, function(x) {
        titi<-x[x$layer==layer,paramname]
        if(length(titi)==0) titi<-NA
        return(titi)
      })))
    } else { #aggregationfunction not null
      toto<-PARAMSSOILS[["paramlayers"]]
      resultparsol<-lapply(toto, function(x) {
        if(all(is.finite(layer))) cond<-(1:10)==layer else cond<-TRUE
        titi<-do.call(aggregationfunction, c(list(x[cond,paramname], ...)))
        if(length(titi)==0) titi<-NA
        return(titi)
      })
      if (all(unlist(lapply(resultparsol, length))==1)) {
        return(unlist(resultparasol)) 
      } else {
        lengthmax<-max(unlist(lapply(resultparsol, length)))
        resultparsol<-lapply(resultparsol, function(x) c(x, rep(NA, lengthmax-length(x))))
        names(resultparsol)<-paste("case", 1:length(resultparsol), sep="")
        #colonnes: cases, lignes: profondeurs du plancher de chaque couche
        return(as.data.frame(resultparsol, row.names=paste("layer", 1:lengthmax)))
      }
    }
  }
}


#' Computes the ATSW (available transpirable water) in a given layer (in all cases)
#' @param layer target soil layer (from 1 to 10 : hard-coded max number of soil layers)
#' @param df data.frame of state variables (ALLDAYDATA or ALLSIMULATEDDATA)
#' @return vector of ATSW in all cases, in the target layer 
#' @examples
#'\dontrun{
#'#doesn't run because 
#'fFindWater(3, df=data.frame(sWater.1=c(1,2,3),
#'sWater.2=c(1,1,1),
#'sWater.3=c(0.5,0.6,0.7),
#'sWater.4=c(NA, 3,2),
#'sRootFrontDepth=c(500,400,600)), what="ATSW")
#'}
fFindWater<-function(layer, df, what=c("ATSW", "TTSW")){
  what<-what[1]
  WL<-unlist(df[,paste("sWater", layer, sep=".")])
  if (what=="ATSW") {
    WLL<-fExtractSoilParameter("pWiltingPoint", layer)
    ATSW<-WL-WLL
    return(ATSW)
  } else if (what=="TTSW") {
    WDUL<-fExtractSoilParameter("pFieldCapacity", layer)
    TTSW<-WL-WDUL
    return(TTSW)
  }
}

#' returns ATSWRZ or TTSWRZ (sum over all layers, of the value (ATSW or TTSW) weighted by the length of proportion of the layer having roots)
#' @param what = "ATSW" or "TTSW"
#' @param nlayersmax maximum number of layers (10 by default)
#' @param df data.frame of state variables (ALLDAYDATA or ALLSIMULATEDDATA)
#' @return vector (one element per case) of ATSWR or TTSWR
#' @examples
#' \dontrun{
#' 
#' }
fFindwaterRZ<-function(what=c("ATSW", "TTSW"), nlayersmax=10, df){
  layers<-1:nlayersmax
  names(layers)<-paste("layer", layers)
  allwater<-as.data.frame(t(as.matrix(as.data.frame(lapply(layers, fFindwater, what=what, df=df)))))#returns list (of cases)one element per case), of vectors (one element per layer)
  alldepths<-fExtractSoilParameter("pLayerThickness", Inf, "I") #colonnes: cases, lignes: epaisseur
  allroots<-mapply(fFindRLYER,
                   df[,"sRootFrontDepth"], #vecteur (cases) de profondeur de racines
                   alldepths, SIMPLIFY=FALSE
  ) #returns list (of cases)one element per case), of vectors (one element per layer)
  return(unlist(lapply(mapply(function(x,y,z) return(x*y/z), allwater, alldepths, allroots, SIMPLIFY=FALSE), sum, na.rm=TRUE)))
}

#' Find the thickness of the rooted part of each layer
#'
#' @param rootFrontDepth depth of the front of root colonisation (atomic value)
#' @param LayerThicknesses vector of layer thickness (one element per layer)
#'
#' @return a vector of rooted thickness (one element per layer): when the roots go deeper, it is the depth of the layer, and in the last rooted layer, it is the length of roots inside this layer
#' @examples
#' thickness<-c(1,3,2,1)
#' rootdepth<-5
#' fFindRLYER(rootdepth, thickness)
fFindRLYER<-function(rootFrontDepth, LayerThicknesses){
  return(pmin(pmax(rootFrontDepth-(cumsum(LayerThicknesses)-LayerThicknesses),0), LayerThicknesses))
}


rRootDepth<-function(){
  daybefore<-length(ALLSIMULATEDDATA)
  currentcropcult<-paste(ALLDAYDATA[,"sCrop"], ALLDAYDATA[,"sCultivar"], sep=".")
  grtd<-ALLDAYDATA[,"cBiologicalDay"]*PARAMSCROPS[[currentcropcult]]$rRootDepth$pPotentialRootGrowth #potential growth
  grtd<-grtd*applyfilters("rRootDepth") #filter on stage (crop parameter)
  grtd[ALLDAYDATA[,"cDryMatterProduction"]==0 #in case no biomass accumulation
       | ALLSIMULATEDDATA[[daybefore]][,"sRootFrontDepth"]>=ALLDAYDATA[,"pMaxDepthWaterExtraction"] #or root depth already at the maximum water extraction depth
       | ALLSIMULATEDDATA[[daybefore]][,"sRootFrontDepth"]>=fExtractSoilParameter("pLayerThickness", Inf, "sum", na.rm=TRUE) #or root already at hte bottom of the soil
  ]<-0 #then no root growth
  rtln<-mapply(function(floors,rootdepths) which.max(floors[floors<rootdepths])+1,
               fExtractSoilParameter("pLayerThickness", Inf, "cumsum"), #colonnes: cases, lignes: profondeurs du plancher de chaque couche
               ALLSIMULATEDDATA[[daybefore]][,"sRootFrontDepth"] #vecteur (cases) de profondeur de racines
               ) #vector (one element per case) of number of the lowest layer with roots
  grtd[fFindWater(rtln, what="ATSW", df=ALLSIMULATEDDATA[[daybefore]])==0]<-0 #in case the lowest layer with roots (=the layer of root tips) is dry, no growth
  ALLDAYDATA[,"sRootFrontDepth"]<<-ALLSIMULATEDDATA[[daybefore]][,"sRootFrontDepth"]+grtd
  return()
}

#' Computes runoff
#'
#' @param vectorRain (corrected) rain
#' @param vectorCurveNumber Curve number  describing the soil infiltration capacity 
#' @param vectorSAT water content at full saturation in top layer
#' @param vectorlayerthickness thickness of top soil layer 
#' @param vectorwatercontent water content in soil layer 1
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
                         vectorlayerthickness,
                         vectorwatercontent,
                         vectorWLL){
  S <- 254 * (100 / vectorCurveNumber - 1)
  SWER <- max(0, 0.15 * ((vectorSAT*vectorlayerthickness - vectorwatercontent) / (vectorSAT*vectorlayerthickness - vectorWLL)))
  vectorrunof<-ifelse(vectorrain-SWER*S<0, 0, (vectorrain - SWER * S) ^ 2 / (vectorrain + (1 - SWER) * S))
  vectorrunof[rain<=0.01]<-0
  return(vectorrunof)
}

#' Potential evapotranspiration following simplified Penman equation
#'
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


rWaterBudget<-function(){
  daybefore<-length(ALLSIMULATEDDATA)
  currentcropcult<-paste(ALLDAYDATA[,"sCrop"], ALLDAYDATA[,"sCultivar"], sep=".")
  
  #input rain+melted snow
  rain<-ALLDAYDATA[,"cPrCorrected"]
  
  #compute runoff
  runof<-fComputeRunoff(vectorrain=rain,
                        vectorCurveNumber=fExtractSoilParameter("pSoilCurveNumber"), 
                        vectorSAT=fExtractSoilParameter("pSaturation", layer=1),
                        vectorlayerthickness=fExtractSoilParameter("pLayerThickness", layer=1),
                        vectorwatercontent=ALLSIMULATEDDATA[[daybefore]][,"sWater.1"],
                        vectorWLL=fExtractSoilParameter("pWiltingPoint", layer=1)
  )
  #find LAI useful to compute soil evaporation (= real LAI until beginning of seed growth, and then LAI at stage TLP (which is not necessarily BSG))
  etlai<-ALLSIMULATEDDATA[[daybefore]][,"sBuLlShitLAI"] # beginning of leaf senescence LAI?? mis a jour dans rUpdateLAI ... TO DO !!!!
  #compute PET
  pet<-fComputePETsimplifiedPenman(tmax=ALLDAYDATA[,"iTASMax"], 
                                   tmin=ALLDAYDATA[,"iTASMin"], 
                                   srad=ALLDAYDATA[,"iRSDS"], 
                                   calb=GENERALPARAMETERS$pCropAlbedo, 
                                   ket=GENERALPARAMETERS$pCanopyExtinctionCoefficient, 
                                   etlai=etlai , salb=fExtractSoilParameter("pSoilAlbedo"))
  #compute soil evaporation
  soilevaporation <- pet * exp(-GENERALPARAMETERS$pCanopyExtinctionCoefficient * etlai)
  soilevaporation[pet>GENERALPARAMETERS$pMinimalSoilEvaporation & soilevaporation<GENERALPARAMETERS$pMinimalSoilEvaporation]<-GENERALPARAMETERS$pMinimalSoilEvaporation
  #modify to take into account the effect of mulch
  soilevaporation <- soilevaporation * (1.5 - 0.2 * log((100 * unlist(lapply(ITKPARAMETERS, "[[", "pStubleWeight")))))
  #real soil evaporation 
  SEVP<-soilevaporation
  DYSE<-ALLSIMULATEDDATA[[daybefore]][,"sDaysSinceSoilWettingWater"]
  DYSE[rain+ALLDAYDATA["cIrrigationWater"]>GENERALPARAMETERS$pSoilWettingWaterQuantity]<-1
  FTSWRZ<-fFindWaterRZ(what="ATSW", nlayersmax=10, df=ALLSIMULATEDDATA[[daybefore]])/fFindWaterRZ(what="TTSW", nlayersmax=10, df=ALLSIMULATEDDATA[[daybefore]])
  conditionstageII<-DYSE>1 | FTSWRZ<0.5 | fFindWater(layer=1, df=ALLSIMULATEDDATA[[daybefore]], what="ATSW")<=1
  SEVP[conditionstageII]<-(soilevaporation*((DYSE+1)^0.5-DYSE^0.5))[conditionstageII]
  DYSE[conditionstageII]<-DYSE[conditionstageII]+1 
 
  #plant transpiration
  VPTMIN<- 0.6108 * Exp(17.27 * ALLDAYDATA["iTASMin"] / (ALLDAYDATA["iTASMin"] + 237.3))
  VPTMAX<- 0.6108 * Exp(17.27 * ALLDAYDATA["iTASMax"] / (ALLDAYDATA["iTASMax"] + 237.3))
  VPD = VPDF * (VPTMAX - VPTMIN) #warning: in SSM.R, VPDF has been moved from location-specific parameters to soil parameters
  TR = DDMP * VPD / TEC         #VPD in kPa, TEC in Pa

  #compute water uptake
  rlyer<-mapply(fFindRLYER,
                ALLDAYDATA[,"sRootFrontDepth"], #vecteur (cases) de profondeur de racine
                fExtractSoilParameter("pLayerThickness", Inf, "I") #colonnes: cases, lignes: epaisseur
                
  ) 
  
  
  #update water content in each layer of soil and everything else
  ALLDAYDATA$sDaysSinceSoilWettingWater<<-DYSE
  ALLDAYDATA$
  
}




PARAMSSOILS<-list(pNLayer=c(2,2,3), pDrainLayer=c(2,2,3), pSoilAlbedo=c(0.12, 0.13,0.15), U=NA, pSoilCurveNumber=c(60,70,80), 
                  paramlayers=list(data.frame(layer=1:2, pLayerThickness=c(300,700), pSaturation=c(0.36, 0.40), pFieldCapacity=c(0.24, 0.25), pWiltingPoint=c(0.1, 0.12), pSoilDryness=c(0.03, 0.04)),
                                   data.frame(layer=1:2, pLayerThickness=c(200,800), pSaturation=c(0.36, 0.40), pFieldCapacity=c(0.24, 0.25), pWiltingPoint=c(0.1, 0.14), pSoilDryness=c(0.04, 0.04)),
                                   data.frame(layer=1:3, pLayerThickness=c(300,200,400), pSaturation=c(0.36, 0.40, 0.38), pFieldCapacity=c(0.24, 0.25, 0.22), pWiltingPoint=c(0.1, 0.12, 0.09), pSoilDryness=c(0.03, 0.04, 0.035))
                  )
)

ALLSIMULATEDDATA<-list(data.frame(sWater.1=c(1,2,3),
                       sWater.2=c(1,1,1),
                        sWater.3=c(NA,NA,0.7)))

ALLDAYDATA<-data.frame(sWater.1=c(1,2,3),
                            sWater.2=c(1,1,1),
                            sWater.3=c(0.5,0.6,0.7),
                            sWater.4=c(NA, 3,2),
                            sRootFrontDepth=c(500,400,600))


fFindwater(3, what="ATSW")






databidon<-data.frame(pLayerThickness.1=c(1,1,1), pLayerThickness.2=c(1,2,3), pLayerThickness.3=c(1,1,1), pLayerThickness.4=c(3,2,1))
profracines<-c(3,3,5)
profondeurs<-as.data.frame(apply(databidon[,paste("pLayerThickness", 1:4, sep=".")], 1, cumsum))

mapply(function(floors,rootdepths) which.max(floors[floors<rootdepths])+1,
       as.data.frame(apply(databidon[,paste("pLayerThickness", 1:4, sep=".")], 1, cumsum)), #colonnes: cases, lignes: profondeurs du plancher de chaque couche
       profracines #vecteur (cases) de profondeur de racines
) #vector (one element per case) of number of the lowest layer with roots

mapply(function(thickness,rootdepth) pmin(pmax(rootdepth-(cumsum(thickness)-thickness),0), thickness),
       as.data.frame(t(databidon[,paste("pLayerThickness", 1:4, sep=".")])), #colonnes: cases, lignes: epaisseur
       profracines #vecteur (cases) de profondeur de racines
) 
mapply(fFindRLYER,
       
       profracines, #vecteur (cases) de profondeur de racines
       as.data.frame(t(databidon[,paste("pLayerThickness", 1:4, sep=".")])) #colonnes: cases, lignes: epaisseur
) 


