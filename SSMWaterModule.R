
#' Extraction of soil Parameters
#'
#' @param paramname name of the parameter to extract
#' @param layer layer from which the parameter should be extracted (single value), or on which the aggregation should be done (=> vector of layers), of Inf to aggregate over all layers, or NULL (default) for general parameters of the soil
#' @param aggregationfunction name of function to aggregate over given layers, or over all layers (if layer=Inf), must be a valid function name (e.g. sum, mean, min, max, cumsum etc...)
#' @param ... additional parameters to pass to aggregationfunction (e.g. na.rm=TRUE)
#' @return vector (one element per case) of parameters
#' @examples
#' \dontrun{ 
#' fExtractSoilParameter("pSoilAlbedo")
#' fExtractSoilParameter("pLayerThickness", 2)
#' fExtractSoilParameter("pLayerThickness", 1:2, "sum", na.rm=TRUE)
#' fExtractSoilParameter("pLayerThickness", Inf, "cumsum")
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
#' @param layer target soil layer
#' @return vector of ATSW in all cases, in the target layer 
#' @examples
#'\dontrun{
#'fFindATSW(3)
#'}
fFindATSW<-function(layer){
  daybefore<-length(ALLSIMULATEDDATA)
  WL<-unlist(ALLSIMULATEDDATA[[daybefore]][,paste("sWater", layer, sep=".")])
  WLLL<-fExtractSoilParameter("pWiltingPoint", layer)
  ATSW<-WL-WLL
  return(ATSW)
}

#' Find the thickness of the rooted part of each layer
#'
#' @param rootFrontDepth depth of the front of root colonisation (atomic value)
#' @param LayerThicknesses vector of layer thickness (o,e element per layer)
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
  grtd[fFindATSW(rtln)==0]<-0 #in case the lowest layer with roots (=the layer of root tips) is dry, no growth
  ALLDAYDATA[,"sRootFrontDepth"]<<-ALLSIMULATEDDATA[[daybefore]][,"sRootFrontDepth"]+grtd
  return()
}

rWaterBudget<-function(){
  #input rain+melted snow
  rain<-ALLDAYDATA[,"cPrCorrected"]
  
  #compute runoff
  runof<-numeric(length(RAIN))
  S<-254*(100/ pSoilCurveNumber -1)
  runof[rain>0.01]<-
  #compute infiltration in each layer
  #compute ETP
  #compute water uptake
  rlyer<-mapply(fFindRLYER,
                ALLDAYDATA[,"sRootFrontDepth"], #vecteur (cases) de profondeur de racine
                fExtractSoilParameter("pLayerThickness", Inf, "I") #colonnes: cases, lignes: epaisseur
                
  ) 
  
  #update water content in each layer of soil
}




PARAMSSOILS<-list(pNLayer=c(2,2,3), pDrainLayer=c(2,2,3), pSoilAlbedo=c(0.12, 0.13,0.15), U=NA, pSoilCurveNumber=c(60,70,80), 
                  paramlayers=list(data.frame(layer=1:2, pLayerThickness=c(300,700), pSaturation=c(0.36, 0.40), pFieldCapacity=c(0.24, 0.25), pWiltingPoint=c(0.1, 0.12), pSoilDryness=c(0.03, 0.04)),
                                   data.frame(layer=1:2, pLayerThickness=c(200,800), pSaturation=c(0.36, 0.40), pFieldCapacity=c(0.24, 0.25), pWiltingPoint=c(0.1, 0.14), pSoilDryness=c(0.04, 0.04)),
                                   data.frame(layer=1:3, pLayerThickness=c(300,200,400), pSaturation=c(0.36, 0.40, 0.38), pFieldCapacity=c(0.24, 0.25, 0.22), pWiltingPoint=c(0.1, 0.12, 0.09), pSoilDryness=c(0.03, 0.04, 0.035))
                  )
)
















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

thickness<-c(1,3,5)
rootdepth<-10
pmin(pmax(rootdepth-(cumsum(thickness)-thickness),0), thickness)
