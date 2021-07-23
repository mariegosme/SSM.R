#fonctions that are not in the core of the SSM model, but are neccessary for SSM.R to run

## special function that returns a logical vector indicating of the given process applies to each case (depending on crop present in the case and its phenology (and the filters in the crop parameters)) 
applyfilters<-function(processname){
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
  possiblecrops<- setdiff(unique(cultivars), "NA.NA")
  if(length(possiblecrops)>0){
    evaluatecrop<-function(text, uniquecrop) {
      possiblestages<-names(ALLCROPS[uniquecrop,"thresholds"][[1]])
      numstages<-1:length(possiblestages); names(numstages)<-possiblestages
      is.before<-function(stage, bd=-Inf, crop=uniquecrop) { #if bd is not provided, the stage is not included
        if(!stage %in% possiblestages) stop(stage," used in ", processname, " filter for cultivar ", uniquecrop, " is not in the list of stages for that cultivar")
        whichcases<- (paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")==uniquecrop)
        currentstages<-numstages[ALLDAYDATA$sGrowthStage[whichcases]]
        currentbd<-ALLDAYDATA$sBiologicalDay[whichcases]
        targetstage<-which(possiblestages==stage)
        compare<-currentstages==targetstage & currentbd<=bd | currentstages<targetstage
        result<-rep(FALSE, nrow(ALLDAYDATA))
        result[whichcases]<-compare
        return(result)
      }
      is.after<-function(stage, bd=Inf) { #if bd is not provided, the stage is not included
        if(!stage %in% possiblestages) stop(stage," used in ", processname, " filter for cultivar ", uniquecrop, " is not in the list of stages for that cultivar")
        whichcases<- (paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")==uniquecrop)
        currentstages<-numstages[ALLDAYDATA$sGrowthStage[whichcases]]
        currentbd<-ALLDAYDATA$sBiologicalDay[whichcases]
        targetstage<-which(possiblestages==stage)
        compare<-currentstages==targetstage & currentbd>=bd | currentstages>targetstage
        result<-rep(FALSE, nrow(ALLDAYDATA))
        result[whichcases]<-compare
        return(result)
      }
      result<-unname(eval(parse(text=text)))
      return(result)
    }
    filtertexts<-rep("FALSE", nrow(ALLDAYDATA))
    filtertexts[cultivars %in% possiblecrops]<-ALLCROPS[cultivars[cultivars %in% possiblecrops], paste(processname, "filter", sep=".")]
    names(filtertexts)<-rownames(ALLDAYDATA)
    if (any(is.null(filtertexts))) stop(
      "process ", processname, " is not in the filter parameters of cultivars ", paste(
        possiblecrops[is.null(filtertexts)], collapse=", "
      ))
    filters<-mapply(FUN=evaluatecrop, text=filtertexts, uniquecrop=cultivars, SIMPLIFY=FALSE)
    resultfilter<-mapply(FUN=any, filters)
    if(any(is.na(resultfilter))) stop("Error in applyfilter for process ", processname, ": it returned NAs")
    return(resultfilter)
  } else return(FALSE)
}

fCreateDay<-function(dateday){
  types<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"]
  names(types)<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "name"]
  df<-cbind(data.frame(iDate=dateday),
            as.data.frame(lapply(types, function(x) return(do.call(x, list(nrow(PARAMSIM$cases)))))))
  rownames(df)<-rownames(PARAMSIM$cases)
 #initialize all variables as their default values
  df[,setdiff(names(df), "iDate")]<-lapply(setdiff(names(df), "iDate"), function(x) VARIABLEDEFINITIONS[x, "defaultInitialvalue"])
  df[,names(types)[types=="numeric"]]<-lapply(df[,names(types)[types=="numeric"]],as.numeric)
  return(df)
}

#creation of day 0 
rCreateDay0<-function() {
  print("initializing the simulation" )
  if(is.null(PARAMSIM$simustart)) stop("you didn't define a starting date, use $setoptions to do it")
  types<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"] ; names(types)<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "name"]
  if (sum( !(types %in% c("numeric", "character", "factor")) )>0) stop("all variables should be either numeric or character or factor, check variables ", paste(names(types)[!types %in% c("numeric", "character")], collapse=","))
  df<-fCreateDay(PARAMSIM$simustart-1)
  df$sCropCult<-sapply(PARAMSIM$cases$rotation,"[[", 1)
  df$sManagement<-sapply(PARAMSIM$cases$management,"[[", 1)
  df$sStubleWeight<-sapply(ALLMANAGEMENTS[df$sManagement], function(x) x$dfSowing$STBLW) #because it is necessary before sowing so it cannot be initialized at sowing like the other variables
  df$cCycleEndType<-factor("not yet", levels=c("normal", "low LAI", "not sowed", "stopDAP", "killed by flood", "not yet"))
    
  #commented out now that crop management has been coded 
  # if(FALSE) {
  #   df$sLastSowing<-0
  #   df$sLastHarvest<- (-Inf)
  #   df$sCrop<-c("WHEAT", "MAIZE", "Chickpea") #written as in the first line in the excel file
  #   df$sCultivar<-c("Ble_Dur_1", "bidule", "Ghab2")
  #   df$sGrowthStage<-c("germination", "SOW", "SOW")
  #   df$sDurationStage<-c(6, 3, 8.5)
  #   df$sGrowthStageNumber<-1
  #   df$sBiologicalDay<-0
  #   df$sPlantdensity<-280
  #   df$sStubleWeight<-2
  #   df$sRootFrontDepth<-200
  # }
  
  NCASE <- nrow(PARAMSIM$cases) # number of cases
  NLYER <- fExtractSoilParameter("pNLayer") # number of layers
  
  # ---- initialization of soil water ----
  df[,paste("sWater", 1:10, sep=".")]<- lapply(
    1:10, function(x) 
      fExtractSoilParameter(paramname="pInitialWater", layers=x)*fExtractSoilParameter(paramname="pLayerThickness", layers=x))

  
  # ---- initialization of soil nitrogen ----
  
  # -- compute soilMass for each layer (to compute initial values) --
  soilMasses <- fComputeSoilMass(
    fExtractSoilParameter(paramname="pLayerThickness", layers=Inf),
    fExtractSoilParameter(paramname="pSoilBulkDensity", layers=Inf),
    fExtractSoilParameter(paramname="pCoarseSoilFraction", layers=Inf))
  
  # -- initialization of MNORG --
  df[,paste("sMineralizableN", 1:10, sep=".")] <- fComputeMineralizableN(
    soilMasses,
    fExtractSoilParameter(paramname="pInitialOrgNPercentage", layers=Inf),
    fExtractSoilParameter(paramname="pFractionMineralizableN",layers=Inf)
  )
  
  # -- initialization of NSOL --
  df[,paste("sSolubleN", 1:10, sep=".")] <- fComputeInitialSolubleN(
    soilMasses,
    fExtractSoilParameter(paramname="pInitialNO3Concentration", layers=Inf),
    fExtractSoilParameter(paramname="pInitialNH4Concentration", layers=Inf)
  )
  
  # -- initialization of NCON --
  df[,paste("sSolubleNConcentration", 1:10, sep=".")] <- fComputeNConcentration(
    df[,paste("sSolubleN", 1:10, sep=".")],
    df[,paste("sWater", 1:10, sep=".")]
  )
  
  # -- initialization of NAVL and NORG
  for (case in 1:NCASE) {
    df[case,paste("sAvailableUptakeN", 1:NLYER[case], sep=".")] <- 0
    df[case,paste("sOrganicN", 1:NLYER[case], sep=".")] <- 0
  }
  # SNAVL is initialized at 0 thanks to its default value in excel = 0
  
  ALLSIMULATEDDATA<<-list(df) #list of data.frames from the previous timesteps (here: day 0)
  return()
}


# creates a new data.frame at the beginning of the day
rCreateDay<-function() { 
  #create frame of data of the day 
  daybefore<-length(ALLSIMULATEDDATA)
  print(paste("creating day", daybefore)) #because there is a day 0
  dateday<-ALLSIMULATEDDATA[[daybefore]][,"iDate"]+1
  df<-fCreateDay(dateday)
  #initialize state variables to the state at preceding timestep
  statevars<-names(df)[substr(x=colnames(df), start=1, stop=1)=="s"]
  df[,statevars]<-ALLSIMULATEDDATA[[daybefore]][,statevars]
  ALLDAYDATA<<-df
  return()
}

#sets the parameters in ALLDAYDATA from the current crop in each case
rSetParamsFromCrops<-function(){
  df<-ALLDAYDATA[,VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel =="CropParameter", "name"]]
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
  possiblecrops<-unique(cultivars)
  missing<-setdiff(names(df), names(ALLCROPS))
  if(length(missing)>0) warning("the following parameters are mising from crop parameters: ", paste(missing, collapse=", "))
  paramstobechanged<-intersect(names(df), names(ALLCROPS))
  df[, paramstobechanged]<-ALLCROPS[cultivars, paramstobechanged]
  ALLDAYDATA[,names(df)]<<-df
  return()
}


#' Extraction of soil Parameters
#'
#' @param paramname name of the parameter to extract (single value)
#' @param layers layer(s) from which the parameter should be extracted (single value, or vector of values of the same length as number of cases), or on which the aggregation should be done (=> vector of layers), or Inf to aggregate over all layers, or NULL (default) for general parameters of the soil
#' @param aggregationfunction name of function to aggregate over given layers, or over all layers (if layer=Inf), must be a valid function name (e.g. sum, mean, min, max, cumsum etc...)
#' @param keepcasesasrows should the result be kept as a matrix with cases as rows and layers as columns (default FALSE: returns a vector if possible)
#' @param ... additional parameters to pass to aggregationfunction (e.g. na.rm=TRUE)
#' @return vector (one element per case) of parameters, except if aggregationfunction returns a vector, in which case, it returns a data.frame with cases as columns and the dimension of the return value of the aggregation function as rows
#' @examples
#' \dontrun{ 
#' fExtractSoilParameter("pSoilAlbedo") #extracts a single parameter from all cases (parameter of the whole soil)
#' fExtractSoilParameter("pLayerThickness", Inf) #extracts a single parameter from all cases and all layers
#' fExtractSoilParameter("pLayerThickness", 2) #extracts a parameter from a single layer (layer-specific parameter)
#' fExtractSoilParameter("pLayerThickness", 1:2, "sum", na.rm=TRUE) #computes an aggregated value over n layers
#' fExtractSoilParameter("pLayerThickness", Inf, "cumsum") # computes an aggregated value over all layers, returning cases as columns and layers as rows
#' fExtractSoilParameter("pLayerThickness", Inf, "range", na.rm=TRUE) # computes an aggregated value over all layers, returning a data.frame with cases as columns, and 2 rows (min and max)
#' fExtractSoilParameter("pLayerThickness", 1:5, "I") #returns the values "as.is" 
#' fExtractSoilParameter("pLayerThickness", c(1,3,2)) #returns th value for layer 1 for case 1, layer 3 for case 2 and layer 2 for case 3
#' fExtractSoilParameter("pLayerThickness", 1:10) #error: if there is no aggregationfunction and if layers is given, it should be the same length as the number of cases (to extract one layer per case) ; to extract all layers, use Inf for layers
#' fExtractSoilParameter("pLayerThickness", c(2,1), whichcases=c(T,F,T)) #returns the value for layer 2 for case 1, and layer 1 for case 3
#' }
fExtractSoilParameter<-function(paramname, layers=NULL, aggregationfunction=NULL, whichcases=TRUE, keepcasesasrows=FALSE, ...){
  if(is.null(layers)) {
    return(ALLSOILS[[paramname]][whichcases])
  } else { #layers not null
    #extracts the parameter
    toto<-matrix(ALLSOILS$paramlayers[whichcases,paramname,], ncol=10) #to maintain a matrix structure even if whichcases returns only 1 case
    if (is.null (aggregationfunction)) {
      if (length(layers)>1 & length(layers)!=dim(toto)[1]) stop("only one layer per case should be extracted with fExtractSoilParameter when no aggregation function is given")
      if (all(is.finite(layers))) {
        #extracts the layers
        if(length(layers)==1 & keepcasesasrows){
          toto<-toto[,layers, drop=FALSE]
        } else toto<-toto[cbind(1:nrow(toto), layers)]
      } #if layers is finite, returns a vector, if layers is Inf, returns a matrix with cases as rows and layers as columns
      return(toto)
    } else { #aggregationfunction not null
      if(all(is.finite(layers))) cond<-(1:10) %in% layers else cond<-TRUE
      toto<-apply(X=toto[,cond, drop=FALSE], MARGIN=1, FUN=aggregationfunction, ...)
      if(!is.null(dim(toto))) toto<-t(toto) #if toto is not a vector, put the cases in rows and the other dimension in columns
      return(toto)
    }
  }
}