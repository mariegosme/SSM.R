#fonctions that are not in the core of the SSM model, but are neccessary for SSM.R to run

## special function that returns a logical vector indicating of the given process applies to each case (depending on crop present in the case and its phenology (and the filters in the crop parameters)) 
applyfilters<-function(processname){
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
  possiblecrops<- unique(cultivars)
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
  filtertexts<-ALLCROPS[possiblecrops, paste(processname, "filter", sep=".")]
  if (any(is.null(filtertexts))) stop(
    "process ", processname, " is not in the filter parameters of cultivars ", paste(
     possiblecrops[is.null(filtertexts)], collapse=", "
  ))
  filters<-mapply(FUN=evaluatecrop, filtertexts, possiblecrops, SIMPLIFY=FALSE)
  resultfilter<-mapply(FUN=any, filters)
  if(any(is.na(resultfilter))) stop("Error in applyfilter for process", processname, ": it returned NAs")
  return(resultfilter)
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
  print("initializing the simulation (with crops because sowing hasnt been coded yet" )
  if(is.null(PARAMSIM$simustart)) stop("you didn't define a starting date, use $setoptions to do it")
  if (sum( !(VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"]%in% c("numeric", "character")) )>0) stop("all variables should be either numeric or character, check variables", paste(names(types)[!types %in% c("numeric", "character")], collapse=","))
  df<-fCreateDay(PARAMSIM$simustart-1)
  #we start without crop (sowing in the future, harvest in the past)
  #df$sLastSowing<-Inf 
  #df$sLastHarvest<- (-Inf)
  
  #icicici : actually we initialize with a crop everywhere because crop management hasnt been coded yet
  df$sLastSowing<-0 
  df$sLastHarvest<- (-Inf)
  df$sCrop<-c("WHEAT", "MAIZE", "Chickpea") #written as in the first line in the excel file
  df$sCultivar<-c("Ble_Dur_1", "bidule", "Ghab2") 
  df$sGrowthStage<-c("germination", "SOW", "SOW")
  df$sDurationStage<-c(6, 3, 8.5)
  df$sGrowthStageNumber<-1
  df$sBiologicalDay<-0
  df$sPlantdensity<-280
  
  
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
#' @param paramname name of the parameter to extract
#' @param layer layer from which the parameter should be extracted (single value, or vector of values of the same length as number of cases), or on which the aggregation should be done (=> vector of layers), or Inf to aggregate over all layers, or NULL (default) for general parameters of the soil
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
#' fExtractSoilParameter("pLayerThickness", c(1,3,2)) #returns th value for layer for case 1, layer 3 for case 2 and layer 2 for case 3
#' }
fExtractSoilParameter<-function(paramname, layer=NULL, aggregationfunction=NULL, ...){
  if(is.null(layer)) {
    return(ALLSOILS[[paramname]])
  } else { #layer not null
    if (is.null (aggregationfunction)) {
      if (length(layer)>1 & length(layer)!=length(ALLSOILS$pNLayer)) stop("only one layer should be extracted for each case with fExtractSoilParameter when no aggregation function is given")
      toto<-lapply(ALLSOILS[["paramlayers"]],"[[", paramname)
      return(mapply(function(n,x) return(x[n]), layer, toto))
    } else { #aggregationfunction not null
      toto<-ALLSOILS[["paramlayers"]]
      resultparsol<-lapply(toto, function(x) {
        if(all(is.finite(layer))) cond<-(1:10)==layer else cond<-TRUE
        titi<-do.call(aggregationfunction, c(list(x[cond,paramname], ...)))
        if(length(titi)==0) titi<-NA
        return(titi)
      })
      if (all(unlist(lapply(resultparsol, length))==1)) {
        return(unlist(resultparsol)) 
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
