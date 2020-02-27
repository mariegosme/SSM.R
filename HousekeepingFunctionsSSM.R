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


#creation of day 0 
rCreateDay0<-function() {
  print("initializing the simulation (with crops because sowing hasnt been coded yet" )
  if(is.null(PARAMSIM$simustart)) stop("you didn't define a starting date, use $setoptions to do it")
  ##### creation of day 0 (before start of simulation)
  vnames<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "name"]
  types<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"]
  names(types)<-vnames
  ##warning: this lapply works only for numeric and character!!!!!!
  if (sum( !types %in% c("numeric", "character"))>0) stop("all variables should be either numeric or character, check variables", paste(names(types)[!types %in% c("numeric", "character")], collapse=","))
  df<-cbind(data.frame(iDate=PARAMSIM$simustart-1),
            as.data.frame(lapply(types, FUN=function(x) return(do.call(x, list(nrow(PARAMSIM$cases)))))))
  rownames(df)<-rownames(PARAMSIM$cases)
  #we initialize the state variables with their default value in allvariables.xlsx
  for(v in vnames[substr(vnames, start=1, stop=1)=="s"]) {
    df[,v]<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name==v,"defaultInitialvalue"]
    if(VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name==v,"typeR"]=="numeric") df[,v]<-as.numeric(df[,v])
  }
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
  types<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"]
  names(types)<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "name"]
  df<-cbind(data.frame(iDate=dateday),
            as.data.frame(lapply(types, function(x) return(do.call(x, list(nrow(PARAMSIM$cases)))))))
  rownames(df)<-rownames(PARAMSIM$cases)
  #initialize state variables to the state at preceding timestep
  df[,substr(x=colnames(df), start=1, stop=1)=="s"]<-ALLSIMULATEDDATA[[daybefore]][,substr(x=colnames(df), start=1, stop=1)=="s"]
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

  