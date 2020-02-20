#fonctions that are not in the core of the SSM model, but are neccessary for SSM.R to run

## special function that returns a logical vector indicating of the given process applies to each case (depending on crop present in the case and its phenology (and the filters in the crop parameters)) 
applyfilters<-function(processname){
  daybefore<-length(ALLSIMULATEDDATA)
  cultivars<-paste(ALLSIMULATEDDATA[[daybefore]]$sCrop, ALLSIMULATEDDATA[[daybefore]]$sCultivar, sep=".")
  possiblecrops<- unique(cultivars)
  evaluatecrop<-function(text, uniquecrop) {
    possiblestages<-names(ALLCROPS[[uniquecrop]]$thresholds)
    numstages<-1:length(possiblestages); names(numstages)<-possiblestages
    is.before<-function(stage, bd=0, crop=uniquecrop) { #if bd is not provided, the stage is not included
      if(!stage %in% possiblestages) stop(stage," used in ", processname, " filter for cultivar ", uniquecrop, " is not in the list of stages for that cultivar")
      whichcases<- (paste(ALLSIMULATEDDATA[[daybefore]]$sCrop, ALLSIMULATEDDATA[[daybefore]]$sCultivar, sep=".")==uniquecrop)
      currentstages<-numstages[ALLSIMULATEDDATA[[daybefore]]$sGrowthStage[whichcases]]
      currentbd<-ALLSIMULATEDDATA[[daybefore]]$sCumulatedPhenoCounts[whichcases]
      targetstage<-which(possiblestages==stage)
      compare<-currentstages==targetstage & currentbd<=bd | currentstages<targetstage
      result<-rep(FALSE, nrow(ALLDAYDATA))
      result[whichcases]<-compare
      return(result)
    }
    is.after<-function(stage, bd=Inf) { #if bd is not provided, the stage is not included
      if(!stage %in% possiblestages) stop(stage," used in ", processname, " filter for cultivar ", uniquecrop, " is not in the list of stages for that cultivar")
      whichcases<- (paste(ALLSIMULATEDDATA[[daybefore]]$sCrop, ALLSIMULATEDDATA[[daybefore]]$sCultivar, sep=".")==uniquecrop)
      currentstages<-numstages[ALLSIMULATEDDATA[[daybefore]]$sGrowthStage[whichcases]]
      currentbd<-ALLSIMULATEDDATA[[daybefore]]$sCumulatedPhenoCounts[whichcases]
      targetstage<-which(possiblestages==stage)
      compare<-currentstages==targetstage & currentbd>=bd | currentstages>targetstage
      result<-rep(FALSE, nrow(ALLDAYDATA))
      result[whichcases]<-compare
      return(result)
    }
    result<-unname(eval(parse(text=text)))
    return(result)
  }
  filtertexts<-sapply(ALLCROPS[possiblecrops], function(cr) {
    if(! processname %in% names(cr)) stop("process ", processname, " is not in the filter parameters of cultivar ", cr$name)
    return(cr[[processname]]$filter)
  })
  filters<-mapply(FUN=evaluatecrop, filtertexts, possiblecrops, SIMPLIFY=FALSE)
  resultfilter<-mapply(FUN=any, filters)
  return(resultfilter)
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


rSetParamsFromCrops<-function(){
  df<-ALLDAYDATA[,VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel =="CropParameter", "name"]]
  cultivars<-paste(ALLDAYDATA$sCrop, ALLDAYDATA$sCultivar, sep=".")
  possiblecrops<-unique(cultivars)
  for (c in possiblecrops) {
    for (modu in unique(VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel =="CropParameter", "module"])) {
      paramnames<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$typeinthemodel =="CropParameter" & VARIABLEDEFINITIONS$module==modu, "name"]
      missing<-setdiff(paramnames, names(ALLCROPS[[c]][[modu]]))
      if(length(missing)>0) stop("the following parameters are mising from crop parameters for ", c, ": ", paste(missing, collapse=", "))
      df[cultivars==c, paramnames]<-ALLCROPS[[c]][[modu]][paramnames]
    }
  }
  ALLDAYDATA[,names(df)]<<-df
  return()
}
  