fCreateDataFrame<-function(d, types, rows) {
  df<-cbind(data.frame(iDate=d),
            as.data.frame(lapply(types, function(x) return(do.call(x, list(length(rows)))))))
  rownames(df)<-rows
  return(df)
}


rCreateDay<-function() { 
  #create frame of data of the day
  laststep<-length(ALLSIMULATEDDATA)
  print(paste("creating day", laststep+1))
  ALLDAYDATA <<- list(fCreateDataFrame(
    d=ALLSIMULATEDDATA[[laststep]][,"iDate"]+1, 
    types=PARAMSIM$types,
    rows=rownames(PARAMSIM$cases)))
  return()
}

rUpdateClimate<-function() {
  print("updating climate")
  step<-length(ALLSIMULATEDDATA)
  date<-ALLSIMULATEDDATA[[step]][1,"iDate"]
  dfclimate<-fGetClimateDay(date=date)
  climatevar<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="climate",]
  climatevariables<-climatevar[,"name"]
  ALLSIMULATEDDATA[[step]][,climatevariables]<<-dfclimate[PARAMSIM$cases$climatename, climatevariables]
}

rUpdateDay<-function() { 
  #add a new day to ALLSIMULATEDDATA
  laststep<-length(ALLSIMULATEDDATA)
  print(paste("creating day", laststep+1))
  ALLSIMULATEDDATA <<- c(ALLSIMULATEDDATA, ALLDAYDATA)
  return()
}