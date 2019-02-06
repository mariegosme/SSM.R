



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



  