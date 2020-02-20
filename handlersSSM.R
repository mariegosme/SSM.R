#mXXXX: Model interface (e.g. mrun, mplot)

mOnetimestep<-function(){
  rCreateDay() #creates ALLDAYDATA with the values initialised at their default values, except stat variables that are initialised at their value from the day before

  rWeatherDay()
  #rUpdatePAR() not coded, and I don't know what is was supposed to be, maybe it was to update PAR depending on tree interception once the agroforestry module would be added?
  rUpdateManagement() #for now, just keeps the crops and cultivars as the day before
  rUpdatePhenology()
  rUpdateLAI()
  rUpdateDMProduction()
  #rUpdateDMDistribution() not codd yet
  
  #at the end of the timestep, adds the new day at the end of the list of all variables
  ALLSIMULATEDDATA <<- c(ALLSIMULATEDDATA, list(ALLDAYDATA))
  return()
}

mRun<-function(howlong=1)
{
  replicate(howlong, mOnetimestep())
  return(paste("the model ran for", howlong, "time steps"))
}

mPlotDynamics<-function(variablestoplot, 
                        colors=NULL, symbols=NULL, linetypes=NULL, 
                        whatcolors=c("cases", "variables"), whatsymbols=c("cases", "variables"), whatlinetypes=c("cases", "variables"),
                        ylim=NULL, xlim=NULL,...)  {
  #variablestoplot: vector of variable names from ALLSIMULATEDDATA
  #colors: named vector of colors, named according to the names of cases or variables, depending on the value of whatcolors, or one color for all points
  #symbols: named vector of symbols, named according to the names of cases or variables, depending on the value of whatsymbols, or one symbol for all points
  #linetypes: named vector of linetypes, named according to the names of cases or variables, depending on the value of whatlinetypes, or one type for all lines
  names(variablestoplot)<-variablestoplot
  if(length(ALLSIMULATEDDATA)==0) stop("You cannot plot a model that hasn't been run for at least 1 time step") 
  #extracts the dynamics of each variable
  dynamics<-lapply(variablestoplot, function(v) as.matrix(as.data.frame(lapply(ALLSIMULATEDDATA, function(x) x[,v, drop=FALSE]))))
  #dynamics is a list (one element for each variable) of matrices (rows= cases, columns=timesteps)
  if (is.null(ylim)) ylim<-range(unlist(dynamics), na.rm=TRUE)
  dates<-sapply(ALLSIMULATEDDATA, function(x) x[1,"iDate"])
  if (is.null(xlim)) xlim<-range(dates, na.rm=TRUE)
  y<-0 
  Date<-as.Date("1978-08-13")
  plot(Date, y, xlim=xlim, ylim=ylim, type="n", ...)
  for (v in variablestoplot) for (i in 1:nrow(dynamics[[1]])) {
    y<-dynamics[[v]][i,,drop=FALSE]
    if (is.null(colors)) { orderedcolors<-1 } else 
      if (length(colors)==1) { orderedcolors<-colors } else 
        if (whatcolors=="cases") { orderedcolors<-colors[rownames(y)] } else 
          if (whatcolors=="variables") { orderedcolors<-colors[v] } else warning("if colors is of length>1, whatcolors should be either cases or variables")
    if (is.null(symbols)) { orderedsymbols<-1 } else 
      if (length(symbols)==1) { orderedsymbols<-symbols } else 
        if (whatsymbols=="cases") { orderedsymbols<-symbols[rownames(y)] } else 
          if (whatsymbols=="variables") { orderedsymbols<-symbols[v] } else warning("if symbols is of length>1, whatsymbols should be either cases or variables")
    if (is.null(linetypes)) { orderedlinetypes<-1 } else 
      if (length(linetypes)==1) { orderedlinetypes<-linetypes } else 
        if (whatlinetypes=="cases") { orderedlinetypes<-linetypes[rownames(y)] } else 
          if (whatlinetypes=="variables") { orderedlinetypes<-linetypes[v] } else warning("if linetypes is of length>1, whatlinetypes should be either cases or variables")
    lines(dates, y, col=orderedcolors, lty=orderedlinetypes)
    points(dates, y, col=orderedcolors, pch=orderedsymbols)
  }
  return(dynamics)
}
#example: mPlotDynamics("iRSDS") ; mPlotDynamics(c("iTASMin", "iTASMax"), colors=c(iTASMin="blue", iTASMax="red"), whatcolors="variables")
#mPlotDynamics(c("iTASMin", "iTASMax", "iRSDS"), 
#              colors=c(iTASMin="blue", iTASMax="red", iRSDS="black"), whatcolors="variables", 
#              linetypes=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlinetypes="variables")

mContains<-function(details=FALSE) 
{
  ici<-environment()
  if (details) toto<-ls.str(envir=parent.env(ici)) else toto<-ls(envir=parent.env(ici))
  return(toto)
}

mGetAllForDebuggingPurposes<-function(){
  ici<-environment()
  toto<-ls(envir=parent.env(ici))
  for (i in toto) assign(i, get(i, envir=parent.env(ici)), envir = .GlobalEnv)
}

mGetGlobal<-function(objectname) return(get(objectname))

mSetGlobal<-function(objectname, value) return(assign(objectname, value))

mCompletePARAMSIM<-function(listofthings){
  duplicated<-intersect(names(listofthings), names(PARAMSIM))
  if (length(duplicated)>0){ warning(paste(paste(duplicated, collapse=", "), "were already present in PARAMSIM, they haven't been modified. To modify existing parameters, use changeParam"))}
  PARAMSIM<<-c(PARAMSIM, listofthings[setdiff(names(listofthings), names(PARAMSIM))])
}

