#mXXXX: Model interface (e.g. mrun, mplot)

mOnetimestep<-function(){
  rCreateDay() #creates ALLDAYDATA with the values initialised at their default values, except stat variables that are initialised at their value from the day before

  rWeatherDay()
  #rUpdatePAR() not coded, and I don't know what is was supposed to be, maybe it was to update PAR depending on tree interception once the agroforestry module would be added?
  rUpdateManagement() #for now, just keeps the crops and cultivars as the day before
  rUpdateStresses() #compute water stress with water from the day before
  rUpdatePhenology()
  rUpdateLAI()
  rUpdateDMProduction()
  rUpdateDMDistribution() 
  rRootDepth()
  rWaterBudget()
  
  #at the end of the timestep, adds the new day at the end of the list of all variables
  ALLSIMULATEDDATA <<- c(ALLSIMULATEDDATA, list(ALLDAYDATA))
  return()
}

mRun<-function(howlong=1)
{
  if(length(ALLSIMULATEDDATA)==0) {
    eReadInInputs() #read in inputs (icicici first version: all possible soils, climates and crops are read at model initialisation, probably not the most eficient way to do it)
    rCreateDay0()
  }
  replicate(howlong, mOnetimestep())
  return(paste("the model ran for", howlong, "time steps"))
}

mPlotDynamics<-function(variablestoplot=NULL, casestoplot=NULL,
                        col=NULL, pch=NULL, lty=NULL, 
                        whatcol=c("cases", "variables"), 
                        whatpch=c("cases", "variables"), 
                        whatlty=c("cases", "variables"),
                        xlim=NULL, ylim=NULL,
                       ...)  {
  #variablestoplot: vector of variable names from ALLSIMULATEDDATA (by default all even if it doesnt look good)
  #casestoplot: vector of cases (i.e. rownames of the data.frames in ALLSIMULATEDDATA)  (by default all cases)
  #col: named vector of col, named according to the names of cases or variables, depending on the value of whatcol, or one color for all points
  #pch: named vector of pch, named according to the names of cases or variables, depending on the value of whatpch, or one symbol for all points
  #lty: named vector of lty, named according to the names of cases or variables, depending on the value of whatlty, or one type for all lines
  if(length(ALLSIMULATEDDATA)==0) stop("You cannot plot a model that hasn't been run for at least 1 time step") 
  if(is.null(variablestoplot)) variablestoplot<-names(ALLSIMULATEDDATA[[1]])
  if(is.null(casestoplot)) casestoplot<-rownames(ALLSIMULATEDDATA[[1]])
  names(variablestoplot)<-variablestoplot
  names(casestoplot)<-casestoplot
  #if xlim is provided, we find the corresponding timesteps
  dates<-do.call("c", lapply(ALLSIMULATEDDATA, function(x) x[1,"iDate"]))
  if(!is.null(xlim)) {
    timestepstoplot<- (dates>=xlim[1] & dates<=xlim[2])
    dates<-dates[timestepstoplot]
  } else timestepstoplot<-TRUE
  #extracts the dynamics of each variable
  dynamics<-lapply(variablestoplot, function(v, cases=TRUE) {
    if (is.numeric(ALLSIMULATEDDATA[[1]][,v])) return(
      as.matrix(as.data.frame(lapply(ALLSIMULATEDDATA[timestepstoplot], function(x) x[cases,v, drop=FALSE])))
    ) else stop("variablestoplot should not contain character variables")
  }, cases=casestoplot)
  #dynamics is a list (one element for each variable) of matrices (rows= cases to plot, columns=timesteps to plot)
  
  if (is.null(col)) { #col not provided but whatcol provided: generate col, whatcol not provided: all black
    if (whatcol=="cases") {col<-matrix(rep(rainbow(start=0, end=5/6,n=nrow(dynamics[[1]])), times=length(variablestoplot)), ncol=length(variablestoplot)) ; colnames(col)<-variablestoplot ; rownames(col)<-casestoplot } else 
      if (whatcol=="variables") {col<-matrix(rep(rainbow(start=0, end=5/6, n=length(dynamics)), each=length(casestoplot)), ncol=length(variablestoplot))  ; colnames(col)<-variablestoplot ; rownames(col)<-casestoplot  } else 
      {col<-matrix(rep("black", length(variablestoplot)*length(casestoplot)), ncol=length(variablestoplot))  ; colnames(col)<-variablestoplot ; rownames(col)<-casestoplot }
  } else #col provided
    if (length(col)==1) { col<-matrix(rep(col, length(variablestoplot)*length(casestoplot)), ncol=length(variablestoplot))  ; colnames(col)<-variablestoplot ; rownames(col)<-casestoplot  } else #col provided and length>1
      if (whatcol=="cases") {
        if (is.null(names(col))) {if (length(col)==length(casestoplot)) names(col)<-casestoplot else stop("if you don't give names to col, the length of col should be the same as the length of casestoplot") }
        col<-matrix(rep(col[casestoplot], times=length(variablestoplot)), ncol=length(variablestoplot)) ; colnames(col)<-variablestoplot ; rownames(col)<-casestoplot
      } else 
        if (whatcol=="variables") {
          if (is.null(names(col))) {if (length(col)==length(variablestoplot)) names(col)<-variablestoplot else stop("if you don't give names to col, the length of col should be the same as the length of variablestoplot") }
          col<-matrix(rep(col[variablestoplot], each=length(casestoplot)), ncol=length(variablestoplot)) ; colnames(col)<-variablestoplot ; rownames(col)<-casestoplot
        } else stop('if col is of length>1, whatcol should be either "cases" or "variables"')
  if (is.null(pch)) { #pch not provided but whatpch provided: generate pch, whatpch not provided: circle
    if (whatpch=="cases") {pch<-matrix(rep(rainbow(start=0, end=5/6,n=nrow(dynamics[[1]])), times=length(variablestoplot)), ncol=length(variablestoplot)) ; colnames(pch)<-variablestoplot ; rownames(pch)<-casestoplot } else 
      if (whatpch=="variables") {pch<-matrix(rep(rainbow(start=0, end=5/6, n=length(dynamics)), each=length(casestoplot)), ncol=length(variablestoplot))  ; colnames(pch)<-variablestoplot ; rownames(pch)<-casestoplot  } else 
      {pch<-matrix(rep(1, length(variablestoplot)*length(casestoplot)), ncol=length(variablestoplot))  ; colnames(pch)<-variablestoplot ; rownames(pch)<-casestoplot }
  } else #pch provided
    if (length(pch)==1) { pch<-matrix(rep(pch, length(variablestoplot)*length(casestoplot)), ncol=length(variablestoplot))  ; colnames(pch)<-variablestoplot ; rownames(pch)<-casestoplot  } else #pch provided and length>1
      if (whatpch=="cases") {
        if (is.null(names(pch))) {if (length(pch)==length(casestoplot)) names(pch)<-casestoplot else stop("if you don't give names to pch, the length of pch should be the same as the length of casestoplot") }
        pch<-matrix(rep(pch[casestoplot], times=length(variablestoplot)), ncol=length(variablestoplot)) ; colnames(pch)<-variablestoplot ; rownames(pch)<-casestoplot
      } else 
        if (whatpch=="variables") {
          if (is.null(names(pch))) {if (length(pch)==length(variablestoplot)) names(pch)<-variablestoplot else stop("if you don't give names to pch, the length of pch should be the same as the length of variablestoplot") }
          pch<-matrix(rep(pch[variablestoplot], each=length(casestoplot)), ncol=length(variablestoplot)) ; colnames(pch)<-variablestoplot ; rownames(pch)<-casestoplot
        } else stop('if pch is of length>1, whatpch should be either "cases" or "variables"')
  if (is.null(lty)) { #lty not provided but whatlty provided: generate lty, whatlty not provided: all normal line
    if (whatlty=="cases") {lty<-matrix(rep(rainbow(start=0, end=5/6,n=nrow(dynamics[[1]])), times=length(variablestoplot)), ncol=length(variablestoplot)) ; colnames(lty)<-variablestoplot ; rownames(lty)<-casestoplot } else 
      if (whatlty=="variables") {lty<-matrix(rep(rainbow(start=0, end=5/6, n=length(dynamics)), each=length(casestoplot)), ncol=length(variablestoplot))  ; colnames(lty)<-variablestoplot ; rownames(lty)<-casestoplot  } else 
      {lty<-matrix(rep(1, length(variablestoplot)*length(casestoplot)), ncol=length(variablestoplot))  ; colnames(lty)<-variablestoplot ; rownames(lty)<-casestoplot }
  } else #lty provided
    if (length(lty)==1) { lty<-matrix(rep(lty, length(variablestoplot)*length(casestoplot)), ncol=length(variablestoplot))  ; colnames(lty)<-variablestoplot ; rownames(lty)<-casestoplot  } else #lty provided and length>1
      if (whatlty=="cases") {
        if (is.null(names(lty))) {if (length(lty)==length(casestoplot)) names(lty)<-casestoplot else stop("if you don't give names to lty, the length of lty should be the same as the length of casestoplot") }
        lty<-matrix(rep(lty[casestoplot], times=length(variablestoplot)), ncol=length(variablestoplot)) ; colnames(lty)<-variablestoplot ; rownames(lty)<-casestoplot
      } else 
        if (whatlty=="variables") {
          if (is.null(names(lty))) {if (length(lty)==length(variablestoplot)) names(lty)<-variablestoplot else stop("if you don't give names to lty, the length of lty should be the same as the length of variablestoplot") }
          lty<-matrix(rep(lty[variablestoplot], each=length(casestoplot)), ncol=length(variablestoplot)) ; colnames(lty)<-variablestoplot ; rownames(lty)<-casestoplot
        } else stop('if lty is of length>1, whatlty should be either "cases" or "variables"')
  #now col, symboles and lty are matrices with cases as rows and variables as columns
  if (is.null(xlim)) Date<-range(dates, na.rm=TRUE) else Date<-xlim
  if (is.null(ylim)) y<-range(unlist(dynamics), na.rm=TRUE) else y<-ylim
  plot(Date, y, type="n", ...)
  for (v in variablestoplot) for (i in casestoplot) {
    y<-dynamics[[v]][i,,drop=FALSE]
    lines(dates, y, col=col[i,v], lty=lty[i,v])
    points(dates, y, col=col[i,v], pch=pch[i,v])
  }
  return(dynamics)
}
#example: mPlotDynamics("iRSDS") ; mPlotDynamics(c("iTASMin", "iTASMax"), col=c(iTASMin="blue", iTASMax="red"), whatcol="variables")
#mPlotDynamics(c("iTASMin", "iTASMax", "iRSDS"), 
#              col=c(iTASMin="blue", iTASMax="red", iRSDS="black"), whatcol="variables", 
#              lty=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlty="variables")

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

mExtractVariable<-function(v){
  if (!(v %in% names(ALLSIMULATEDDATA[[1]]))) stop(v, "is not in the variables saved each day")
  toto<-as.data.frame(lapply(ALLSIMULATEDDATA, function(x) return(unname(x[,v]))))
  names(toto)<-paste("day", 0:(ncol(toto)-1))
  return(toto)
}

mExtractCase<-function(case){
  if (!(case %in% rownames(ALLSIMULATEDDATA[[1]]))) stop(case, "is not in the cases names in the simulation options")
  toto<-do.call(rbind, lapply(ALLSIMULATEDDATA[2:10], function(x) return(x[case,])))
  rownames(toto)<-NULL
  return(toto)
}

mCompletePARAMSIM<-function(listofthings){
  duplicated<-intersect(names(listofthings), names(PARAMSIM))
  if (length(duplicated)>0){ warning(paste(paste(duplicated, collapse=", "), "were already present in PARAMSIM, they haven't been modified. To modify existing parameters, use changeParam"))}
  #NB changeParam is not coded yet, but it will have to have more checks than complePARAMSIM to check that changing some simulation parameter won't break the model (e.g. same number of cases)
  PARAMSIM<<-c(PARAMSIM, listofthings[setdiff(names(listofthings), names(PARAMSIM))])
}

mSummary<-function(){
  nsteps<-length(ALLSIMULATEDDATA)
  daterange<-c(ALLSIMULATEDDATA[[1]][1,"iDate"],ALLSIMULATEDDATA[[length(ALLSIMULATEDDATA)]][1,"iDate"])
  ncases<-nrow(ALLSIMULATEDDATA[[1]])
  casenames<-rownames(ALLSIMULATEDDATA[[1]])
  print(paste("number of timesteps (including day 0):", nsteps))
  print(paste("simulations dates from", daterange[1], "to", daterange[2]))
  print(paste("number of cases (location-climate-management):", ncases))
  print(paste("names of the first cases:", paste(casenames[1:min(10,length(casenames))], collapse=", ")))
  return(list(nsteps=nsteps, daterange=daterange, ncases=ncases, casenames=casenames))
}

mExportDataFrame<-function(){
  return(do.call(rbind, lapply(ALLSIMULATEDDATA, function(x) return(cbind(PARAMSIM$cases, x)))))
}