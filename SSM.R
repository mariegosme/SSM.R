options(stringsAsFactors=FALSE)
library(openxlsx) # necessary to read input parameters and climate scenarios as excel files
require(ncdf4) #necessary to obtain climate data from gridded climate data
#variable names
#### naming rules:
#iXXXX: input variable (e.g. iPr : Precipitation)
#pXXXX: parameter (e.g. pPhyllochron)
#cXXXX: computed variable (e.g. cDeltaLAI)
#sXXXX: state variable (e.g. sLAI)
#fXXXX: function (e.g. fPhotosynthesis)
#rXXXX: pRocedure (e.g. rUpdateLAI)




VARIABLEDEFINITIONS<-read.xlsx("allvariables.xlsx", sheet="savedEachDay", colNames=TRUE)
types<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"] ; names(types)<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "name"]
#### creation of the variable to store the simulation parameters
PARAMSIM<-list(types=types) #types of each variable to create : vector of types (numeric, character, logical) named by variable name
 #other elements of PARAMSIM (simustart, cases, directory, climateformat and soilformat) will be defined after setup

#pour debuggage, creation de PARAMSIM complet:
'
mycases<-data.frame(climatename="Ain Hamra - Meknes", soilname="325_-35", lat=c(35, 45), long=-5)
rownames(mycases)<-c("Meknes35degres", "Meknes45degres")
PARAMSIM<-list(types=types,
                 simustart=as.Date("1997-11-01"), #date of start of the simulation
                 cases=mycases, #cases (e.g. spatial locations, soils, latitudes etc... = rows in ALLSIMULATEDDATA)
                 directory="/Users/user/Documents/a_System/modelisation/SSM/simulations/premieressai", #directory where your input/output folders are
                 climateformat="standardSSM",
                 soilformat="standardSSM"
               )

'

##### creation of the list of days only the first time
ALLSIMULATEDDATA<-list()

##### definition of constants

#### PARAMETERS
ALLPARAMETERS<-list(pCoefPAR=0.48)

#### initialisation of variable to store climate if read once at the beginning
ALLCLIMATE<-list()
ALLSOILS<-list()

### definition of functions (which return calculated variables)
fCreateDataFrame<-function(d, types, rows) {
  df<-cbind(data.frame(iDate=d),
            as.data.frame(lapply(types, function(x) return(do.call(x, list(length(rows)))))))
  rownames(df)<-rows
  return(df)
}

fGetClimateDay<-function(date){ #returns a data.frame with the date, climate name and climate variables in columns, and possibly several rows, named by the climate name
  if (PARAMSIM$climateformat %in% c("standardSSM")) {
    selectday<-ALLCLIMATE[ALLCLIMATE$date==date,]
    rownames(selectday)<-selectday$climatename
    return(selectday)
  } else if (PARAMSIM$climateformat=="netCDF") {
    #do something that returns a data.frame with colums 
    #date, 
    #optionnally one or more columns to define the cases (e.g. location, climate model etc...)
    #and all necessary climate variables (named as in the variable definition file)
    #and rownames should be climate names as used in the simulation options (PARAMSIM$case$climatename)
  }
}

fComputePAR<-function(globalradiation, CoefPAR=ALLPARAMETERS$pCoefPAR) {
  return(CoefPAR*globalradiation)
  
}

#### definition of procedures (which update state variables)
rCreateDay<-function() { 
  #add a new day to ALLSIMULATEDDATA
  laststep<-length(ALLSIMULATEDDATA)
  print(paste("creating day", laststep+1))
  ALLSIMULATEDDATA <<- c(ALLSIMULATEDDATA, list(fCreateDataFrame(
    d=ALLSIMULATEDDATA[[laststep]][,"iDate"]+1, 
    types=PARAMSIM$types,
    rows=rownames(PARAMSIM$cases))))
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

rUpdatePAR<-function(){
  print("updating PAR")
  
  step<-length(ALLSIMULATEDDATA)
  PAR<-fComputePAR(globalradiation=ALLSIMULATEDDATA[[step]]$iRSDS, CoefPAR=ALLPARAMETERS$pCoefPAR)
  ALLSIMULATEDDATA[[step]]$cPAR<<-PAR
}


##### handlers
###read in inputs
mReadInInputs<-function(){
  if (PARAMSIM$climateformat=="standardSSM") { #if climate read from excel, read file only once and load it in the workspace
    climatevar<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="climate",]
    newnames<-climatevar$name ; names(newnames)<-climatevar$translationSSM
    pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/climates.xlsx", sep=""))
    locations<-getSheetNames(pathtoExcel)
    ALLCLIMATE<<-data.frame()
    for (sheet in locations) {
      onesheet<-read.xlsx(pathtoExcel, sheet=sheet, colNames=TRUE, startRow =10)
      #transforms year-DOY into date
      onesheet$date<-as.Date(strptime(paste(onesheet$YEAR, onesheet$DOY), format="%Y %j"))
      #check that the simulation start is in the climate scenario
      if (! (PARAMSIM$simustart %in% onesheet$date)) warning(paste(PARAMSIM$simustart, "is not in sheet", sheet, "in file", pathtoExcel))
      #translate names
      names(onesheet)[names(onesheet) %in% names(newnames)]<-newnames[names(onesheet)[names(onesheet) %in% names(newnames)]]
      #check that the sheet contains all necessary variables
      missingvariable<-setdiff(newnames, names(onesheet))
      if (length(missingvariable)>0) warning(paste("Sheet", sheet, "in file", pathtoExcel, "misses variables", paste(missingvariable, collapse=",")))
      onesheet$climatename<-sheet
      ALLCLIMATE<<-rbind(ALLCLIMATE, onesheet)
    }
  } else if (PARAMSIM$climateformat=="netCDF")  {
    #do something, e.g. just open the metadata
  } 
  
  if (PARAMSIM$soilformat=="standardSSM") { #if soil read from excel, read file only once and load it in the workspace
    pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/soils.xlsx", sep=""))
    locations<-getSheetNames(pathtoExcel)
    ALLSOILS<<-data.frame()
    for (sheet in locations) {
      #to do: decide how to tracks soil parameters and variables for each layer
    }
  } else if (PARAMSIM$soilformat=="")  {
  } 
}

mOnetimestep<-function(){
  nbtimesteps<-length(ALLSIMULATEDDATA)
  if(nbtimesteps==0) { #only the first time step: creates day with date = simustart
    mReadInInputs()
    d<-as.Date(PARAMSIM$simustart)
    types<-PARAMSIM$types
    cases<-rownames(PARAMSIM$cases)
    ALLSIMULATEDDATA<<-list(fCreateDataFrame(
      d=d, 
      types=types,
      rows=cases)
    ) } else {
      rCreateDay()
    }
  rUpdateClimate()
  rUpdatePAR()
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

mGetGlobal<-function(objectname) return(get(objectname))

mSetGlobal<-function(objectname, value) return(assign(objectname, value))

mCompletePARAMSIM<-function(listofthings){
  duplicated<-intersect(names(listofthings), names(PARAMSIM))
  if (length(duplicated)>0){ warning(paste(paste(duplicated, collapse=", "), "were already present in PARAMSIM, they haven't been modified. To modify existing parameters, use changeParam"))}
  PARAMSIM<<-c(PARAMSIM, listofthings[setdiff(names(listofthings), names(PARAMSIM))])
}

