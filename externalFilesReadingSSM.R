#eXXXX: read input scenarios from external files (e.g. eReadInInputs)




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

###read in inputs
eReadInInputs<-function(){
  eReadClimate()
  eReadSoil()
  eReadCrops()
}

eReadClimate<-function(){ 
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
} #end read climate

eReadSoil<-function(){ 
  if (PARAMSIM$soilformat=="standardSSM") { #if soil read from excel, read file only once and load it in the workspace
    pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/soils.xlsx", sep=""))
    locations<-getSheetNames(pathtoExcel)
    ALLSOILS<<-data.frame()
    for (sheet in locations) {
      #to do: decide how to tracks soil parameters and variables for each layer
    }
  } else if (PARAMSIM$soilformat=="")  {
  } 
} #end read soil

eReadCrops<-function(){ 
  if (PARAMSIM$cropformat=="standardSSM") { #if soil read from excel, read file only once and load it in the workspace
    pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/crops.xlsx", sep=""))
    availablecrops<-getSheetNames(pathtoExcel)
    ALLCROPS<<-list()
    for (crop in availablecrops) {
      dfcrop<-read.xlsx(pathtoExcel, sheet=crop, colNames=TRUE, startRow =2) #first column = parameter name, second = units (sometimes), other columns: cultivars
      rownames(dfcrop)<-dfcrop$name
      for (cultivar in names(dfcrop)[-(1:2)]) {
        pasflat<-is.na(as.numeric(dfcrop[,cultivar]))
        flatparams<-as.list(as.numeric(dfcrop[!pasflat, cultivar])) ; names(flatparams)<-dfcrop[!pasflat, "name"]
        complexparams<-dfcrop[, c("name", cultivar)]
        stages<-unlist(strsplit(complexparams["stagelist", cultivar], split=','))
        descriptionstages<-list()
        for (sta in stages) {
          texte<-gsub(pattern='"', replacement="'", x=complexparams[paste("description", sta, sep="_"),cultivar])
          toto<-list(eval(parse(text=texte)))
          names(toto)<-sta
          descriptionstages<-c(descriptionstages, toto)
        }
        ALLCROPS[[crop]][[cultivar]]<<-c(flatparams, list(phenology=descriptionstages))
      }

      
    }
  } else stop("crop format should only standard SSM")
} #end read crops



