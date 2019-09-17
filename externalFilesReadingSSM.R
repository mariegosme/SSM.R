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
    climatevar<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="weather" & VARIABLEDEFINITIONS$typeinthemodel=="input",]
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

eReadCrops_OLD<-function(){
  if (PARAMSIM$cropformat=="standardSSM") { #if soil read from excel, read file only once and load it in the workspace
    pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/crops.xlsx", sep=""))
    availablecrops<-getSheetNames(pathtoExcel)
    ALLCROPS<<-list()
    for (crop in availablecrops) {
      dfcrop<-read.xlsx(pathtoExcel, sheet=crop, colNames=TRUE, startRow =2) #first column = parameter name, second = units (sometimes), other columns: cultivars
      rownames(dfcrop)<-dfcrop$name
      for (cultivar in names(dfcrop)[-(1:3)]) {
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

#' Reads in Excel crop parameter file (example crops2.xlsx)
#'
#' @param xlsxfile path to the excel file of crop parameters
#' @param allvariablesfile path to the allvariables.xlsx file, with the translations of parameter names fromSSM to SSM.R
#' @return returns a list of parameters
#' @examples eReadExcelCropParameters(xlsxfile="exampleinputs/crops2.xlsx",
#'   allvariablesfile="allvariables.xlsx"
#' )
eReadExcelCropParameters<-function(xlsxfile, allvariablesfile){
  if (!require(openxlsx)) {warning("function readExcelCropParameters needs packege openxlsx"); return(list())}
  #read in translations of parameter names from SSM to SSM.R
  trad<-read.xlsx(allvariablesfile, sheet="savedEachDay")
  trad<-trad[trad$typeinthemodel=="CropParameter",]
  modules<-unique(trad$module[!is.na(trad$module)]); names(modules)<-modules
  readmodule<-function(module, data, trad, numerocolonne){
    toto<-trad[!is.na(trad$module) & trad$module==module,]
    nomsSSM<-toto$translationSSM ; names(nomsSSM)<-toto$name
    paramsSSM<-lapply(nomsSSM, function(param) return(data[param,numerocolonne]))
    types<-toto$typeR ; names(types)<-toto$name
    paramsSSM[types[names(paramsSSM)]=="numeric"]<-as.numeric(paramsSSM[types[names(paramsSSM)]=="numeric"])
    paramsSSMR<-list()
    if (length(data[paste(module, "filter", sep="."), numerocolonne])>0) paramsSSMR<-list(filter=data[paste(module, "filter", sep="."), numerocolonne])
    return(c(paramsSSMR, paramsSSM))
  }
  paramscrops<-list()
  onglets<-getSheetNames(xlsxfile)
  for(sheet in onglets) {
    data<-read.xlsx(xlsxfile, sheet=sheet, rowNames=TRUE)
    for (i in 2:ncol(data)) {
      paramsmanuels<-list(
        name=paste(colnames(data)[i], data["name",i], sep="."),
        thresholds=data["thresholds", i],
        waterstress=list(filter=data["waterstress.filter", i])
      )
      paramsautomatiques<-lapply(modules, readmodule, data=data, trad=trad, numerocolonne=i)
      lu<-list(c(paramsmanuels, paramsautomatiques))
      names(lu)<-lu[[1]]$name
      paramscrops<-c(paramscrops, lu)
    }
  }
  return(paramscrops)
}



