#eXXXX: read input scenarios from external files (e.g. eReadInInputs)




fGetClimateDay<-function(date){ #returns a data.frame with the date, climate name and climate variables in columns, and possibly several rows, named by the climate name
  if (PARAMSIM$climateformat %in% c("standardSSM")) {
    selectday<-ALLCLIMATES[ALLCLIMATES$date==date,]
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
  eReadCrop()
  eReadManagement()
}

eReadClimate<-function(){
  if (PARAMSIM$climateformat=="standardSSM") { #if climate read from excel, read file only once and load it in the workspace
    climatevar<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="weather" & VARIABLEDEFINITIONS$typeinthemodel=="input",]
    newnames<-climatevar$name ; names(newnames)<-climatevar$translationSSM
    pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/climates.xlsx", sep=""))
    locations<-getSheetNames(pathtoExcel)
    ALLCLIMATES<<-data.frame()
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
      ALLCLIMATES<<-rbind(ALLCLIMATES, onesheet)
    }
  } else if (PARAMSIM$climateformat=="netCDF")  {
    stop("netCDF format not yet supported for climate")
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
  } else  {
    stop("Only standard SSM format is supported for soil data")
  }
} #end read soil

eReadCrop<-function(){
  ALLCROPS<<-eReadExcelCropParameters(xlsxfile=normalizePath(paste(PARAMSIM$directory, "/input/crops.xlsx", sep="")),
                           allvariablesfile="allvariables.xlsx")
}#end read crops

# eReadCrops_OLD<-function(){
#   if (PARAMSIM$cropformat=="standardSSM") { #if soil read from excel, read file only once and load it in the workspace
#     pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/crops.xlsx", sep=""))
#     availablecrops<-getSheetNames(pathtoExcel)
#     ALLCROPS<<-list()
#     for (crop in availablecrops) {
#       dfcrop<-read.xlsx(pathtoExcel, sheet=crop, colNames=TRUE, startRow =2) #first column = parameter name, second = units (sometimes), other columns: cultivars
#       rownames(dfcrop)<-dfcrop$name
#       for (cultivar in names(dfcrop)[-(1:3)]) {
#         pasflat<-is.na(as.numeric(dfcrop[,cultivar]))
#         flatparams<-as.list(as.numeric(dfcrop[!pasflat, cultivar])) ; names(flatparams)<-dfcrop[!pasflat, "name"]
#         complexparams<-dfcrop[, c("name", cultivar)]
#         stages<-unlist(strsplit(complexparams["stagelist", cultivar], split=','))
#         descriptionstages<-list()
#         for (sta in stages) {
#           texte<-gsub(pattern='"', replacement="'", x=complexparams[paste("description", sta, sep="_"),cultivar])
#           toto<-list(eval(parse(text=texte)))
#           names(toto)<-sta
#           descriptionstages<-c(descriptionstages, toto)
#         }
#         ALLCROPS[[crop]][[cultivar]]<<-c(flatparams, list(phenology=descriptionstages))
#       }
# 
# 
#     }
#   } else stop("crop format should only standard SSM")
# } #end read crops

#' Reads in Excel crop parameter file in the same format as crops in SSM, except with extra lines at the end for filters (example crops2.xlsx)
#'
#' @param xlsxfile path to the excel file of crop parameters
#' @param allvariablesfile path to the allvariables.xlsx file, with the translations of parameter names fromSSM to SSM.R (necessary because we use the SSM format, with row names different from SSM.R names)
#' @return returns a list of parameters (one element per species.cultivar). parameters are lists (one element per module as defined in allvariables.xlsx)
#' @examples eReadExcelCropParameters_severalsheets(xlsxfile="inputs/crops.xlsx",
#'   allvariablesfile="allvariables.xlsx"
#' )
# eReadExcelCropParameters_severalsheets<-function(xlsxfile, allvariablesfile){
#   if (!require(openxlsx)) {warning("function readExcelCropParameters needs package openxlsx"); return(list())}
#   #read in translations of parameter names from SSM to SSM.R
#   trad<-read.xlsx(allvariablesfile, sheet="savedEachDay")
#   trad<-trad[trad$typeinthemodel=="CropParameter",]
#   modules<-c(unique(trad$module[!is.na(trad$module)]), "LAI_Secondary", "DMDistribution_SeedGrowing") #don't forget to add modules that don't have specific parameters in allvariables, but that have a filter
#   names(modules)<-modules
#   readmodule<-function(module, data, trad, numerocolonne){
#     toto<-trad[!is.na(trad$module) & trad$module==module,]
#     nomsSSM<-toto$translationSSM ; names(nomsSSM)<-toto$name
#     paramsSSM<-lapply(nomsSSM, function(param) return(data[param,numerocolonne]))
#     types<-toto$typeR ; names(types)<-toto$name
#     paramsSSM[types[names(paramsSSM)]=="numeric"]<-as.numeric(paramsSSM[types[names(paramsSSM)]=="numeric"])
#     paramsSSMR<-list()
#     if (length(data[paste(module, "filter", sep="."), numerocolonne])>0) paramsSSMR<-list(filter=gsub(pattern= "&amp;", replacement="&", fixed=TRUE, x=data[paste(module, "filter", sep="."), numerocolonne]))
#     return(c(paramsSSMR, paramsSSM))
#   }
#   paramscrops<-list()
#   onglets<-getSheetNames(xlsxfile)
#   for(sheet in onglets) {
#     data<-read.xlsx(xlsxfile, sheet=sheet, rowNames=TRUE)
#     for (i in 2:ncol(data)) {
#       paramsmanuels<-list(
#         name=paste(colnames(data)[i], data["name",i], sep="."),
#         thresholds=eval(parse(text=data["thresholds", i])),
#         waterstress=list(filter=data["waterstress.filter", i])
#       )
#       paramsautomatiques<-lapply(modules, readmodule, data=data, trad=trad, numerocolonne=i)
#       lu<-list(c(paramsmanuels, paramsautomatiques))
#       names(lu)<-lu[[1]]$name
#       paramscrops<-c(paramscrops, lu)
#     }
#   }
#   return(paramscrops)
# }

#warning: now ALLCROPS is a data.frame, with crop.cultivar as rownames
#' Reads in Excel crop parameter file in the same format as crops in SSM, except with extra lines at the end for filters (example crops2.xlsx)
#'
#' @param xlsxfile path to the excel file of crop parameters
#' @param allvariablesfile path to the allvariables.xlsx file, with the translations of parameter names fromSSM to SSM.R (necessary because we use the SSM format, with row names different from SSM.R names)
#' @return returns a data.frame, with crop.cultivar as rownames
#' @examples eReadExcelCropParameters(xlsxfile="/Users/user/Documents/b_maison/congeMat/D4DECLIC/runSSM/input/crops.xlsx",
#'   allvariablesfile="allvariables.xlsx"
#' )
eReadExcelCropParameters<-function(xlsxfile, allvariablesfile){
  if (!require(openxlsx)) {warning("function readExcelCropParameters needs package openxlsx"); return(list())}
  #read in translations of parameter names from SSM to SSM.R
  trad<-read.xlsx(allvariablesfile, sheet="savedEachDay")
  trad<-trad[trad$typeinthemodel=="CropParameter",]
  modules<-c(unique(trad$module[!is.na(trad$module)]), "LAI_Secondary", "DMDistribution_SeedGrowing") #don't forget to add modules that don't have specific parameters in allvariables, but that have a filter
  names(modules)<-modules
  readmodule<-function(module, data, trad, numerocolonne){
    toto<-trad[!is.na(trad$module) & trad$module==module,]
    nomsSSM<-toto$translationSSM ; names(nomsSSM)<-toto$name
    paramsSSM<-lapply(nomsSSM, function(param) return(data[param,numerocolonne]))
    types<-toto$typeR ; names(types)<-toto$name
    paramsSSM[types[names(paramsSSM)]=="numeric"]<-as.numeric(paramsSSM[types[names(paramsSSM)]=="numeric"])
    paramsSSMR<-list()
    if (length(data[paste(module, "filter", sep="."), numerocolonne])>0) paramsSSMR<-list(filter=gsub(pattern= "&amp;", replacement="&", fixed=TRUE, x=data[paste(module, "filter", sep="."), numerocolonne]))
    return(c(paramsSSMR, paramsSSM))
  }
  paramscrops<-list()
  data<-read.xlsx(xlsxfile, sheet="allcrops", rowNames=FALSE, colNames=FALSE)
  tdata<-as.data.frame(t(as.matrix(data[3:nrow(data), 3:ncol(data)])))
  names(tdata)<-gsub(pattern=" ", replacement="", gsub(pattern=":", replacement="", gsub(pattern="=", replacement="", data[3:nrow(data), "X1"])))
  tdata$name<-paste(tdata$CROP, tdata$Cultivar, sep=".")
  if (any(duplicated(names(tdata)))) stop("excel crop parameters file contains duplicated parameter names:", paste(names(tdata)[duplicated(names(tdata))], collapse=", "))
  translations<-trad$name ; names(translations)<-trad$translationSSM
  names(tdata)[names(tdata) %in% names(translations)]<-translations[names(tdata) [names(tdata) %in% names(translations)]]
  if (any(duplicated(names(tdata)))) stop("allvariables.xlsx names created duplicated parameter names:", paste(names(tdata)[duplicated(names(tdata))], collapse=", "))
  #transformation of types
  numericparam<-intersect(trad[trad$typeR=="numeric", "name"], names(tdata))
  tdata[,numericparam]<-lapply(tdata[,numericparam], as.numeric)
  #evaluation of thresholds (column threshold becomes a list whith as many elements as rows in ALLCROPS, of lists with as many elements as stages in each crop)
  tdata$thresholds<-lapply(tdata$thresholds, function(x) eval(parse(text=x)))
  #evaluation of actionsAtStageChange  (column actionsAtStageChange becomes a list whith as many elements as rows in ALLCROPS, of lists with as many elements as actions to do in each crop)
  tdata$actionsAtStageChange<-lapply(tdata$actionsAtStageChange, function(x) eval(parse(text=x)))
  #replace the amp symbol by & in filters
  tdata[,grepl(pattern=".filter", x= names(tdata), fixed=TRUE)]<-lapply(tdata[,grepl(pattern=".filter", x= names(tdata), fixed=TRUE)], function(x) gsub(pattern="&amp;", replacement="&", x=x))
  rownames(tdata)<-tdata$name
  return(tdata)
}


eReadManagement<-function(){
  if (PARAMSIM$managformat=="standardSSM") { #read file only once and load it in the workspace
    pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "/input/managementPlans.xlsx", sep=""))
    locations<-getSheetNames(pathtoExcel)
    if(length(locations)>1) warning("Your managementPlans.xlsx file has several sheets, but only the first one will be used")
    firstcol<-read.xlsx(pathtoExcel, sheet=1, colNames=FALSE, cols=1, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    startManag<- which(firstcol$X1=="Code")
    allmanag<-list()
    for (i in 1:length(startManag)){
      dfCode<-read.xlsx(pathtoExcel, sheet=1, rows=startManag[i]:(startManag[i]+1), cols=1:3)
      dfSowing<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+2):(startManag[i]+3), cols=1:11)
      nitrogenScenario<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+5), cols=2, colNames=FALSE)[1,1]
      nitrogenNumber<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+6), cols=2, colNames=FALSE)[1,1]
      nitrogenDatetype<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+7), cols=2, colNames=FALSE)[1,1]
      nitrogendf<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+9):(startManag[i]+9+nitrogenNumber), cols=1:4)
      names(nitrogendf)<-c("NapplNumber", "DAPorCBD", "amount", "FracVol")
      waterLevel<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+6), cols=7, colNames=FALSE)[1,1]
      waterScenario<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+5), cols=6, colNames=FALSE)[1,1]
      waterNumber<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+6), cols=6, colNames=FALSE)[1,1]
      waterDatetype<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+7), cols=6, colNames=FALSE)[1,1]
      waterdf<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+9):(startManag[i]+9+waterNumber), cols=5:6)
      names(waterdf)<-c("DAPorCBDorDOY", "amount")
      managementPlan<-list(dfCode, dfSowing, nitrogenScenario, nitrogenNumber, nitrogenDatetype, nitrogendf, waterLevel, waterScenario, waterNumber, waterDatetype, waterdf)
      names(managementPlan)<-c("dfCode", "dfSowing", "nitrogenScenario", "nitrogenNumber", "nitrogenDatetype", "nitrogendf", "waterLevel", "waterScenario", "waterNumber", "waterDatetype", "waterdf")
      toto<-list(managementPlan) ; names(toto)<-paste("row", startManag[i]-1)
      allmanag<-c(allmanag, toto)
    }
    ALLMANAGEMENTS<<-allmanag
  } else  {
    stop("Only standard SSM format is supported for crop management data")
  }
}

