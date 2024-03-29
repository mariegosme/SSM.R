#eXXXX: read input scenarios from external files (e.g. eReadInInputs)




fGetClimateDay<-function(date){ #returns a data.frame with the date, climate name and climate variables in columns, and possibly several rows, named by the climate name
  if (PARAMSIM$climateformat %in% c("standardSSM", "D4Declicplatform")) {
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
  return()
}

eReadClimate<-function(){
  if (PARAMSIM$climateformat=="standardSSM") { #if climate read from excel, read file only once and load it in the workspace
    climatevar<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="rWeatherDay" & VARIABLEDEFINITIONS$typeinthemodel=="input",]
    newnames<-climatevar$name ; names(newnames)<-climatevar$translationSSM
    if(!is.null(PARAMSIM$directory)) pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "input/climates.xlsx", sep="/")) else pathtoExcel<-normalizePath("input/climates.xlsx")
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
  } else if (PARAMSIM$climateformat=="D4Declicplatform") {
    if(exists("USERID", envir=ICI)) {
      if (dir.exists(paste(paste0("user_", USERID)))){
        path<-normalizePath(paste(paste0("user_", USERID), "inputplatform/climates.csv", sep="/")) 
      } else path<-normalizePath("inputplatform/climates.csv")
    } else path<-normalizePath("inputplatform/climates.csv")
    csvcontent<-read.csv(path) #contains lat, lon, rotation, date
   
    csvcontent$date<-as.Date(paste(csvcontent$Year, csvcontent$Month, csvcontent$Day, sep="-"))
    translations<-c("iRSDS","iTASMax","iTASMin","iPr","date")
    names(translations)<-c("rsds","tasmax","tasmin","pr", "date") 
    csvcontent<-csvcontent[,names(translations)]
    names(csvcontent)<-translations
    #translate headers : need "iRSDS"       "iTASMax"     "iTASMin"     "iPr"         "date"   
    climatevar<-setdiff(VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$module=="rWeatherDay" & VARIABLEDEFINITIONS$typeinthemodel=="input","name"], "date")
    missingvariable<-setdiff(climatevar, names(csvcontent))
    if (length(missingvariable)>0) warning(paste("file inputplatform/climate.csv misses variables", paste(missingvariable, collapse=",")))
    #transforms year-DOY into date
    #csvcontent$date<-as.Date(strptime(paste(csvcontent$YEAR, csvcontent$DOY), format="%Y %j"))
    csvcontent$climatename<-"sim1"
    ALLCLIMATES<<-csvcontent
  }
  return()
} #end read climate

eReadSoil<-function(){
  if (PARAMSIM$soilformat=="standardSSM") { #if soil read from excel, read file only once and load it in the workspace
    if (!require(openxlsx)) {warning("function eReadSoil needs package openxlsx"); return(list())}
    if(!is.null(PARAMSIM$directory)) pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "input/soils.xlsx", sep="/")) else pathtoExcel<-normalizePath("input/soils.xlsx")
    locations<-getSheetNames(pathtoExcel)
    if (any(! PARAMSIM$cases$soilname %in% locations)) stop("soils", setdiff(PARAMSIM$cases$soilname, locations), "are not in the soils.xlsx file")
    #read the new names of soil parameters from sheet "other" of allvariables.xlsx
    trad<-read.xlsx("allvariables.xlsx", sheet="other")
    trad<-trad[trad$typeinthemodel=="SoilParameter",]
    newnames<-trad$name ; names(newnames)<-trad$translationSSM
    ALLSOILS<-list()
    if(any(! PARAMSIM$cases$soilname %in% getSheetNames(pathtoExcel))) stop("the following soils are not in the sheets of ", pathtoExcel, ": ", paste(setdiff(PARAMSIM$cases$soilname, getSheetNames(pathtoExcel), collapse=", ")))
    for (sheet in PARAMSIM$cases$soilname) {
      df1<-read.xlsx(pathtoExcel, sheet=sheet, colNames=TRUE, startRow =3, rows=3:4, cols=1:6) 
      names(df1)[names(df1) %in% names(newnames)]<-newnames[names(df1)[names(df1) %in% names(newnames)]]
      #df2<-read.xlsx(pathtoExcel, sheet=sheet, colNames=TRUE, startRow =6, rows=6:(6+df1$pNLayer)) 
      df2<-read.xlsx(pathtoExcel, sheet=sheet, colNames=TRUE, startRow =6, rows=6:16)  #we read all 10 layers, so that all objects have the same length (to allow using arrays instead of lists)      
      names(df2)[names(df2) %in% names(newnames)]<-newnames[names(df2)[names(df2) %in% names(newnames)]]
      if (length(ALLSOILS)==0) {
        ALLSOILS<-as.list(df1)
        ALLSOILS$paramlayers<-array(0, dim=c(nrow(PARAMSIM$case), ncol(df2), 10), 
                                    dimnames=list(case=rownames(PARAMSIM$case), variable=colnames(df2), layer=1:10))
        ncases<-1
        ALLSOILS$paramlayers[ncases,,]<-t(as.matrix(df2)) # list(df2)
      } else { 
        ncases<-ncases+1
        for (n in names(df1)) ALLSOILS[[n]]<-c(ALLSOILS[[n]], df1[,n])
        ALLSOILS$paramlayers[ncases,,]<-t(as.matrix(df2))#c(ALLSOILS$paramlayers, list(df2))
      }
    }
  } else if(PARAMSIM$soilformat=="D4Declicplatform"){
    translationssoil<-c("SAT", "DUL", "LL", "DRAINF", "REF_BULK")
    names(translationssoil)<-c("pSaturation", "pFieldCapacity", "pWiltingPoint", "pSoilDrainageFactor", "pSoilBulkDensity")
    #other info in database: EXTR OC (organic carbon?), DULg, PO, e
    #missing info for SSM: "pSoilDryness", "pInitialWater", "pCoarseSoilFraction", 
    #"pInitialOrgNPercentage", "pFractionMineralizableN", "pInitialNO3Concentration", "pInitialNH4Concentration"
    #pInitialResiduesWeight 
    ALLSOILS<-list()
    if(exists("USERID", envir=ICI)) {
      if (dir.exists(paste(paste0("user_", USERID)))){
        path<-normalizePath(paste(paste0("user_", USERID), "inputplatform/Soil.csv", sep="/")) 
      } else path<-normalizePath("inputplatform/Soil.csv")
    } else path<-normalizePath("inputplatform/Soil.csv")
    csvcontent<-read.csv(path) 
    
    if(nrow(csvcontent)>1) {
      warning("Soil.csv in inputplatform contained more than 1 row, only the first row is used")
      csvcontent<-csvcontent[1,]
    }
    topsoildepth<-csvcontent$t_depth
    subsoilthickness<-csvcontent$s_depth-csvcontent$t_depth
      ALLSOILS$pNLayer<-2
      ALLSOILS$pDrainLayer<-2
    
    ALLSOILS$pSoilAlbedo<-csvcontent$salb
    ALLSOILS$pU<-6 #explication d'Helene: U est un parametre qui ne sert que lorsqu'on active l'option de calcul de l'evaporation selon la formule de Ritchie (evaporation en 2 temps, "semethod=1" dans le code, je crois et dans ce cas-la, on le fixe toujours a 6 (U indique une nombre de jours necessaires depuis la derniere pluie pour la transition  entre les deux stades d'evaporation si je me souviens bien)
    ALLSOILS$pSoilCurveNumber<-csvcontent$cn
    ALLSOILS$pVPDcoef<-0.75 # A coefficient to calculate VPD; 0.65 for humid and subhumid climates and 0.75 for arid and semi-arid climates
    ALLSOILS$paramlayers<-array(0, dim=c(1, 16, 10), 
                                dimnames=list(case="sim1", 
                                              variable=c("Layer#", "pLayerThickness", "pSaturation", "pFieldCapacity",
                                                         "pWiltingPoint", "pSoilDryness", "pInitialWater", "pSoilDrainageFactor",
                                                         "pCoarseSoilFraction", "pSoilBulkDensity", "pInitialOrgNPercentage", 
                                                         "pFractionMineralizableN", "pInitialNO3Concentration",
                                                         "pInitialNH4Concentration", "pInitialResiduesWeight", "pInitialResiduesNCon"), layer=1:10))
    ALLSOILS$paramlayers[1,"pLayerThickness",1]<-topsoildepth
    ALLSOILS$paramlayers[1,"pLayerThickness",2]<-subsoilthickness
    ALLSOILS$paramlayers[1,"Layer#",1]<-1
    ALLSOILS$paramlayers[1,"Layer#",2]<-2
    ALLSOILS$paramlayers[1,names(translationssoil),1]<-unname(unlist(csvcontent[,tolower(paste("T_", translationssoil,sep=""))]))
    ALLSOILS$paramlayers[1,names(translationssoil),2]<-unname(unlist(csvcontent[,tolower(paste("S_", translationssoil,sep=""))]))
    for (n in 1:2) { #the other parameters are fixed until I understand how to compute them from soil data
      ALLSOILS$paramlayers[1,"pSoilDryness",n]<-ALLSOILS$paramlayers[1,"pWiltingPoint",n]/3 #explication d'Helene: ADRY est fixe par defaut egal  a LL/3 en l'absence de donnees permettant de faire mieux (meme si dans les sols tres argileux a fentes de retrait cela  peut etre significativement  faux)
      ALLSOILS$paramlayers[1,"pInitialWater",n]<-ALLSOILS$paramlayers[1,"pWiltingPoint",n] #explication d'Helene: iWL est le stock d'eau au demarrage de la simulation... donc ca depend de quand tu demarres la simulation. En general en Mediterranee, on demarre les simulations le 30 aout avec un sol vide (iwL=LL)
      ALLSOILS$paramlayers[1,"pCoarseSoilFraction",n]<-0 #explication d'Helene: FG...ne peut pas se deduire de la base de donnees avec cette base la (HSWD). Soit tu le fixes ce parametre a 0 (en considerant que les valeurs de %A,%L et %L fournies dans la bd sont pour la totalite du sol, soit tu le fixes a une valeur constante (e.g; 0.15 si tu as de bonnes raisons de penser que cela sera plus representatif que 0). Dans certaines bases de donnees, %S est divise en deux sous categories "sables grossiers"/"coarse sands" et "sables fins/fine sands" dans ce cas, j'utilise le premier pour definir FG et l'autre pour definir %S et par ricochet SAT, DUL, EXT  etc.
      ALLSOILS$paramlayers[1,"pInitialOrgNPercentage",n]<-0.02 #explication d'Helene: NORG se calcule a partir de MO avec l'approximation NO=MO/20 (d'apres la composition moyenne de la matiere organique du sol)
      ALLSOILS$paramlayers[1,"pFractionMineralizableN",n]<-0.1 #explication d'Helene: FMIN est souvent fixe a 0.1, voire 0.01 dans les couches profondes du sols (d'apres plusieurs estimations faites a partir de mesures dans le sols mediterraneens)
      ALLSOILS$paramlayers[1,"pInitialNH4Concentration",n]<-0 #explication d'Helene: cf ci-dessous
      ALLSOILS$paramlayers[1,"pInitialResiduesWeight",n]<-0 # explication: nouveau parametre ajoute par Achille, il faudra trouver des valeurs initiales raisonables
      ALLSOILS$paramlayers[1,"pInitialResiduesNCon",n]<-200 # explication: nouveau parametre ajoute par Achille, il faudra trouver des valeurs initiales raisonables
      
    }
    #explication d'Helene: NO3- et NH4+ ne sont pas lisibles dans une base de donnees non plus car hautement variables. Quand je n'ai pas d'info, je mets NH4+=0 et je me concentre sur le NO3-,. Selon le systeme de culture, j'estime qu'il est d'environ 20 a 50 kg de N residuel au semi, dont les 2/3 dans la couche superieure du sol. Je fais la conversion en utilisant la densite apparente pour trouver la valeur en g N. g-1 sol.
    #we suppose that the residual nitrate concentration is 35 kgN/ha, 2/3 of which is in the top layer
    #35 kg over whole soil => 23.3 in top layer (this doesn't make sense to use a constant proportion if the layer thickness is not the same but whatever), and 11.7 in bottom layer
    #23.3 kg N = 23.3*4.4268 kg NO3 = 23.3*4.4268 *10e6 mg
    #soil volume = 100*100*100*100*Pthick cm3 => soil weight = 10e8*Pthick * BulkDensity g (=>10e5*Pthick*bulkDens kg)
    ALLSOILS$paramlayers[1,"pInitialNO3Concentration",1]<-(23.3*4.4268)/(ALLSOILS$paramlayers[1,"pSoilBulkDensity",1]*ALLSOILS$paramlayers[1,"pLayerThickness",1])*10 
    ALLSOILS$paramlayers[1,"pInitialNO3Concentration",2]<-(11.7*4.4268)/(ALLSOILS$paramlayers[1,"pSoilBulkDensity",2]*ALLSOILS$paramlayers[1,"pLayerThickness",2])*10
    
    for (n in 3:10) {
      ALLSOILS$paramlayers[1,"Layer#",n]<-n
    }
    } else  {
    stop("Only standard SSM format or D4Declicplatform is supported for soil data")
  }
  ALLSOILS<<-ALLSOILS
  return()
} #end read soil

eReadCrop<-function(){
  if(!is.null(PARAMSIM$directory)) pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "input/crops.xlsx", sep="/")) else pathtoExcel<-normalizePath("input/crops.xlsx")
  #first, look at the necessary crops
  requiredCrops<-unique(do.call(c, as.list(PARAMSIM$cases$rotation)))
  toto<-eReadExcelCropParameters(xlsxfile=pathtoExcel,
                                 allvariablesfile="allvariables.xlsx")
  if(any(! requiredCrops %in% rownames(toto))) stop("Crops ", paste(setdiff(requiredCrops, rownames(toto)), collapse=", "), " are missing from file crops.xlsx, which contains only ", paste(rownames(toto), collapse=","))
  ALLCROPS<<-toto
  return()
}#end read crops

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
  paramscrops<-list()
  data<-read.xlsx(xlsxfile, sheet="allcrops", rowNames=FALSE, colNames=FALSE, na.strings ="-")
  tdata<-as.data.frame(t(as.matrix(data[3:nrow(data), 3:ncol(data)])))
  names(tdata)<-gsub(pattern=" ", replacement="", gsub(pattern=":", replacement="", gsub(pattern="=", replacement="", data[3:nrow(data), "X1"])))
  tdata$name<-paste(tdata$CROP, tdata$Cultivar, sep=".")
  if (any(duplicated(names(tdata)))) stop("excel crop parameters file contains duplicated parameter names:", paste(names(tdata)[duplicated(names(tdata))], collapse=", "))
  translations<-trad$name ; names(translations)<-trad$translationSSM
  names(tdata)[names(tdata) %in% names(translations)]<-translations[names(tdata) [names(tdata) %in% names(translations)]]
  if (any(duplicated(names(tdata)))) stop("allvariables.xlsx names created duplicated parameter names:", paste(names(tdata)[duplicated(names(tdata))], collapse=", "))
  #transformation of types
  numericparam<-intersect(trad[trad$typeR=="numeric", "name"], names(tdata))
  #find which columns resultd in NA
  for (v in numericparam) {
    if (sum(is.na(tdata[,v]))<sum(is.na(as.numeric(tdata[,v])))) print(paste("parameter", nomsexcel[v], "is supposed to be numeric but contains characters in file", xlsxfile))
  }
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
  #check that we have one management for each crop in the rotation in PARAMSIM
  lengthrotation<-sapply(PARAMSIM$cases$rotation, length)
  numbermanag<-sapply(PARAMSIM$cases$management, length)
  notthesame<-lengthrotation!=numbermanag
  if(any(notthesame)) stop("In SimulationOptions.xlsx, you don't have the same number of management plans as crops in the rotation in lines ", paste(which(notthesame), collapse=", "))
  requiredManag<-unique(do.call(c, as.list(PARAMSIM$cases$management)))
  if (PARAMSIM$managformat=="standardSSM") { #read file only once and load it in the workspace
    if(!is.null(PARAMSIM$directory)) pathtoExcel<-normalizePath(paste(PARAMSIM$directory, "input/managementPlans.xlsx", sep="/")) else pathtoExcel<-normalizePath("input/managementPlans.xlsx")
    locations<-getSheetNames(pathtoExcel)
    if(length(locations)>1) warning("Your managementPlans.xlsx file has several sheets, but only the first one will be used")
    firstcol<-read.xlsx(pathtoExcel, sheet=1, colNames=FALSE, cols=1, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    startManag<- which(firstcol$X1=="Code")
    allmanag<-list()
    for (i in 1:length(startManag)){
      dfCode<-read.xlsx(pathtoExcel, sheet=1, rows=startManag[i]:(startManag[i]+1), cols=1:3)
      dfSowing<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+2):(startManag[i]+3), cols=1:12)
      # ----------Achille 24 / 08 / 2021 Added variables for tillage module----------
      tillageScenario<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+5), cols=9, colNames=FALSE)[1,1]
      tillageTotalNumber<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+6), cols=9, colNames=FALSE)[1,1]
      tillageDatetype<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+7), cols=9, colNames=FALSE)[1,1]
      tillagedf<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+9):(startManag[i]+9+tillageTotalNumber), cols=8:10)
      names(tillagedf) <- c("DAPorCBD", "frac", "tillageNumber")
      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
      nitrogenScenario<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+5), cols=2, colNames=FALSE)[1,1]
      nitrogenNumber<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+6), cols=2, colNames=FALSE)[1,1]
      if(is.null(nitrogenNumber)) nitrogenNumber<-0
      nitrogenDatetype<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+7), cols=2, colNames=FALSE)[1,1]
      nitrogendf<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+9):(startManag[i]+9+nitrogenNumber), cols=1:4)
      names(nitrogendf)<-c("NapplNumber", "DAPorCBD", "amount", "FracVol")
      waterLevel<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+6), cols=7, colNames=FALSE)[1,1]
      waterScenario<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+5), cols=6, colNames=FALSE)[1,1]
      waterNumber<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+6), cols=6, colNames=FALSE)[1,1]
      if(is.null(waterNumber)) waterNumber<-0
      waterDatetype<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+7), cols=6, colNames=FALSE)[1,1]
      waterdf<-read.xlsx(pathtoExcel, sheet=1, rows=(startManag[i]+9):(startManag[i]+9+waterNumber), cols=5:6)
      names(waterdf)<-c("DAPorCBDorDOY", "amount")
      managementPlan<-list(dfCode, dfSowing, nitrogenScenario, nitrogenNumber, nitrogenDatetype, nitrogendf, waterLevel, waterScenario, waterNumber, waterDatetype, waterdf)
      names(managementPlan)<-c("dfCode", "dfSowing", "nitrogenScenario", "nitrogenNumber", "nitrogenDatetype", "nitrogendf", "waterLevel", "waterScenario", "waterNumber", "waterDatetype", "waterdf")
      toto<-list(managementPlan) ; names(toto)<-managementPlan$dfCode$Code #names(toto)<-paste("row", startManag[i]-1)
      allmanag<-c(allmanag, toto)
    }
    } else if(PARAMSIM$managformat=="D4Declicplatform"){
      if(exists("USERID", envir=ICI)) {
        if (dir.exists(paste(paste0("user_", USERID)))){
          path<-normalizePath(paste(paste0("user_", USERID), "inputplatform/managementPlans.json", sep="/")) 
          
        } else path<-normalizePath("inputplatform/managementPlans.json")
      } else path<-normalizePath("inputplatform/managementPlans.json")
      library(jsonlite)
      print(paste("reading management plan from json", path))
      allmanag<-fromJSON(readLines(path))
      allmanag<-lapply(1:nrow(allmanag$CMP), function(i) return(as.list(allmanag$CMP[i,])))
      names(allmanag)<-sapply(allmanag, function(liste) liste$name)
      allmanag<-lapply(allmanag, function (liste) {
        liste$name<-NULL; 
        liste$dfCode<-liste$dfCode[[1]]
        liste$dfSowing<-liste$dfSowing[[1]]
        liste$"nitrogendf"<-liste$"nitrogendf"[[1]]
        liste$"waterdf"<-liste$"waterdf"[[1]]
        return(liste)
      })
    } else  {
      stop("Only standard SSM or D4Declicplatform formats are supported for crop management data")
    }
  if(any(! requiredManag %in% names(allmanag))) stop("management plans ", paste(setdiff(requiredManag, names(allmanag)), collapse=", "), " are missing from file managementPlans.xlsx")
  ALLMANAGEMENTS<<-allmanag
}


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
