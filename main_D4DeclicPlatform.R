options(stringsAsFactors=FALSE)
library(openxlsx)
Sys.setlocale("LC_TIME", "English") #on Windows
Sys.setlocale("LC_TIME", "en_GB.UTF-8") #on Mac
Sys.setlocale(category = "LC_TIME", locale = "en_US") #on other systems
Sys.setlocale(category = "LC_TIME", locale = "C") #on other systems

#' main program for running SSM.R on the D4Declic platform
#'
#' @param NbDaysToRun number of simulation days to run
#' @param inputsfromplatform whether or not to look for use the platform formats (csv and json) instead of standard SSM (excel) inputs
#' @param userid sequential number of the user. If NULL( the default), the input and outputs are not specific for one given user and are overwritten
#' @param userid_fileonly should the user id be used only to name the wholedata_n.csv file (the default, n is replaced by userid) or should we have a folder with both input and outputs specific for a user?
runModelD4DECLIC<-function(NbDaysToRun, inputsfromplatform=TRUE, userid=NULL, userid_fileonly=TRUE){
  setup<-function() 
  {
    #setup= fonction qui cree un objet "modele", contenant ses fonctions de manipulation, a partir du chemin du dossier qui contient le code du modele et le fichier excel des variables
    ICI<-environment()
    if(is.null(userid) & !userid_fileonly) stop("it's not possible to create folders for the user (userid_fileonly=FALSE) because there is no user ID (userid=NULL)")
    if(!is.null(userid)) USERID<-userid
    source("headersSSM.R", local=TRUE)
    source("initialisationEnvironmenVariablesSSM.R", local=TRUE)
    source("externalFilesReadingSSM.R", local=TRUE)
    source("HousekeepingFunctionsSSM.R", local=TRUE)
    source("handlersSSM.R", local=TRUE)
    source("functionsSSM.R", local=TRUE)
    return(list(contains=mContains, # fonction qui liste les objets presents dans le modele (contains) et les extrait (getglobal),
                #doinside=evalICI,
                #getparam=getparam, #fonction qui renvoie les parametres du modele pour verification (getparam), et qui les modifie (setparam),
                #setparam=print("reverifier setparam, en particulier ce qui doit etre recalcule une fois au debut de la simu"), #setparam,
                GetAllForDebuggingPurposes=mGetAllForDebuggingPurposes,
                setoptions=mCompletePARAMSIM,
                getglobal=mGetGlobal,
                setglobal=mSetGlobal,
                extractVariable=mExtractVariable,
                #setoptions=setoptions, #fonction qui regle les options d'affichage et de memorisation (setoptions),
                #restart=print("reverifier restart, en particulier ce qui doit etre recalcule une fois au debut de la simu"), #restart, #fonction qui remet le modele a 0 (restart),
                run=mRun,  #fonction qui lance la simu pour n pas de temps (run),
                #map=cartesorties,
                plot=mPlotDynamics,#fonction qui plote la dynamique d'une ou plusieurs variables enregistrees (plot)
                summary=mSummary, #fonction qui resume l etat du modele: nombre de pas de temps et gamme de dates,
                ExportDataFrame=mExportDataFrame, #puts all data into one dataframe (long format with column case)
                ExportSynthesis=fExportSynthesis # synthesis of main variables (maxLAI, maxrootdepth, yield) for each cropping season
                
                
    ))
  }
  if (inputsfromplatform) {
    #read simulation options from csv, declare formats as "platform"
    if (!is.null(userid)) {
      SimulationOptionspath<-normalizePath(paste(paste0("user_", userid), "inputplatform/SimulationOptions.csv", sep="/")) 
      if (!userid_fileonly) if (!dir.exists(normalizePath(paste(paste0("user_", userid), "outputplatform", sep="/")))) { dir.create(normalizePath(paste(paste0("user_", userid), "outputplatform", sep="/")))}
    } else {
      SimulationOptionspath<-normalizePath("inputplatform/SimulationOptions.csv")
    }
    csvcontent<-read.csv(SimulationOptionspath) #one row of data with columns lat, lon, rotation, management, date (rotation and management are like 'Chickpea.Ghab2'_'MAIZE.bidule'_'WHEAT.Ble_Dur_1')
    modeloptions<-list(climateformat="D4Declicplatform",
                       cropformat="standardSSM",
                       soilformat="D4Declicplatform",
                       managformat="D4Declicplatform",
                       Neffect=FALSE)
    
    startingDate<-as.Date(csvcontent$startingDate)
    if (is.null(csvcontent$startingDate)) stop(paste("SimulationOptions.csv for inputsfromplatform must contain a column with startingDate but file", SimulationOptionspath, "does not"))
    if (is.na(startingDate)) stop("SimulationOptions.csv for inputsfromplatform must contain a startingDate in the form yyyy-mm-dd")
    modeloptions$simustart<-startingDate
    if (is.null(csvcontent$lat)) stop("SimulationOptions.csv for inputsfromplatform must contain a column with lat")
    csvcontent$lat<-as.numeric(csvcontent$lat)
    if (is.na(csvcontent$lat)) stop("column lat in SimulationOptions.csv for inputsfromplatform must be a numeric")
    if (is.null(csvcontent$lon)) stop("SimulationOptions.csv for inputsfromplatform must contain a column with lon")
    csvcontent$lon<-as.numeric(csvcontent$lon)
    if (is.na(csvcontent$lon)) stop("column lon in SimulationOptions.csv for inputsfromplatform must be a numeric")
    modeloptions$cases<-data.frame(name="sim1", climatename="sim1", soilname="sim1", lat=csvcontent$lat, long=csvcontent$lon)
    rownames(modeloptions$cases)<-"sim1"
    #breaks down rotation into crops
    crops<-lapply(strsplit(as.character(csvcontent$rotation), split="'_'"), gsub, pattern="'" , replacement="", fixed=TRUE)[[1]]
    modeloptions$cases$rotation<-list(crops)
    #use a standard crop management for each crop
    #standardmanagement<-c("ROTATION_BLE", "ROTATION_BLE_IRRIGUE", "ROTATION_BLE_IRRIGUE", "ROTATION_POISCHICHE", "ROTATION_BLE_IRRIGUE", "ROTATION_BLE_IRRIGUE", "Gorgan-RFD")
    #names(standardmanagement)<-c("WHEAT.Ble_Dur_1", "WHEAT.Ble_Tendre_1", "WHEAT.Ble_Tendre_2","Chickpea.Ghab2", "WHEAT.Avoine_Romani", "WHEAT.Cocorit", "MAIZE.bidule")
    #management<-standardmanagement[crops]
    management<-lapply(strsplit(as.character(csvcontent$management), split="'_'"), gsub, pattern="'" , replacement="", fixed=TRUE)[[1]]
    modeloptions$cases$management<-list(unname(management))
  } else { #old method 
    #read the model options from excel file (currently general options contains only simustart)
    toto<-read.xlsx(normalizePath("input/SimulationOptions.xlsx"), sheet="generalOptions")
    rownames(toto)<-toto$name
    options<-toto$name ; names(options)<-toto$name
    modeloptions<-lapply(options, function(x) do.call(toto[x, "transformation"], list(toto[x,"value"])))
    modeloptions<-c(modeloptions, list( 
      #directory=getwd(), #it will use the input folder already present in the model (used to be exampleinput)
      climateformat="standardSSM",
      cropformat="standardSSM",
      soilformat="standardSSM",
      managformat="standardSSM",
      Neffect=FALSE
    ))
    mycases<-read.xlsx(normalizePath("input/SimulationOptions.xlsx"), sheet="cases")
    rownames(mycases)<-mycases$name
    mycases$rotation<-sapply(mycases$rotation, function(x) eval(parse(text=paste("c(", x, ")"))), USE.NAMES = FALSE)
    mycases$management<-sapply(mycases$management, function(x) eval(parse(text=paste("c(", x, ")"))), USE.NAMES = FALSE)
    modeloptions$cases<-mycases
  }
  
  #create an instance of the model
  mymodel<-setup()
  #set the simulation options
  mymodel$setoptions(modeloptions)
  #run the model for n timesteps
  mymodel$run(NbDaysToRun)
  
  
  #exports
  filesexport<-list(synthesis="outputplatform/synthesictable.csv",
                    graph1="outputplatform/graph1.jpg",
                    graph2="outputplatform/graph2.jpg",
                    graph3="outputplatform/graph3.jpg",
                    wholedata="outputplatform/wholedata.csv"
                    )
  if (userid_fileonly) {
    filesexport<-lapply(filesexport, normalizePath)
    if (!is.null(userid)) filesexport$wholedata<-gsub(pattern=".csv", replacement=paste0("_", userid,".csv"), x=filesexport$wholedata, fixed=TRUE)
  } else {
    filesexport<-lapply(filesexport, function(x) normalizePath(paste(paste0("user_", userid), x, sep="/"), mustWork = FALSE))
  } 
  #export a simple graph as jpeg
  jpeg(filesexport$graph1, width=500, height = 400)
  dynamiques<-mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
                           col=c(iTASMin="blue", iTASMax="red", iRSDS="black"), whatcol="variables",
                           lty=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlty="variables",
                           pch=NA, main="Min and max temperature and solar radiation for simulation" )
  dev.off()
  #export a 2nd simple graph as  jpeg
  jpeg(filesexport$graph2, width=500, height = 400)
  summar<-mymodel$summary()
  couleurs<-rainbow(summar$ncases) ; names(couleurs)<-summar$casenames
  dynamiques<-mymodel$plot(c("sGrowthStageNumber"),
                           col=couleurs, whatcol="cases",
                           lty=1,
                           pch=NA, main="Growth stages")
  dev.off()
  #export a simple graph as jpeg
  jpeg(filesexport$graph3, width=500, height = 400)
  dynamiques<-mymodel$plot(c("sWater.1", "sWater.2"),
                           col=c(sWater.1="blue", sWater.2="red"), whatcol="variables",
                           lty=1,
                           pch=NA, main="Water content in top (blue) and bottom (red) soil layers")
  dev.off()
  #export all the model data to csv
  toto<-mymodel$ExportDataFrame()
  write.table(toto, file=filesexport$wholedata, row.names=FALSE)
  #create a summary table to return simple data
  toto$year<-format(toto$iDate, format="%Y")
  outputdata<-aggregate(toto[,c("sRootFrontDepth", "sLAI", "sAccumulatedGrainDryMatter")], 
                        by=toto[,c("year", "sCropCult")],
                        max)
  #export synthesis table
  synthesis<-mymodel$ExportSynthesis()
  write.table(synthesis, file=filesexport$synthesis, row.names=FALSE)
  return(synthesis)
}

#toto<-runModelD4DECLIC(10)
#toto<-runModelD4DECLIC(700, userid=2)
#toto<-runModelD4DECLIC(10, inputsfromplatform=TRUE)
toto<-runModelD4DECLIC(10, inputsfromplatform=TRUE, userid=76, userid_fileonly=FALSE)
