options(stringsAsFactors=FALSE)
library(openxlsx)
runModelD4DECLIC<-function(NbDaysToRun, inputsfromplatform=FALSE){
  setup<-function() 
  {
    #setup= fonction qui cree un objet "modele", contenant ses fonctions de manipulation, a partir du chemin du dossier qui contient le code du modele et le fichier excel des variables
    ICI<-environment()
    source("headersSSM.R", local=TRUE)
    source("initialisationEnvironmenVariablesSSM.R", local=TRUE)
    source("externalFilesReadingSSM.R", local=TRUE)
    source("HousekeepingFunctionsSSM.R", local=TRUE)
    source("functionsSSM.R", local=TRUE)
    source("handlersSSM.R", local=TRUE)
    return(list(contains=mContains, # fonction qui liste les objets présents dans le modèle (contains) et les extrait (getglobal),
                #doinside=evalICI,
                #getparam=getparam, #fonction qui renvoie les paramètres du modèle pour vérification (getparam), et qui les modifie (setparam),
                #setparam=print("reverifier setparam, en particulier ce qui doit etre recalcule une fois au debut de la simu"), #setparam,
                GetAllForDebuggingPurposes=mGetAllForDebuggingPurposes,
                setoptions=mCompletePARAMSIM,
                getglobal=mGetGlobal,
                setglobal=mSetGlobal,
                extractVariable=mExtractVariable,
                #setoptions=setoptions, #fonction qui règle les options d'affichage et de mémorisation (setoptions),
                #restart=print("reverifier restart, en particulier ce qui doit etre recalcule une fois au debut de la simu"), #restart, #fonction qui remet le modele à 0 (restart),
                run=mRun,  #fonction qui lance la simu pour n pas de temps (run),
                #map=cartesorties,
                plot=mPlotDynamics,#fonction qui plote la dynamique d'une ou plusieurs variables enregistréées (plot)
                summary=mSummary, #fonction qui résume l état du modèle: nombre de pas de temps et gamme de dates,
                ExportDataFrame=mExportDataFrame
    ))
  }
  if (inputsfromplatform) {
    #read simulation options from csv, declare formats as "platform"
    modeloptions<-list(climateformat="D4Declicplatform",
                       cropformat="standardSSM",
                       soilformat="D4Declicplatform",
                       managformat="standardSSM",
                       Neffect=FALSE)
    
    csvcontent<-read.csv(normalizePath("inputplatform/SimulationOptions.csv"), quote="'") #contains lat, lon, rotation
    startingDate<-as.Date(csvcontent$startingDate)
    if (is.null(csvcontent$startingDate)) stop("SimulationOptions.csv for inputsfromplatform must contain a column with startingDate")
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
    crops<-lapply(strsplit(csvcontent$rotation, split='"_"'), gsub, pattern='"' , replacement="", fixed=TRUE)[[1]]
    modeloptions$cases$rotation<-list(crops)
    #use a standard crop management for each crop
    standardmanagement<-c("ROTATION_BLE", "ROTATION_BLE_IRRIGUE", "ROTATION_BLE_IRRIGUE", "ROTATION_POISCHICHE", "ROTATION_BLE_IRRIGUE", "ROTATION_BLE_IRRIGUE", "Gorgan-RFD")
    names(standardmanagement)<-c("WHEAT.Ble_Dur_1", "WHEAT.Ble_Tendre_1", "WHEAT.Ble_Tendre_2","Chickpea.Ghab2", "WHEAT.Avoine_Romani", "WHEAT.Cocorit", "MAIZE.bidule")
    management<-standardmanagement[crops]
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
  #export a simple graph as pdf and jpeg
  pdf(normalizePath("outputplatform/graph1.pdf"), width=5, height = 4)
  dynamiques<-mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
                           col=c(iTASMin="blue", iTASMax="red", iRSDS="black"), whatcol="variables",
                           lty=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlty="variables",
                           pch=NA, main="Min and max temperature and solar radiation")
  dev.off()
  jpeg(normalizePath("outputplatform/graph1.jpg"), width=500, height = 400)
  dynamiques<-mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
                           col=c(iTASMin="blue", iTASMax="red", iRSDS="black"), whatcol="variables",
                           lty=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlty="variables",
                           pch=NA, main="Min and max temperature and solar radiation")
  dev.off()
  #export all the model data to csv
  toto<-mymodel$ExportDataFrame()
  write.table(toto, file=normalizePath("outputplatform/wholedata.csv"))
  #create a summary table to return simple data
  toto$year<-format(toto$iDate, format="%Y")
  outputdata<-aggregate(toto[,c("sRootFrontDepth", "sLAI", "sAccumulatedGrainDryMatter")], 
                        by=toto[,c("year", "sCropCult")],
                        max)
  return(outputdata)
}

#toto<-runModelD4DECLIC(10)
#toto<-runModelD4DECLIC(3*365, inputsfromplatform=TRUE)
