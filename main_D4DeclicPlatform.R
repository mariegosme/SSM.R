options(stringsAsFactors=FALSE)
library(openxlsx)
runModelD4DECLIC<-function(NbDaysToRun){
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
  #read the model options from excel file
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
  cases<-read.xlsx(normalizePath("input/SimulationOptions.xlsx"), sheet="cases")
  modeloptions$cases<-cases
  #create an instance of the model
  mymodel<-setup()
  #set the simulation options
  mymodel$setoptions(modeloptions)
  #run the model for n timesteps
  mymodel$run(NbDaysToRun)
  return(mymodel$ExportDataFrame())
}

#toto<-runModelD4DECLIC(10)