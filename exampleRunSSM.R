library(openxlsx)
##### a faire dans la console de l utilisateur

setup<-function(modelfolder) #moldelfolder is the folder containing files SSM.R and allvariables.xlsx
{
  #setup= fonction qui cree un objet "modele", contenant ses fonctions de manipulation, a partir du chemin du dossier qui contient le code du modele et le fichier excel des variables
  ICI<-environment()
  setwd(modelfolder) #folder where the "model data" (e.g. list of variables and function definition files) are
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


# prepare cases (these will be the rows of ALLSIMULATEDDATA:
#mycases<-data.frame(climatename="Ain Hamra - Meknes", soilname="325_-35", lat=c(35, 35, 45), long=-5)
#climatename: one of the sheet names of file climates (if climate in standard SSM format)
#soilname: one of the sheet names of file soils (if in standard SSM format)
#to do: define crop rotation and management (once management procedure is completed)
#rownames(mycases)<-c("Meknes35degresWheat", "Meknes35degresMaize", "Meknes35degresChickpea") #these will be the cases names used in the plots, outputs etc...
#OR read from excel file
mycases<-read.xlsx("/Users/lamacina/Documents/GitHub/SSM.R/input/SimulationOptions.xlsx", sheet="cases")
rownames(mycases)<-mycases$name
mycases$rotation<-list(sapply(mycases$rotation, function(x) eval(parse(text=paste("c(", x, ")"))), USE.NAMES = FALSE))
mycases$management<-list(sapply(mycases$management, function(x) eval(parse(text=paste("c(", x, ")"))), USE.NAMES = FALSE))

paramsim<-list(
  simustart=as.Date("1997-11-01"), #date of start of the simulation
  cases=mycases, #cases (e.g. spatial locations, soils, latitudes etc... = rows in ALLSIMULATEDDATA)
  #directory="/Users/user/Documents/a_System/modelisation/SSM/simulations/premieressai", #directory where your input (with climates and soils files) and output folders are
  #directory="/Users/user/Documents/b_maison/congeMat/D4DECLIC/runSSM",#directory where your input (with climates and soils files) and output folders are
  directory="/Users/LA_MACINA/Documents/GitHub/SSM.R",
  climateformat="standardSSM",
  cropformat="standardSSM",
  soilformat="standardSSM",
  managformat="standardSSM",
  Neffect=TRUE
)

#build the model
#mymodel<-setup("/Users/user/Documents/a_System/modelisation/SSM/traductionSSM_R/")
mymodel<-setup("/Users/LAMACINA/Documents/GitHub/SSM.R/")
#set the simulation options
mymodel$setoptions(paramsim)
mymodel$run(0) #just to initialise the model
mymodel$GetAllForDebuggingPurposes()

#run the model for 100 timesteps
mymodel$run(200)

#plot the dynamics of some variables
#checking weather module
if (FALSE) {
  dynamiques<-mymodel$plot(c("iTASMin", "iTASMax", "iRSDS", "iPr"),
                           casestoplot="Mauguio",
                           col=c(iTASMin="red", 
                                 iTASMax="blue", 
                                 iRSDS="green",
                                 iPr="black"),
                           whatcol="variables", lty=1, pch="")
  
  #mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
  #             col=c(Meknes35degres=1, Meknes45degres=8), whatcol="cases",
  #             lty=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlty="variables")
}

#checking Management module
if (FALSE) {
  #mymodel$extractVariable("sLastSowing")
  dynamiques<-mymodel$plot("sLastSowing", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("sLastHarvest", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("cCycleEndType", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=0, pch=15)
}

#checking stresses module
if (FALSE) {
  dynamiques<-mymodel$plot("sWater.1", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("sWater.2", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  cols<-1:2 ; names(cols)<-paste("sWater", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("sWater", 1:2, sep="."), 
                           lty=c(Meknes35degresWheat=1, 
                                 Meknes35degresMaize=2, 
                                 Meknes35degresChickpea=3),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  dynamiques<-mymodel$plot("sRootFrontDepth", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("cEfficientRootLength", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("cFTSWweightedByRoots", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot(c("cCoefWaterstressGrowth", "cCoefWaterstressLeafArea", "cCoefWaterstressDevelopment"),
                           casestoplot=c("Meknes35degresWheat"),
                           col=c(cCoefWaterstressGrowth="orange", 
                                 cCoefWaterstressLeafArea="blue", 
                                 cCoefWaterstressDevelopment="red"),
                           whatcol="variables", lty=1, pch="")
  #warning= WSFD can be higher than 1, is it normal?
}

#checking phenology module
if (FALSE) {
  dynamiques<-mymodel$plot("sGrowthStageNumber", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("sBiologicalDaysSinceSowing", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  
  #conc: photoperiod stops wheat growth
  dynamiques<-mymodel$plot(variablestoplot=c("cCoefPhotoPeriod", "cPhotoDuration", "pCriticalPhotoPeriod", "pPhotoPeriodSensitivity"),
                           casestoplot=c("Meknes35degresWheat"),
                           col=c("red", "blue", "green", "orange"),
                           whatcol="variables", lty=1, pch="")
  #because fComputeCoefPhotoperiodWheat with the current wheat parameters starts being more than 0 at 11.6 h of daylength :
  #plot(seq(0,24, by=0.2), mymodel$getglobal("fComputeCoefPhotoperiodWheat")(seq(0,24, by=0.2), CriticalPhotoPeriod=14, PhotoPeriodSensitivity=0.17), type="l")
  #and in novembre, the daylength is already below 11.6
  #plot(seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by=1), 
  #    mymodel$getglobal("fPhotoperiodDuration")(seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by=1), lat=35), type="l")
  #abline(h=11.6) ; abline(v=as.Date("2019-11-01"))
  dynamiques<-mymodel$plot(c("cCoefVernalization","cCoefPhotoPeriod", "cCoefTemp", "cCoefWaterstressDevelopment", "cCoefDrySoilSurface"),
                           casestoplot=c("SidiKacem"),
                           col=c(cCoefVernalization="green",
                                 cCoefPhotoPeriod="yellow", 
                                 cCoefTemp="red", 
                                 cCoefWaterstressDevelopment="blue",
                                 cCoefDrySoilSurface="orange"),
                           whatcol="variables", lty=1, pch=NA)
}

#checking LAI module : decrease without N
if (FALSE) {
  dynamiques<-mymodel$plot("sLAI", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("cDecreaseLAI", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("cDecreaseLAI", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="", ylim=c(0,3000))
  
  dynamiques<-mymodel$plot("cHeat", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("cFrost", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="", ylim=c(0,2500))
  
}

#checking DMProduction module
if(FALSE){
  dynamiques<-mymodel$plot("cRUE", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  dynamiques<-mymodel$plot("cDryMatterProduction", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
}

#checking DMDistribution module
if(FALSE){
  dynamiques<-mymodel$plot("sAccumulatedLeafDryMatter", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  dynamiques<-mymodel$plot("sAccumulatedStemDryMatter", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  dynamiques<-mymodel$plot("sAccumulatedGrainDryMatter", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
}

#checking root growth module
if(FALSE){
  dynamiques<-mymodel$plot("sRootFrontDepth", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
}

#checking water module
if(FALSE){
  dynamiques<-mymodel$plot("cRunoff", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("cPET", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("cActualSoilEvaporation", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("cTranspiration", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  dynamiques<-mymodel$plot("cDrain", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="") 
  
  
  cols<-1:2 ; names(cols)<-paste("sWater", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("sWater", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="") 
  
}

# checking nitrogen module
if (FALSE) {
  # pour les couleurs:
  # r?parties sur le spectre: rainbow(n)
  # contigues: heat.colors(n)
  dynamiques<-mymodel$plot("cSoilTemp", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("cSoilTempOnDenitrification", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("cSoilTempOnMineralization", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  dynamiques<-mymodel$plot("cTotalSolubleN", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  cols<-1:2 ; names(cols)<-paste("sSolubleN", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("sSolubleN", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  dynamiques<-mymodel$plot("sCumulatedNMineralization", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  cols<-1:2 ; names(cols)<-paste("cNMineralization", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("cNMineralization", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  cols<-1:2 ; names(cols)<-paste("cMoistureOnMineralization", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("cMoistureOnMineralization", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  
  cols<-1:2 ; names(cols)<-paste("sMineralizableN", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("sMineralizableN", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  cols<-1:2 ; names(cols)<-paste("sAvailableUptakeN", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("sAvailableUptakeN", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  dynamiques<-mymodel$plot("sTotalAvailableUptakeN", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  cols<-1:2 ; names(cols)<-paste("cNSoilUptake", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("cNSoilUptake", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="",
                           )
  
  cols<-1:2 ; names(cols)<-paste("cActualTranspirableWater", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("cActualTranspirableWater", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  cols<-1:2 ; names(cols)<-paste("cFractionTranspirableWater", 1:2, sep=".")
  dynamiques<-mymodel$plot(paste("cFractionTranspirableWater", 1:2, sep="."), 
                           lty=c(Meknes=1,
                                 Turgutlu=2,
                                 SidiKacem=3,
                                 Mauguio=4,
                                 Bizerte=5),
                           whatlty="cases", 
                           whatcol="variables", 
                           col=cols, 
                           pch="")
  
  dynamiques<-mymodel$plot("cFractionTranspirableWater.1", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
  
  
  dynamiques<-mymodel$plot("cFractionTranspirableWater.2", 
                           col=c(Meknes="green",
                                 Turgutlu="red",
                                 SidiKacem="blue",
                                 Mauguio="yellow",
                                 Bizerte="purple"),
                           whatcol="cases", lty=1, pch="")
}


# checking Plant nitrogen module

if (FALSE) {
  
dynamiques<-mymodel$plot("sNitrogenContent", 
                         col=c(Meknes="green",
                               Turgutlu="red",
                               SidiKacem="blue",
                               Mauguio="yellow",
                               Bizerte="purple"),
                         whatcol="cases", lty=1, pch="")




dynamiques<-mymodel$plot("sAccumulatedNStem", 
                         col=c(Meknes="green",
                               Turgutlu="red",
                               SidiKacem="blue",
                               Mauguio="yellow",
                               Bizerte="purple"),
                         whatcol="cases", lty=1, pch="")


dynamiques<-mymodel$plot("sAccumulatedLeafNitrogen", 
                         col=c(Meknes="green",
                               Turgutlu="red",
                               SidiKacem="blue",
                               Mauguio="yellow",
                               Bizerte="purple"),
                         whatcol="cases", lty=1, pch="")

dynamiques<-mymodel$plot("sAccumulatedNGrain", 
                         col=c(Meknes="green",
                               Turgutlu="red",
                               SidiKacem="blue",
                               Mauguio="yellow",
                               Bizerte="purple"),
                         whatcol="cases", lty=1, pch="")


dynamiques<-mymodel$plot("sTotalAccumulatedNitrogen", 
                         col=c(Meknes="green",
                               Turgutlu="red",
                               SidiKacem="blue",
                               Mauguio="yellow",
                               Bizerte="purple"),
                         whatcol="cases", lty=1, pch="")

dynamiques<-mymodel$plot("cDemandNAccumulation", 
                         col=c(Meknes="green",
                               Turgutlu="red",
                               SidiKacem="blue",
                               Mauguio="yellow",
                               Bizerte="purple"),
                         whatcol="cases", lty=1, pch="")


}
