
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
              plot=mPlotDynamics#,#fonction qui plote la dynamique d'une ou plusieurs variables enregistréées (plot)
              #summary=resume #fonction qui résume l état du modèle: t actuel, ... (summary),
  ))
}


# prepare cases (these will be the rows of ALLSIMULATEDDATA:
mycases<-data.frame(climatename="Ain Hamra - Meknes", soilname="325_-35", lat=c(35, 35, 45), long=-5)
#climatename: one of the sheet names of file climates (if climate in standard SSM format)
#soilname: one of the sheet names of file soils (if in standard SSM format)
#to do: define crop rotation and management (once management procedure is completed)
rownames(mycases)<-c("Meknes35degresWheat", "Meknes35degresMaize", "Meknes35degresChickpea") #these will be the cases names used in the plots, outputs etc...
paramsim<-list(
  simustart=as.Date("1997-11-01"), #date of start of the simulation
  cases=mycases, #cases (e.g. spatial locations, soils, latitudes etc... = rows in ALLSIMULATEDDATA)
  #directory="/Users/user/Documents/a_System/modelisation/SSM/simulations/premieressai", #directory where your input (with climates and soils files) and output folders are
  directory="/Users/user/Documents/b_maison/congeMat/D4DECLIC/runSSM",#directory where your input (with climates and soils files) and output folders are
  climateformat="standardSSM",
  cropformat="standardSSM",
  soilformat="standardSSM",
  managformat="standardSSM",
  Neffect=FALSE
)

#build the model
#mymodel<-setup("/Users/user/Documents/a_System/modelisation/SSM/traductionSSM_R/")
mymodel<-setup("/Users/user/Documents/b_maison/congeMat/D4DECLIC/SSM/")
#set the simulation options
mymodel$setoptions(paramsim)
#run the model for 100 timesteps
mymodel$run(300)

#plot the dynamics of some variables
#checking weather module
if (FALSE) {
  dynamiques<-mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
                           colors=c(iTASMin="blue", iTASMax="red", iRSDS="black"), whatcolors="variables",
                           linetypes=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlinetypes="variables",
                           symbols=c(Meknes35degres=1, Meknes45degres=8), whatsymbols="cases")
  
  #mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
  #             colors=c(Meknes35degres=1, Meknes45degres=8), whatcolors="cases",
  #             linetypes=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlinetypes="variables")
}

#checking phenology module
if (FALSE) {
  dynamiques<-mymodel$plot("sGrowthStageNumber", 
                           col=c(Meknes35degresWheat="lightgreen", 
                                 Meknes35degresMaize="cornflowerblue", 
                                 Meknes35degresChickpea="purple"),
                           whatcol="cases", lty=1, pch="") 
  dynamiques<-mymodel$plot("sBiologicalDay", 
                           col=c(Meknes35degresWheat="lightgreen", 
                                 Meknes35degresMaize="cornflowerblue", 
                                 Meknes35degresChickpea="purple"),
                           whatcol="cases", lty=1, pch="") 
  dynamiques<-mymodel$plot(c("cCoefPhotoPeriod", "cCoefTemp", "cCoefWaterstressDevelopment", "cDeltaBiologicalDay"),
                           casestoplot=c("Meknes35degresWheat"),
                           col=c(cCoefPhotoPeriod="orange", 
                                 cCoefTemp="blue", 
                                 cCoefWaterstressDevelopment="red",
                                 cBiologicalDay="black"),
                           whatcol="variables")
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
  
}

#checking LAI module
if (FALSE) {
  dynamiques<-mymodel$plot("sLAI", 
                           col=c(Meknes35degresWheat="lightgreen", 
                                 Meknes35degresMaize="cornflowerblue", 
                                 Meknes35degresChickpea="purple"),
                           whatcol="cases", lty=1, pch="") 
  dynamiques<-mymodel$plot("sLAI", 
                           col=c(Meknes35degresWheat="lightgreen", 
                                 Meknes35degresMaize="cornflowerblue", 
                                 Meknes35degresChickpea="purple"),
                           whatcol="cases", lty=1, pch="", ylim=c(0,37000))
  dynamiques<-mymodel$plot("sLAI", 
                           col=c(Meknes35degresWheat="lightgreen", 
                                 Meknes35degresMaize="cornflowerblue", 
                                 Meknes35degresChickpea="purple"),
                           whatcol="cases", lty=1, pch="", xlim=as.Date(c("1997-11-01", "1997-11-10"))) 
  #strangely maize has exponential growth much more rapid than the other crops, but all crops have an increase then a decrease of LAI
  dynamiques<-mymodel$plot("cDecreaseLAI", 
                           col=c(Meknes35degresWheat="lightgreen", 
                                 Meknes35degresMaize="cornflowerblue", 
                                 Meknes35degresChickpea="purple"),
                           whatcol="cases", lty=1, pch="")
  
}