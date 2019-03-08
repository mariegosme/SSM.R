
##### a faire dans la console de l utilisateur

setup<-function(modelfolder) #moldelfolder is the folder containing files SSM.R and allvariables.xlsx
{
  #setup= fonction qui cree un objet "modele", contenant ses fonctions de manipulation, a partir du chemin du dossier qui contient le code du modele et le fichier excel des variables
  ICI<-environment()
  setwd(modelfolder) #folder where the "model data" (i.e. list of variables) are
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
              setoptions=mCompletePARAMSIM,
              getglobal=mGetGlobal,
              setglobal=mSetGlobal,
              #setoptions=setoptions, #fonction qui règle les options d'affichage et de mémorisation (setoptions),
              #restart=print("reverifier restart, en particulier ce qui doit etre recalcule une fois au debut de la simu"), #restart, #fonction qui remet le modele à 0 (restart),
              run=mRun,  #fonction qui lance la simu pour n pas de temps (run),
              #map=cartesorties,
              plot=mPlotDynamics#,#fonction qui plote la dynamique d'une ou plusieurs variables enregistréées (plot)
              #summary=resume #fonction qui résume l état du modèle: t actuel, ... (summary),
  ))
}


# prepare cases (these will be the rows of ALLSIMULATEDDATA:
mycases<-data.frame(climatename="Ain Hamra - Meknes", soilname="325_-35", lat=c(35, 45), long=-5)
#climatename: one of the sheet names of file climates (if climate in standard SSM format)
#soilname: one of the sheet names of file soils (if in standard SSM format)
rownames(mycases)<-c("Meknes35degres", "Meknes45degres") #these will be the cases names used in the plots, outputs etc...
PARAMSIM<-list(
  simustart=as.Date("1997-11-01"), #date of start of the simulation
  cases=mycases, #cases (e.g. spatial locations, soils, latitudes etc... = rows in ALLSIMULATEDDATA)
  directory="/Users/user/Documents/a_System/modelisation/SSM/simulations/premieressai", #directory where your input (with climates and soils files) and output folders are
  climateformat="standardSSM",
  cropformat="standardSSM",
  soilformat="standardSSM",
  Neffect=TRUE
)

#build the model
mymodel<-setup("/Users/user/Documents/a_System/modelisation/SSM/traductionSSM_R/")
#set the simulation options
mymodel$setoptions(parametersfirsttry)
#run the model for 4 timesteps
mymodel$run(4)
#plot the dynamics of some variables
dynamiques<-mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
              colors=c(iTASMin="blue", iTASMax="red", iRSDS="black"), whatcolors="variables",
              linetypes=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlinetypes="variables",
             symbols=c(Meknes35degres=1, Meknes45degres=8), whatsymbols="cases")

mymodel$plot(c("iTASMin", "iTASMax", "iRSDS"),
             colors=c(Meknes35degres=1, Meknes45degres=8), whatcolors="cases",
             linetypes=c(iTASMin=1, iTASMax=1, iRSDS=2), whatlinetypes="variables")


paramscrops<-list(wheat=list(
               name="wheat",
               thresholds=list(germination=6,emergence=5,tillering=8,stemElongation=6,Booting=6,earing=15,anthesis=43,maturation=8,senescence=Inf),
               paramsLAI=list(filter="is.after('germination', 4) & is.before('floraison', paramscrops$wheat$thresholds$floraison-2)", #4 days after the beginning of germination and 2 days before the end of floraison
                              speed=2),
               vernalisation=list(filter="is.after('emergencen', 0) & is.before('tillering',0)"),
               photoperiod=list(filter="is.after('emergence', 0) & is.before('tillering', 0)"),
               waterstress=list(filter="is.after('emergence', 0) & is.before('senescence', 10)")


             ))
