
#uppercase variables: global variables of the model that can be modified by procedures without being passed as arguments


VARIABLEDEFINITIONS<-read.xlsx("allvariables.xlsx", sheet="savedEachDay", colNames=TRUE) #all variables (not only state variables) are saved for traceability
types<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"] ; names(types)<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "name"] #date has a special status: it is the only variable that is compulsory, because it is created by itself in creatdataframe
#### creation of the variable to store the simulation parameters
PARAMSIM<-list(types=types) #types of each variable to create : vector of types (numeric, character, logical) named by variable name
#other elements of PARAMSIM (simustart, cases, directory, climateformat and soilformat) will be defined after setup

#pour debuggage, creation de PARAMSIM complet:
'
mycases<-data.frame(climatename="Ain Hamra - Meknes", soilname="325_-35", lat=c(35, 45), long=-5)
rownames(mycases)<-c("Meknes35degres", "Meknes45degres")
PARAMSIM<-list(types=types,
simustart=as.Date("1997-11-01"), #date of start of the simulation
cases=mycases, #cases (e.g. spatial locations, soils, latitudes etc... = rows in ALLSIMULATEDDATA)
directory="/Users/user/Documents/a_System/modelisation/SSM/simulations/premieressai", #directory where your input/output folders are
climateformat="standardSSM",
soilformat="standardSSM"
)

'

##### creation of the list of days only the first time
ALLSIMULATEDDATA<-list()

##### definition of constants

#### PARAMETERS
ALLPARAMETERS<-list(pCoefPAR=0.48)

#### initialisation of variable to store climate if read once at the beginning
ALLCLIMATE<-list()
ALLSOILS<-list()
