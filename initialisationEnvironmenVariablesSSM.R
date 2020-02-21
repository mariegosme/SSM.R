
#uppercase variables: global variables of the model that can be modified by procedures without being passed as arguments

VARIABLEDEFINITIONS<-read.xlsx("allvariables.xlsx", sheet="savedEachDay", colNames=TRUE) #all variables (not only state variables) are saved for traceability
#we sort them by type, module and name
VARIABLEDEFINITIONS<-VARIABLEDEFINITIONS[order(VARIABLEDEFINITIONS$module, VARIABLEDEFINITIONS$name),]
VARIABLEDEFINITIONS<-VARIABLEDEFINITIONS[order(VARIABLEDEFINITIONS$typeinthemodel, decreasing=TRUE),]
ALLSIMULATEDDATA<-list() #list of data.frames from the previous timesteps (here: day 0)
ALLDAYDATA<-data.frame() #data.frame (row = case, column = variable) of current daily variables  
##### definition of constants
GENERALPARAMETERS<-read.xlsx("allvariables.xlsx", sheet="generalPhysicalParameters", colNames=TRUE) #general parameters
rownames(GENERALPARAMETERS)<-GENERALPARAMETERS$name
#### initialisation of variable to store climate if read once at the beginning
ALLCLIMATES<-list()
ALLSOILS<-list()
ALLCROPS<-list()
ALLMANAGEMENTS<-list()
#initialisation of PARAMSIM
PARAMSIM<-list()
