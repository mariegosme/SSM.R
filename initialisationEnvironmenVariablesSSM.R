
#uppercase variables: global variables of the model that can be modified by procedures without being passed as arguments

VARIABLEDEFINITIONS<-read.xlsx("allvariables.xlsx", sheet="savedEachDay", colNames=TRUE) #all variables (not only state variables) are saved for traceability
##### creation of day 0 (before start of simulation)
types<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "typeR"]
names(types)<-VARIABLEDEFINITIONS[VARIABLEDEFINITIONS$name!="iDate", "name"]
##warning: this lapply works only for numeric and character!!!!!!
if (sum( !types %in% c("numeric", "character"))>0) stop("all variables should be either numeric or character, check variables", paste(names(types)[!types %in% c("numeric", "character")], collapse=","))
df<-cbind(data.frame(iDate=PARAMSIM$simustart-1),
            as.data.frame(lapply(types, FUN=function(x) return(do.call(x, list(nrow(PARAMSIM$cases)))))))
rownames(df)<-rownames(PARAMSIM$cases)
#we start without crop (sowing in the future, harvest in the past)
df$sLastSowing<-Inf 
df$sLastHarvest<- (-Inf)
#icicici : actually we initialize with a crop everywhere because crop management hasnt been coded yet
df$sLastSowing<-0 
df$sLastHarvest<- (-Inf)
df$sCrop<-"WHEAT"
df$sCultivar<-c("durum wheat", "toto")
df$sGrowthStage<-"germination"
df$sCumulatedPhenoCounts<-0
df$sPlantdensity<-280

ALLSIMULATEDDATA<-list(df) #list of data.frames from the previous timesteps (here: day 0)
ALLDAYDATA<-data.frame() #data.frame (row = case, column = variable) of current daily variables  
rm(types, df)
##### definition of constants
GENERALPARAMETERS<-read.xlsx("allvariables.xlsx", sheet="generalPhysicalParameters", colNames=TRUE) #general parameters
rownames(GENERALPARAMETERS)<-GENERALPARAMETERS$name
#### initialisation of variable to store climate if read once at the beginning
ALLCLIMATE<-list()
ALLSOILS<-list()
ALLCROPS<-list()
ALLMANAGEMENTS<-list()
