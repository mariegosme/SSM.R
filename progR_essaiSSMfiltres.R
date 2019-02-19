options(stringsAsFactors = FALSE)
rm(list=ls())
#ne pas oublier de rajouter le niveau cultivar
paramscrops<-list(wheat=list(
  name="wheat",
  thresholds=list(germination=10, croissance=30, floraison=5, maturation=10),
  paramsLAI=list(filter="is.after('germination', 4) & is.before('floraison', paramscrops$wheat$thresholds$floraison-2)", #4 days after the beginning of germination and 2 days before the end of floraison
                 speed=2),
  paramsvernalisation=list(filter="is.before('floraison')", #during germination and croissance
                           effetverna=0.5)
  ),
  maize=list(
    name="maize",
    thresholds=list(petite=50, grande=50),
    paramsLAI=list(filter="TRUE", #all the time
                   speed=4),
    paramsvernalisation=list(filter="is.before('grande')",#during the petite stage
                             effetverna=0.3)
))


applyfilters<-function(processname){
  possiblecrops<- unique(ALLDAYDATA$crop)
  cultivars<-paste(ALLDAYDATA$crop, ALLDAYDATA$cultivar, sep="_")
  evaluatecrop<-function(text, uniquecrop) {
    possiblestages<-names(paramscrops[[uniquecrop]]$thresholds)
    numstages<-1:length(possiblestages); names(numstages)<-possiblestages
    is.before<-function(stage, bd=0, crop=uniquecrop) { #if bd is not provided, the stage is not included
      currentstages<-numstages[ALLDAYDATA$stage[ALLDAYDATA$crop==uniquecrop]]
      currentbd<-ALLDAYDATA$bd[ALLDAYDATA$crop==uniquecrop]
      targetstage<-which(possiblestages==stage)
      compare<-currentstages==targetstage & currentbd<=bd | currentstages<targetstage
      result<-rep(NA, nrow(ALLDAYDATA))
      result[ALLDAYDATA$crop==uniquecrop]<-compare
      return(result)
    }
    is.after<-function(stage, bd=Inf) { #if bd is not provided, the stage is not included
      currentstages<-numstages[ALLDAYDATA$stage[ALLDAYDATA$crop==uniquecrop]]
      currentbd<-ALLDAYDATA$bd[ALLDAYDATA$crop==uniquecrop]
      targetstage<-which(possiblestages==stage)
      compare<-currentstages==targetstage & currentbd>=bd | currentstages>targetstage
      result<-rep(NA, nrow(ALLDAYDATA))
      result[ALLDAYDATA$crop==uniquecrop]<-compare
      return(result)
    }
    result<-eval(parse(text=text))
    return(result)
  }
  filtertexts<-sapply(paramscrops[possiblecrops], function(cr) return(cr[[processname]]$filter))

  filters<-mapply(FUN=evaluatecrop, filtertexts, possiblecrops, SIMPLIFY=FALSE)
  allfilters<-as.data.frame(c(list(crop=ALLDAYDATA$crop, filters))) #so that the lengths are homogenized
  resultfilter<-rep(FALSE, nrow(ALLDAYDATA))
  for(crop in possiblecrops) {
    resultfilter[ALLDAYDATA$crop==crop]<-allfilters[[crop]][ALLDAYDATA$crop==crop]
  }
  return(resultfilter)
}
#exple: toto<-applyfilters("paramsLAI")
fDeltaLAI<-function(speed) return(speed)

rLAI<-function() {
  resultfilter<-applyfilters("paramsLAI")
  speeds<-mapply(function(cropname, cultivarname) return(paramscrops[[cropname]][[cultivarname]]$paramsLAI$speed), ALLDAYDATA$crop, ALLDAYDATA$cultivar, SIMPLIFY = TRUE, USE.NAMES=FALSE) #on recupere les parametres de LAI pour chaque culture (ici, un seul parametre: speed)
  ALLDAYDATA$LAI[resultfilter]<<-ALLDAYDATA$LAI[resultfilter]+fDeltaLAI(speeds)[resultfilter]
  return()
}

nextstage<-function(crop, current) {
  possiblestages<-names(paramscrops[[crop]]$thresholds)
  return(possiblestages[which(current==possiblestages)+1])
}

rPheno<-function() {
  increment<-rep(1, nrow(ALLDAYDATA)) # de base, l increment est de 1 chaque jour
  resultfilter<-applyfilters("paramsvernalisation") #on cree le filtre des TRUE FALSE de l application de la vernalisation
  vernalisationeffect<-sapply(ALLDAYDATA$crop, function(cropname) return(paramscrops[[cropname]]$paramsvernalisation$effetverna)) #on chope le (les) parametre(s) de vernalisation (ici, un seul "vernalisationeffect)
  increment[resultfilter]<-increment[resultfilter]*vernalisationeffect[resultfilter] #on applique la fonction de vernalisation
  ALLDAYDATA$bd<<-ALLDAYDATA$bd+increment

  thresholds<-mapply(function(cropname, stage) return(paramscrops[[cropname]]$thresholds[[stage]]), ALLDAYDATA$crop, as.character(ALLDAYDATA$stage), SIMPLIFY = TRUE, USE.NAMES=FALSE) #on choope les
  changestage<-ALLDAYDATA$bd>thresholds
  ALLDAYDATA$stage[changestage]<<-unlist(mapply(nextstage, ALLDAYDATA$crop[changestage], ALLDAYDATA$stage[changestage], SIMPLIFY = TRUE, USE.NAMES=FALSE))
  # when the stage changed, we start the counter with the remaining of increment-threshold
  ALLDAYDATA[changestage,"bd"]<<-ALLDAYDATA$bd[changestage]-thresholds[changestage] #if we changed stages, we start not from 0 but from the "extra units accuulated during the timestep
  #ALLDAYDATA$bd[ALLDAYDATA$bd>thresholds]<<-0
  return()
}


#init
ALLDAYDATA<-data.frame(
  crop=c("wheat","wheat", "wheat", "wheat","maize"),
  stage=c("germination", "maturation","croissance","croissance", "petite"),
  bd=c(5,7,25, 29, 37),
  LAI=0
)
ALLDAYDATA
#essaye
rLAI()
rPheno()
#str(ALLDAYDATA)
ALLDAYDATA
ALLDAYDATA<-ALLDAYDATA[1,]

str(filters)
str(allfilters)
str(resultfilter)



rLAI(); rPheno(); ALLDAYDATA
