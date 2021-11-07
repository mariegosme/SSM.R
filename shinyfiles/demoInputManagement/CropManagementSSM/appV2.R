#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
if (! "DT" %in% installed.packages()) install.packages(DT)
if (! "jsonlite" %in% installed.packages()) install.packages(jsonlite)
library(DT)
library(jsonlite)
inputpath<-"~/a_ABSys/D4Declic/platform/SSM/inputplatform"
examplemanag<-fromJSON(readLines(normalizePath(paste(inputpath, "managementPlans.json", sep="/"), winslash="/")))
getdfCode<-function(listemanag) { 
  toto<-data.frame("Available_management_plans"=sapply(listemanag, function(x) return(x[["dfCode"]]$Code)))
  rownames(toto)<-NULL
  return(toto)
}
namesITK<-getdfCode(examplemanag)
choicesFixFind<-c("0 - Fixed sowing date", 
                  "1 - Sow in the 5th day of a 5-day rainfree period",
                  "2 - Sow in the 5th day  of a 5-day rainfree period + average temp > SowTmp",
                  "3 - Sow in the 5th day  of a 5-day rainfree period + average temp < SowTmp",
                  "4 - Sow when FTSW1 > SowWat",
                  "5 - Sow when FTSW1 < SowWat",
                  "6 - Sow when cumulative rainfall over a 5-day period > SowWat",
                  "7 - Sow when soil profile ATSW > SowWat" )
choicesN<-c("0=non-N-limited conditions", 
            "1=not coded, do not use!",
            "2=N-limited conditions")
choicesH2O<-c("0=potential production",
              "1=automated irrigated",
              "2=rainfed",
              "3=fixed irrigated")
choicesdates<-c("1=DAP", 
                "2=CBD", 
                "3=DOY")

completeNdf<-function(dfUI) {
  dfUI$NapplNumber<-1:nrow(dfUI)
  dfUI$DAPorCBD<-dfUI$date
  dfUI$FracVol<-dfUI$pctN
  return(dfUI[,c("NapplNumber", "DAPorCBD", "amount", "FracVol")])
}
simplifyNdf<-function(dfjson) {
  if(class(dfjson)=="data.frame"){
    dfjson$date<-dfjson$DAPorCBD
    dfjson$pctN<-dfjson$FracVol
    return(dfjson[,c("date", "amount", "pctN")])
  } else return(data.frame(date=numeric(), amount=numeric(), "pctN"=numeric()))
}
completewaterdf<-function(dfUI) {
  dfUI$DAPorCBDorDOY<-dfUI$date
  return(dfUI[,c("DAPorCBDorDOY", "amount")])
}
simplifywaterdf<-function(dfjson) {
  if(class(dfjson)=="data.frame"){
    dfjson$date<-dfjson$DAPorCBDorDOY
    return(dfjson[,c("date", "amount")])
  } else return(data.frame(date=numeric(), amount=numeric()))
}


# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Crop management"),

    # Sidebar with existing CMP 
    sidebarLayout(
        sidebarPanel(width = 3,
          DTOutput('existingManagement'),
          selectizeInput("chooseCMP", label="Choose a CMP for each crop in the rotation (known bug: same CMP cannot be chosen twice)",
                         choices= namesITK$"Available_management_plans", multiple = TRUE),
          actionButton("exportCMP", "Export succession of CMP")
        ),
        #main panel with the options to create a new CMP (or modify an existing one)
        mainPanel(verticalLayout(
          flowLayout(
            wellPanel(
              titlePanel("Sowing"),
              numericInput("Fpdoy", "beginning of Sowing period (DOY)",1, min=1, max=366, step=1),
              numericInput("Lpdoy", "end of Sowing period (DOY)",1, min=1, max=366, step=1),
              selectInput("FixFind", label="Sowing decision", choices=choicesFixFind),
              numericInput("SowTmp", "temperature threshold for sowing (for types 2 and 3)",0, min=-10, max=50, step=1),
              numericInput("SowWat", "water threshold for sowing (for types 4 to 7)",0, min=0, max=50, step=1),
              numericInput("Pden", "Plant density (pl/m2)",0, min=0, max=1000, step=1),
              numericInput("STBLW", "Weight of stuble left from preceding crop (g/m2)",0, min=0, max=1000, step=1)
            ),#end sowing
            wellPanel(
              titlePanel("Nitrogen"),
              selectInput("nitrogenScenario", label="Fertilization decision", choices=choicesN),
              selectInput("nitrogenDatetype", label="type of date for nitrogen applications", choices=choicesdates),
              verticalLayout(
                splitLayout(
                  numericInput("dateN", "date",0, min=-10, max=50, step=1),
                  numericInput("amountN", "amount",0, min=-10, max=50, step=1),
                  numericInput("pctN", "pctN",0, min=-10, max=50, step=1)
                ),
                splitLayout(
                  actionButton("addN", "Add"),
                  actionButton("removeN", "Rm")
                ),
                DTOutput('tableN')
              )
              
            ),#end Nitrogen
            wellPanel(
              titlePanel("Irrigation"),
              selectInput("waterScenario", label="Irrigation decision", choices=choicesH2O),
              numericInput("waterLevel", "waterLevel",value=numeric(), min=0, step=1),
              selectInput("waterDatetype", label="type of date for irrigations", choices=choicesdates),
              verticalLayout(
                splitLayout(
                  numericInput("dateH2O", "date",0, min=-10, max=50, step=1),
                  numericInput("amountH2O", "amount",0, min=-10, max=50, step=1)
                ),
                splitLayout(
                  actionButton("addH2O", "Add"),
                  actionButton("removeH2O", "Rm")
                ),
                DTOutput('tableH2O')
              )
            )#end Irrigation
          ),
          wellPanel(
            titlePanel("Save"),
            
            textInput("Code", HTML("name of crop management<br />change the name if you don't want to overwrite!")),
            actionButton("saveCMP", "Save")
          )
        ))
    )
)

# Define server logic 
server <- function(input, output, session) {
  #initialisation reactives
  selectedCMP<-reactiveValues(Code="empty")
  allmanag<-reactiveValues(CMP=examplemanag)
  tables<-reactiveValues(tableN=data.frame(date=numeric(), amount=numeric(), pctN=numeric()), 
                         tableH2O=data.frame(date=numeric(), amount=numeric()))

  
  #initialisation tables
  output$existingManagement = renderDT(
    getdfCode(allmanag$CMP), options = list(lengthChange = FALSE),
    selection = list(mode = 'single')
  )
  output$tableN = renderDT(
    tables$tableN, options = list(lengthChange = FALSE, searching = FALSE, lengthMenu = c(),autoWidth = TRUE),
    rownames= FALSE,
    selection = list(mode = 'multiple')
  )
  output$tableH2O = renderDT(
    tables$tableH2O, options = list(lengthChange = FALSE, searching = FALSE, lengthMenu = c(),autoWidth = TRUE),
    rownames= FALSE,
    selection = list(mode = 'multiple')
  )
  
  
  #observe events
  observeEvent(input$exportCMP, {
    userCMP<-toJSON(allmanag$CMP[input$chooseCMP])
    write(userCMP, file=normalizePath(paste(inputpath, "CMPfromUser.json", sep="/"), winslash="/"))
    showModal(modalDialog(
         title = "Panos, don't forget to also export the vector of CMPs to SimulationOptions.csv",
       ))
  })
  observeEvent(input$existingManagement_row_last_clicked, {
    CMP<-allmanag$CMP[[input$existingManagement_row_last_clicked]]
    codeselectionne<-CMP$dfCode$Code
    updateTextInput(session, inputId="Code", value = codeselectionne)
    #update sowing section
    updateNumericInput(session, inputId="Fpdoy", value = CMP$dfSowing$Fpdoy)
    updateNumericInput(session, inputId="Lpdoy", value = CMP$dfSowing$Lpdoy)
    if(!is.na(as.numeric(CMP$dfSowing$FixFind))) fixfind<-CMP$dfSowing$FixFind else fixfind<-0
    textfixfind<-choicesFixFind[fixfind+1]
    updateSelectInput(session, inputId="FixFind", selected =textfixfind)
    updateNumericInput(session, inputId="SowTmp", value = CMP$dfSowing$SowTmp)
    updateNumericInput(session, inputId="SowWat", value = CMP$dfSowing$SowWat)
    updateNumericInput(session, inputId="Pden", value = CMP$dfSowing$Pden)
    updateNumericInput(session, inputId="STBLW", value = CMP$dfSowing$STBLW)
    #update nitrogen section
    if(!is.na(as.numeric(CMP$nitrogenScenario))) Nscenario<-CMP$nitrogenScenario else Nscenario<-0
    textNscenario<-choicesN[Nscenario+1]
    updateSelectInput(session, inputId="nitrogenScenario", selected =textNscenario)
    if(!is.na(as.numeric(CMP$nitrogenDatetype))) Ndate<-CMP$nitrogenDatetype else Ndate<-0
    textNdate<-choicesdates[Ndate+1]
    updateSelectInput(session, inputId="nitrogenDatetype", selected =textNdate)
    nitrogendf<-simplifyNdf(CMP$nitrogendf)
    #update irrigation section
    if(!is.na(as.numeric(CMP$waterScenario))) waterScenario<-CMP$waterScenario else waterScenario<-0
    textH20scenario<-choicesH2O[waterScenario+1]
    updateSelectInput(session, inputId="waterScenario", selected =textH20scenario)
    updateNumericInput(session, inputId="waterLevel", value = CMP$waterLevel)
    if(!is.na(as.numeric(CMP$waterDatetype))) waterDatetype<-CMP$waterDatetype else waterDatetype<-0
    textH2Odate<-choicesdates[waterDatetype+1]
    updateSelectInput(session, inputId="waterDatetype", selected =textH2Odate)
    waterdf<-simplifywaterdf(CMP$waterdf)
    
    
    
    #store in reactives the CMP being changed
    tables$tableN<<-nitrogendf
    tables$tableH2O<<-waterdf
    selectedCMP$Code<<-codeselectionne
  })
  
  observeEvent(input$addN, {
    oldtable<-tables$tableN
    oldtable<-rbind(oldtable, data.frame(date=input$dateN, amount=input$amountN, pctN=input$pctN))
    oldtable[order(as.numeric(oldtable$date)),] #icicic strange this doesn' t reorder the talbe !
    tables$tableN<<-oldtable
  })
  observeEvent(input$removeN, {
    selected<-input$tableN_rows_selected
    if (length(selected)>0) {
      oldtable<-tables$tableN[-selected,] 
      tables$tableN<<-oldtable
    } else {
      showModal(modalDialog(
        title = "In order to remove nitrogen applications, you need to select line(s) in the table below",
      ))
    }
  })
  observeEvent(input$addH2O, {
    oldtable<-tables$tableH2O
    oldtable<-rbind(oldtable, data.frame(date=input$dateH2O, amount=input$amountH2O))
    oldtable[order(as.numeric(oldtable$date)),] #icicic strange this doesn' t reorder the talbe !
    tables$tableH2O<<-oldtable
  })
  observeEvent(input$removeH2O, {
    selected<-input$tableH2O_rows_selected
    if (length(selected)>0) {
      oldtable<-tables$tableH2O[-selected,] 
      tables$tableH2O<<-oldtable
    } else {
      showModal(modalDialog(
        title = "In order to remove irrigations, you need to select line(s) in the table below",
      ))
    }
  })
  
  
  observeEvent(input$saveCMP, {
    toto<-allmanag$CMP
    newname<-input$Code
    newlist<-list(list(dfCode=data.frame(Code=newname, Description=""),
                  dfSowing=data.frame(Fpdoy=input$Fpdoy,
                                      Lpdoy=input$Lpdoy,
                                      FixFind=as.numeric(substr(input$FixFind, start=1, stop=1)),
                                      SowTmp=input$SowTmp,
                                      SowWat=input$SowWat,
                                      Pden=input$Pden,
                                      STBLW=input$STBLW),
                  nitrogenScenario=as.numeric(substr(input$nitrogenScenario, start=1, stop=1)),
                  nitrogenNumber=nrow(tables$tableN),
                  nitrogenDatetype=as.numeric(substr(input$nitrogenDatetype, start=1, stop=1)),
                  nitrogendf=completeNdf(tables$tableN),
                  waterLevel=input$waterLevel,
                  waterScenario=as.numeric(substr(input$waterScenario, start=1, stop=1)),
                  waterNumber=nrow(tables$tableH2O),
                  waterDatetype=as.numeric(substr(input$waterDatetype, start=1, stop=1)),
                  waterdf=completeH2Odf(tables$tableH2O),
                  ))



    names(newlist)<-newname
    if(newname %in% names(toto))  toto[newname]<-newlist else toto<-c(toto, newlist)


    allmanag$CMP<<-toto
    namesITK<-getdfCode(toto)
    #numeroligne<-which(namesITK$"Available_management_plans"==newname)
    #output$existingManagement = renderDT(
    #  namesITK, options = list(lengthChange = FALSE),
    #  selection = list(mode = 'single', selected = numeroligne)
    #)
   updateSelectizeInput(session, "chooseCMP", choices=namesITK$"Available_management_plans")
  })
  
  #workaround to allow multiple selection of CMPs in the succession
  # observeEvent(input$chooseCMP, {
  #   choices <- seq_len(length(input$chooseCMP)+1)
  #   names(choices) <- rep("^", length(choices))
  #   updateSelectInput(session, "chooseCMP", choices = choices, selected = isolate(input$chooseCMP))
  # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


# showModal(modalDialog(
#   title = "Display CMP",
#   fixfind
# ))
