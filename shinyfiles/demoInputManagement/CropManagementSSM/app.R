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
examplemanag<-fromJSON(readLines("~/a_ABSys/D4Declic/platform/SSM/inputplatform/managementPlans.json"))
getCode<-function(listemanag) return(sapply(listemanag, function(x) return(x[["dfCode"]]$Code)))
namesITK<-data.frame("Available Management plans"=getCode(examplemanag)); rownames(namesITK)<-NULL
choicesFixFind<-c("0 - Fixed sowing date", 
                  "1 - Sow in the 5th day of a 5-day rainfree period",
                  "2 - Sow in the 5th day  of a 5-day rainfree period + average temp > SowTmp",
                  "3 - Sow in the 5th day  of a 5-day rainfree period + average temp < SowTmp",
                  "4 - Sow when FTSW1 > SowWat",
                  "5 - Sow when FTSW1 < SowWat",
                  "6 - Sow when cumulative rainfall over a 5-day period > SowWat",
                  "7 - Sow when soil profile ATSW > SowWat" )
choicesN<-c("0=non-N-limited conditions", 
            "2=N-limited conditions")
choicesH2O<-c("0=potential production",
              "1=automated irrigated",
              "2=rainfed",
              "3=fixed irrigated")
choicesdates<-c("1=DAP", 
                "2=CBD", 
                "3=DOY")

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Crop management"),

    # Sidebar with existing CMP 
    sidebarLayout(
        sidebarPanel(
          DTOutput('existingManagement'),
          width = 3
        ),

        # Show a plot of the generated distribution
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
                  numericInput("amountN", "date",0, min=-10, max=50, step=1),
                  numericInput("%N", "date",0, min=-10, max=50, step=1)
                ),
                splitLayout(
                  actionButton("addN", "Add"),
                  actionButton("removeN", "Rm")
                ),
                DTOutput('Ntable')
              ),
              
              
              numericInput("SowTmp", "temperature threshold for sowing (for types 2 and 3)",0, min=-10, max=50, step=1),
              numericInput("SowWat", "water threshold for sowing (for types 4 to 7)",0, min=0, max=50, step=1),
              numericInput("Pden", "Plant density (pl/m2)",0, min=0, max=1000, step=1),
              numericInput("STBLW", "Weight of stuble left from preceding crop (g/m2)",0, min=0, max=1000, step=1)
            ),#end Nitrogen
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
  selectedCMP<-reactiveValues(Code="empty")
  allmanag<-reactiveValues(CMP=examplemanag)                

  output$existingManagement = renderDT(
    namesITK, options = list(lengthChange = FALSE),
    selection = list(mode = 'single', selected = 1)
  )
  output$Ntable = renderDT(
    data.frame(date=numeric(), amount=numeric(), "pctN"=numeric()), options = list(lengthChange = FALSE),
    selection = list(mode = 'single', selected = 1)
  )
  
  
  
  observeEvent(input$existingManagement_row_last_clicked, {
    codeselectionne<-unname(getCode(allmanag$CMP[input$existingManagement_row_last_clicked]))
    updateTextInput(session, inputId="Code", value = codeselectionne)
    updateNumericInput(session, inputId="Fpdoy", value = allmanag$CMP[[codeselectionne]]$dfSowing$Fpdoy)
    updateNumericInput(session, inputId="Lpdoy", value = allmanag$CMP[[codeselectionne]]$dfSowing$Lpdoy)
    if(!is.na(as.numeric(allmanag$CMP[[codeselectionne]]$dfSowing$FixFind))) fixfind<-allmanag$CMP[[codeselectionne]]$dfSowing$FixFind else fixfind<-0
    textfixfind<-choicesFixFind[fixfind+1]
    updateSelectInput(session, inputId="FixFind", selected =textfixfind)
    updateNumericInput(session, inputId="SowTmp", value = allmanag$CMP[[codeselectionne]]$dfSowing$SowTmp)
    updateNumericInput(session, inputId="SowWat", value = allmanag$CMP[[codeselectionne]]$dfSowing$SowWat)
    updateNumericInput(session, inputId="Pden", value = allmanag$CMP[[codeselectionne]]$dfSowing$Pden)
    updateNumericInput(session, inputId="STBLW", value = allmanag$CMP[[codeselectionne]]$dfSowing$STBLW)
    selectedCMP$Code<<-codeselectionne
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
                                      Pden$input$Pden,
                                      STBLW$input$STBLW)))
    
    
    
    names(newlist)<-newname
    if(newname %in% names(toto))  toto[newname]<-newlist else toto<-c(toto, newlist)
    
    
    allmanag$CMP<<-toto
    namesITK<-data.frame("Available Management plans"=getCode(toto)); rownames(namesITK)<-NULL
    numeroligne<-which(namesITK$"Available Management plans"==newname)
    output$existingManagement = renderDT(
      namesITK, options = list(lengthChange = FALSE),
      selection = list(mode = 'single', selected = numeroligne)
    )
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


# showModal(modalDialog(
#   title = "Display CMP",
#   fixfind
# ))
# showModal(modalDialog(
#   title = "FixFind from input",
#   substr(input$FixFind, start=1, stop=1)
# ))
