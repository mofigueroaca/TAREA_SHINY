library(ggplot2)

ui<-shinyUI(fluidPage(
    
    # Aplication title 
    headerPanel(img(src = "arbol1.png", height = 100, width = 1500)), # image made with Tess (http://www.peda.com/tess/)
    titlePanel("Benefit/Cost Analysis of Urban Trees"),
    
    sidebarPanel(
        
        p("This app intends to give you a gross estimate of the cost of mantaining trees in an urban setting.
          Calculations are based on an article writen by Kane & Kirwan (2009).
          Estimates of runoff reduction, pollution reduction, carbon sequestration 
          and energy savings by the trees are calculated with a linear function that 
          uses the diameter at breast height (DBH). 
          The values obtained are multipled by the number of trees.
          Costs used in this excercise are given in the article but the authors warns the readers that
          they might change, sometimes significantly, depending on a particular situation so 
          these values should not be used except for the demonstration purposes."),
        p("Please select number of trees and the average diameter at breast height of the trees"),
    
    # Inputs
    
        numericInput(inputId = "NumTree", label = "Number of trees", value = 0, min = 0, max = 100, step = 5),
        numericInput(inputId = "DBH", label = "Diameter at Breast Height", value = 0, min = 0, max = 25, step = 5)
    ), 
    
    # Outputs
    mainPanel(
        h3("Cost/Benefit Results"),
        h4("The number of trees you entered"),
        verbatimTextOutput("NumTree"),
        h4("The average diameter at breast height of your trees is"),
        verbatimTextOutput("DBH"),
        h4("The value in dollars of Runoff reduction (RunRed) is:"),
        verbatimTextOutput("RunRed"),
        h4("The value in dollars of Pollution reduction (PollRed) is:"),
        verbatimTextOutput("PollRed"),
        h4("The value in dollars of Carbon Sequestration (CarbSeq) is:"),
        verbatimTextOutput("CarbSeq"),
        h4("The value in dollars of Energy Savings (Energy) is:"),
        verbatimTextOutput("EnerSave"),
        h4("The cost in dollars of Tree Mantainance (Cost) is:"),
        verbatimTextOutput("Cost"),
        h4("The overall cost in dollars of Tree Mantainance (Total) is:"),
        verbatimTextOutput("Total"),
        plotOutput("barras"),
        plotOutput("barras2")
    )
    
))

# Helper functions for estimation of the value in dollars of different ecosystem services

RunoffReduction <- function(NumTree, DBH) NumTree*((0.0303*DBH^2) + (0.182*DBH) + 2.29)

PollutionReduction <- function(NumTree, DBH) NumTree*((0.16*DBH^2) + (0.334*DBH) + 2.57)

CarbonSequestration <- function(NumTree, DBH) NumTree*((0.27*DBH^2) + (0.095*DBH) + 6.85)

EnergySavings<-function(NumTree, DBH){
    if(DBH<=8){
        EnSavings = 6.5
    } else if ((DBH>=8.1) && (DBH<=20)){
        EnSavings=16
    } else {
        EnSavings=42
    }
    EnSavings=NumTree*EnSavings
    return(EnSavings)
}

# Estimation of cost of mantainance given de DBH which is a measure of maturity of the tree
MantCost<-function(NumTree, DBH){
    if(DBH<=8){
        Cost = 100
    } else if ((DBH>=8.1) && (DBH<=20)){
        Cost = 84
    } else {
        Cost = 93
    }
    Costs=NumTree*Cost
    return(Costs)
}

# Server operations

server<-shinyServer(function(input, output){
    
    output$NumTree <- renderPrint({input$NumTree})
    output$DBH <- renderPrint({input$DBH})
    output$RunRed <- renderPrint({ RunoffReduction(input$NumTree, input$DBH) })
    output$PollRed <- renderPrint({ PollutionReduction(input$NumTree, input$DBH) })
    output$CarbSeq <- renderPrint({ CarbonSequestration(input$NumTree, input$DBH) })
    output$EnerSave <- renderPrint({ EnergySavings(input$NumTree, input$DBH) })
    output$Cost <- renderPrint({MantCost(input$NumTree, input$DBH)})
    output$Total <- renderPrint({
        MantCost(input$NumTree, input$DBH)-(RunoffReduction(input$NumTree, input$DBH)+
                                                PollutionReduction(input$NumTree, input$DBH)+
                                                CarbonSequestration(input$NumTree, input$DBH)+
                                                EnergySavings(input$NumTree, input$DBH))
    })
    
    # Plot of the values of ecosystem services (ES) and the cost of mantainance
    
    output$barras <- renderPlot({ 
        
        RR<- RunoffReduction(input$NumTree, input$DBH)
        PR<- PollutionReduction(input$NumTree, input$DBH)
        CS<- CarbonSequestration(input$NumTree, input$DBH)
        EnS<-EnergySavings(input$NumTree, input$DBH)
        C<- MantCost(input$NumTree, input$DBH)
        datos<-data.frame(ES=c("Cost","RunRed", "PollRed", "CarbSeq", "Energy"), Values=c(C,RR,PR,CS,EnS))
        
        ggplot(datos, aes(x = ES, y = Values, fill = ES)) + geom_bar(stat = "identity")})
    
    # Plot that compares the cost versus the sum of the values of ES 
    output$barras2 <- renderPlot({
        
        RR<- RunoffReduction(input$NumTree, input$DBH)
        PR<- PollutionReduction(input$NumTree, input$DBH)
        CS<- CarbonSequestration(input$NumTree, input$DBH)
        EnS<-EnergySavings(input$NumTree, input$DBH)
        C<- MantCost(input$NumTree, input$DBH)
        Savs<- RR+PR+CS+EnS
        datos2<-data.frame(Balance = c("Cost","Savings"), Values=c(C,Savs))
        ggplot(datos2, aes(x = Balance, y = Values, fill = Balance)) + geom_bar(stat = "identity")
    })
    
}

)


shinyApp(ui=ui, server=server)
