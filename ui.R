#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Word Prediction Algorithm"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        textInput("textInput", "Enter your phrase here", "What do you want me to predict?"),
                        br(),
                        actionButton("predictButton", "Predict"),
                        br(),
                        p(br(), "Click to predict the next word.")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        tabsetPanel(type="tabs",
                                tabPanel("Word Prediction Tab",
                                         p("Best fit:"),
                                         verbatimTextOutput("predictionText1", placeholder=TRUE),
                                         p("Second-best fit:"),
                                         verbatimTextOutput("predictionText2", placeholder=TRUE),
                                         p("Third-best fit:"),
                                         verbatimTextOutput("predictionText3", placeholder=TRUE)),
                                
                                      ##   textOutput("textOutput1")),
                                tabPanel("Prediction Details and History",
                                         p("Last phrase entered:"),
                                         verbatimTextOutput("lastpredictionText", placeholder=TRUE),
                                         p("Result bigram:"),
                                         p("Restult trigram:"),
                                         p("Result quadgram:")),
                                tabPanel("Documentation", includeHTML("Documentation.html"))
                        )
                )
        )
))
