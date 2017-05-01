#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
                # generate bins based on input$bins from ui.R
                predictionText <- eventReactive(input$predictButton, {
                        input$textInput
                })
                
                output$lastpredictionText <- renderText({
                        input$textInput
                })
                
                output$predictionText1 <- renderText({
                        predictionText()
                })
                
                output$predictionText2 <- renderText({
                        predictionText()
                })
                
                output$predictionText3 <- renderText({
                        predictionText()
                })

})
