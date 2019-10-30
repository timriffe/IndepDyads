#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(here)
library(tidyverse)
library(rlang)
library(colorspace)
library(here)

# These are the data prelims
Dat <- readRDS(here::here("IndepDyads","Data","RAND_2016v1_APCTresults.rds"))
Dat <- Dat %>% 
    rename("TT" = "T") %>% 
    mutate(P = C + A,
           D = P + TT,
           L = A + TT
    )
# choices
varnames <- readRDS(here::here("IndepDyads","Data","varnames_fit.rds"))
names(varnames) <- varnames
sexes           <- c("f","m")
names(sexes)    <- c("Women", "Men")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("HRS Prevalence Explorer"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # 1)
            selectInput("sex", "Gender:",
                        sexes),
            # 2)
            selectInput("varname", "Variable:",
                        varnames),
            # 3)
            # pick abscissa
            selectInput("x", "abscissa:",
                        c("A" = "A",
                          "P" = "P",
                          "C" = "C",
                          "T" = "T",
                          "D" = "D",
                          "L" = "L")),
            
            # 4)
            # pick ordinate
            # NOTE: remove abscicca from choice set
            # NOTE: each choice of abscissa can be part of (2 triad identities)
            # or one [independent dyad], which means that all dyads are valid
            selectInput("y", "ordinate:",
                        c("A" = "A",
                          "P" = "P",
                          "C" = "C",
                          "T" = "T",
                          "D" = "D",
                          "L" = "L")),
            
            # 5)
            # NOTE: choice set should depend on abscissa and ordinate
            # choose(2,APC) -> T-D-L
            # choose(2,TAL)   -> C-P-D
            # choose(2,TPD)   -> A-L-C
            # choose(2,CPD)   -> T-A-L
            # Independents: choose any of the remaining four as control
            selectInput("control", "control for:",
                        c("A" = "A",
                          "P" = "P",
                          "C" = "C",
                          "T" = "T",
                          "D" = "D",
                          "L" = "L")),
            
            # 6)
            # NOTE: min, max, and value should depend on control measure:
            # ideally these ranges would be detected from the subset Dat
            # A: 70,110,80
            # P: 1993,2015,2000
            # C: 1900,1935,1920
            # T: 0,12,5
            # D: 1993,2015,2000
            # L: 70,110,80
            sliderInput("control_val",
                        "control value:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the surface
        mainPanel(
           plotOutput("surf")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
    output$surf <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        sliceAPCTDL(data = Dat,
                    .varname = input$varname, 
                    .Sex = input$sex,
                    abcissae = input$x,
                    ordinate = input$y,
                    slider = input$control, # needs to not have quotes!
                    slider_value = input$control_val)
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
