
source(here::here("IndepDyads","R","HRS","PrevalenceExplorer","aux_scripts/prepare_session.R"))
source(here::here("IndepDyads","R","HRS","PrevalenceExplorer","aux_scripts/prepare_data.R"))
#options(warn = 0)
shinyUI(
	fluidPage(
		tags$style(type="text/css",
				   ".shiny-output-error { visibility: hidden; }",
				   ".shiny-output-error:before { visibility: hidden; }"
		),
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
							ids,
							  selected = "P"),
				
				# 4)
				# pick ordinate
				# NOTE: remove abscicca from choice set
				# NOTE: each choice of abscissa can be part of (2 triad identities)
				# or one [independent dyad], which means that all dyads are valid
				# TR: should be taken care of in server
				selectInput("y", "ordinate:",
							ids),
				
				# 5)
				# pick control measure (ala slice, slider)
				# NOTE: choice set should depend on abscissa and ordinate
				# helper function defined above needs previous two inputs!
				# getControlChoices(x,y)
				# TR: should be dealt with already
				selectInput("control", "control for:",
							ids),
				
				# 6) value of the previous (slider)
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
							min = 0,
							max = 100,
							value = 50)
			),
	
			# Show a plot of the surface
			mainPanel(
				h2(textOutput("plot_title"), align = "center"),
				plotOutput("surf")
			
			)
		)
	)
)