
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
							  selected = "A"),
				
				# 4)
				# pick ordinate
				# choices determined in:
				# getControlChoices(x,y)
				selectInput("y", "ordinate:",
							ids),
				
				# 5)
				# pick control measure (aka slice, slider)
				# getControlChoices(x,y)
				# TR: should be dealt with already
				selectInput("control", "control for:",
							ids),
				
				# 6) value of the previous (slider)
				# These ranges are detected from the subset Dat
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