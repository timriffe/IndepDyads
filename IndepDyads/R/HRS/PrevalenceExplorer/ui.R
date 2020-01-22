
source(here::here("IndepDyads","R","HRS","PrevalenceExplorer","aux_scripts/prepare_session.R"))
source(here::here("IndepDyads","R","HRS","PrevalenceExplorer","aux_scripts/prepare_data.R"))

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
							varList),
				# 3)
				# pick abscissa
				selectInput("x", "Abscissa:",
							ids,
							selected = "Age"),
				
				# 4)
				# pick ordinate
				# choices determined in:
				# getControlChoices(x,y)
				selectInput("y", "Ordinate:",
							ids),
				
				# 5)
				# pick control measure (aka slice, slider)
				# getControlChoices(x,y)
				# TR: should be dealt with already
				selectInput("control", "Control for:",
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