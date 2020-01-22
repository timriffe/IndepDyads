
source(here::here("IndepDyads","R","HRS","PrevalenceExplorer","aux_scripts/prepare_session.R"))
source(here::here("IndepDyads","R","HRS","PrevalenceExplorer","aux_scripts/prepare_data.R"))

shinyServer(function(input, output, session){
	# 1) set ordinate
	observe({
		x <- input$x
		y <- input$y
		# Can use character(0) to remove all choices
		if (is.null(x)){
			x <- "0"
		}
		
		if (is.null(y) | y == "0" | x == y){
		# Can also set the label and select items
		updateSelectInput(session, "y",
						  label = "Select ordinate:",
						  choices = ids[ids != x]
		)
		}
	},
	priority = 6)
	
	# set control measure
	observe({
		x <- input$x
		y <- input$y
		# Can use character(0) to remove all choices
		if (is.null(x)){
			x <- "0"
		}
		if (is.null(y)){
			y <- "0"
		}
		# Can also set the label and select items
		updateSelectInput(session, "control",
						  label = "Select control measure:",
						  choices = getControlChoices(x,y)
		)
	},
	priority = 5)
	
	# set control value
	# change slider range
	observe({
		control <- input$control
		
		# Can use character(0) to remove all choices
		if (is.null(control)){
			control <- character(0)
		}
		
		MM <- Dat %>% 
			pull(control) %>% 
			range()
		# Can also set the label and select items
		
		# if previous control value out of range
		# due to changing the measure, then place it in
		# the middle.
		val <- input$control_val
		if (val < MM[1] | val > MM[2]){
			val <- floor(mean(MM))
		}
		# Control the value, min, max, and step.
		# Step size is 2 when input value is even; 1 when value is odd.
		updateSliderInput(session, 
						  "control_val", 
						  value = val,
						  min = MM[1], 
						  max = MM[2], 
						  step = 1
		)
		
	},
	priority = 4)
	
	# finally, draw plot
	output$surf <- renderPlot({
		# generate bins based on input$bins from ui.R
		
		sliceAPCTDL(data = Dat,
					.varname = input$varname, 
					.Sex = input$sex,
					abcissae = input$x,
					ordinate = input$y,
					slider = input$control, # needs to not have quotes!
					slider_value = input$control_val)
		
	}, width=800, height=800, res = 180)
	
	# TR: edit this to pick out proper names
	# get title
	output$plot_title <- renderText(
		paste0(varlabs[input$varname], " ",sexesinv[input$sex],": ", idsinv[input$control]," = ",input$control_val))
})

