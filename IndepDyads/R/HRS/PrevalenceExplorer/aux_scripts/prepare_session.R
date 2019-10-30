# prepare session

# Functions needed:

# this function does both data filter and plot.
# 1) one function that does it all
sliceAPCTDL <- function(data, 
						.varname = "adl3", 
						.Sex = "m",
						abcissae = C,
						ordinate = TT,
						slider = P,
						slider_value = 1995){
	data %>% 
		filter({{slider}} == slider_value,
			   varname == .varname,
			   Sex == .Sex) %>% 
		ggplot(mapping = aes(x = {{abcissae}}, y = {{ordinate}}, fill = pi, z = pi)) +
		geom_tile() +
		coord_equal() +
		scale_fill_continuous_sequential(
			palette = "Blues") + 
		geom_contour()
}

# 2:
# or separate the filter operation
slice_APCTDL <- function(data, 
						.varname = "adl3", 
						.Sex = "m",
						slider = P,
						slider_value = 1995){
	data %>% 
		filter({{slider}} == slider_value,
			   varname == .varname,
			   Sex == .Sex) 
}

# from the plot operation
plot_slice <- function(data, 
					   abcissae = C,
					   ordinate = TT){
	ggplot(mapping = aes(x = {{abcissae}}, 
						 y = {{ordinate}}, 
						 fill = pi, 
						 z = pi)) +
		geom_tile() +
		coord_equal() +
		scale_fill_continuous_sequential(
			palette = "Blues") + 
		geom_contour() + 
		theme(legend.position = "none")
}

# and then call them together as modules. Will args pass as hoped?
# is this worth fighting for?
plot_APCTDL <- function(data, 
						.varname = "adl3", 
						.Sex = "m",
						abcissae = C,
						ordinate = TT,
						slider = P,
						slider_value = 1995){
	
	slice <- slice_APCTDL(data, .varname, .Sex, slider, slider_value)
	plot_slice(slice, abcissae, ordinate)
}






