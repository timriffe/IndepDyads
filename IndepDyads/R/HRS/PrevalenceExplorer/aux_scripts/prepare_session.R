# prepare session
library(shiny)
library(here)
library(tidyverse)
library(rlang)
library(colorspace)
library(directlabels)
# Functions needed:
# NOTE: this function gives valid results, but the independent plane
# controls ARE NOT the orthogonal controls, which still need to be
# formalized. The ID controls should be parallel to the bimedian of
# the tetrahedral representation. Just need to get the arithmetic down,
# but it's sure to be a linear combination of +1 and -1 coefs on two 
# or four of the remaining measures. Or something like that.
getControlChoices <- function(x,y){
	ids <- c(Age="A",Period="P",`Birth year`="C",`Time to death`="T",`Death year`="D",Lifespan="L")
	
	if (x == "0"){
		return(ids)
	}
	if (y == "0" | x == y){
		return(ids[!ids == x])
	}
	
	TM <- matrix(c(NA,"C","P","L","I","T",
				   "C",NA,"A","D","T","I",
				   "P","A",NA,"I","L","D",
				   "L","D","I",NA,"P","A",
				   "I","T","L","P",NA,"C",
				   "T","I","D","A","C",NA),6,
				 dimnames = list(Ab = ids,
				 				Or = ids))
	
	ids <- rownames(TM)
	id3 <- TM[x,y]
	if (id3 == "I"){
		return(ids[!ids %in% c(x,y)])
	}
	return(ids[!ids %in% c(x,y,id3)])
}

# this function does both data filter and plot.
# 1) one function that does it all
sliceAPCTDL <- suppressWarnings(function(data, 
						.varname = "adl3", 
						.Sex = "m",
						abcissae = "C",
						ordinate = "T",
						slider = "P",
						slider_value = 1995){
	ids             <- c(A = "Age", P = "Period", C = "Birth year", `T` = "Time to death", 
						 D = "Death year", L = "Lifespan")

	rg <- data %>% pull(slider) %>% range()
	if (! slider_value >= rg[1] & slider_value <= rg[2]){
		slider_value = floor(mean(rg))
	}
	
	xrg <- data %>% pull(abcissae) %>% range()
	yrg <- data %>% pull(ordinate) %>% range()
    zrg <- data %>% filter(varname == .varname) %>% pull(pi) %>% range()
	binwidth <- diff(pretty(zrg, n = 12))[1]
	
	data %>% 
		filter(!!sym(slider) == slider_value,
			   varname == .varname,
			   Sex == .Sex) %>% 
		# TR: this then gets the axis names as just x and y ...
		# need to override below or else just modify this line
		mutate(x = !!sym(abcissae)+.5,
			   y = !!sym(ordinate)+.5) %>% 
		ggplot(mapping = aes(x = x, 
							 y = y, 
							 fill = pi, 
							 z = pi)) +
		#scale_x_continuous(limits = c(xrg[1],xrg[1]+35)) + 
		geom_tile() +
		xlim(xrg[1],xrg[1]+30) + 
		ylim(yrg[1],yrg[1]+30) + 
		coord_equal() +
		scale_fill_continuous_sequential(
			palette = "Blues"#,
			#limits = c(0,1)
			) + 
		geom_contour(binwidth = binwidth) + 
		geom_dl(aes(label=..level..), method="bottom.pieces", 
				stat="contour",
				binwidth = binwidth)+
		theme(text = element_text(size=20),
			  legend.position = "none",
			  panel.spacing = margin(0,0,0,0)) +
		labs(x = ids[abcissae], y = ids[ordinate])
})

# # 2:
# # or separate the filter operation
# slice_APCTDL <- function(data, 
# 						.varname = "adl3", 
# 						.Sex = "m",
# 						slider = P,
# 						slider_value = 1995){
# 	data %>% 
# 		filter({{slider}} == slider_value,
# 			   varname == .varname,
# 			   Sex == .Sex) 
# }
# 
# # from the plot operation
# plot_slice <- function(data, 
# 					   abcissae = C,
# 					   ordinate = TT){
# 	ggplot(mapping = aes(x = {{abcissae}}, 
# 						 y = {{ordinate}}, 
# 						 fill = pi, 
# 						 z = pi)) +
# 		geom_tile() +
# 		coord_equal() +
# 		scale_fill_continuous_sequential(
# 			palette = "Blues") + 
# 		geom_contour() + 
# 		theme(legend.position = "none")
# }
# 
# # and then call them together as modules. Will args pass as hoped?
# # is this worth fighting for?
# plot_APCTDL <- function(data, 
# 						.varname = "adl3", 
# 						.Sex = "m",
# 						abcissae = C,
# 						ordinate = TT,
# 						slider = P,
# 						slider_value = 1995){
# 	
# 	slice <- slice_APCTDL(data, .varname, .Sex, slider, slider_value)
# 	plot_slice(slice, abcissae, ordinate)
# }
# 
# 
# 
# 
# 
# 


