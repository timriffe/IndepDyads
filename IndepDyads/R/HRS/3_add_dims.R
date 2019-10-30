
library(here)
library(tidyverse)
library(rlang)
library(colorspace)
library(here)
Dat <- readRDS(here::here("IndepDyads","Data","RAND_2016v1_APCTresults.rds"))

Dat <- Dat %>% 
	   rename("TT" = "T") %>% 
	   mutate(P = C + A,
		   D = P + TT,
		   L = A + TT
		   )



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
		geom_contour() + 
		theme(legend.position = "none")
}


sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = C,
			ordinate = TT,
			slider = P,
			slider_value = p)




# ------ CT ------- #

# slide on P
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = C,
			ordinate = TT,
			slider = P,
			slider_value = 1995)

# slide on D
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = C,
			ordinate = TT,
			slider = D,
			slider_value = 2000)

# slide on A
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = C,
			ordinate = TT,
			slider = A,
			slider_value = 80)

# slide on L
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = C,
			ordinate = TT,
			slider = L,
			slider_value = 80)

# ------ AD ------- #

# slide on C
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = D,
			ordinate = A,
			slider = C,
			slider_value = 1925)

# slide on P
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = D,
			ordinate = A,
			slider = P,
			slider_value = 2000)

# slide on L
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = D,
			ordinate = A,
			slider = L,
			slider_value = 80)

# slide on TT (this one is interesting)
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = D,
			ordinate = A,
			slider = TT,
			slider_value = 0)

# ------ LP ------- #

# slide on A
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = L,
			slider = A,
			slider_value = 80)

# slide on C
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = L,
			slider = C,
			slider_value = 1925)

# slide on D
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = L,
			slider = D,
			slider_value = 2000)

# slide on TT
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = L,
			slider = TT,
			slider_value = 0) # identical to AD at TT = 0
# ------------------------------------ #
# triad identities
# ----------------- #
# TAL               #
# ----------------- #
# slide on C
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = A,
			ordinate = TT,
			slider = C,
			slider_value = 1925) 

# slide on P
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = A,
			ordinate = TT,
			slider = P,
			slider_value = 2000) 

# slide on D
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = A,
			ordinate = TT,
			slider = D,
			slider_value = 2000) 

# ----------------- #
# CDL               #
# ----------------- #
# slide on A
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = D,
			ordinate = C,
			slider = A,
			slider_value = 80) 

# slide on TT
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = D,
			ordinate = C,
			slider = TT,
			slider_value = 0) 

# slide on D
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = C,
			slider = D,
			slider_value = 2000) 

# ----------------- #
# TPD               #
# ----------------- #

# slide on A
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = TT,
			slider = A,
			slider_value = 80) 

# slide on L
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = TT,
			slider = L,
			slider_value = 80) 

# slide on C
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = TT,
			slider = C,
			slider_value = 1925) 

# ----------------- #
# TPD               #
# ----------------- # 

# OHHHHHHHHH #
# for indpendent plane, need a single control point...
# not a slider plane. independent planes don't fall within
# a single time measure, they cut through them all. Hmmmm.


