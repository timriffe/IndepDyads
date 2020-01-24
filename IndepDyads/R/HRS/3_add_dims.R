
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

# orthoganal controls:
# testing bimedian slices, so far hmm.
# equivalent:
# LP bimedian line (D - C) / 2 - P = L / 2 - P
# AD bimedian line (P - C) / 2 - D = A / 2 - D
# TC bimedian line (D - P) / 2 - C = T / 2 - C
Dat <- 
	Dat %>% 
	mutate(LPnorm = round((D + C) / 2 - P),
		   ADnorm = round((P + C) / 2 - D),
		   TCnorm = round((D + P) / 2 - C))
range(Dat$LPnorm)
range(Dat$ADnorm)
range(Dat$TCnorm)
# LP
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae =L,
			ordinate = P,
			slider = LPnorm,
			slider_value = -35)

sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae =P,
			ordinate = L,
			slider = ADnorm,
			slider_value = -45)
# AD
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae =D,
			ordinate = A,
			slider = ADnorm,
			slider_value = -45)

head(Dat,20)
# AD
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae =A,
			ordinate = D,
			slider = ADnorm,
			slider_value = -45)
# TC
head(Dat)
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = TT,
			ordinate = C,
			slider = TCnorm,
			slider_value = 85)

sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = C,
			ordinate = TT,
			slider = P,
			slider_value = p)

# end bimedian testing


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
# identical to TPD with A = 80	

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
# APC               #
# ----------------- # 

# slide on D
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = A,
			slider = D,
			slider_value = 2000) 

# slide on TT
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = A,
			slider = TT,
			slider_value = 0) 

# slide on L
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = P,
			ordinate = A,
			slider = L,
			slider_value = 80) 
# TPD with A = 80 is a rotation of APC with L = 80



# OHHHHHHHHH #
# for indpendent plane, need a single control point...
# not a slider plane. independent planes don't fall within
# a single time measure, they cut through them all. Hmmmm.


