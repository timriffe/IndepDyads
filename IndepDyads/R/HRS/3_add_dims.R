
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
		geom_contour()
}

for (p in 1993:2014){
sliceAPCTDL(Dat,
			.varname = "adl3",
			.Sex = "m",
			abcissae = C,
			ordinate = TT,
			slider = P,
			slider_value = p)
	
}



# ------ CT ------- #

# slide on P
Dat %>% 
	filter(P == 1995,
		   varname == "adl3",
		   Sex == "m") %>% 
	dim()


	ggplot(mapping = aes(x = C, y = TT, fill = pi))+
	geom_tile() +
	coord_equal() +
	scale_fill_continuous_sequential(
		palette = "Blues")

# slide on D
Dat %>% 
	filter(D == 2000,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = C, y = TT, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on A
Dat %>% 
	filter(A == 80,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = C, y = TT, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on L
Dat %>% 
	filter(L == 80,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = C, y = TT, fill = pi))+
	geom_tile() +
	coord_equal()

# ------ AD ------- #

# slide on C
Dat %>% 
	filter(C == 1925,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = D, y = A, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on P
Dat %>% 
	filter(P == 2000,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = D, y = A, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on L
Dat %>% 
	filter(L == 85,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = D, y = A, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on TT
Dat %>% 
	filter(TT == 0,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = D, y = A, fill = pi))+
	geom_tile() +
	coord_equal()

# ------ LP ------- #

# slide on A
Dat %>% 
	filter(A == 80,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = P, y = L, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on C
Dat %>% 
	filter(C == 1925,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = P, y = L, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on D
Dat %>% 
	filter(D == 2000,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = P, y = L, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on TT
Dat %>% 
	filter(TT == 2,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = P, y = L, fill = pi))+
	geom_tile() +
	coord_equal()

# ------------------------------------ #
# triad identities
# TAL

# slide on C
Dat %>% 
	filter(C == 1920,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = A, y = TT, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on P
Dat %>% 
	filter(P == 2000,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = A, y = TT, fill = pi))+
	geom_tile() +
	coord_equal()

# slide on D
Dat %>% 
	filter(D == 2010,
		   varname == "adl3",
		   Sex == "m") %>% 
	ggplot(mapping = aes(x = A, y = TT, fill = pi))+
	geom_tile() +
	coord_equal()



