
# Author: tim
###############################################################################
# Instructions: 
# tend to steps 1-3 as appropriate, explained in situ.
# the script will produce a single list data object
# containing 3d arrays for 78 variables. (age,ttd,single-year cohorts), sexes separate.
# the object created at the end is called boot.results.list, saving out to ResultsP.Rdata.
# ------------------
library(parallel)
# ------------------------
# 1) set parameters
nboot        <- 999    # ? how many should we do? 999?
do.this      <- TRUE    # change this to TRUE
make.figs    <- TRUE     # shall we make the summary historgrams?

# 2) set working directory: you'll need to modify the working dir. Possibly by generalizing the below
# code or else commenting it out altogether and setting manually. Up to you

Cores <- detectCores()-1 # (you can also set this manually, override the above)

# 3) last thing to check:
# load in data, either change this path (commenting this one out so I keep it)
# or else make sure Data_long.Rdata is in a folder called Data inside the working directory...
#Dat        <- local(get(load("Data/Data_long.Rdata")))
Dat        <- readRDS(here::here("IndepDyads","Data","RAND_2016v1_long.rds"))

# the rest should work fine without further ado. Two figures will also be created 
# in the working directory. These can be examined or thrown out.

# ------------------------
# get packages loaded
# ------------------------
# gives mclapply()-like functionality in Windows. 
# I usually would use parallel package, but that won't cut it on Windows
if (.Platform$OS.type == "windows"){
	# does Hydra have devools? I hope so.
	if (!"parallelsugar" %in% rownames(installed.packages())){
		library(devtools)
		install_github('nathanvan/parallelsugar')
	}
	library(parallelsugar)
} else {
	# this is for all else (Tim)
	library(parallel)
}

library(splines)
library(data.table)
library(reshape2)
library(lattice)
# ------------------------
# the following functions are defined here to 
# avoid having to source and set another path.

# ------------------------
source(here::here("IndepDyads","R","HRS","2a_apct.boot.R"))

# no ages under 65
#oops age is in months!
Dat        <- Dat[(Dat$age / 12) >= 65, ]
# birth year must be known
Dat        <- Dat[!is.na(Dat$b_yr), ]
# group to quinquennial
Dat$Coh5   <- Dat$b_yr - Dat$b_yr %% 5 
# Coh5keep are cohorts kept for fitting
Coh5keep   <- seq(1900, 1930, by = 5) 
# select only cohorts used for fitting
Dat        <- Dat[Dat$Coh5 %in% Coh5keep, ]
## integer age representations
Dat$ta_int <- floor(Dat$ta)
Dat$ca_int <- floor(Dat$ca)
Dat$la_int <- floor(Dat$ta + Dat$ca)
# let's remove people with ta = -1
Dat        <- Dat[Dat$ta >= 0,]
# even tho most neg are very close to zero

# TR: I think this list is already solidified, but just in case.
varnames <- readRDS(here::here("IndepDyads","Data","varnames_fit.rds"))

# -----------------------
# this is the slow part!
# -----------------------
# TR: this is toggled at the head of the script.
# just to make sure resources not too tied up
if (do.this){
	
	meltSEX <- function(SEX){
		A         <- melt(SEX$Surf, varnames = c("T","A","C"), value.name = "pi")
		A$Sex     <- SEX$sex
		A$varname <- SEX$varname
		A
	}
	
	meltVAR <- function(X){
		rbind(meltSEX(X$Male),
			  meltSEX(X$Female))
	}
	
	boot.results.list <- mclapply(varnames, function(varname, Dat, nboot){
				fem 	<- apct.boot.wrapper(
							Dat[Dat$sex == "f", ], 
							varname = varname, 
							sex = "f", 
							nboot = nboot)
				mal 	<- apct.boot.wrapper(
							Dat[Dat$sex == "m", ], 
							varname = varname, 
							sex = "m", 
							nboot = nboot)
				
			
                out 	<- list(Female=fem, Male = mal)
				out$var <- varname
				out
			}, Dat = Dat,  
			   nboot = nboot,    # nboot is set at the head of the script.
			   mc.cores = Cores)   # ncores is set just above here
	names(boot.results.list) <- varnames
	
	boot.results.long <- do.call("rbind", lapply(boot.results.list, meltVAR))
	boot.results.long <- subset(boot.results.long,!is.na(pi))
	
	# Maarten: change path if necessary
	saveRDS(boot.results.long, file = here::here("IndepDyads","Data","RAND_2016v1_APCTresults.rds"))
	
}

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() 

