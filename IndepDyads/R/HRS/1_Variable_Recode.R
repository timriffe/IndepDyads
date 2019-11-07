# --------------------------------------
rm(list=ls(all.names = TRUE))
# This script recodes most variables, makes some new ones.
# This is the step after extracting from the RAND version 2016v1
# file in H0_RAND_reshape.R
# --------------------------------------
# source("R/1_Variable_Recode.R")
# Sets working directory for Tim's machines
library(here)
library(lubridate)
library(data.table)
library(tidyverse)

#---------------------------------------------------------
# start utility function preamble
#----------------------------------------------------------
# convert yes no coded questions into binary
convertYN <- function(x){
	x                   <- as.character(x)
    xx                  <- rep(NA, length(x))
    xx[grepl("yes", x)] <- 1
    xx[grepl("no", x)]  <- 0
    invisible(xx)
}
#-----------------------------------------------------
# convert odd binary with first and second try into single binary
# TR: changed Aug 4, 2016. No more intermediate values: percent incorrect.
convertCI <-  function(x){
  xx                              <- rep(NA, length(x))
  x                               <- as.character(x)
  xx[x == "1.correct"]            <- 0
  xx[x == "2.correct, 1st try"]   <- 0
  #xx[x == "1.correct, 2nd try"]  <- .5
  xx[x == "1.correct, 2nd try"]   <- 0
  xx[x == "0.incorrect"]          <- 1
  invisible(as.numeric(xx))
}
#-----------------------------------------------------
# convert CESD variables into binary.
# 1 = yes or most of the time
convertCESD <- function(x){
  if (class(x) == "numeric"){
		return(x)
  }
	
  x                                  <- as.character(x)
  xx                                 <- rep(NA, length(x))
  xx[x == "0.no"]                    <- 0
  xx[x == "4. none or almost none"]  <- 0
  xx[x == "4.none or almost none"]   <- 0
  # TR: changed Aug 4, 2016. No more intermediate values: percent incorrect.
#  xx[x == "3. some of the time"]     <- .5
#  xx[x == "2. most of the time" ]    <- .75
  xx[x == "3. some of the time"]     <- 0
  xx[x == "3.some of the time"]      <- 0
  xx[x == "2. most of the time" ]    <- 1
  xx[x == "2.most of the time" ]     <- 1
  xx[x ==  "1.yes"]                  <- 1
  xx[x ==  "1. all or almost all"]   <- 1
  xx[x ==  "1.all or almost all"]    <- 1
  xx
}

#-----------------------------------------------------
# convert dates to R internal format
convertDates <- function(Dat){
    # can't be done with apply because we can't have Date class matrices.
	# all date columns can be detected with the pattern _dt, it turns out.
    DateInd       <- grep(pattern = "_dt",colnames(Dat))
    for (i in DateInd){
        Dat[,i]    <- as.Date(Dat[, i], origin = "1960-1-1")
    }
    invisible(Dat)
}

#-----------------------------------------------------
# two functions to get exact years lived and left
getThanoAge <- function(Date, DeathDate){
    out <- rep(NA, length(Date))
    Ind <- !is.na(Date)
    out[Ind] <- lubridate::decimal_date(DeathDate[Ind]) - lubridate::decimal_date(Date[Ind])
    out
}
getChronoAge <- function(Date, BirthDate){
    out <- rep(NA, length(Date))
    Ind <- !is.na(Date) & !is.na(BirthDate)
    out[Ind] <- lubridate::decimal_date(Date[Ind]) - lubridate::decimal_date(BirthDate[Ind])
    out
}


# -------------------------------#
# Weight imputation function     #
# see code for annotation        #
# -------------------------------#
imputeWeights <- function(wt,intv_dt){
	# positive weights, also used for indexing
	ind <- wt > 0
	# if all weights 0, replace w NA
    if (all(wt == 0)){
        wt2 <- NA * wt
        return(wt2)
    }
	# if only one valid weight, all later
	# observations will keep that weight.
    if (sum(ind) == 1){
        wt2 <- approx(x = intv_dt[ind],
                y = wt[ind],
                xout = intv_dt,
                rule = 1:2,
                method = "constant",
                f = .5)$y
    }
	# if at least two valid observations, we
	# interpolate linearly for any missing,
	# but extrapolate (rightward) with constant
    if (sum(ind)>=2){
        wt2 <- approx(x = intv_dt[ind],
                y = wt[ind],
                xout = intv_dt,
                rule = 1:2,
                method = "linear")$y 
    }
    return(wt2)
}

# end utility function preamble
#----------------------------------------------------------

#----------------------------------------------------------
# load in long files from PHC
Dat         <- readRDS(here::here("IndepDyads","Data","RAND_2016v1_long.rds"))
varnames    <- readRDS( here::here("IndepDyads","Data","varnames.rds"))

# varnames[!varnames %in% colnames(Dat)]
# remove missed interviews
Dat         <- Dat[!is.na(Dat$intv_dt), ]

# change all factors to character (to be later recoded in some instances)
factor.columns       <- sapply(Dat, is.factor)
Dat[,factor.columns] <- apply(Dat[,factor.columns], 2, as.character)

# make sex column easier to use:

Dat$sex     <- ifelse(Dat$sex == "1.male","m","f")

# reduce to deceased-only
Dat$dead    <- ifelse(is.na(Dat$d_dt), 0, 1) 
Dat         <- Dat[Dat$dead == 1, ]
#(dead_cases <- nrow(Dat)) # 82609
# stats for paper
#nrow(Dat) 
#dead_cases / length(unique(Dat$id)) 
# as of 2016v1: 5.368548 
# as of vP:     4.822015 / person on average
#hist(rle(sort(Dat$id))$lengths)

# convert dates to native R format
Dat         <- convertDates(Dat)

# --------------------------------------------------#
# merge weights (big assumption here:               #
# weights in institutions are valid and             #
# comparable with weights outside institutions.     #
# soooo annoying ppl in institutions don't have     #
# comparable weights.                               #
# --------------------------------------------------#
# TR: update: since for the bootstrap method we resample
# using weights on first appearance, this is innocuous
# (no one starts HRS in nursing home)
Dat$nh_wt[is.na(Dat$nh_wt)] <- 0
Dat$p_wt 	<- Dat$p_wt + Dat$nh_wt

# --------------------------------------------------#
# now we do weight interpolation/extrapolation      #
# --------------------------------------------------#
Dat 		<- data.table(Dat)
Dat 		<- Dat[, p_wt2 := imputeWeights(p_wt,intv_dt), by = list(id) ]
Dat 		<- Dat[!is.na(Dat$p_wt2),]

#(cases_with_weights <- nrow(Dat))
#cases_with_weights / length(unique(Dat$id)) 
# RAND 2016v1: 5.201893
# RAND vP: 4.683211

# --------------------------------------------------#
# calculate thanatological ages                     #
# --------------------------------------------------#
Dat$ta 		<- getThanoAge(Dat$intv_dt, Dat$d_dt)
Dat$ca 		<- getChronoAge(Dat$intv_dt, Dat$b_dt)
Dat$la_int 	<- floor(Dat$ta + Dat$ca)
# hist(Dat$ca - (Dat$age /12) )
# there is one individual with an NA b_dt, and NA age,
# but thano age is known
# --------------------------------------------------#
# locate yes/no, correct/incorrect columns          #
# --------------------------------------------------#


YNcols <- apply(Dat, 2, function(x){
        xx <- unique(x)
        length(xx) <= 4 & any(grepl("yes",xx))
        })
CIcols <- apply(Dat, 2, function(x){
          xx <- unique(x)
          length(xx) <= 5 & any(grepl("correct",xx))
        }) 

# which columns are these anyway?
#colnames(Dat)[YNcols]
#colnames(Dat)[CIcols] 

# convert to binary
Dat         <- data.frame(Dat)
Dat[YNcols] <- lapply(Dat[YNcols], convertYN)
Dat[CIcols] <- lapply(Dat[CIcols], convertCI)



Dat <- Dat %>% mutate(
	srhfairpoor = case_when(srh == "1.excellent" ~ 0L,
							srh == "2.very good" ~ 0L,
							srh == "3.good" ~ 0L,
							srh == "4.fair" ~ 1L,
							srh == "5.poor" ~ 1L,
							is.na(srh) ~ as.integer(NA)),
	
	srhpoor = case_when(    srh == "1.excellent" ~ 0L,
			   			    srh == "2.very good" ~ 0L,
			   			    srh == "3.good" ~ 0L,
			   				srh == "4.fair" ~ 0L,
			   				srh == "5.poor" ~ 1L,
			   				is.na(srh) ~ as.integer(NA)),
	
	srmfairpoor = case_when(srm == "1.excellent" ~ 0L,
							srm == "2.very good" ~ 0L,
							srm == "3.good" ~ 0L,
							srm == "4.fair" ~ 1L,
							srm == "5.poor" ~ 1L,
							is.na(srm) ~ as.integer(NA)),
	
	srmpoor = case_when(    srm == "1.excellent" ~ 0L,
							srm == "2.very good" ~ 0L,
							srm == "3.good" ~ 0L,
							srm == "4.fair" ~ 0L,
							srm == "5.poor" ~ 1L,
						    is.na(srm) ~ as.integer(NA)),
	
	pastmem = case_when(    pastmem == "3.worse" ~ 1L,
							pastmem == "2.same" ~ 0L,
							pastmem == "1.better" ~ 0L,
							is.na(pastmem) ~ as.integer(NA)))

# do cesd questions (1 bad, 0 good)
cesdquestions       <- colnames(Dat)[grepl("cesd", colnames(Dat))]
cesdquestions       <- cesdquestions[cesdquestions != "cesd"]
Dat[cesdquestions]  <- lapply(Dat[cesdquestions],convertCESD)

# cesd_enjoy is flipped yet again, because 1 is 'yes I enjoyed life',


# ---------------------------------------------------------------
# various recodings in a mutate call
Dat <- Dat %>% mutate(# cesd_enjoy and cesd_happy flip so that high is bad
	                  cesd_enjoy = 1 - cesd_enjoy,
	                  cesd_happy = 1 - cesd_happy,
	                  # total word recall (max 20)
	                  twr = 1 - twr / 20,
					  # vocab: 1 worst 0 best
					  vocab = 1 - vocab / 10,
					  # total mental: 1 worst, 0 best  (max 15)
					  tm = 1 - tm /15,
					  # delayed word recall  (max 10)
					  dwr = 1 - dwr / 10,
					  # immediate word recall (max 10)
					  iwr = 1 - iwr / 10,
					  # mobility index, break at 1
					  mob = ifelse(mob > 1, 1, 0),
					  # large muscle difficulty index,
					  lg_mus = ifelse(lg_mus > 1, 1, 0),
					  # Gross motor difficulty index
					  gross_mot = ifelse(gross_mot > 1, 1, 0),
					  # Fine motor difficulty index
					  fine_mot = ifelse(fine_mot > 0, 1, 0),
					  # serial 7s hist(Dat$serial7s) # < 4
					  serial7s = ifelse(serial7s < 4, 1, 0),
					  # Number of chronic conditions > 2 table(Dat$cc)
					  cc = ifelse(cc > 2, 1, 0),
                      # Drinking days/week
					  alc_days = ifelse(alc_days > 1, 1, 0),
					  # alc_drinks > 0
					  alc_drinks = ifelse(alc_drinks > 0, 1, 0),
					  # adl 3 > 0
					  adl3 = ifelse(adl3 > 0, 1, 0),
					  # adl 5 cut points
					  adl5_1 = ifelse(adl5 > 0, 1, 0),
					  adl5_2 = ifelse(adl5 > 1, 1, 0),
					  adl5_3 = ifelse(adl5 > 2, 1, 0),
					  # iadl3 (any)
					  iadl3 = ifelse(iadl3 > 0, 1, 0),
					  # iadl 5 cut points
					  iadl5_1 = ifelse(iadl5 > 0, 1, 0),
					  iadl5_2 = ifelse(iadl5 > 1, 1, 0),
					  iadl5_3 = ifelse(iadl5 > 2, 1, 0),
					  # Depression score 2+
					  cesd = ifelse(cesd > 1, 1, 0),
					  # define underweight, obese, normalweight
					  underweight =  ifelse(bmi < 18.5, 1, 0),
					  obese =  ifelse(bmi > 30, 1, 0),
					  normalweight = as.integer((underweight + bmi) == 0),
					  # nursing home yes or no?
					  nh_nights = ifelse(nh_nights > 0, 1, 0),
					  nh_stays = ifelse(nh_stays > 0, 1, 0),
					  # hospital night yes or no?
					  hosp_nights = ifelse(hosp_nights > 0, 1, 0),
					  hosp_stays = ifelse(hosp_stays > 0, 1, 0),
					  # doc visits standardize ref period, need helper column
					  wave3p = ifelse(wave > 2, 2, 1),
					  doc_visits = floor(doc_visits / wave3p),
					  doc_visits = ifelse(doc_visits > 8, 1, 0),
					  wave3p = NULL
)

# -----
# TR: need refactor of varnames_fit choices
# START HERE NEXT SITTING
# -----


varnames_fit <- varnames
varnames_fit <- varnames_fit[!varnames_fit %in% c("srh","srm","tr20w","tr40w","adl5","iadl5","bmi")]
varnames_fit <- c(varnames_fit, 
		          "srhfairpoor",
				  "srhpoor",
				  "srmfairpoor",
				  "srmpoor",
				  "twr",
				  "adl5_1", "adl5_2", "adl5_3",
				  "iadl5_1", "iadl5_2", "iadl5_3",
				  "underweight",
				  "obese",
				  "normalweight")
		  
# let's just make sure this will work:
stopifnot(all(varnames_fit %in% colnames(Dat)))	  
# make sure all will work as prevalence:
stopifnot(all(
     sapply(varnames_fit, function(vn, Dat){
			max(Dat[[vn]], na.rm = TRUE)
		}, Dat = Dat) == 1)
)
# all in [0,1]
stopifnot(all(
				sapply(varnames_fit, function(vn, Dat){
							diff(range(na.omit(Dat[[vn]])))
						}, Dat = Dat) == 1)
)
# these are the ones that should be fit as of now.
names(varnames_fit) <- NULL
saveRDS(varnames_fit, file = here::here("IndepDyads","Data","varnames_fit.rds"))


# use quasibinom to fit due to these three variables.
#varnames_fit[which(sapply(varnames_fit, function(vn, Dat){
#							length(unique(na.omit(Dat[[vn]])))
#						}, Dat = Dat) > 2)]
#"vocab" "tm"    "twr"

# -------------------
# check cases by wave ( tapering in recent waves because selected down to deaths..)
#checkwaves <- function(var,Dat){
#  table(Dat[[var]],Dat[["wave"]])
#}
#checkwaves("adl3_",Dat)
#checkwaves("adl5_",Dat)
#checkwaves("iadl3_",Dat)
#checkwaves("iadl5_",Dat)
#checkwaves("cesd",Dat)
# -------------------

# -------------------------------------------------------
# for binning purposes, akin to 'completed age'
Dat$tafloor <- floor(Dat$ta)
Dat$cafloor <- floor(Dat$ca)

# I guess actual interview date could be some weeks prior to registered
# interview date? There are two negative thano ages at wave 4 otherwise, but
# still rather close. Likely died shortly after interview.
#(Dat[Dat$tafloor < 0, ])
# there is one individual with an erroneous death date (or id!), throwing out.
Dat                          <- Dat[Dat$ta > -1, ]
Dat$tafloor[Dat$tafloor < 0] <- 0
# We use higher bin widths fur purposes of visualizing raw data,
# Just diagnostics. larger widths help cancel out noise. This
# Such binning can be done as an alternative to the loess smoothing,
# where we take weighted means in cells. It'd probably make sense
# to keep the final year of life in a single year width, but the 
# general pattern ought to remain visible.
Dat$cafloor2 <- Dat$cafloor - Dat$cafloor %% 2
Dat$tafloor2 <- Dat$tafloor - Dat$tafloor %% 2

Dat$cafloor3 <- Dat$cafloor - Dat$cafloor %% 3
Dat$tafloor3 <- Dat$tafloor - Dat$tafloor %% 3


#----------------------------------------------
# save out, so this doesn't need to be re-run every time
saveRDS(Dat,file = here::here("IndepDyads","Data","RAND_2016v1_long.rds"))

#graphics.off()



# next step would be CreateMatrices.R, usually

#lapply(Dat[varnames_check],unique)
# how many NAs per variable?
#varnames_check[!varnames_check%in%colnames(Dat)]
# NAs <- lapply(Dat[varnames],function(x){
# 			sum(is.na(x))
# 		})
#sort(unlist(NAs) / nrow(Dat))

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() 
# end

# -------------------------------------------------------------------------
# Last checked 7-Nov-2019
# varnames <- c(adl3_ = "adl3", adl5_ = "adl5", iadl3_ = "iadl3", iadl5_ = "iadl5", 
#   cesd = "cesd", lim_work = "lim_work", srh = "srh", bmi = "bmi", 
#   back = "back", hosp = "hosp", hosp_stays = "hosp_stays", hosp_nights = "hosp_nights", 
#   nh = "nh", nh_stays = "nh_stays", nh_nights = "nh_nights", nh_now = "nh_now", 
#   doc = "doc", doc_visits = "doc_visits", hhc = "hhc", meds = "meds", 
#   surg = "surg", dent = "dent", shf = "shf", adl_walk = "adl_walk", 
#   adl_dress = "adl_dress", adl_bath = "adl_bath", adl_eat = "adl_eat", 
#   adl_bed = "adl_bed", adl_toilet = "adl_toilet", iadl_map = "iadl_map", 
#   iadl_money = "iadl_money", iadl_meds = "iadl_meds", iadl_shop = "iadl_shop", 
#   iadl_meals = "iadl_meals", mob = "mob", lg_mus = "lg_mus", gross_mot = "gross_mot", 
#   fine_mot = "fine_mot", bp = "bp", diab = "diab", cancer = "cancer", 
#   lung = "lung", heart = "heart", stroke = "stroke", psych = "psych", 
#   arth = "arth", cc = "cc", alc_ev = "alc_ev", alc_days = "alc_days", 
#   alc_drinks = "alc_drinks", smoke_ev = "smoke_ev", smoke_cur = "smoke_cur", 
#   cesd_depr = "cesd_depr", cesd_eff = "cesd_eff", cesd_sleep = "cesd_sleep", 
#   cesd_lone = "cesd_lone", cesd_sad = "cesd_sad", cesd_going = "cesd_going", 
#   cesd_enjoy = "cesd_enjoy", srm = "srm", pastmem = "pastmem", 
#   serial7s = "serial7s", serial7s = "bwc20", name_mo = "name_mo", name_dmo = "name_dmo", 
#   name_yr = "name_yr", name_dwk = "name_dwk", name_sci = "name_sci", 
#   name_cac = "name_cac", name_pres = "name_pres", name_vp = "name_vp", 
#   vocab = "vocab", tm = "tm")
# saveRDS(varnames, file = here::here("IndepDyads","Data","varnames.rds"))