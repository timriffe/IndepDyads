

#Dat <- readRDS(here::here("IndepDyads","R","HRS","PrevalenceExplorer","data","RAND_2016v1_APCTresults.rds"))
# eliminate needing to move objects for now
Dat <- readRDS(here::here("IndepDyads","Data","RAND_2016v1_APCTresults.rds"))

Dat <- Dat %>% 
	#rename("TT" = "T") %>% 
	mutate(P = C + A,
		   D = P + `T`,
		   L = A + `T`
	)

# choices
# varnames        <- readRDS(here::here("IndepDyads","R","HRS","PrevalenceExplorer","data","varnames_fit.rds"))
varnames        <- readRDS(here::here("IndepDyads","Data","varnames_fit.rds"))

names(varnames) <- varnames
sexes           <- c("f","m")
names(sexes)    <- c("Women", "Men")
sexesinv        <- c(f = 'Women', m = 'Men ')
ids             <- c(Age="A",Period="P",`Birth year`="C",`Time to death`="T",`Death year`="D",Lifespan="L")
idsinv          <- names(ids)
names(idsinv)   <- ids
# colsRAND        <- read_csv(here::here("IndepDyads","Data","namechangesHRS.csv"))

# colsRAND       <- colsRAND %>% filter(HRSnew %in% varnames)
# nrow(colsRAND)
# length(varnames)


# TR: TODO: get full names for all variables. Presently only 73 of 85 are available
# when vec and inv are ready, then change both title in server (bottom) and var selection
# routine in ui.R

# library(readr)
# namechangesHRS <- read_csv("IndepDyads/Data/namechangesHRS.csv")
# head(namechangesHRS)
# 
# all(varnames %in% namechangesHRS$HRSnew)
# sum(varnames %in% namechangesHRS$HRSnew)
# length(varnames)
# varnames[!varnames %in% namechangesHRS$HRSnew]
# namechangesHRS$HRSnew


