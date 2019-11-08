

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
ids             <- c(A="A",P="P",C="C",`T`="T",D="D",L="L")

# colsRAND        <- read_csv(here::here("IndepDyads","Data","namechangesHRS.csv"))

# colsRAND       <- colsRAND %>% filter(HRSnew %in% varnames)
# nrow(colsRAND)
# length(varnames)
