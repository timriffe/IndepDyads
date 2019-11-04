

Dat <- readRDS(here::here("IndepDyads","R","HRS","PrevalenceExplorer","data","RAND_2016v1_APCTresults.rds"))
Dat <- Dat %>% 
	rename("TT" = "T") %>% 
	mutate(P = C + A,
		   D = P + TT,
		   L = A + TT
	)

# choices
varnames        <- readRDS(here::here("IndepDyads","R","HRS","PrevalenceExplorer","data","varnames_fit.rds"))
names(varnames) <- varnames
sexes           <- c("f","m")
names(sexes)    <- c("Women", "Men")
ids             <- c(A="A",P="P",C="C",TT="TT",D="D",L="L")

