

Dat <- readRDS(here::here("IndepDyads","Data","RAND_2016v1_APCTresults.rds"))
Dat <- Dat %>% 
	rename("TT" = "T") %>% 
	mutate(P = C + A,
		   D = P + TT,
		   L = A + TT
	)
