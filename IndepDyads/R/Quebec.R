# Author: tim
###############################################################################
library(foreign)
library(lubridate)
setwd("/home/tim/git/IndepDyads/IndepDyads")

# this is the MPIDR version from 2012, currently checking to see if it has been updated.
path      <- "/home/tim/Data/Quebec/FrenchCanadian.individuals.2012-01-27/RPQA.MarcKlemp.individus.2012-01-27.sav"
# TR: line to read file
Q         <- read.spss(path)
# TR: line to read file on MG's system
# Hm <-   read.spss("U:/quebec/Quebec/Quebec/FrenchCanadian.individuals.2012-01-27/RPQA.MarcKlemp.individus.2012-01-27.sav")
Q         <- as.data.frame(Q)

# NB check for age heaping for Pancho.




#Q$Lf2 <- Q$dateDecesAnnee - Q$dateNaissAnnee 
#image(log(acast(Q, dateDecesAnnee~Lf2,length)))
# ---------------------------------------#
# begin data prep                        #
# ---------------------------------------#



# missing month random assignment? or should I use month dist of known? Can always swap out
ind <- (Q$dateNaissMois == 0 | is.na(Q$dateNaissMois)) & !is.na(Q$dateNaissAnnee)
sum(ind)
Q$dateNaissMois[ind] <- sample(1:12,size=sum(ind),replace=TRUE)

days        <- c(31,28,31,30,31,30,31,31,30,31,30,31)
names(days) <- 1:12
# missing day is 15th always
ind <- (Q$dateNaissJour == 0 | is.na(Q$dateNaissJour)) & !is.na(Q$dateNaissAnnee)
Q$dateNaissJour[ind] <- floor(days[as.character(Q$dateNaissMois[ind])] / 2)


# missing month random assignment? or should I use month dist of known? Can always swap out
ind <- (Q$dateDecesMois == 0 | is.na(Q$dateDecesMois)) & !is.na(Q$dateDecesAnnee)
Q$dateDecesMois[ind] <- sample(1:12,size=sum(ind),replace=TRUE)

# missing day is 15th always
ind <- (Q$dateDecesJour == 0 | is.na(Q$dateDecesJour)) & !is.na(Q$dateDecesAnnee)
Q$dateDecesJour[ind] <- floor(days[as.character(Q$dateDecesMois[ind])] / 2)

# there appear to be some cases of sept 31st, and similar. Catch and make day valid
# harmless to deduct a day from feb 29, for example.
ind <- Q$dateNaissJour > days[as.character(Q$dateNaissMois)]
ind[is.na(ind)] <- FALSE
Q$dateNaissJour[ind] <- Q$dateNaissJour[ind] - 3

ind <- Q$dateDecesJour > days[as.character(Q$dateDecesMois)]
ind[is.na(ind)] <- FALSE
Q$dateDecesJour[ind] <- Q$dateDecesJour[ind] - 3

# compose Birthday
Q$BD   	  <- as.Date(with(Q, paste(dateNaissAnnee, 
						dateNaissMois, 
						dateNaissJour, 
						sep = "-")))

#compose Deathday, need to hangle NAs
DD 		  <- with(Q, paste(dateDecesAnnee, 
				dateDecesMois, 
				dateDecesJour, 
				sep = "-"))

# missing days/months appear to have been imputed, good!
#any(is.na(Q$dateDecesMois) & !is.na(Q$dateDecesAnnee))
#any(is.na(Q$dateDecesJour) & !is.na(Q$dateDecesMois))
DD[is.na(Q$dateDecesAnnee)] 	<- NA
Q$DD 							<- as.Date(DD)

#any(is.na(Q$DD) & !is.na(Q$dateDecesAnnee))
#any(is.na(Q$BD) & !is.na(Q$dateNaissAnnee))
# calculate decimal lifespan
Q$L       <- decimal_date(Q$DD) - decimal_date(Q$BD)
Q$L[Q$L < 0] <- 0

# a once-off check for heaping for Pancho. None found.
#Q$bd <- decimal_date(Q$BD)
#yearsap <- 1600:1800
#agesap <- 0:100
#AP <- matrix(nrow = 101,ncol = 201,dimnames=list(0:100,1600:1800))
#for (a in agesap){
#	for (p in 1:length(yearsap)){
#		
#		AP[a+1,p] <- sum(floor(yearsap[p] - Q$bd) == a &
#						Q$dateNaissAnnee <= yearsap[p] & 
#						Q$dateDecesAnnee >= yearsap[p], na.rm = TRUE)
#	}
#}
#barplot(AP[,"1800"],space = 0, border = NA)
#abline(v=seq(0,100,by=10),col = "white")


# LP plane
# loop over year and L
range(Q$L,na.rm=TRUE) # 0-108

# dateNaissAnnee
# dateDecesAnnee
#range(Q$dateNaissAnnee, na.rm=TRUE)
#range(Q$dateDecesAnnee, na.rm=TRUE)
nrow(Q)
Q <- Q[!is.na(Q$L), ]

years <- 1572:1870
Q$Lf <- floor(Q$L)
LP <- matrix(NA, ncol = length(years), nrow = 109, dimnames = list(0:108,years))
for (l in 0:108){
	for (p in 1:length(years)){
		LP[l+1,p] <- sum(Q$Lf == l & 
						Q$dateNaissAnnee <= years[p] & 
						Q$dateDecesAnnee >= years[p], na.rm = TRUE)
	}
}

# Re Joel's epidemic comment
epidemicl <- c(1732,1755,1759,1761,1772,1775,1702,1687,1714)
epidemicr <- c(1733,1757,1759,1761,1772,1776,1703,1687,1715)


colramp <- colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"),space="Lab")
brks <- seq(0,9,by=.5)
#png("Figures/QuebecLP.png",width=700,height=400)
pdf("Figures/QuebecLP.pdf",width=7,height=4)
image(years,0:108,log(t(LP)),asp=1,breaks=brks,col=colramp(length(brks)-1),
		xlab = "Period", ylab = "Length of life", ylim=c(0,100), las = 1)
#contour(years,0:108,log(t(LP)),add=TRUE,levels=brks2,lwd=.5,col = gray(.2))
rect(epidemicl,0,epidemicr,-10,border="red",xpd=TRUE)
dev.off()

library(reshape2)


# CT
library(reshape2)
LC <- acast(Q,Lf~dateNaissAnnee)
LC <- LC[-nrow(LC),]
LC <- LC[,-ncol(LC)]

LC <- LC[nrow(LC):1, ]
TC <- apply(LC,2,cumsum)
TC <- TC[nrow(TC):1, ]

colramp <- colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"),space="Lab")
brks <- seq(0,9,by=.5)
#png("Figures/QuebecTC.png",width=700,height=400)
pdf("Figures/QuebecTC.pdf",width=7,height=4)
image(as.integer(colnames(TC)),as.integer(rownames(TC)),log(t(TC)),asp=1,breaks=brks,col=colramp(length(brks)-1),
		xlab = "Birth cohort", ylab = "Thanatological age", ylim=c(0,100),las=1)
dev.off()

# AD
LD <- acast(Q,Lf~dateDecesAnnee)
LD <- LD[-nrow(LD),]
LD <- LD[,-ncol(LD)]
LD <- LD[nrow(LD):1, ]
AD <- apply(LD,2,cumsum)
AD <- AD[nrow(AD):1, ]
#png("Figures/QuebecAD.png",width=700,height=400)
pdf("Figures/QuebecAD.pdf",width=7,height=4)
image(as.integer(colnames(AD)),as.integer(rownames(AD)),log(t(AD)),asp=1,breaks=brks,col=colramp(length(brks)-1),
		xlab = "Year of death", ylab = "Age", ylim=c(0,100),las=1)
dev.off()