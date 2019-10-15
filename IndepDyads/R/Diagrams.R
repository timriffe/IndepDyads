# little helper icons for in-line references.
source(here::here("IndepDyads","R","prepare_session.R"))
# an LP mortality triangle

fig.path <- here::here("IndepDyads","Figures","LP_mort_tri.pdf")
pdf(fig.path, width=.15,height=.15)
par(xaxs='i',yaxs='i',mai=c(.02,.02,.02,.02))
plot(NULL, type = 'n', xlim=c(0,1),ylim=c(0,1),asp=1,axes=FALSE)
polygon(c(0,1,1),c(1,0,1),lwd=1,xpd=TRUE,lend=0)
dev.off()

# globals aesthetics:

# lifeline color:
llcol  <- gray(.3)

# -------------------------------------------------------------------------------
# LP diagram
# --------------------------------------------------------------------------- #
#source("/home/tim/git/APCT/APCT/R/Functions.R") # need to install raster
# LP years
# years <- 1572:1870


P <- seq(1700,2000,by=50)
pdf(here::here("IndepDyads","Figures","LPdiagram.pdf"),height=5,width=8)
#dev.new(height=5,width=8)
par(mai=c(.2, .2, 0, 0), xaxs = "i", yaxs = "i")
plot(NULL, xlim = c(1795,2080), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)

# add intermediate grids
xat <- seq(1840,2060,by=20)
yat <- seq(20,100,by=20)
segments(1820,yat, 2060,yat, col = muted(AssignColour("L"), l = 70, c = 50),
		 lwd=.5,lty=1)
segments(xat, 0, xat,100, col = muted(AssignColour("P"), l = 70, c = 50),
		 lwd=.5,lty=1)

# axes
text(1820,seq(0,100,by=20),seq(0,100,by=20),pos=2,xpd=TRUE)
text(seq(1820,2060,by=20),0,seq(1820,2060,by=20),pos=1,xpd=TRUE)
segments(1820,0,1820,100,lwd=2, col = AssignColour("L"))
segments(1820,0,2060,0,lwd=2, col = AssignColour("P"))

# life lines
set.seed(2)
B = runif(6,1800,2000)
L = runif(6,0,100)
segments(B, L, B+L, L, lwd = 2, col = llcol)
# birth points
points(B,L,pch=19,col=llcol,xpd=TRUE)
# death points
points(B+L,L,pch=13,col=llcol,xpd=TRUE,lwd=1)
#rect(1820,0,1900,100,border=NA,col="#00000020")
#rect(2000,0,2060,100,border=NA,col="#00000020")
text(1950,-10,"Period",xpd=TRUE,pos=1)
text(1805,80,"Completed Lifespan",xpd=TRUE,pos=2,srt=90)
text(B[1:2]+5,L[1:2]+5,LETTERS[1:2], font = 2)
dev.off()

# --------------------------------------------------------------------------- #
# TC diagram
# --------------------------------------------------------------------------- #
# C range 1572:1798
# T range 0:110
cc <- 1572:1798
tt <- 0:110

CC <- seq(1580,1800,by=20)
TT <- seq(0,100,by=20)

pdf(here::here("IndepDyads","Figures","TCdiagram.pdf"),height=5,width=10)
# dev.new(height=5,width=8)
par(mai=c(.2, .2, 0, 0), xaxs = "i", yaxs = "i")
plot(NULL, xlim = range(CC), ylim = range(TT), axes = FALSE, ylab = "", xlab = "", asp = 1)

# add intermediate grids
segments(min(CC),TT, max(CC),TT, col = muted(AssignColour("T"), l = 70, c = 50),
		 lwd=.5,lty=1)
segments(CC, min(TT), CC,max(TT), col = muted(AssignColour("C"), l = 70, c = 50),
		 lwd=.5,lty=1)
# axes
text(min(CC),TT,TT,pos=2,xpd=TRUE)
text(CC,min(TT),CC,pos=1,xpd=TRUE)
segments(min(CC),min(TT),min(CC),max(TT),lwd=2, col = AssignColour("T"))
segments(min(CC),min(TT),max(CC),min(TT),lwd=2, col = AssignColour("C"))

# life lines (would be better to use same 6? Just need same space...)
set.seed(1)
B <- runif(6,min(CC),max(CC))
L <- runif(6,0,100)
segments(B, 0, B, L, lwd = 2, col = llcol)
# birth points
points(B,L,pch=19,col=llcol,xpd=TRUE)
# death points
points(B,rep(0,6),pch=13,col=llcol,xpd=TRUE,lwd=1)
text(mean(CC),-10,"Cohort",xpd=TRUE,pos=1)
text(min(CC),mean(TT),"Time to death",xpd=TRUE,pos=2,srt=90)
text(B[c(5,1)],L[c(5,1)]+5,LETTERS[1:2], font = 2)

dev.off()

# --------------------------------------------------------------------------- #
# AD diagram
# --------------------------------------------------------------------------- #




