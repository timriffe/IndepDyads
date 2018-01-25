
# construct a tetrahedral octahedral honecomb w cubic dual.


library(rgl)
#source("R/Functions.R")
xyz2ternxyz <- function(xyz){
	ternxyz   <- xyz
	
	ternxyz$x <- xyz$x - (xyz$y * .5)
	ternxyz$y <- xyz$y * sqrt(3) / 2 + xyz$z * (sqrt(3) / 2 - 1 / sqrt(3))
	ternxyz$z <- xyz$z * sqrt(6) / 3
	ternxyz
}

# tetrahedron function
# this will need apctdl logic
tetrahedron_a <- function(){
	
}



# let's start with unit space: c(-2,2) for everything?
xlim <- c(-2,2)
ylim <- c(-2,2)
zlim <- c(-2,2)

# set up device

plot3d(xlim = xlim, ylim = ylim, zlim = zlim, 
		box = FALSE, 
		aspect = c(3, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
bg3d("white")






# pp <- dget("Data/TALrglview2.R")

startYr <- 1800
endYr   <- 2100

rgl.close()
omega <- 125 # for the sake of dimensioning
# =====================================
# set up space, a box, a prelim aspect ratio (final adjustment later)
plot3d(xlim = c(startYr, endYr), ylim = c(0, omega), zlim = c(0, omega), 
		box = FALSE, 
		aspect = c(3, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
bg3d("white")
# now 3 calendar lines
cal1         <- xyz2ternxyz(data.frame(x = c(startYr, endYr), y = c(0, 0), z = c(0, 0)))
leftagebound <- xyz2ternxyz(data.frame(x = c(startYr, startYr), y = c(0, omega), z = c(0, 0)))
thanoaxis    <- xyz2ternxyz(data.frame(x = c(startYr, startYr), y = c(0, 0), z = c(0, omega)))
#cal2 <- xyz2ternxyz(data.frame(x = c(startYr, endYr), y = c(omega, omega), z = c(0, 0)))
cal3 <- xyz2ternxyz(data.frame(x = c(startYr, endYr-100), y = c(0,0), z = c(omega, omega)))

rgl.linestrips(cal1$x, cal1$y, cal1$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(leftagebound$x, leftagebound$y, leftagebound$z, color = AssignColour("A"), lwd = 2)
#rgl.linestrips(cal2$x, cal2$y, cal2$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(cal3$x, cal3$y, cal3$z, color = AssignColour("P"), lwd = 2)
rgl.linestrips(thanoaxis$x, thanoaxis$y, thanoaxis$z, color = AssignColour("T"), lwd = 2)

periods <- seq(startYr+25, endYr + omega, by = 25)
for (i in 1:length(periods)){
	cohi <- xyz2ternxyz(data.frame(x = c(periods[i], periods[i]), y = c(ifelse(periods[i]<endYr,0,periods[i]-endYr), omega), z = c(0, 0)))
	rgl.linestrips(cohi$x, cohi$y, cohi$z, color = AssignColour("P"))
}


ages <- seq(25, omega, by = 25)
for (i in 1:length(ages)){
	cohi <- xyz2ternxyz(data.frame(x = c(startYr, endYr + ages[i]), y = ages[i], z = c(0, 0)))
	rgl.linestrips(cohi$x, cohi$y, cohi$z, color = AssignColour("A"))
}

ns   <- seq(25,200,by=25)
# an example rgl TAL (2000 birth cohort)
for (n in ns){
	xyztern <- xyz2ternxyz(data.frame(
					x = c(2000, 2000+omega, 2000) - n,
					y = c(0, omega, 0),
					z = c(0, 0, omega)))
	rgl.linestrips(xyztern$x[c(1,2)],xyztern$y[c(1,2)],xyztern$z[c(1,2)],color = gray(.5))
	rgl.linestrips(xyztern$x[c(2,3)],xyztern$y[c(2,3)],xyztern$z[c(2,3)],color = gray(.5))
	rgl.linestrips(xyztern$x[c(1,3)],xyztern$y[c(1,3)],xyztern$z[c(1,3)],color = gray(.5))
}



# equal aspect ratio
x <- diff(par3d("bbox")[1:2])
y <- diff(par3d("bbox")[3:4])
z <- diff(par3d("bbox")[5:6])
decorate3d(xlim = c(startYr, endYr), ylim = c(0, omega-25), zlim = c(0, omega-25), 
		aspect = c(x, y, z), box = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "")

# need a way to move parallax w mouse roller




# draw triangle:
#rgl.triangles(xyztern$x,xyztern$y,xyztern$z,alpha=0.1,color = gray(.8),
#		ambient="black",specular="black",emission="black",shininess=0)

#draw quad:
#rgl.quads(xyztern$x,xyztern$y,xyztern$z,alpha=0.3,color = gray(.8),
#		ambient="black",specular="white",emission="black")




