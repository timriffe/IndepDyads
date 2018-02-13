
# construct a tetrahedral octahedral honecomb w cubic dual.

setwd("/home/tim/git/IndepDyads/IndepDyads/")
library(rgl)

#source("R/Functions.R")

xyz2ternxyz <- function(xyz){
	ternxyz   <- xyz
	
	ternxyz$x <- xyz$x - (xyz$y * .5)
	ternxyz$y <- xyz$y * sqrt(3) / 2 + xyz$z * (sqrt(3) / 2 - 1 / sqrt(3))
	ternxyz$z <- xyz$z * sqrt(6) / 3
	ternxyz
}

# tetrahedron function- rework this to a unit sphere tetrahedron.
tetverts <- function(){
	verts <- data.frame(x=c(0,1,.5,.5),y=c(0,0,sqrt(3)/2,sqrt(3)/6),z=c(0,0,0,sqrt(3)/2))
	rownames(verts) <- 1:4
	verts
}

tetverts_unit <- function(rad = 1){
	verts <- matrix(c(c( sqrt(8/9), 0 , -1/3 ),
    c( -sqrt(2/9), sqrt(2/3), -1/3 ),
    c( -sqrt(2/9), -sqrt(2/3), -1/3 ),
    c( 0 , 0 , 1 )),ncol = 3,byrow=TRUE,dimnames=list(1:4,c("x","y","z"))) * rad
	verts <- as.data.frame(verts)
	verts
}

tetrahedron_a <- function(unit=TRUE,rad=1){
	
	# vertex coords
	if (unit){
		verts <- tetverts_unit(rad=rad)
	} else {
		verts <- tetverts()
	}

	vx    <- verts$x
	vy    <- verts$y
	vz    <- verts$z
	
	# vertex pairings
	vi1 <- c(1,1,1,2,2,3)
	vi2 <- c(2,3,4,3,4,4)
	
	edges <- data.frame(
			edge = c("P","C","D","A","T","L"),
			x1 = vx[vi1],
			x2 = vx[vi2],
			y1 = vy[vi1],
			y2 = vy[vi2],
			z1 = vz[vi1],
			z2 = vz[vi2])
	edges$xm <- (vx[vi1] + vx[vi2]) / 2
	edges$ym <- (vy[vi1] + vy[vi2]) / 2
	edges$zm <- (vz[vi1] + vz[vi2]) / 2
	rownames(edges) <- edges$edge
	edges
}

edge.mids <- function(unit=TRUE,rad=1,frac=.5){
	if (unit){
		verts <- tetverts_unit(rad=rad)
	} else {
		verts <- tetverts()
	}
	
	vx    <- verts$x
	vy    <- verts$y
	vz    <- verts$z
	
	# vertex pairings
	vi1   <- c(1,1,1,2,2,3)
	vi2   <- c(2,3,4,3,4,4)
	
	xm    <- vx[vi1] * frac + vx[vi2] * (1 - frac)
	ym    <- vy[vi1] * frac + vy[vi2] * (1 - frac)
	zm    <- vz[vi1] * frac + vz[vi2] * (1 - frac)
	mids  <- data.frame(edge = c("P","C","D","A","T","L"), xm = xm, ym = ym, zm = zm)
	rownames(mids) <- mids$edge
	mids
}



medians <- function(unit=TRUE,rad=1){
	if (unit){
		verts <- tetverts_unit(rad=rad)
	} else {
		verts <- tetverts()
	}
	edges <- tetrahedron_a(unit=unit,rad=rad)
	meds  <- data.frame(x1 = verts$x, y1 = verts$y, z1 = verts$z)
	# need opposite centroids.
	
	# should use 3 opposite verts. Ergo 
	# v1 connects to v3 / 3 + (v1 + v2)/3
	
	centroid <- function(verti = 1, verts){
		this.tri <- as.matrix(verts[-verti, ])
		P1 <- this.tri[1, ]
		PM <- colMeans(this.tri[-1, ])
		PM + (P1 - PM) / 3
	}
	cents   <- t(sapply(1:4, centroid, verts = verts))
	meds$x2 <- cents[, "x"]
	meds$y2 <- cents[, "y"]
	meds$z2 <- cents[, "z"]
	rownames(meds) <- c("TAL","CDL","TPD","APC")
	meds
}

AssignColour <- function (x) {
	if (x == "A") result <- "#D23737"
	if (x == "P") result <- "#3191C9"
	if (x == "C") result <- "#D2BC2D"
	if (x == "T") result <- "#4EC93B"
	if (x == "D") result <- "#881F93"
	if (x == "L") result <- "#C5752B"
	return(result)
}
tet  <- tetrahedron_a()
tetm <- edge.mids(rad=1.1,frac=.33) 
triads <- c("APC","TPD","CDL","TAL")
meds <- medians(rad=1.3)
# let's start with unit space: c(-2,2) for everything?
xlim <- c(-3,3)
ylim <- c(-3,3)
zlim <- c(-3,3)

# set up device
rgl.close()
par3d(FOV=0)
plot3d(xlim = xlim, ylim = ylim, zlim = zlim, 
		box = FALSE, 
		aspect = c(1, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
bg3d("white")
for (i in 1:nrow(tet)){
	rgl.linestrips(
			x = c(tet$x1[i], tet$x2[i]), 
			y = c(tet$y1[i], tet$y2[i]), 
			z = c(tet$z1[i], tet$z2[i]), 
			color = AssignColour(tet$edge[i]), lwd = 4)
	text3d(x = tetm$xm[i], y = tetm$ym[i], z = tetm$zm[i], tetm$edge[i], col = AssignColour(tetm$edge[i]), cex = 3)
}
#verts <- tetverts()
# label vertices for now:
#for (i in 1:4){
#	text3d(x = verts$x,verts$y,verts$z,1:4, cex = 2,col="black")
#}

# the three independant view axes (bimedian)
#tetbm  <- tetrahedron_a(rad=1.5)
#indeps <- matrix(c("A","T","L","D","C","P"),ncol=2)
#for (i in 1:3){
#	rgl.linestrips(
#			x = c(tetbm[indeps[i,1],"xm"], tetbm[indeps[i,2],"xm"]), 
#			y = c(tetbm[indeps[i,1],"ym"], tetbm[indeps[i,2],"ym"]), 
#			z = c(tetbm[indeps[i,1],"zm"], tetbm[indeps[i,2],"zm"]), 
#			color = "#FF00FF50", lwd = 2)
#}

# the four triad normal view axes (medians)

for (i in 1:4){
	rgl.linestrips(
			x = c(meds[triads[i],"x1"], meds[triads[i],"x2"]), 
			y = c(meds[triads[i],"y1"], meds[triads[i],"y2"]), 
			z = c(meds[triads[i],"z1"], meds[triads[i],"z2"]), 
			color = "#00FFFF50", lwd = 2)
	text3d(x = meds[triads[i],"x2"],meds[triads[i],"y2"],meds[triads[i],"z2"],triads[i], cex = 1,col="black")
}

#text3d(-.5,-.5,-.5,"Tim Riffe (2018)", cex = 1.5,col="black")
#text3d(1,1,1,"demographic time view axes", cex = 1.5,col="black")

writeASY(title = "DepViewAxes",outtype="pdflatex",width=3,height=3,defaultFontsize = 14)
getwd()

rgl.close()
par3d(FOV=0)
plot3d(xlim = xlim, ylim = ylim, zlim = zlim, 
		box = FALSE, 
		aspect = c(1, 1, 1),
		axes = FALSE, 
		type ='n',
		xlab = "",
		ylab = "",
		zlab = "")
bg3d("white")
for (i in 1:nrow(tet)){
	rgl.linestrips(
			x = c(tet$x1[i], tet$x2[i]), 
			y = c(tet$y1[i], tet$y2[i]), 
			z = c(tet$z1[i], tet$z2[i]), 
			color = AssignColour(tet$edge[i]), lwd = 4)
	text3d(x = tetm$xm[i], y = tetm$ym[i], z = tetm$zm[i], tetm$edge[i], col = AssignColour(tetm$edge[i]), cex = 3)
}
#verts <- tetverts()
# label vertices for now:
#for (i in 1:4){
#	text3d(x = verts$x,verts$y,verts$z,1:4, cex = 2,col="black")
#}

# the three independant view axes (bimedian)
tetbm   <- tetrahedron_a(rad=1.5)
tetbml  <- tetrahedron_a(rad=1.6)
indeps  <- matrix(c("A","T","L","D","C","P"),ncol=2)
indepsl <- paste0(indeps[,1],indeps[,2])
for (i in 1:3){
	rgl.linestrips(
			x = c(tetbm[indeps[i,1],"xm"], tetbm[indeps[i,2],"xm"]), 
			y = c(tetbm[indeps[i,1],"ym"], tetbm[indeps[i,2],"ym"]), 
			z = c(tetbm[indeps[i,1],"zm"], tetbm[indeps[i,2],"zm"]), 
			color = "#FF00FF50", lwd = 2)
	text3d(x = tetbml[indeps[i,1],"xm"],tetbml[indeps[i,1],"ym"],tetbml[indeps[i,1],"zm"],indepsl[i], cex = 1,col="black")
}

# the four triad normal view axes (medians)
#triads <- c("APC","TPD","CDL","TAL")
#meds <- medians(rad=1.3)
#for (i in 1:4){
#	rgl.linestrips(
#			x = c(meds[triads[i],"x1"], meds[triads[i],"x2"]), 
#			y = c(meds[triads[i],"y1"], meds[triads[i],"y2"]), 
#			z = c(meds[triads[i],"z1"], meds[triads[i],"z2"]), 
#			color = "#00FFFF50", lwd = 2)
#	text3d(x = meds[triads[i],"x2"],meds[triads[i],"y2"],meds[triads[i],"z2"],triads[i], cex = 1,col="black")
#}

#text3d(-.5,-.5,-.5,"Tim Riffe (2018)", cex = 1.5,col="black")
#text3d(1,1,1,"demographic time view axes", cex = 1.5,col="black")

writeASY(title = "IndepViewAxes",outtype="pdflatex",width=3,height=3,defaultFontsize = 14)
getwd()
