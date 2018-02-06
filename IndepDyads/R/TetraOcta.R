
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
tetverts <- function(){
	verts <- data.frame(x=c(0,1,.5,.5),y=c(0,0,sqrt(3)/2,sqrt(3)/6),z=c(0,0,0,sqrt(3)/2))
	rownames(verts) <- 1:4
	verts
}


tetrahedron_a <- function(){
	
	# vertex coords
	verts <- tetverts()
	vx    <- verts$vx
	vy    <- verts$vy
	vz    <- verts$vz
	
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
medians <- function(){
	verts <- tetverts()
	edges <- tetrahedron_a()
	medians <- data.frame(x1 = verts$x,y1verts$y,z1=verts$z,
			

	x2 = c(verts$x[4]/3 + edges["A","xm"]*2/3,
	       verts$x[3]/3 + edges["D","xm"]*2/3,
           verts$x[4]/3 + edges["P","xm"]*2/3,
           verts$x[1]/3 + edges["A","xm"]*2/3),
	y2 = c(verts$y[4]/3 + edges["A","ym"]*2/3,
			verts$y[3]/3 + edges["D","ym"]*2/3,
			verts$y[4]/3 + edges["P","ym"]*2/3,
			verts$y[1]/3 + edges["A","ym"]*2/3),
	z2 = c(verts$z[4]/3 + edges["A","zm"]*2/3,
			verts$z[3]/3 + edges["D","zm"]*2/3,
			verts$z[4]/3 + edges["P","zm"]*2/3,
			verts$z[1]/3 + edges["A","zm"]*2/3))
	
	rownames(medians) <- c("TAL","CDL","TPD","APC")
	medians
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
tet <- tetrahedron_a()


# let's start with unit space: c(-2,2) for everything?
xlim <- c(-2,2)
ylim <- c(-2,2)
zlim <- c(-2,2)

# set up device

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
  text3d(x = tet$xm[i], y = tet$ym[i], z = tet$zm[i], tet$edge[i], col = AssignColour(tet$edge[i]), cex = 2)
}
verts <- tetverts()
# label vertices for now:
for (i in 1:4){
	text3d(x = verts$x,verts$y,verts$z,1:4, cex = 2,col="black")
}

# the three independant view axes (bimedian)
indeps <- matrix(c("A","T","L","D","C","P"),ncol=2)
for (i in 1:3){
	rgl.linestrips(
			x = c(tet[indeps[i,1],"xm"], tet[indeps[i,2],"xm"]), 
			y = c(tet[indeps[i,1],"ym"], tet[indeps[i,2],"ym"]), 
			z = c(tet[indeps[i,1],"zm"], tet[indeps[i,2],"zm"]), 
			color = "#FF00FF50", lwd = 2)
}

# the four triad normal view axes (medians)
triads <- c("APC","TPD","CDL","TAL")
meds <- medians()
for (i in 1:4){

	
	rgl.linestrips(
			x = c(meds[triads[i],"x1"], meds[triads[i],"x2"]), 
			y = c(meds[triads[i],"y1"], meds[triads[i],"y2"]), 
			z = c(meds[triads[i],"z1"], meds[triads[i],"z2"]), 
			color = "#00FFFF50", lwd = 2)
}
