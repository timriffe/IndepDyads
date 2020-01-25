
# construct a tetrahedral octahedral honecomb w cubic dual.
library(here)

library(rgl)

#source("R/Functions.R")
angle2rad <- function(angle){
	angle * pi/180
}

yaw <- function(alpha){
	alpha <- angle2rad(alpha)
	matrix(c(1,0,0,0,cos(alpha),sin(alpha),0,-sin(alpha),cos(alpha)),3,3)
}

pitch <- function(beta){
	beta <- angle2rad(beta)
	matrix(c(cos(beta),0,-sin(beta),0,1,0,sin(beta),0,cos(beta)),3,3)
}

roll <- function(gamma){
	gamma <- angle2rad(gamma)
	matrix(c(cos(gamma),sin(gamma),0,-sin(gamma),cos(gamma),0,0,0,1),3,3)
}


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

tetverts_unit <- function(rad = 1, alpha = 0, beta = 0, gamma = 0){
	# 3d rotation matrix
	R <- yaw(alpha) %*% pitch(beta) %*% roll(gamma) 
	xyz <- c("x","y","z")
	verts <- matrix(c(c( sqrt(8/9), 0 , -1/3 ),
    c( -sqrt(2/9), sqrt(2/3), -1/3 ),
    c( -sqrt(2/9), -sqrt(2/3), -1/3 ),
    c( 0 , 0 , 1 )), ncol = 3, byrow = TRUE, dimnames = list(LETTERS[1:4], xyz)) * rad
	# now rotate
	verts <- R %*% t(verts)
	
	verts <- as.data.frame(t(verts))
	colnames(verts) <- xyz
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

edge.mids <- function(rad=1,frac=.5,alpha=0,beta=0,gamma=0){
	
	verts <- tetverts_unit(rad=rad,alpha=alpha,beta=beta,gamma=gamma)
	
	
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



medians <- function(rad=1, alpha=0, beta=0, gamma=0){
	
	verts <- tetverts_unit(rad=rad,alpha=alpha,beta=beta,gamma=gamma)

	#edges <- tetrahedron_a(unit=unit,rad=rad)
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




apcmedx;apcmedy;apcmedz
tpdmedx;tpdmedy;tpdmedz
talmedx;talmedy;talmedz
cdlmedx;cdlmedy;cdlmedz

get_tikz_coords <- function(rad = 1, 
						   alpha = 0, 
						   beta = 0, 
						   gamma = 0,
						   .median = TRUE,
						   .bimedian = FALSE,
							...){
	
# work on turning this into gsubbable text
blob <- "

\\begin{tikzpicture}[line join = round, line cap = round]

\\coordinate (A) at (Ax,Ay,Az); % incident to DPC (event node)
\\coordinate (B) at (Bx,By,Bz); % incident to TPA (step clocks!)
\\coordinate (C) at (Cx,Cy,Cz); % incident to LAC (origin anchor)
\\coordinate (D) at (Dx,Dy,Dz); % incident to TLD (destination anchor)


% median lines: pick one
% \\draw[->,color={rgb:red,1;green,1;blue,1}, densely dotted, line width = {0.2pt}] (A) -- (3*talmedx, 3*talmedy, 3*talmedz); % $TAL$ median
% \\draw[->,color={rgb:red,1;green,1;blue,1}, densely dotted, line width = {0.2pt}] (B) -- (3*cdlmedx, 3*cdlmedy, 3*cdlmedz); % CDL median
% \\draw[->,color={rgb:red,1;green,1;blue,1}, densely dotted, line width = {0.2pt}] (C) -- (3*tpdmedx, 3*tpdmedy, 3*tpdmedz); % TPD median;
\\draw[->,color={rgb:red,1;green,1;blue,1}, densely dotted, line width = {0.2pt}] (D) -- (3*apcmedx, 3*apcmedy, 3*apcmedz); % APC median;

% bimedian lines: pick one
% \\draw[->,color={rgb:red,1;green,1;blue,1}, densely dotted, line width = {0.2pt}] (1.1*cdx,1.1*cdy,1.1*cdz) -- (1.1*abx,1.1*aby,1.1*abz); % $LP$ bimedian
% \\draw[->,color={rgb:red,1;green,1;blue,1}, densely dotted, line width = {0.2pt}] (1.1*bcx,1.1*bcy,1.1*bcz) -- (1.1*adx,1.1*ady,1.1*adz); % $AD$ bimedian
% \\draw[->,color={rgb:red,1;green,1;blue,1}, densely dotted, line width = {0.2pt}] (1.1*bdx,1.1*bdy,1.1*bdz) -- (1.1*acx,1.1*acy,1.1*acz); % $TC$ bimedian

% light shaded faces
\\draw[-, fill={rgb:red,1;green,1;blue,1}, opacity=.05] (A)--(D)--(B)--cycle; % TDP
\\draw[-, fill={rgb:red,1;green,1;blue,1}, opacity=.05] (A)--(D)--(C)--cycle; % CDL
\\draw[-, fill={rgb:red,1;green,1;blue,1}, opacity=.05] (B)--(D)--(C)--cycle; % TAL
\\draw[-, fill={rgb:red,1;green,1;blue,1}, opacity=.05] (A)--(B)--(C)--cycle; % APC

% color edges
\\draw[-, color ={rgb:red,136;green,31;blue,147}, line width = {0.3pt}] (A)--(D); % D
\\draw[-, color ={rgb:red,197;green,117;blue,43}, line width = {0.3pt}] (D)--(C); % L
\\draw[-, color ={rgb:red,78;green,201;blue,59}, line width = {0.3pt}] (D)--(B); % T
\\draw[-, color ={rgb:red,210;green,55;blue,55}, line width = {0.3pt}] (B)--(C); % A
\\draw[-, color ={rgb:red,49;green,145;blue,201}, line width = {0.3pt}] (A)--(B); % P
\\draw[-, color ={rgb:red,210;green,188;blue,45}, line width = {0.3pt}] (A)--(C); % C

% edge labels
\\node[above, color={rgb:red,49;green,145;blue,201}] at (abx,aby,abz) {\\tiny $p$}; % AB mean
\\node[below, color={rgb:red,210;green,188;blue,45}] at (acx,acy,acz) {\\tiny$c$};  % AC mean
\\node[above, color={rgb:red,136;green,31;blue,147}] at (adx,ady,adz) {\\tiny$d$};  % AD mean
\\node[left, color={rgb:red,78;green,201;blue,59}] at (bdx,bdy,bdz) {\\tiny$t$};   % BD mean
\\node[left, color={rgb:red,197;green,117;blue,43}] at (cdx,cdy,cdz) {\\tiny$l$};  % CD mean
\\node[right, color={rgb:red,210;green,55;blue,55}] at (bcx,bcy,bcz) {\\tiny$a$};  % BC mean

% node helper labels
%\\node at (A) {\\small A};
% \\node at (B) {\\small B};
% \\node at (C) {\\small C};
% \\node at (D) {\\small D};

\\end{tikzpicture}

"
	
	verts <- tetverts_unit(rad = rad,
						   alpha = alpha,
						   beta = beta,
						   gamma = gamma)
	meds  <- medians(rad = rad, 
				 	 alpha = alpha, 
					 beta = beta, 
					 gamma = gamma)
	verts <- zapsmall(as.matrix(verts))
	meds  <- zapsmall(as.matrix(meds))
	# median parameters to gsub in:
	library(magrittr)
	blob <- 
		blob %>% 
		# sub in edge coords as well as bimedians (commented out)
		gsub(pattern="abx",replacement = (verts["A","x"]+ verts["B","x"])/2, .) %>% 
		gsub(pattern="aby",replacement = (verts["A","y"]+ verts["B","y"])/2, .) %>% 
		gsub(pattern="abz",replacement = (verts["A","z"]+ verts["B","z"])/2, .) %>%
		
		gsub(pattern="acx",replacement = (verts["A","x"]+ verts["C","x"])/2, .) %>% 
		gsub(pattern="acy",replacement = (verts["A","y"]+ verts["C","y"])/2, .) %>% 
		gsub(pattern="acz",replacement = (verts["A","z"]+ verts["C","z"])/2, .) %>%
		
		gsub(pattern="adx",replacement = (verts["A","x"]+ verts["D","x"])/2, .) %>% 
		gsub(pattern="ady",replacement = (verts["A","y"]+ verts["D","y"])/2, .) %>% 
		gsub(pattern="adz",replacement = (verts["A","z"]+ verts["D","z"])/2, .) %>%
		
		gsub(pattern="bdx",replacement = (verts["B","x"]+ verts["D","x"])/2, .) %>% 
		gsub(pattern="bdy",replacement = (verts["B","y"]+ verts["D","y"])/2, .) %>% 
		gsub(pattern="bdz",replacement = (verts["B","z"]+ verts["D","z"])/2, .) %>%
		
		gsub(pattern="cdx",replacement = (verts["C","x"]+ verts["D","x"])/2, .) %>% 
		gsub(pattern="cdy",replacement = (verts["C","y"]+ verts["D","y"])/2, .) %>% 
		gsub(pattern="cdz",replacement = (verts["C","z"]+ verts["D","z"])/2, .) %>%
		
		gsub(pattern="bcx",replacement = (verts["B","x"]+ verts["C","x"])/2, .) %>% 
		gsub(pattern="bcy",replacement = (verts["B","y"]+ verts["C","y"])/2, .) %>% 
		gsub(pattern="bcz",replacement = (verts["B","z"]+ verts["C","z"])/2, .) 
	# sub in vertex coords
	blob <- blob %>% 
		gsub(pattern="Ax",replacement =  verts["A","x"], .) %>% 
		gsub(pattern="Ay",replacement =  verts["A","y"], .) %>% 
		gsub(pattern="Az",replacement =  verts["A","z"], .) %>%
		gsub(pattern="Bx",replacement =  verts["B","x"], .) %>% 
		gsub(pattern="By",replacement =  verts["B","y"], .) %>% 
		gsub(pattern="Bz",replacement =  verts["B","z"], .) %>%
		gsub(pattern="Cx",replacement =  verts["C","x"], .) %>% 
		gsub(pattern="Cy",replacement =  verts["C","y"], .) %>% 
		gsub(pattern="Cz",replacement =  verts["C","z"], .) %>%
		gsub(pattern="Dx",replacement =  verts["D","x"], .) %>% 
		gsub(pattern="Dy",replacement =  verts["D","y"], .) %>% 
		gsub(pattern="Dz",replacement =  verts["D","z"], .) 

	    # medians
		blob <- 
			blob %>% 
			# sub in vertex coords
			gsub(pattern="talmedx",replacement =  meds["TAL","x2"], .) %>% 
			gsub(pattern="talmedy",replacement =  meds["TAL","y2"], .) %>% 
			gsub(pattern="talmedz",replacement =  meds["TAL","z2"], .) %>%
			gsub(pattern="cdlmedx",replacement =  meds["CDL","x2"], .) %>% 
			gsub(pattern="cdlmedy",replacement =  meds["CDL","y2"], .) %>% 
			gsub(pattern="cdlmedz",replacement =  meds["CDL","z2"], .) %>%
			gsub(pattern="tpdmedx",replacement =  meds["TPD","x2"], .) %>% 
			gsub(pattern="tpdmedy",replacement =  meds["TPD","y2"], .) %>% 
			gsub(pattern="tpdmedz",replacement =  meds["TPD","z2"], .) %>%
			gsub(pattern="apcmedx",replacement =  meds["APC","x2"], .) %>% 
			gsub(pattern="apcmedy",replacement =  meds["APC","y2"], .) %>% 
			gsub(pattern="apcmedz",replacement =  meds["APC","z2"], .) 
	

	cat(blob, ...)
}


get_tikz_coords(1,
				alpha = 20,
				beta = -30,
				gamma = 30,
				.median = TRUE, 
				file = here("IndepDyads",
                            "S1",
                            "fig2_medians.tex"))

get_tikz_coords(1,
				alpha = 20,
				beta = -30,
				gamma = 30,
				.median = FALSE,
				.bimedian = TRUE,
				file = here("IndepDyads",
							"S1",
							"fig2_bimedians.tex"))


