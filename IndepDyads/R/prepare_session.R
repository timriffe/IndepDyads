# make sure all dependencies installed
# the script modified from one found here:
#
# https://github.com/ikashnitsky/demres-geofacet/blob/master/R/session-preparation.R
################################################################################

# install pacman to streamline further package installation
if (!require("pacman", character.only = TRUE)){
	install.packages("pacman", dep = TRUE)
	if (!require("pacman", character.only = TRUE))
		stop("Package not found")
}

# Some packages to install from CRAN:
pkgs <- c(
	"foreign",
	"lubridate",
	"here",
	"reshape2",
	"colorspace",
	"scales"
)


# install the missing packages from CRAN
# only runs if at least one package is missing
if(!sum(!p_isinstalled(pkgs))==0){
	p_install(
		package = pkgs[!p_isinstalled(pkgs)], 
		character.only = TRUE
	)
}

# load the packages
p_load(pkgs, character.only = TRUE)

# ------------------------------------------------ #
# helper function for color-coding APCTDL diagrams #
# ------------------------------------------------ #
AssignColour <- function (x) {
	x             <- toupper(x)
	recvec        <- c("#D23737","#3191C9","#D2BC2D","#4EC93B","#881F93","#C5752B")
	names(recvec) <- c("A","P","C","T","D","L")
	recvec[x]
}

cat("packages installed and loaded, move on to next step\n")