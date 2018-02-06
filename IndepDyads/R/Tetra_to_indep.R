
# Author: tim
###############################################################################
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/IndepDyads/IndepDyads")
} 

# try to make gif animation of a tetrahedron to independant dyad cross.

AssignColour <- function (x) {
	if (x == "A") result <- "#D23737"
	if (x == "P") result <- "#3191C9"
	if (x == "C") result <- "#D2BC2D"
	if (x == "T") result <- "#4EC93B"
	if (x == "D") result <- "#881F93"
	if (x == "L") result <- "#C5752B"
	return(result)
}
# see /home/tim/workspace/Other/APCTgifs/tetrahedrongif.R
# for tweenr demonstration
library(tweenr)
