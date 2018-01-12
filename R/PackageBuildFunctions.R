#This is code for publishing documentation for the rKIN package
require(devtools)
#Build the Rd files for each function
devtools::document()

devtools::use_testthat()

devtools::check()

devtools::use_vignette("my-vignette")

#run R CMD check on CRANs servers
devtools::build_win()

#create the zip of the package
devtools::build()

data("rodents")
testKin<- estKIN(data = rodents, x = "Ave_C", y = "Ave_N", group = "Species", scaler = 5, smallSamp = TRUE)
testMCP<- estMCP(data = rodents, x = "Ave_C", y = "Ave_N", group = "Species", smallSamp = FALSE)
testElp<- estEllipse(data = rodents, x = "Ave_C", y = "Ave_N", group = "Species", smallSamp = FALSE)



plotKIN(testKin, scaler = 1)
plotKIN(testMCP)
plotKIN(testElp)
getArea(testKin)
calcOverlap(testKin)
plot(testKin$estObj$Spec2)

length(testKin$estObj$Spec2@polygons)
testKin$estObj$Spec2@polygons[[2]]
testKin$estObj$Spec2@data
plot(testKin$estObj$Spec2[2,])

g.int<- rgeos::gIntersection(testKin$estObj$Spec2[3,], testKin$estObj$Spec1[1,])

