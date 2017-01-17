<!-- README.md is generated from README.Rmd. Please edit that file -->
rKIN README
-----------

This package applies methods used to estimate animal homerange, but instead of geospatial coordinates, we use isotopic coordinates. The estimation methods include: 1) 2-dimensional bivariate normal kernel utilization density estimator, 2) bivariate normal ellipse estimator, and 3) minimum convex polygon estimator, all applied to stable isotope data. Additionally, functions to determine niche area, polygon overlap between groups and levels (confidence contours) and plotting capabilities.

**To install and load `rKIN` in R, run the following (requires the devtools package):**

`devtools::install_github("salbeke/rKIN@master")`

`require(rKIN)`
