library(rgdal)
library(alphahull)
library(igraph)
library(shapefiles)
filepath = "/Users/clintnewsom/projects/NYPL/map-vectorizer/test/"
layer = "test-traced"
myPolygons  = readOGR(filepath,layer=layer)
ogrInfo(filepath,layer)
plot(myPolygons)