library(rgdal)
library(alphahull)
library(igraph)
library(shapefiles)
args <- commandArgs(trailingOnly = TRUE)
path = args[1]
layer = args[2]
finalout = args[3]
myPolygons  = readOGR(path,layer=layer)
alpha=1.6
numsample=1000
minarea=20
maxarea=3000
num = length(myPolygons)
poly.list = vector('list')
logfile <- file(paste(finalout, "-rlog.txt", sep=""), "a") # log file
for (i in 1: num) {
	poly = myPolygons@polygons[[i]]
	if (poly@area > minarea && poly@area < maxarea) {
		types = c("hexagonal","regular","nonaligned","stratified")
		attempts = 1
		x.as = 0
		error = 1
		# try to find a functional ashape with any of the sampling types above
		while (attempts <= length(types) && error > 0) {
			pts = spsample(poly, n=numsample, type=types[attempts])
			x.coords = coordinates(pts)
			x.as = tryCatch(
				{
					ashape(x.coords,alpha=alpha)
				}
				,warning = function (w) {}
				,error = function (e) {}
			)
			# cat("Attempts: ", attempts, " class: ", class(x.as), "\n")
			if ( class(x.as)=="ashape" ) {
				x.asg = graph.edgelist(cbind(as.character(x.as$edges[, "ind1"]), as.character(x.as$edges[, "ind2"])), directed = FALSE)
				error = 0
				if (error == 0 && any(degree(x.asg) != 2)) {
					# try to fix circularity
					x.asg <- delete.vertices(x.asg, which(degree(x.asg) == 1))
				}
				if (error == 0 && any(degree(x.asg) != 2)) {
					# cat(poly@ID, ": Graph not circular", "\n")
					error = 2
				}
				if (!is.connected(x.asg)) {
					# cat(poly@ID, ": Graph not connected", "\n")
					error = 1
				}
				if (error == 0 && clusters(x.asg)$no > 1) {
					# cat(poly@ID, ": Graph composed of more than one circle", "\n")
					error = 3
				}
				if (error == 0) {
					# print("Added new polygon")
					cutg = x.asg - E(x.asg)[1]
					# find chain end points
					ends = names(which(degree(cutg) == 1))
					path = get.shortest.paths(cutg, ends[1], ends[2])[[1]]
					# this is an index into the points
					pathX = as.numeric(V(x.asg)[path]$name)
					# join the ends
					pathX = c(pathX, pathX[1])
					p = Polygon(x.as$x[pathX, ])
					# now we simplify the polygon (reduce edge count)
					temp <- as.data.frame(p@coords)
					names(temp) <- c("x","y")
					simple = dp(temp,0.5)
					p@coords <- as.matrix(cbind(simple$x, simple$y))
					# finished simplifying
					ps = Polygons(list(p),1)
					sps = SpatialPolygons(list(ps), , myPolygons@proj4string)
					data = data.frame(DN=i) # the polygon ID
					output = SpatialPolygonsDataFrame(sps, data)
					writeOGR(output, paste(finalout, "-", i, "-polygon.shp", sep=""), layer, driver="ESRI Shapefile")
				}
			}
			attempts = attempts + 1
		}
		if (error > 0) {
			writeLines(paste(i," could not be written: ", error, sep=""), logfile)
		}
	}
}
