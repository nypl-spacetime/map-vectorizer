#!/usr/bin/python
import re, sys, getopt, subprocess, shlex, os, datetime, ogr, glob

def main(argv):
	inputfile = ''
	tempgdalfile = ''
	instructions = 'vectorize_map.py <inputfile>'
	defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'
	chunksize = 50000
	currentchunk = 0
	totalsubsets = 0
	basecolors = [
		[255,255,244] # paper
		,[253,217,193] # pink
		,[216,216,138] # green
		,[255,254,193] # yellow
	]

	try:
		opts, args = getopt.getopt(argv,"hi:o:",["ifile=","ofile="])
	except getopt.GetoptError:
		print instructions
		sys.exit(2)
	for opt, arg in opts:
		if opt == '-h':
			print instructions
			sys.exit()
		elif opt in ("-i", "--ifile"):
			inputfile = arg

	if len(argv) == 1:
		inputfile = argv[0]

	if inputfile == '':
		print instructions
		sys.exit(2)

	print ""
	print ""
	print ""
	print ""
	print ""
	print "NYPL Labs Map Vectorizer v0.1"
	print "============================="
	print "By: Mauricio Giraldo Arteaga @mgiraldo / @nypl_labs"
	print ""

	gimp_path = raw_input("GIMP executable path [" + defaultgimp +  "]: ")

	if gimp_path == '':
		gimp_path = defaultgimp

	starttime = datetime.datetime.now()

	fullpath = os.path.abspath(__file__)
	base_name = inputfile[:inputfile.find(".tif")]

	# create a folder to store all this crap
	directory = base_name 
	if not os.path.exists(directory):
		os.makedirs(directory)

	# GIMP processing
	dir_base_name = directory + "/" + base_name

	thresholdfile = dir_base_name + "-threshold-tmp.tif"
	comparativefile = dir_base_name + "-comparative-tmp.tif"

	print ""
	print "Thresholdizing:"
	print "---------------"
	print inputfile + " into threshold file: " + thresholdfile

	command = gimp_path + ' -i -b \'(nypl-create-threshold "' + inputfile + '" "' + thresholdfile + '")\' -b \'(gimp-quit 0)\''
	print command
	os.system(command)

	print inputfile + " into comparative file: " + comparativefile
	command = gimp_path + ' -i -b \'(nypl-create-comparative "' + inputfile + '" "' + comparativefile + '")\' -b \'(gimp-quit 0)\''
	print command
	os.system(command)

	tempgdalfile = dir_base_name + "-tmp.tif"


	# GDAL transformation

	print "\n"
	print 'Origin GeoTIFF :', inputfile
	print 'Destination    :', tempgdalfile

	# BETTER (SOME) ERROR HANDLING SHOULD BE DONE!!!!!

	# first get geotiff data from original
	geoText = subprocess.Popen(["gdalinfo", inputfile], stdout=subprocess.PIPE).communicate()[0]
	pattern = re.compile(r"Upper Left\s*\(\s*([0-9\-\.]*),\s*([0-9\-\.]*).*\n.*\n.*\nLower Right\s*\(\s*([0-9\-\.]*),\s*([0-9\-\.]*).*")
	geoMatch = pattern.findall(geoText)
	# print pattern
	print "\n"
	print "Geodata obtained:"
	print "-----------------"
	print "W", geoMatch[0][0]
	print "N", geoMatch[0][1]
	print "E", geoMatch[0][2]
	print "S", geoMatch[0][3]
	print "\n"

	W = geoMatch[0][0]
	N = geoMatch[0][1]
	E = geoMatch[0][2]
	S = geoMatch[0][3]
	
	print "Applying to destination:"
	print "------------------------"
	# print outputgdal
	outputwsg = dir_base_name + "-wsg-tmp.tif"
	command = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84" -of GTiff -co "INTERLEAVE=PIXEL" -a_ullr ' + W + ' ' + N + ' ' + E + ' ' + S + ' ' + thresholdfile + ' ' + outputwsg
	print command
	os.system(command)

	print ""
	outputgdal = dir_base_name + "-gdal-tmp.tif"
	command = 'gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3785 -r bilinear ' + outputwsg + ' ' + outputgdal
	print command
	os.system(command)

	# transform comparative
	comparativewsg = dir_base_name + "-comparative-wsg-tmp.tif"
	command = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84" -of GTiff -co "INTERLEAVE=PIXEL" -a_ullr ' + W + ' ' + N + ' ' + E + ' ' + S + ' ' + comparativefile + ' ' + comparativewsg
	print command
	os.system(command)

	print ""
	comparativegdal = dir_base_name + "-comparative-gdal-tmp.tif"
	command = 'gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3785 -r bilinear ' + comparativewsg + ' ' + comparativegdal
	print command
	os.system(command)

	# QGIS POLYGONIZE

	print ""
	print "Polygonizing (coarse):"
	print "----------------------"
	shapefile = dir_base_name + '.shp'
	command = 'gdal_polygonize.py ' + outputgdal + ' -f "ESRI Shapefile" ' + shapefile + ' ' + base_name
	print command
	os.system(command)

	# Split resulting megapolygon file into smaller chunks
	# most code from: http://cosmicproject.org/OGR/cris_example_write.html

	print ""
	print "Splitting megapolygon file into chunks"
	print "--------------------------------------"

	#####

	# 2 get the shapefile driver
	driver = ogr.GetDriverByName('ESRI Shapefile')

	# 3 open the input data source and get the layer
	inDS = driver.Open(shapefile, 0) #shows cover at given points
	if inDS is None:
		print 'Could not open shapefile'
		sys.exit(1)
	inLayer = inDS.GetLayer()

	# 5 get the FieldDefn's for the id and cover fields in the input shapefile
	feature = inLayer.GetFeature(0)
	idFieldDefn = feature.GetFieldDefnRef('DN')

	# 7 loop through the input features
	inFeature = inLayer.GetNextFeature()
	while inFeature:
		if currentchunk == 0 or currentchunk >= chunksize:
			currentchunk = 0
			totalsubsets = totalsubsets + 1
			# this is a new temp file
			# 4 create a new data source and layer
			fn = dir_base_name + '-tmp-' + str(totalsubsets) + '.shp'
			if os.path.exists(fn):driver.DeleteDataSource(fn)
			outDS = driver.CreateDataSource(fn)
			if outDS is None:
				print 'Could not create temp shapefile'
				sys.exit(1)
			outLayer = outDS.CreateLayer(base_name, geom_type=ogr.wkbPolygon)

			#create new field in the output shapefile
			outLayer.CreateField(idFieldDefn)

			# 6 get the FeatureDefn for the output layer
			featureDefn = outLayer.GetLayerDefn()

		# create a new feature
		outFeature = ogr.Feature(featureDefn)#using featureDefn created in step 6

		# set the geometry
		geom = inFeature.GetGeometryRef()
		outFeature.SetGeometry(geom) #move it to the new feature

		# set the attributes
		DN = inFeature.GetField('DN')
		outFeature.SetField('DN', DN) #move it to the new feature

		# add the feature to the output layer
		outLayer.CreateFeature(outFeature)

		# destroy the output feature
		outFeature.Destroy()

		# destroy the input feature and get a new one
		inFeature.Destroy()
		inFeature = inLayer.GetNextFeature()

		currentchunk = currentchunk + 1

	# close the data sources
	inDS.Destroy()
	outDS.Destroy() #flush out the last changes here

	print ""
	print "Produced " + str(totalsubsets) + " temporary shapefiles"
	print ""

	#####

	# R Simplification

	print ""
	print "Polygonizing (simplify):"
	print "------------------------"

	path = fullpath[:fullpath.find("/vectorize_map.py")] + '/' + directory

	# First simplify each temporary shapefile
	currentsubset = 1
	while currentsubset <= totalsubsets:
		rinput = path + '/' + base_name + '-tmp-' + str(currentsubset) + '.shp'
		routput = path + '/' + base_name + '-tmp-' + str(currentsubset)
		layer = base_name + '-tmp-' + str(currentsubset)
		command = 'R --vanilla --silent --slave -f simplify_map.R --args ' + rinput + ' ' + layer + ' ' + routput
		print command
		os.system(command)
		currentsubset = currentsubset + 1

	# Now combine all subsets into a macroset

	# 4 create a new data source and layer
	fn = dir_base_name + '-traced.shp'

	if os.path.exists(fn):driver.DeleteDataSource(fn)
	outDS = driver.CreateDataSource(fn)
	if outDS is None:
		print 'Could not create final shapefile'
		sys.exit(1)
	outLayer = outDS.CreateLayer(base_name, geom_type=ogr.wkbPolygon)

	#create new field in the output shapefile
	outLayer.CreateField(idFieldDefn)

	# 6 get the FeatureDefn for the output layer
	featureDefn = outLayer.GetLayerDefn()

	polygonfiles = []
	for files in os.listdir(path):
		if files.endswith(".shp") and files.find('-polygon') != -1:
			polygonfile = path + "/" + files
			extractedfile = path + "/" + files[:files.find(".shp")] + "-extracted.tif"
			# extract bitmap from original
			command = "gdalwarp -q -t_srs EPSG:3785 -cutline " + polygonfile + " -crop_to_cutline -of GTiff " + comparativegdal + " " + extractedfile
			os.system(command)
			# calculate color
			# shrink to 1x1 and find value
			pixelvalue = subprocess.Popen(["convert", extractedfile, "-resize", "1x1","txt:-"], stdout=subprocess.PIPE).communicate()[0]
			pattern = re.compile(r"0,0: \(([0-9]*),([0-9]*),([0-9]*),[0-9]*.*")
			values = pattern.findall(pixelvalue)
			if len(values) > 0:
				red = int(values[0][0])
				green = int(values[0][1])
				blue = int(values[0][2])
				nearest = 100000
				nearestcolor = []
				for color in basecolors:
					dred = (color[0] - red) * (color[0] - red)
					dgreen = (color[1] - green) * (color[1] - green)
					dblue = (color[2] - blue) * (color[2] - blue)
					dist = dred + dgreen + dblue
					if dist < nearest:
						nearest = dist
						nearestcolor = color
				# only add if NOT paper
				if nearestcolor != basecolors[0]:
					# add to array
					polygonfiles.append(polygonfile)

	for files in polygonfiles:
		# 3 open the input data source and get the layer
		tempfile = files #dir_base_name + '-tmp-' + str(currentsubset) + '-traced.shp'
		inDS = driver.Open(tempfile, 0) #shows cover at given points
		if inDS is None:
			print 'Could not open temporary shapefile'
			break
		inLayer = inDS.GetLayer()

		# 7 loop through the input features
		inFeature = inLayer.GetNextFeature()
		while inFeature:
			# create a new feature
			outFeature = ogr.Feature(featureDefn) #using featureDefn created in step 6

			# set the geometry
			geom = inFeature.GetGeometryRef()
			outFeature.SetGeometry(geom) #move it to the new feature

			# !!! IMPORTANT MUST FIX !!! set the attributes
			DN = inFeature.GetField('DN')
			outFeature.SetField('DN', DN) #move it to the new feature

			# add the feature to the output layer
			outLayer.CreateFeature(outFeature)

			# destroy the output feature
			outFeature.Destroy()

			# destroy the input feature and get a new one
			inFeature.Destroy()
			inFeature = inLayer.GetNextFeature()

		# close the data sources
		inDS.Destroy()
	
	outDS.Destroy() #flush out the last changes here

	print ""
	print "Applying projection file to result..."
	print "-------------------------------------"
	os.system("cp " + dir_base_name + ".prj " + dir_base_name + "-traced.prj")

	# Cleaning
	print ""
	print "Cleaning..."
	print "-----------"
	os.system("rm " + outputgdal)
	os.system("rm " + outputwsg)
	os.system("rm " + thresholdfile)
	os.system("rm " + dir_base_name + "-tmp-*.shp")
	os.system("rm " + dir_base_name + "-tmp-*.dbf")
	os.system("rm " + dir_base_name + "-tmp-*.shx")
	os.system("rm " + dir_base_name + "-tmp*.tif")

	endtime = datetime.datetime.now()
	deltatime = endtime-starttime

	print "Operation took " + str(deltatime.seconds) + " seconds"

if __name__ == "__main__":
	main(sys.argv[1:])

