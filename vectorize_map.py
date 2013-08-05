#!/usr/bin/python
import re, sys, getopt, subprocess, shlex, os, datetime, ogr, glob, csv

import cv2, sys
from cv2 import cv
import os
import numpy as np

tempgdalfile = ''
instructions = 'vectorize_map.py <input file or dir>'
defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'
gimp_path = defaultgimp
chunksize = 50000 # how to split the mega polygon file
currentchunk = 0
totalsubsets = 0
# colors sh/could be an external config file
basecolors = [
	[206,202,185] # paper
	,[199,179,173] # pink
	,[179,155,157] # dark red
	,[149,156,141] # green
	,[199,195,163] # light yellow
	,[195,189,154] # yellow
	,[255,225,40] # bright yellow
	,[137,174,163] # greenish blue
	,[187,194,192] # light blue
	,[161,175,190] # "navy" blue
]
starttime = 0

def main(argv):

	global instructions
	global defaultgimp
	global gimp_path
	global basecolors
	global starttime

	try:
		opts, args = getopt.getopt(argv,"hi:o:",["ifile=","ofile="])
	except getopt.GetoptError:
		print instructions
		sys.exit(2)
	for opt, arg in opts:
		if opt == '-h':
			print instructions
			sys.exit()
		elif opt in ("-i"):
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

	# test for config file
	# TODO: integer checking
	# FIRST LINE SHOULD ALWAYS BE PAPER COLOR
	config_file = "vectorize_config.txt"
	if os.path.isfile(config_file):
		tempcolors = []
		with open(config_file, 'r') as configcsv:
			configcolors = csv.reader(configcsv, delimiter=',')
			for row in configcolors:
				tempcolors.append([int(row[0]), int(row[1]), int(row[2])])
			if len(tempcolors) > 2:
				basecolors = tempcolors

	starttime = datetime.datetime.now()

	totalfiles = 0
	# if input is a directory iterate through it
	if os.path.isdir(inputfile) == True:
		for ff in os.listdir(inputfile):
			if ff.endswith(".tif"):
				totalfiles = totalfiles + 1
				processfile(ff, inputfile)
	else:
		# if input is a file, process it
		processfile(inputfile, "")
		totalfiles = 1

	endtime = datetime.datetime.now()
	deltatime = endtime-starttime
	print "Processed  " + str(totalfiles) + " files\n"
	print "Operation took " + str(deltatime.seconds) + " seconds"

def processfile(inputfile, basedir):
	#
	# NOTE
	#
	# This still needs a lot of work for when dealing with subfolders and such.
	# Best case is image file is located right next to vectorizer_map.py
	#
	global tempgdalfile
	global instructions
	global defaultgimp
	global gimp_path
	global chunksize
	global currentchunk
	global totalsubsets

	currentchunk = 0
	totalsubsets = 0

	print "\n\nProcessing file: " + inputfile
	# right now assuming vectorizer, simplifier and input are in the same folder
	fullpath = os.path.abspath(__file__)

	base_name = inputfile[:inputfile.find(".tif")]
	base_name = base_name[base_name.rfind("/")+1:]

	# create a folder to store all this
	if basedir != '':
		directory = basedir + '/' + base_name
		inputfile = basedir + '/' + inputfile
	else:
		directory = base_name

	if not os.path.exists(directory):
		os.makedirs(directory)

	path = fullpath[:fullpath.find("/vectorize_map.py")] + '/' + directory

	# GIMP processing
	dir_base_name = directory + "/" + base_name

	# create a log file
	logfile = open(directory + "/py-log.txt", "w")

	thresholdfile = dir_base_name + "-threshold-tmp.tif"
	comparativefile = dir_base_name + "-comparative-tmp.tif"

	print "\n\n"
	print "Thresholdizing:"
	print "---------------"
	print inputfile + " into threshold file: " + thresholdfile

	command = gimp_path + ' -i -b \'(nypl-create-threshold "' + inputfile + '" "' + thresholdfile + '")\' -b \'(gimp-quit 0)\''
	logfile.write(command + "\n")
	# print command
	os.system(command)

	# print inputfile + " into comparative file: " + comparativefile
	# command = gimp_path + ' -i -b \'(nypl-create-comparative "' + inputfile + '" "' + comparativefile + '")\' -b \'(gimp-quit 0)\''
	# logfile.write(command + "\n")
	# # print command
	# os.system(command)

	tempgdalfile = dir_base_name + "-tmp.tif"


	# GDAL transformation

	print "\n"
	print 'Origin GeoTIFF :', inputfile
	print 'Destination	:', tempgdalfile

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
	logfile.write(command + "\n")
	# print command
	os.system(command)

	print ""
	outputgdal = dir_base_name + "-gdal-tmp.tif"
	command = 'gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3785 -r bilinear ' + outputwsg + ' ' + outputgdal
	logfile.write(command + "\n")
	# print command
	os.system(command)

	# # transform comparative
	# comparativewsg = dir_base_name + "-comparative-wsg-tmp.tif"
	# command = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84" -of GTiff -co "INTERLEAVE=PIXEL" -a_ullr ' + W + ' ' + N + ' ' + E + ' ' + S + ' ' + comparativefile + ' ' + comparativewsg
	# logfile.write(command + "\n")
	# # print command
	# os.system(command)

	# print ""
	# comparativegdal = dir_base_name + "-comparative-gdal-tmp.tif"
	# command = 'gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3785 -r bilinear ' + comparativewsg + ' ' + comparativegdal
	# logfile.write(command + "\n")
	# # print command
	# os.system(command)

	# QGIS POLYGONIZE

	print ""
	print "Polygonizing (coarse):"
	print "----------------------"
	shapefile = dir_base_name + '.shp'
	command = 'gdal_polygonize.py ' + outputgdal + ' -f "ESRI Shapefile" ' + shapefile + ' ' + base_name
	logfile.write(command + "\n")
	# print command
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

	# First simplify each temporary shapefile
	currentsubset = 1
	while currentsubset <= totalsubsets:
		rinput = path + '/' + base_name + '-tmp-' + str(currentsubset) + '.shp'
		routput = path + '/' + base_name + '-tmp-' # + str(currentsubset)
		layer = base_name + '-tmp-' + str(currentsubset)
		command = 'R --vanilla --silent --slave -f simplify_map.R --args ' + rinput + ' ' + layer + ' ' + routput + ' ' + path + ' ' + str(currentsubset)
		logfile.write(command + "\n")
		# print command
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

	# new field definitions for this shapefile
	# color definition
	colorDefn = ogr.FieldDefn("Color", ogr.OFTInteger)
	colorDefn.SetWidth(2)
	colorDefn.SetPrecision(0)
	outLayer.CreateField( colorDefn )

	# dot count definition
	dotCountDefn = ogr.FieldDefn("Dot Count", ogr.OFTInteger)
	dotCountDefn.SetWidth(2)
	dotCountDefn.SetPrecision(0)
	outLayer.CreateField( dotCountDefn )

	# dot type definition
	dotTypeDefn = ogr.FieldDefn("Dot Type", ogr.OFTInteger)
	dotTypeDefn.SetWidth(1)
	dotTypeDefn.SetPrecision(0)
	outLayer.CreateField( dotTypeDefn )

	polygonfiles = []
	for files in os.listdir(path):
		if files.endswith(".shp") and files.find('-polygon') != -1:
			polygonfile = path + "/" + files
			# apply a projection so gdalwarp doesnt complain
			polygonfilename = files[:files.find(".shp")]
			os.system("cp " + dir_base_name + ".prj " + path + "/" + polygonfilename + ".prj")
			extractedfile = path + "/" + polygonfilename + "-extracted.tif"
			# extract bitmap from original
			command = "gdalwarp -q -t_srs EPSG:3785 -cutline " + polygonfile + " -crop_to_cutline -of GTiff " + inputfile + " " + extractedfile
			logfile.write(command + "\n")
			# print command
			os.system(command)
			# calculate color
			# shrink to 1x1 and find value
			pixelvalue = subprocess.Popen(["convert", "-quiet", extractedfile, "-resize", "1x1","txt:-"], stdout=subprocess.PIPE).communicate()[0]
			pattern = re.compile(r"0,0: \(([\s0-9]*),([\s0-9]*),([\s0-9]*).*")
			values = pattern.findall(pixelvalue)
			if len(values) > 0:
				red = int(values[0][0])
				green = int(values[0][1])
				blue = int(values[0][2])
				nearest = 100000
				nearestcolor = []
				nearestcolorindex = -1
				for i, color in enumerate(basecolors):
					dred = (color[0] - red) * (color[0] - red)
					dgreen = (color[1] - green) * (color[1] - green)
					dblue = (color[2] - blue) * (color[2] - blue)
					dist = dred + dgreen + dblue
					if dist < nearest:
						nearest = dist
						nearestcolor = color
						nearestcolorindex = i
				# only add if NOT paper
				if nearestcolor != basecolors[0]:
					# check for dots
					circle_data = circleDetect(extractedfile)
					# add to array
					polygonfiles.append([polygonfile, nearestcolorindex, circle_data])
				else:
					logfile.write("Ignored (paper color): " + polygonfilename + "\n")
			else:
				logfile.write("Ignored (regex match error): " + polygonfilename + "\n")

	for files in polygonfiles:
		# 3 open the input data source and get the layer
		tempfile = files[0] #dir_base_name + '-tmp-' + str(currentsubset) + '-traced.shp'
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

			DN = inFeature.GetField('DN')
			outFeature.SetField('DN', DN) #move it to the new feature

			outFeature.SetField('Color', files[1])

			outFeature.SetField('Dot Count', files[2]["count"])

			outFeature.SetField('Dot Type', files[2]["is_outline"])

			# outFeature.SetField('circle_count', files[2]["circle_count"])
			# outFeature.SetField('circle_type', files[2]["is_outline"])
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
	os.system("rm " + dir_base_name + "-tmp-*.prj")
	os.system("rm " + dir_base_name + "-tmp*.tif")
	os.system("rm " + dir_base_name + ".*")

	# close log file
	logfile.close()

def circleDetect(inputfile):
	max_dist = 20 # distance between circles to consider it an empty circle

	im=cv2.imread(inputfile)

	gray=cv2.cvtColor(im,cv.CV_RGB2GRAY)

	# blur = cv2.GaussianBlur(gray, (9,9), 2, 2)

	# canny = cv.CreateImage(cv.GetSize(im),IPL_DEPTH_8U,1)

	# rgbcanny = cv.CreateImage(cv.GetSize(im),IPL_DEPTH_8U,3)

	# cvCanny(gray, canny, 40, 240, 3)

	circles = cv2.HoughCircles(gray, cv.CV_HOUGH_GRADIENT, 1, 2, np.array([]), 200, 8, 4, 8)

	if not (isinstance(circles, np.ndarray) and circles.shape[1] > 0):
		return {"count":0, "is_outline": 0, "circles":circles}

	total_circles = circles.shape[1]

	if total_circles == 1:
		# only one circle and it is filled
		return {"count":total_circles, "is_outline": 0, "circles":circles}
	
	outline_circles = 0

	current_circle = -1
	current_x = circles[0][0][0]
	current_y = circles[0][0][1]
	# an array of circles with distance less than max_dist
	# starts with the first circle
	unique_circles = [[current_x, current_y]]
	delta_x = 0
	delta_y = 0
	for n in range(1, total_circles):
		circle = circles[0][n]
		current_x = circle[0]
		current_y = circle[1]
		# distance to all the unique circles
		last_unique = circle
		is_inside = False
		for unique in unique_circles:
			last_unique = unique
			delta_x = unique[0] - current_x
			delta_y = unique[1] - current_y
			square_dist = (delta_x*delta_x) + (delta_y*delta_y)
			if square_dist <= max_dist:
				# circle is inside another unique
				is_inside = True
				# we assume all are outlines if at least one is outline
				outline_circles = 1
				break
		if not is_inside:
			unique_circles.append([current_x, current_y])
		# cv2.circle(im,(circle[0],circle[1]),circle[2],(0,0,255), 1)

	return {"count":len(unique_circles), "is_outline": outline_circles, "circles":circles}

if __name__ == "__main__":
	main(sys.argv[1:])

