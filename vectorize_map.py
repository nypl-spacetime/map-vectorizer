#!/usr/bin/python
import re 
import sys
import getopt
import subprocess
import shlex
import os 
import datetime
import ogr
import glob
import csv
import cv2
import numpy as np
from cv2 import cv
from config import *

def setup_gimp():
    global gimp_path
    global basecolors
    global brightness
    global contrast
    global thresholdblack
    global thresholdwhite
    global starttime

    gimp_path = raw_input("GIMP executable path [" + defaultgimp +  "]: ")
    starttime = datetime.datetime.now()
    config_file = "vectorize_config.txt"

    if gimp_path == '':
        gimp_path = defaultgimp

    if os.path.isfile(config_file):
        tempcolors = []
        index = 0
        with open(config_file, 'r') as configcsv:
            configdata = csv.reader(configcsv, delimiter=',')
            for row in configdata:
                if index > 0:
                    tempcolors.append([int(row[0]), int(row[1]), int(row[2])])
                else:
                    # brightness/contrast/threshold values
                    brightness = int(row[0])
                    contrast = int(row[1])
                    thresholdblack = int(row[2])
                    thresholdwhite = int(row[3])
                index = index + 1
            if len(tempcolors) > 2:
                basecolors = tempcolors

def process_files(inputfile):
    totalfiles = 0
    # If input is a directory iterate through it
    if os.path.isdir(inputfile) == True:
        for ff in os.listdir(inputfile):
            if ff.endswith(".tif"):
                totalfiles = totalfiles + 1
                processfile(ff, inputfile)
    else:
        # if input is a file, process it
        processfile(inputfile)
        totalfiles = 1

    endtime = datetime.datetime.now()
    deltatime = endtime - starttime
    print "Processed  " + str(totalfiles) + " files\n"
    print "Operation took " + str(deltatime.seconds) + " seconds"

def processfile(inputfile, basedir = ""):

    """NOTE: This still needs a lot of work for when dealing 
       with subfolders and such. 
       Best case is image file is located in same dir as vectorizer_map.py
    """

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

    logfile.write("Log file for " + inputfile + " with colors:\n\n")
    logfile.write(str(basecolors) + "\n\n")

    thresholdfile = dir_base_name + "-threshold-tmp.tif"
    comparativefile = dir_base_name + "-comparative-tmp.tif"

    print "\n\n"
    print "Thresholdizing:"
    print "---------------"
    print inputfile + " into threshold file: " + thresholdfile

    contraststring = '(gimp-brightness-contrast drawable ' + str(brightness) + ' ' + str(contrast) + ')'
    thresholdstring = '(gimp-threshold drawable ' + str(thresholdblack) + ' ' + str(thresholdwhite) + ')'
    gimpcommand = '(let* ((image (car (file-tiff-load RUN-NONINTERACTIVE "' + inputfile + '" "' + inputfile + '"))) (drawable (car (gimp-image-get-layer-by-name image "Background")))) (gimp-selection-none image) ' + contraststring + ' ' + thresholdstring + ' (gimp-file-save RUN-NONINTERACTIVE image drawable "' + thresholdfile + '" "' + thresholdfile + '") (gimp-image-delete image))'

    if (not os.path.isfile(thresholdfile)):
        command = gimp_path + ' -i -b \'' + gimpcommand + '\' -b \'(gimp-quit 0)\''
        logfile.write(command + "\n")
        # print command
        os.system(command)

    tempgdalfile = dir_base_name + "-tmp.tif"


    # GDAL transformation

    print "\n"
    print 'Origin GeoTIFF :', inputfile
    print 'Destination      :', tempgdalfile

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
    if (not os.path.isfile(outputwsg)):
        command = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84" -of GTiff -co "INTERLEAVE=PIXEL" -a_ullr ' + W + ' ' + N + ' ' + E + ' ' + S + ' ' + thresholdfile + ' ' + outputwsg
        logfile.write(command + "\n")
        # print command
        os.system(command)

    print ""
    outputgdal = dir_base_name + "-gdal-tmp.tif"
    if (not os.path.isfile(outputgdal)):
        command = 'gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3785 -r bilinear ' + outputwsg + ' ' + outputgdal
        logfile.write(command + "\n")
        # print command
        os.system(command)

    # QGIS POLYGONIZE

    print ""
    print "Polygonizing (coarse):"
    print "----------------------"
    shapefile = dir_base_name + '.shp'
    if (not os.path.isfile(shapefile)):
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

    ##### @todo: move all R code to separate fns

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
    dotCountDefn = ogr.FieldDefn("DotCount", ogr.OFTInteger)
    dotCountDefn.SetWidth(2)
    dotCountDefn.SetPrecision(0)
    outLayer.CreateField( dotCountDefn )

    # dot type definition
    dotTypeDefn = ogr.FieldDefn("DotType", ogr.OFTInteger)
    dotTypeDefn.SetWidth(1)
    dotTypeDefn.SetPrecision(0)
    outLayer.CreateField( dotTypeDefn )

    # cross count definition
    crossCountDefn = ogr.FieldDefn("CrossCount", ogr.OFTInteger)
    crossCountDefn.SetWidth(2)
    crossCountDefn.SetPrecision(0)
    outLayer.CreateField( crossCountDefn )

    # cross data definition
    crossDataDefn = ogr.FieldDefn("CrossData", ogr.OFTString)
    crossDataDefn.SetWidth(255)
    outLayer.CreateField( crossDataDefn )

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
                    circle_data = cvFeatureDetect(extractedfile)
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
            outFeature.SetField('DN', DN ) #move it to the new feature

            outFeature.SetField('Color', int(files[1]) )

            outFeature.SetField('DotCount', int(files[2]["count"]) )

            outFeature.SetField('DotType', int(files[2]["is_outline"]) )

            outFeature.SetField('CrossCount', int(files[2]["cross_count"]) )

            outFeature.SetField('CrossData', str(files[2]["cross_data"]) )

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

    print ""
    print "Creating GeoJSON output..."
    print "--------------------------"
    jsonfile = dir_base_name + '-traced.json'
    command = 'ogr2ogr -t_srs EPSG:4326 -s_srs EPSG:3857 -f "GeoJSON" ' + jsonfile + ' ' + fn
    logfile.write(command + "\n")
    # print command
    os.system(command)

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

def cvFeatureDetect(inputfile):
    max_dist = 20 # distance between circles to consider it an empty circle

    retval = {}

    im=cv2.imread(inputfile)

    gray=cv2.cvtColor(im,cv.CV_RGB2GRAY)

    circles = cv2.HoughCircles(gray, cv.CV_HOUGH_GRADIENT, 1, 2, np.array([]), 200, 8, 4, 8)

    total_circles = 0

    outline_circles = 1

    unique_circles = []

    if not (isinstance(circles, np.ndarray) and circles.shape[1] > 0):
        retval = {"count":0, "is_outline": 0, "circles":circles}
    else:
        total_circles = circles.shape[1]

    if total_circles == 1:
        # only one circle and it is filled
        retval = {"count":total_circles, "is_outline": 0, "circles":circles}
    else :
        # this is wrong... use for now
        outline_circles = 0

    if total_circles > 0:
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

    retval = {"count":len(unique_circles), "is_outline": outline_circles, "circles":circles}

    # NOW DETECT CROSSES
    # code based on http://nbviewer.ipython.org/5861365

    score_threshold = 0.954 # certainty there IS a cross

    cross1 = cv2.imread("cross1.jpg")

    cross_count = 0
    cross_data = {}

    if cross1.shape[0] < im.shape[0] and cross1.shape[1] < im.shape[1]:
        graycross1 = cv2.cvtColor(cross1,cv.CV_RGB2GRAY)
        match1 = cv2.matchTemplate(gray, graycross1, cv2.TM_CCORR_NORMED)
        min_score, max_score, (min_x, min_y), (max_x, max_y) = cv2.minMaxLoc(match1)

        if (max_score >= score_threshold):
            # only testing 1 cross for now
            cross_count = 1
            corner_topL = (max_x, max_y)
            corner_botR = (corner_topL[0]+cross1.shape[1], corner_topL[1]+cross1.shape[0])
            cross_data = {"top_left":corner_topL, "bottom_right":corner_botR, "score": max_score}

    retval["cross_count"] = cross_count
    retval["cross_data"] =cross_data

    return retval

def main(argv):
    global instructions
    global defaultgimp
    global gimp_path
    global basecolors
    global brightness
    global contrast
    global thresholdblack
    global thresholdwhite
    global starttime

    # Process CLI args
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

    print author_information

    setup_gimp()
    process_files(inputfile)

if __name__ == "__main__":
    main(sys.argv[1:])
