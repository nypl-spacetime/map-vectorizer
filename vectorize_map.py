#!/usr/bin/python

import os, logging, string, subprocess, re, ogr, numpy as np, osr

from map_vectorizer import detect
from map_vectorizer.config import parser
from map_vectorizer.util import average_color

def list_tiffs(inputfile):
    # If input is a directory iterate through it
    if os.path.isdir(inputfile) == True:
        for ff in os.listdir(inputfile):
            if ff.endswith(".tif"):
                yield (ff, inputfile)
    else:
        # if input is a file, process it
        # but first look to see if there is a path prepending it
        if inputfile.endswith(".tif"):
            yield (inputfile[inputfile.rfind("/")+1:], inputfile[:inputfile.rfind("/")+1])


def thresholdize(inputfile, dir_base_name):
    args = parser.parse_args()
    brightness = args.vectorize_config['brightness']
    contrast = args.vectorize_config['contrast']
    thresholdblack = args.vectorize_config['thresholdblack']
    thresholdwhite = args.vectorize_config['thresholdwhite']
    thresholdfile = dir_base_name + "-threshold-tmp.tif"
    gimp_path = args.gimp_path

    print "\n\n"
    print "Thresholdizing:"
    print "---------------"
    print inputfile + " into threshold file: " + thresholdfile

    contraststring = '(gimp-brightness-contrast drawable ' + str(brightness) + ' ' + str(contrast) + ')'
    thresholdstring = '(gimp-threshold drawable ' + str(thresholdblack) + ' ' + str(thresholdwhite) + ')'
    gimpcommand = '(let* ((image (car (file-tiff-load RUN-NONINTERACTIVE "' + inputfile + '" "' + inputfile + '"))) (drawable (car (gimp-image-get-layer-by-name image "Background")))) (gimp-selection-none image) ' + contraststring + ' ' + thresholdstring + ' (gimp-file-save RUN-NONINTERACTIVE image drawable "' + thresholdfile + '" "' + thresholdfile + '") (gimp-image-delete image))'

    if (not os.path.isfile(thresholdfile)):
        command = gimp_path + ' -i -b \'' + gimpcommand + '\' -b \'(gimp-quit 0)\''
        logging.debug(command)
        # print command
        os.system(command)

    outputwsg = dir_base_name + "-wsg-tmp.tif"
    outputgdal = dir_base_name + "-gdal-tmp.tif"

    # first get geotiff data from original
    logging.debug( string.join(["gdalinfo", os.path.abspath(inputfile)]) )
    geoText = subprocess.Popen(["gdalinfo", os.path.abspath(inputfile)], stdout=subprocess.PIPE).communicate()[0]
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
    if (not os.path.isfile(outputwsg)):
        command = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84" -of GTiff -co "INTERLEAVE=PIXEL" -a_ullr ' + W + ' ' + N + ' ' + E + ' ' + S + ' ' + thresholdfile + ' ' + outputwsg
        logging.debug(command)
        # print command
        os.system(command)

    print ""
    if (not os.path.isfile(outputgdal)):
        command = 'gdalwarp -s_srs EPSG:4326 -t_srs EPSG:3785 -r bilinear ' + outputwsg + ' ' + outputgdal
        logging.debug(command)
        # print command
        os.system(command)

def polygonize(dir_base_name, base_name):
    args = parser.parse_args()

    chunksize = args.chunksize
    currentchunk = 0
    totalsubsets = 0

    outputgdal = dir_base_name + "-gdal-tmp.tif"
    # QGIS POLYGONIZE

    print ""
    print "Polygonizing (coarse):"
    print "----------------------"
    shapefile = dir_base_name + '.shp'
    if (not os.path.isfile(shapefile)):
        command = 'gdal_polygonize.py ' + outputgdal + ' -f "ESRI Shapefile" ' + shapefile + ' ' + base_name
        logging.debug(command)
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

    return totalsubsets

def simplify(path, base_name, totalsubsets):
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
        logging.debug(command)
        # print command
        os.system(command)
        currentsubset = currentsubset + 1

def consolidate(inputfile, path, dir_base_name, base_name):
    # Now combine all subsets into a macroset

    # 4 create a new data source and layer
    fn = dir_base_name + '-traced.shp'

    # 2 get the shapefile driver
    driver = ogr.GetDriverByName('ESRI Shapefile')

    # 3 open the input data source and get the layer
    shapefile = dir_base_name + '.shp'
    inDS = driver.Open(shapefile, 0) #shows cover at given points
    if inDS is None:
        print 'Could not open shapefile'
        sys.exit(1)
    inLayer = inDS.GetLayer()

    # 5 get the FieldDefn's for the id and cover fields in the input shapefile
    feature = inLayer.GetFeature(0)
    idFieldDefn = feature.GetFieldDefnRef('DN')

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

    # add lat/lon as OFTReal attributes
    outLayer.CreateField(ogr.FieldDefn("CentroidY", ogr.OFTReal))
    outLayer.CreateField(ogr.FieldDefn("CentroidX", ogr.OFTReal))

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
            logging.debug(command)
            # print command
            os.system(command)
            # calculate color
            # shrink to 1x1 and find value
            # logging.debug( string.join(["convert", "-quiet", os.path.abspath(extractedfile), "-resize", "1x1","txt:-"]) )
            # pixelvalue = subprocess.Popen(["convert", "-quiet", os.path.abspath(extractedfile), "-resize", "1x1","txt:-"], stdout=subprocess.PIPE).communicate()[0]
            # pattern = re.compile(r"0,0: \(([\s0-9]*),([\s0-9]*),([\s0-9]*).*")
            # values = pattern.findall(pixelvalue)
            extractedpath = os.path.abspath(extractedfile)
            if os.path.exists(extractedpath) == False:
                continue
            values = average_color(extractedpath)
            if len(values) > 0:
                red = int(values[0])
                green = int(values[1])
                blue = int(values[2])
                nearest = 100000
                nearestcolor = []
                nearestcolorindex = -1

                args = parser.parse_args()
                basecolors = args.vectorize_config['basecolors']

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
                    circle_data = cv_feature_detect(extractedfile)
                    # add to array
                    polygonfiles.append([polygonfile, nearestcolorindex, circle_data])
                else:
                    logging.debug("Ignored (paper color): " + polygonfilename + "\n")
            else:
                logging.debug("Ignored (regex match error): " + polygonfilename + "\n")

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

            source_srs = osr.SpatialReference()
            source_srs.ImportFromEPSG(3785) # NOTE: notice this is hardcoded

            target_srs = osr.SpatialReference()
            target_srs.ImportFromEPSG(4326) # NOTE: notice this is hardcoded

            transform = osr.CoordinateTransformation(source_srs, target_srs)

            centroid = geom.Centroid()

            centroid.Transform(transform)

            outFeature.SetField('CentroidY', centroid.GetY())
            outFeature.SetField('CentroidX', centroid.GetX())

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

def process_file(inputfile, basedir = ""):

    """NOTE: This still needs a lot of work for when dealing
       with subfolders and such.
       Best case is image file is located in same dir as vectorizer_map.py
    """

    args = parser.parse_args()

    gimp_path = args.gimp_path

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

    path = os.path.abspath(directory)#fullpath[:fullpath.find("/vectorize_map.py")] + '/' + directory

    # GIMP processing
    dir_base_name = os.path.join(directory, base_name)

    # create a log file
    # logfile = open(directory + "/py-log.txt", "w")
    logging.basicConfig(filename=os.path.join(directory, "py-log.txt"),
                        format='%(asctime)s %(message)s',level=logging.DEBUG)


    logging.debug("Log file for " + inputfile + " with colors:\n\n")
    logging.debug(str(args.vectorize_config['basecolors']) + "\n\n")

    thresholdize(inputfile, dir_base_name)

    totalsubsets = polygonize(dir_base_name, base_name)

    simplify(path, base_name, totalsubsets)

    consolidate(inputfile, path, dir_base_name, base_name)

    print ""
    print "Creating GeoJSON output..."
    print "--------------------------"
    jsonfile = dir_base_name + '-traced.json'
    shapefile = dir_base_name + '-traced.shp'
    command = 'ogr2ogr -t_srs EPSG:4326 -s_srs EPSG:3857 -f "GeoJSON" ' + jsonfile + ' ' + shapefile
    logging.debug(command)
    # print command
    os.system(command)

    # Cleaning
    print ""
    print "Cleaning..."
    print "-----------"
    os.system("rm " + dir_base_name + "-gdal-tmp.tif")
    os.system("rm " + dir_base_name + "-wsg-tmp.tif")
    os.system("rm " + dir_base_name + "-threshold-tmp.tif")
    os.system("rm " + dir_base_name + "-tmp-*.shp")
    os.system("rm " + dir_base_name + "-tmp-*.dbf")
    os.system("rm " + dir_base_name + "-tmp-*.shx")
    os.system("rm " + dir_base_name + "-tmp-*.prj")
    os.system("rm " + dir_base_name + "-tmp*.tif")
    os.system("rm " + dir_base_name + ".*")

    # close log file
    logging.shutdown()


def cv_feature_detect(inputfile):
    result = {}

    circles = detect.circles(inputfile)

    result["count"] = circles["count"]
    result["is_outline"] = circles["is_outline"]
    result["circles"] = circles["circles"]

    crosses = detect.crosses(inputfile)

    result["cross_count"] = crosses["count"]
    result["cross_data"] = crosses["data"]

    return result

def main():
    args = parser.parse_args()
    for i, (ff, inputfile) in enumerate(list_tiffs(args.inputfile)):
       process_file(ff, inputfile)
       print('Processed %d file(s)' % (i+1))

if __name__ == "__main__":
    main()
