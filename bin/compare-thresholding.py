import os, logging, string, subprocess

def gimp(inputfile):
    dir_base_name = '/tmp/abc'
    brightness = -50
    contrast = 95
    thresholdblack = 160
    thresholdwhite = 255

    thresholdfile = dir_base_name + "-threshold-tmp.tif"
    gimp_path = '/Users/t/Applications/GIMP.app/Contents/MacOS/GIMP'

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

gimp('test.tif')
