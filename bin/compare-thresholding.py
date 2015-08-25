#!/usr/bin/env python3
import os, logging, string, subprocess, re

def gimp(inputfile:'file', thresholdfile:'file',
         brightness, contrast, thresholdblack, thresholdwhite,
         gimp_path = '/Users/t/Applications/GIMP.app/Contents/MacOS/GIMP'):

    contraststring = '(gimp-brightness-contrast drawable ' + str(brightness) + ' ' + str(contrast) + ')'
    thresholdstring = '(gimp-threshold drawable ' + str(thresholdblack) + ' ' + str(thresholdwhite) + ')'
    gimpcommand = '(let* ((image (car (file-tiff-load RUN-NONINTERACTIVE "' + inputfile + '" "' + inputfile + '"))) (drawable (car (gimp-image-get-layer-by-name image "Background")))) (gimp-selection-none image) ' + contraststring + ' ' + thresholdstring + ' (gimp-file-save RUN-NONINTERACTIVE image drawable "' + thresholdfile + '" "' + thresholdfile + '") (gimp-image-delete image))'

    if (not os.path.isfile(thresholdfile)):
        command = gimp_path + ' -i -b \'' + gimpcommand + '\' -b \'(gimp-quit 0)\''
        logging.debug(command)
        os.system(command)

gimp('test.tif', '/tmp/threshold.tif', -50, 95, 160, 255)
