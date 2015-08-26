#!/usr/bin/env python3
import os, logging, string, subprocess, re
from itertools import product

def gimp(inputfile:'file', thresholdfile:'file',
         brightness, contrast, thresholdblack, thresholdwhite,
         gimp_path = '/Users/t/Applications/GIMP.app/Contents/MacOS/GIMP'):

    contraststring = '(gimp-brightness-contrast drawable ' + str(brightness) + ' ' + str(contrast) + ')'
    thresholdstring = '(gimp-threshold drawable ' + str(thresholdblack) + ' ' + str(thresholdwhite) + ')'
    gimpcommand = '(let* ((image (car (file-tiff-load RUN-NONINTERACTIVE "' + inputfile + '" "' + inputfile + '"))) (drawable (car (gimp-image-get-layer-by-name image "Background")))) (gimp-selection-none image) ' + contraststring + ' ' + thresholdstring + ' (gimp-file-save RUN-NONINTERACTIVE image drawable "' + thresholdfile + '" "' + thresholdfile + '") (gimp-image-delete image))'

    command = gimp_path + ' -i -b \'' + gimpcommand + '\' -b \'(gimp-quit 0)\''
    os.system(command)

brightness = range(-127, 128, 16)
contrast = range(0, 100, 10)
thresholdblack = range(0, 255, 16)
thresholdwhite = range(0, 255, 16)

for args in product(brightness, contrast, thresholdblack, thresholdwhite):
    gimp('test.tif', '/tmp/gimp_%d_%d_%d_%d.tif' % args, *args)
