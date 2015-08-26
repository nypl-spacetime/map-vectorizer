#!/usr/bin/env python3
import subprocess, os
from itertools import product

from PIL import Image, ImageChops

def gimp_one(inputfile:'file', thresholdfile:'file',
             brightness, contrast, thresholdblack, thresholdwhite,
             gimp_path = '/Users/t/Applications/GIMP.app/Contents/MacOS/GIMP'):
    args = {
        'inputfile': inputfile,
        'thresholdfile': thresholdfile,
        'brightness': brightness,
        'contrast': contrast,
        'thresholdblack': thresholdblack,
        'thresholdwhite': thresholdwhite,
    }
    command = [gimp_path, '-i', '-b', gimp_func % args, '-b' '(gimp-quit 0)']
    subprocess.call(command, stdout = subprocess.PIPE, stderr = subprocess.PIPE)

def compare(left, right):
    diff = ImageChops.difference(a, b)
    diff = diff.convert('L')
    diff = diff.point(point_table)
    new = diff.convert('RGB')
    new.paste(b, mask=diff)
    return new


gimp_func = '''
(let*
  (
    (image 
      (car
        (file-tiff-load RUN-NONINTERACTIVE "%(inputfile)s" "%(inputfile)s") )) 
    (drawable 
      (car 
        (gimp-image-get-layer-by-name image "Background")))) 
  (gimp-selection-none image) 
  (gimp-brightness-contrast drawable %(brightness)d %(contrast)d)
  (gimp-threshold drawable %(thresholdblack)d %(thresholdwhite)d) 
  (gimp-file-save RUN-NONINTERACTIVE image drawable "%(thresholdfile)s" "%(thresholdfile)s")
  (gimp-image-delete image))'
'''


def compare(left, right):
    diff = ImageChops.difference(Image.open(left), Image.open(right))
    return sum(diff.histogram())

def gimp_many():
    brightness = range(-127, 128, 16)
    contrast = range(0, 100, 10)
    thresholdblack = range(0, 255, 16)
    thresholdwhite = range(0, 255, 16)

    for args in product(brightness, contrast, thresholdblack, thresholdwhite):
        fn = '/tmp/gimp_%d_%d_%d_%d.tif' % args
        if not os.path.isfile(fn):
            gimp_one('test.tif', fn, *args)

if __name__ == '__main__':
    gimp_many() # main()
