#!/usr/bin/env python3
import subprocess
from itertools import product

from PIL import Image, ImageChops

def gimp(inputfile:'file', thresholdfile:'file',
         brightness, contrast, thresholdblack, thresholdwhite,
         gimp_path = '/Users/t/Applications/GIMP.app/Contents/MacOS/GIMP'):

    contraststring = '(gimp-brightness-contrast drawable ' + str(brightness) + ' ' + str(contrast) + ')'
    thresholdstring = '(gimp-threshold drawable ' + str(thresholdblack) + ' ' + str(thresholdwhite) + ')'
    gimpcommand = '(let* ((image (car (file-tiff-load RUN-NONINTERACTIVE "' + inputfile + '" "' + inputfile + '"))) (drawable (car (gimp-image-get-layer-by-name image "Background")))) (gimp-selection-none image) ' + contraststring + ' ' + thresholdstring + ' (gimp-file-save RUN-NONINTERACTIVE image drawable "' + thresholdfile + '" "' + thresholdfile + '") (gimp-image-delete image))'

    command = gimp_path + ' -i -b \'' + gimpcommand + '\' -b \'(gimp-quit 0)\''
    print(command)
    subprocess.call(command, shell = True, stdout = subprocess.PIPE, stderr = subprocess.PIPE)

def compare(left, right):
    diff = ImageChops.difference(a, b)
    diff = diff.convert('L')
    diff = diff.point(point_table)
    new = diff.convert('RGB')
    new.paste(b, mask=diff)
    return new

# /Users/t/Applications/GIMP.app/Contents/MacOS/GIMP -i -b '(let* ((image (car (file-tiff-load RUN-NONINTERACTIVE "test.tif" "test.tif"))) (drawable (car (gimp-image-get-layer-by-name image "Background")))) (gimp-selection-none image) (gimp-brightness-contrast drawable -127 0) (gimp-threshold drawable 0 16) (gimp-file-save RUN-NONINTERACTIVE image drawable "/tmp/gimp_-127_0_0_16.tif" "/tmp/gimp_-127_0_0_16.tif") (gimp-image-delete image))' -b '(gimp-quit 0)'

if False:
    a = Image.open('a.png')
    b = Image.open('b.png')
    c = black_or_b(a, b)
    c.save('c.png')




def main():
    brightness = range(-127, 128, 16)
    contrast = range(0, 100, 10)
    thresholdblack = range(0, 255, 16)
    thresholdwhite = range(0, 255, 16)

    for args in product(brightness, contrast, thresholdblack, thresholdwhite):
        fn = '/tmp/gimp_%d_%d_%d_%d.tif' % args
        print('Writing %s' % fn)
        gimp('test.tif', fn, *args)

if __name__ == '__main__':
    main()
