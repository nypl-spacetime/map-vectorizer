#!/usr/bin/python
import subprocess
import os
import argparse

from . import vectorize_config_parser

if os.name == 'posix':
    try:
        defaultgimp = subprocess.check_output(["which", "gimp"])[:-1]
    except subprocess.CalledProcessError:
        defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'
else:
    defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'

author_information = 'NYPL Labs Map Vectorizer v0.2 by Mauricio Giraldo Arteaga @mgiraldo / @nypl_labs / Thomas Levine (https://thomaslevine.com)'

parser = argparse.ArgumentParser(description = author_information)
parser.add_argument('inputfile', metavar = '<input file or dir>')
if os.path.isfile(defaultgimp):
    parser.add_argument('--gimp-path', default = defaultgimp)
else:
    parser.add_argument('--gimp-path', required = True)
parser.add_argument('--chunksize', default = 50000,
                    help = 'how to split the mega polygon file')

# Load the default vectorize_config
fn = os.path.abspath(os.path.join(__file__, '..', 'vectorize_config_default.txt'))
def vectorize_config(fn):
    if os.path.isfile(fn):
        with open(fn) as fp:
            vectorize_config = vectorize_config_parser.parse(fp)
        return vectorize_config
    else:
        raise ValueError('%s is not a file.' % fn)

parser.add_argument('--image-processing-configuration-file', '-p',
                    default = vectorize_config(fn), type = vectorize_config,
                    dest = 'vectorize_config')
