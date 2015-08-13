#!/usr/bin/python
import subprocess
import os
import argparse

if os.name == 'posix':
    try:
        defaultgimp = subprocess.check_output(["which", "gimp"])[:-1]
    except subprocess.CalledProcessError:
        defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'
else:
    defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'

author_information = 'NYPL Labs Map Vectorizer v0.1 by Mauricio Giraldo Arteaga @mgiraldo / @nypl_labs'

parser = argparse.ArgumentParser(description = author_information)
parser.add_argument('input', metavar = '<input file or dir>')
parser.add_argument('--gimp-path', default = defaultgimp)
parser.add_argument('--directory', default = ''),
parser.add_argument('--path', default = ''),
parser.add_argument('--base_name', default = ''),
parser.add_argument('--dir_base_name', default = ''),
parser.add_argument('--chunksize', default = 50000,
                    help = 'how to split the mega polygon file')
parser.add_argument('--currentchunk', default = 0),
parser.add_argument('--totalsubsets', default = 0),
fn = os.path.abspath(os.path.join(__file__, '..', 'vectorize_config_default.txt'))
parser.add_argument('--image-processing-configuration-file', '-p', default = fn,
                    dest = 'vectorize_config')
