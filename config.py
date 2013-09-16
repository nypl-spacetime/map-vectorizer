#!/usr/bin/python
import subprocess
import os

tempgdalfile = ''
instructions = 'vectorize_map.py <input file or dir>'
if os.name == 'posix':
    try:
        defaultgimp = subprocess.check_output(["which", "gimp"])[:-1]
    except subprocess.CalledProcessError:
        defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'
else:
    defaultgimp = '/Applications/Gimp.app/Contents/MacOS/gimp-2.8'

gimp_path = defaultgimp
chunksize = 50000 # how to split the mega polygon file
currentchunk = 0
totalsubsets = 0
brightness = -50
contrast = 95
thresholdblack = 145
thresholdwhite = 255
basecolors = [
     [206,202,185] # paper
    ,[199,179,173] # pink
    ,[179,155,157] # dark red
    ,[149,156,141] # green
    ,[199,195,163] # light yellow
    ,[195,189,154] # yellow
    ,[255,225,40]  # bright yellow
    ,[137,174,163] # greenish blue
    ,[187,194,192] # light blue
    ,[161,175,190] # "navy" blue
]

author_information = """
\n\n\n\n
NYPL Labs Map Vectorizer v0.1 
=============================
By: Mauricio Giraldo Arteaga @mgiraldo / @nypl_labs
"""
