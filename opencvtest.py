#!/usr/bin/python

import cv2, sys
from cv2 import cv
import os
import numpy as np

def showme(pic):
    cv2.imshow('window',pic)
    cv2.waitKey()
    cv2.destroyAllWindows()

def main(argv):
    inputfile = 'test/test-tmp-1-34227-polygon-extracted.tif'

    if len(argv) == 1:
        inputfile = argv[0]

    im=cv2.imread(inputfile)

    gray=cv2.cvtColor(im,cv.CV_RGB2GRAY)

    # blur = cv2.GaussianBlur(gray, (9,9), 2, 2)

    # canny = cv.CreateImage(cv.GetSize(im),IPL_DEPTH_8U,1)

    # rgbcanny = cv.CreateImage(cv.GetSize(im),IPL_DEPTH_8U,3)

    # cvCanny(gray, canny, 40, 240, 3)

    circles = cv2.HoughCircles(gray, cv.CV_HOUGH_GRADIENT, 1, 2, np.array([]), 200, 8, 4, 8)

    try:
        n = np.shape(circles)
        circles=np.reshape(circles,(n[1],n[2]))
        print circles
        for circle in circles:
            cv2.circle(im,(circle[0],circle[1]),circle[2],(0,0,255), 1)
        showme(im)
    except:
        print "no circles found"

if __name__ == "__main__":
    main(sys.argv[1:])
