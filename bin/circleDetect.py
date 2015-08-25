#!/usr/bin/python

import cv2
import sys
import os
from cv2 import cv
import numpy as np

def showme(pic):
    cv2.imshow('window',pic)
    cv2.waitKey()
    cv2.destroyAllWindows()

def main(argv):
    inputfile = 'test/test-tmp-1-34227-polygon-extracted.tif'

    if len(argv) == 1:
        inputfile = argv[0]

    circleDetect(inputfile)

def circleDetect(inputfile):
    max_dist = 20 # distance between circles to consider it an empty circle

    im = cv2.imread(inputfile)

    gray = cv2.cvtColor(im,cv.CV_RGB2GRAY)

    # blur = cv2.GaussianBlur(gray, (9,9), 2, 2)

    # canny = cv.CreateImage(cv.GetSize(im),IPL_DEPTH_8U,1)

    # rgbcanny = cv.CreateImage(cv.GetSize(im),IPL_DEPTH_8U,3)

    # cvCanny(gray, canny, 40, 240, 3)

    circles = cv2.HoughCircles(gray, cv.CV_HOUGH_GRADIENT, 1, 2, np.array([]), 200, 8, 4, 8)

    if not (isinstance(circles, np.ndarray) and circles.shape[1] > 0):
        print "no circles found"
        return {"count":0, "is_outline": False, "circles":circles}

    total_circles = circles.shape[1]

    print "circles: " + str(total_circles)
    print circles

    if total_circles == 1:
        # only one circle and it is filled
        print "one full circle"
        return {"count":total_circles, "is_outline": False, "circles":circles}

    outline_circles = False

    current_circle = -1
    current_x = circles[0][0][0]
    current_y = circles[0][0][1]
    # an array of circles with distance less than max_dist
    # starts with the first circle
    unique_circles = [[current_x, current_y]]
    delta_x = 0
    delta_y = 0
    for n in range(1, total_circles):
        circle = circles[0][n]
        current_x = circle[0]
        current_y = circle[1]
        # distance to all the unique circles
        last_unique = circle
        is_inside = False
        print "x: " + str(current_x) + " y:" + str(current_y)
        for unique in unique_circles:
            last_unique = unique
            delta_x = unique[0] - current_x
            delta_y = unique[1] - current_y
            square_dist = (delta_x*delta_x) + (delta_y*delta_y)
            print "square_dist:" + str(square_dist)
            if square_dist <= max_dist:
                # circle is inside another unique
                is_inside = True
                # we assume all are outlines if at least one is outline
                outline_circles = True
                break
        if not is_inside:
            print "not inside"
            unique_circles.append([current_x, current_y])
        # cv2.circle(im,(circle[0],circle[1]),circle[2],(0,0,255), 1)

    print unique_circles
    print "total uniques:" + str(len(unique_circles))
    return {"count":len(unique_circles), "is_outline": outline_circles, "circles":circles}

    # try:
    #       n = np.shape(circles)
    #       circles=np.reshape(circles,(n[1],n[2]))
    #       print circles
    #       for circle in circles:
    #               cv2.circle(im,(circle[0],circle[1]),circle[2],(0,0,255), 1)
    #       showme(im)
    # except:
    #       print "no circles found"

if __name__ == "__main__":
    main(sys.argv[1:])
