import os

import numpy as np
import cv2 
import cv2 as cv

CROSS = os.path.abspath(os.path.join(__file__, '..', 'detector_templates', 'cross1.jpg'))

def _gray(im):
    return cv2.cvtColor(im,cv2.COLOR_RGB2GRAY)

def crosses(inputfile):
    im=cv2.imread(inputfile)
    gray = _gray(im)

    # NOW DETECT CROSSES
    # code based on http://nbviewer.ipython.org/5861365

    score_threshold = 0.954 # certainty there IS a cross

    cross1 = cv2.imread(CROSS)

    cross_count = 0
    cross_data = {}

    if cross1.shape[0] < im.shape[0] and cross1.shape[1] < im.shape[1]:
        graycross1 = cv2.cvtColor(cross1,cv2.COLOR_RGB2GRAY)
        match1 = cv2.matchTemplate(gray, graycross1, cv2.TM_CCORR_NORMED)
        min_score, max_score, (min_x, min_y), (max_x, max_y) = cv2.minMaxLoc(match1)

        if (max_score >= score_threshold):
            # only testing 1 cross for now
            cross_count = 1
            corner_topL = (max_x, max_y)
            corner_botR = (corner_topL[0]+cross1.shape[1], corner_topL[1]+cross1.shape[0])
            cross_data = {"top_left":corner_topL, "bottom_right":corner_botR, "score": max_score}

    return {"count":cross_count, "data": cross_data}

def circles(inputfile):
    im=cv2.imread(inputfile)
    gray = _gray(im)

    max_dist = 20 # distance between circles to consider it an empty circle

    circles = cv2.HoughCircles(gray, cv2.HOUGH_GRADIENT, 1, 2, np.array([]), 200, 8, 4, 8)

    total_circles = 0

    outline_circles = 1

    unique_circles = []

    if not (isinstance(circles, np.ndarray) and circles.shape[1] > 0):
        return {"count":0, "is_outline": 0, "circles":circles}
    else:
        total_circles = circles.shape[1]

    if total_circles == 1:
        # only one circle and it is filled
        return {"count":total_circles, "is_outline": 0, "circles":circles}
    else :
        # this is wrong... use for now
        outline_circles = 0

    if total_circles > 0:
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
            for unique in unique_circles:
                last_unique = unique
                delta_x = unique[0] - current_x
                delta_y = unique[1] - current_y
                square_dist = (delta_x*delta_x) + (delta_y*delta_y)
                if square_dist <= max_dist:
                    # circle is inside another unique
                    is_inside = True
                    # we assume all are outlines if at least one is outline
                    outline_circles = 1
                    break
            if not is_inside:
                unique_circles.append([current_x, current_y])
            # cv2.circle(im,(circle[0],circle[1]),circle[2],(0,0,255), 1)

    return {"count":len(unique_circles), "is_outline": outline_circles, "circles":circles}

