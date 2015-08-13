import csv

def parse(config_file):
    '''
    :param str config_file: Filename
    :rtype dict:
    :returns: 
    '''
    tempcolors = []
    index = 0
    with open(config_file, 'r') as configcsv:
        configdata = csv.reader(configcsv, delimiter=',')
        for row in configdata:
            if index > 0:
                tempcolors.append([int(row[0]), int(row[1]), int(row[2])])
            else:
                # brightness/contrast/threshold values
                brightness = int(row[0])
                contrast = int(row[1])
                thresholdblack = int(row[2])
                thresholdwhite = int(row[3])
            index = index + 1
        if len(tempcolors) > 2:
            basecolors = tempcolors

    return {
        'basecolors': basecolors,
        'brightness': brightness,
        'contrast': contrast,
        'thresholdblack': thresholdblack,
        'thresholdwhite': thresholdwhite,
    }
