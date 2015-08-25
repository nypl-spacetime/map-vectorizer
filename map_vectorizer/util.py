from PIL import Image

def average_color(image):
    img = Image.open(image)
    # grab width and height
    width, height = img.size
    # make a list of all pixels in the image
    pixels = img.load()
    data = []
    for x in range(width):
        for y in range(height):
            cpixel = pixels[x, y]
            data.append(cpixel)
    r = 0
    g = 0
    b = 0
    counter = 0
    # loop through all pixels
    # if alpha value is greater than 200/255, add it to the average
    # (note: could also use criteria like, if not a black pixel or not a white pixel...)
    for x in range(len(data)):
        try:
            if data[x][3] > 200:
                r+=data[x][0]
                g+=data[x][1]
                b+=data[x][2]
                counter+=1
        except:
            r+=data[x][0]
            g+=data[x][1]
            b+=data[x][2]
            counter+=1
    # compute average RGB values
    rAvg = r/counter if counter > 0 else 0
    gAvg = g/counter if counter > 0 else 0
    bAvg = b/counter if counter > 0 else 0
    return (rAvg, gAvg, bAvg)
    # return Image.open(image).resize((1,1)).getpixel((0,0))
