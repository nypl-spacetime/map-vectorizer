Map polygon and feature extractor
==============
#### An NYPL Labs project

**Author:** Mauricio Giraldo Arteaga [@mgiraldo] / NYPL Labs [@nypl_labs]

A open-source map vectorizer. Provided **as is** by NYPL Labs. Project based on a workflow suggested by Michael Resig.

NOTE: A proper `README` and instructions will be created. For now this is a list of requirements to get the project running.

## Like OCR for maps

This project aims to automate a manual process: geographic polygon and attribute data extraction from maps including those from [insurance atlases](http://digitalcollections.nypl.org/search/index?filters%5Btitle_uuid_s%5D%5B%5D=Maps%20of%20the%20city%20of%20New%20York%7C%7C323e4180-c603-012f-0c9f-58d385a7bc34&keywords=&layout=false#/?scroll=24) published in the 19th and early 20th centuries. [Here is some background](http://www.nypl.org/blog/2012/06/13/nyc-historical-gis-project) on why we're doing this and [here is one of the maps](http://digitalcollections.nypl.org/items/510d47e0-c7cc-a3d9-e040-e00a18064a99) we're extracting polygons from. This [example map layer](http://maps.nypl.org/warper/layers/859) shows what these atlases look like once geo-rectified, i.e. geographically normalized.

The New York Public Library has **hundreds of atlases** with **tens of thousands** of these sheets and there is no way we can extract data manually in a reasonable amount of time. Just so you get an idea, it took NYPL staff coordinating a small army of volunteers **three years to produce** 170,000 polygons with attributes (from just four of hundreds of atlases at NYPL).

It **will now take a day** to produce a comparable number of polygons with some basic metadata.

The goal is to extract the following data (✔ = mostly solved so far, ✢ = in progress):

* ✔ shape
* ✔ color
* ✢ dot presence
* ✢ dot count
* ✢ dot type (full vs outline)
* presence of skylights
* numbers if any (not optimistic about this one but maybe you know a way)

#### Example input
![Example input map](https://raw.github.com/NYPL/map-vectorizer/master/example_input.png)

#### Example output
![The resulting shapefile output superimposed](https://raw.github.com/NYPL/map-vectorizer/master/example_output.png)

#### Extra feature detection
![Extra feature detection for the polygon](https://raw.github.com/NYPL/map-vectorizer/master/feature_detection.png)

## Dependencies

A few things to be installed in your system in order to work properly. So far it has been **tested on Mac OS X Lion** so these instructions apply to that configuration only. I am sure you will be able to adapt it to your current configuration.

* [Python] with [OpenCV]
* [ImageMagick]
* [R] with the `rgdal`, `alphahull`, `igraph` and `shapefiles` libraries. Make sure R is in your PATH (so you can run it via command-line by typing `R`)
* [GIMP]
* [GDAL Tools]
* It is also a good idea to install [QGIS] to test your results

## First run

These step by step instructions should work as-is. If not, **check all the above are working** before submitting an issue.

1. Add the `gimp-scripts/` folder to the GIMP Script-Fu folders in  `Preferences > Scripts`. Make sure to run GIMP at least once if you restart your machine (not sure why it behaves this way... I am trying to make the project non-GIMP-dependent so this won't become an issue)
2. Add executable privileges to the main `vectorize_map.py` script like so: 
`chmod +x vectorize_map.py`. 
The other Python files are some test files we use and might be excluded in later commits but feel free to browse them.
3. Take note of the path where the GIMP executable is installed (another reason why I want to remove GIMP from the process).

And finally:

4. Run the script on the provided test GeoTIFF:
`./vectorize_map.py test.tif`
5. Accept the GIMP folder location or input a different one and press ENTER.

This should take about 70 seconds to process. **If it takes less there might be an error** (or your machine rulez). Take a look at the console output to find the possible culprit.

If it works, you will see a `test` folder with a `test-traced` set of files (`.shp`, `.dbf`, `.prj` and `.shx`) and two log files.

## Acknowledgements

* Michael Resig
* Chris Garrard for his [sample code to assemble and disassemble shapefiles](http://cosmicproject.org/OGR/cris_example_write.html)
* Barry Rowlingson for his [tutorial on converting alpha shapes to polygons](http://rpubs.com/geospacedman/alphasimple)

## Change log

* Added a config file (rename `vectorize_config_default.txt` to `vectorize_config.txt`).
* Added `consolidator.py` to assemble a set of shapefiles in a folder into a single file.
* Added very rough OpenCV circle and cross detection (not working very well but it is a starting point).
* Added GeoJSON output.

[@mgiraldo]: https://twitter.com/mgiraldo
[@nypl_labs]: https://twitter.com/nypl_labs
[Python]: http://www.python.org/
[OpenCV]: http://opencv.org/
[ImageMagick]: http://www.imagemagick.org/script/download.php
[R]: http://www.r-project.org/
[GIMP]: http://www.gimp.org/
[GDAL Tools]: http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries
[QGIS]: http://qgis.org/
