Teh NYPL Labs Map Vectorizor
==============

A open-source map vectorizer. Provided **as-is** by NYPL Labs.

Author: Mauricio Giraldo Arteaga [@mgiraldo] / NYPL Labs [@nypl_labs]

NOTE: A proper `README` and instructions will be created. For now this is a list of requirements to get the project running.

## Dependencies

*Teh Vectorizor* needs a few things to be installed in your system in order to work properly. So far it has been **tested on Mac OS X Lion** so these instructions apply to that configuration only. I am sure you will be able to adapt it to your current configuration.

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
The other Python files are some test files I use and might be excluded in later commits but feel free to browse them.
3. Take note of the path where the GIMP executable is installed (another reason why I want to remove GIMP from the process).

And finally:

4. Run the script on the provided test GeoTIFF:
`./vectorize_map.py test.tif`
5. Accept the GIMP folder location or input a different one and press ENTER.

This should take about 40 seconds to process. **If it takes less there might be an error** (or your machine rulez). Take a look at the console output to find the possible culprit.

If it works, you will see a `test` folder with a `test-traced` set of files (`.shp`, `.dbf`, `.prj` and `.shx`) and two log files.

[@mgiraldo]: https://twitter.com/mgiraldo
[@nypl_labs]: https://twitter.com/nypl_labs
[Python]: http://www.python.org/
[OpenCV]: http://opencv.org/
[ImageMagick]: http://www.imagemagick.org/script/download.php
[R]: http://www.r-project.org/
[GIMP]: http://www.gimp.org/
[GDAL Tools]: http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries