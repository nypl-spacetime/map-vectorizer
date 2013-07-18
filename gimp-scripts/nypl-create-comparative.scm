;nypl-create-comparative.scm
; by Mauricio Giraldo Arteaga / NYPL Labs
; @mgiraldo @nypl_labs

; Version 1.0 (20130620)

; Description
;
; Creates a comparative image to use for map vectorizing
;

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (nypl-create-comparative filename fileoutname)
   (let* ((image (car (file-tiff-load RUN-NONINTERACTIVE filename filename)))
          (drawable (car (gimp-image-get-layer-by-name image "Background"))))
     (gimp-selection-none image)
     ; (plug-in-gauss RUN-NONINTERACTIVE image drawable 1 1 0)
     (gimp-brightness-contrast drawable -50 95)
     (gimp-file-save RUN-NONINTERACTIVE image drawable fileoutname fileoutname)
     (gimp-image-delete image)))

(script-fu-register "nypl-create-comparative"
        		    "<Image>/Filters/NYPL/_Create Map Comparative"
                    "Create map comparative for analyzing polygons."
                    "Mauricio Giraldo Arteaga / NYPL Labs"
                    "Mauricio Giraldo Arteaga / NYPL Labs"
                    "June 2013"
                    "RGB* GRAY*"
                    SF-IMAGE      "source image"      0
                    SF-IMAGE      "destination image"      0
)