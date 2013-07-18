;nypl-create-threshold.scm
; by Mauricio Giraldo Arteaga / NYPL Labs
; @mgiraldo @nypl_labs

; Version 1.0 (20130620)

; Description
;
; Creates a threshold image to use for map vectorizing
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

(define (nypl-create-threshold filename fileoutname)
   (let* ((image (car (file-tiff-load RUN-NONINTERACTIVE filename filename)))
          (drawable (car (gimp-image-get-layer-by-name image "Background"))))
     ; (gimp-context-set-sample-threshold 0.1)
     ; (gimp-context-set-feather-radius 5 5)
     ; (gimp-context-set-foreground '(0 0 0))
     ; (gimp-image-select-color image CHANNEL-OP-ADD drawable '(198 194 180))
     ; (gimp-edit-fill drawable FOREGROUND-FILL)
     (gimp-selection-none image)
     ; (plug-in-gauss RUN-NONINTERACTIVE image drawable 1 1 0)
     (gimp-brightness-contrast drawable -50 95) ;(gimp-brightness-contrast drawable 0 60)
     (gimp-threshold drawable 145 255)
     ;(plug-in-sobel RUN-NONINTERACTIVE image drawable 1 1 1)
     ;(gimp-threshold drawable 127 255)
     ;(gimp-context-set-background '(0 0 0))
     (gimp-file-save RUN-NONINTERACTIVE image drawable fileoutname fileoutname)
     (gimp-image-delete image)))

(script-fu-register "nypl-create-threshold"
        		    "<Image>/Filters/NYPL/_Create Map Threshold"
                    "Create map threshold for vectorizing."
                    "Mauricio Giraldo Arteaga / NYPL Labs"
                    "Mauricio Giraldo Arteaga / NYPL Labs"
                    "June 2013"
                    "RGB* GRAY*"
                    SF-IMAGE      "source image"      0
                    SF-IMAGE      "destination image"      0
)