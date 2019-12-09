;; venncolorcircles.scm: Make three colored circles to illustrate additive
;; or subtractive color by combination in a venn diagram
;; Copyright (C) 2005 by Akkana Peck; 2011 by Patrick Horgan.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License.

(define (script-fu-venn-color-circles diameter center-to-center-ratio additive c1 c2 c3)
    (let* (
	   ;; calculate the sin of 60 once since we need it several times
	   (sin60 (sin (/ (* 2 *pi*) 6)))
	   ;; ccd is center to center distance. .866 (sin 60) for the
	   ;; center-to-center-ratio makes it so that the edge of each circle
	   ;; touches the line between the other two circle's centers. 1.5
	   ;; makes it so that the circles don't touch.
	   (ccd (/ (/ diameter 2) center-to-center-ratio) )
           ;; Calculate image size based on circle diameter and center to
	   ;; center distance
	   ;; height is a radius from the bottom circles + one from the
	   ;; top circle (that makes a diameter) plus the verticle distance
	   ;; between the center of the top circle and the bottom circles,
	   ;; which is the sin of 60 times the center to center distance
	   (imageh (+ diameter (* ccd sin60) ) )
	   ;; we get the width from the bottom two circles, their outer radii
	   ;; add up to a diameter, plus the center to center distance.
           (imagew (+ diameter ccd))
           ;; The layer mode we'll be using for the color circles
           (layermode (if (= additive TRUE) ADDITION-MODE SUBTRACT-MODE))
           ;; Make the new image
           (newimg (car (gimp-image-new imagew imageh RGB)))
           ;; and a background layer
           (bg (car (gimp-layer-new newimg imagew imageh
                                    RGBA-IMAGE "background"
                                    100 NORMAL-MODE)))
	   ;; name to give to the file
	   (filename (if (= additive TRUE)
		      "AdditiveColors"
		      "SubtractiveColors"))
           ;; Save the old foreground color
           (old-fg-color (car (gimp-context-get-foreground)))
	   ;; color to draw the background circles
	   (bgcolor (if (= additive TRUE) '(0 0 0) '(255 255 255)))
	   ;; convenience colors for the color circles (normally red green and blue)
	   (red c1)
	   (green c2)
	   (blue c3)
          )

      ;; from here don't remember any undo stuff
      (gimp-image-undo-disable newimg)
      ;; stick our background layer into the image
      (gimp-image-insert-layer newimg bg 0 -1)
      ;; now draw a circle in bgcolor behind each of the rgb circles
      ;; This is important to make the additions and subtractions work right
      (draw-circle-in-layer newimg bg (/ ccd 2) 0 diameter bgcolor )
      (draw-circle-in-layer newimg bg 0 (* ccd sin60)  diameter bgcolor )
      (draw-circle-in-layer newimg bg ccd (* ccd sin60) diameter bgcolor )
      (gimp-image-set-filename newimg filename)

      ;; Draw the red circle
      ;; if the diameter is d + ccd, then the centered top circle has
      ;; 1/2 ccd on each side
      (draw-circle newimg (/ ccd 2) 0 diameter red "red" layermode)

      ;; Draw the green circle
      ;; the ccd is multiplied by sin60 cause we want the vertical distance
      ;; that corresponds to the center to center distance.  The height is
      ;; that vertical distance plus the top radius and bottom radius.  That
      ;; means that the top of the bottom circles is a diameter less than that
      ;; or exactly (* ccd sin60)
      (draw-circle newimg 0 (* ccd sin60) diameter green "green" layermode)

      ;; Draw the blue circle
      ;; the y is the same as the green circle, the x comes from the width
      ;; being r+r+ccd==d+ccd so subtracting the diameter of the blue circle
      ;; from the width leaves ccd
      (draw-circle newimg ccd (* ccd sin60) diameter blue "blue" layermode)

      ;; Clean up
      (gimp-context-set-foreground old-fg-color)
      (gimp-image-undo-enable newimg)
      (gimp-display-new newimg)
     )
)

(define (draw-circle-in-layer img layer x y diameter color)
      ;; draw a circle into an existing layer of an image
      (gimp-image-select-ellipse img CHANNEL-OP-REPLACE x y diameter diameter)
      (gimp-context-set-foreground color)
      (gimp-edit-fill layer FOREGROUND-FILL)
      (gimp-selection-none img)
)

(define (draw-circle img x y diameter color layername layermode)
      ;; create a layer with specified name and mode and get a circle in it
  (let*
      ((newlayer (car (gimp-layer-new img
	     (car (gimp-image-width img))
	     (car (gimp-image-height img))
	     RGBA-IMAGE layername
	     100
	     NORMAL-MODE))
       )
      )
      (gimp-image-insert-layer img newlayer 0 -1)
      (gimp-edit-clear newlayer)
      (draw-circle-in-layer img newlayer x y diameter color)
      (gimp-selection-none img)
      (gimp-layer-set-mode newlayer layermode)
  )
)

(script-fu-register "script-fu-venn-color-circles"
     _"<Image>/File/Create/Venn Color Circles"
    "Make an image demonstrating additive or color \
    subtractive color combination with venn diagrams."
    "Akkana Peck and Patrick Horgan"
    "copyright 2005 Akkana Peck;\
    2011 Patrick Horgan"
    "August 2005 and March 2011"
    ""
    SF-ADJUSTMENT _"Diameter" '(200 50 1500 1 10 0 0)
    SF-ADJUSTMENT _"Spacing" '(.866 0.5 5 .001 .1 3 0)
    SF-TOGGLE     _"Additive" TRUE
    SF-COLOR      _"Top Color" '(255 0 0)
    SF-COLOR      _"Left Color" '(0 255 0)
    SF-COLOR      _"Right Color" '(0 0 255)
)
