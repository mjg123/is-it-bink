(ns bink.look
  (:use incanter.core)
  (:use incanter.processing))

(defn show-good-looks []
  (let [w 600 h 800
	radius 50
	delay 25
	grav 0.1

	balls (ref [])

	new-ball (fn [x y] (ref [x y 0 0]))
	
	;; define a sketch object (i.e. PApplet)
	sktch (sketch
	       
	       ;; define the setup function
	       (setup []

		      (doto this
			(size w h)
			(stroke-weight 2)
			(framerate (/ 1000.0 delay))
			smooth))
		      
	       ;; define the draw function
	       (draw []

		     (dosync
		      (doseq [b @balls]
			(let [[x y dx dy] @b]
			  (ref-set b [(+ dx x) (+ dy y) dx (+ grav dy)]))))

		     (doto this
		       (background 0)
		       (fill 0)
		       (stroke 255 0 0))		     
		     
		     (doseq [b @balls]
		       (let [[x y _ _] @b]
			 (doto this
			   (ellipse x y 5 5)))))
	       
	       (mouseClicked [e]
			     (dosync
			      (ref-set balls (conj @balls (new-ball (mouse-x e)
								   (mouse-y e)))))))]
    
    ;; use the view function to display the sketch
    (view sktch :title "BINK" :size [w h])))