(ns bink.look
  (:use incanter.core)
  (:use incanter.processing)
  (:require [bink.listen :as l]))

(def w 600)
(def h 800)
(def resti 0.94) ; coefficient of restitution (1.00 = elastic, 0.94 = pingpong ball, 0.81 = baseball)

(defn- new-balls [x y]
  [
   (ref [x y 0 0])
;   (ref [x y 0 -1]) ;; uncomment for starburst effect
;   (ref [x y 0 -2])
;   (ref [x y -1 -1])
;   (ref [x y 1 -1])
   ])

(defn- create-line []
  (let [x1 (rand-int w)
	x2 (rand-int w)
	y1 (rand-int h)
	y2 (rand-int h)]
    [(min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)]))

(defn xp
  "2d cross-product"
  [ux uy vx vy]
  (- (* ux vy) (* uy vx)))

(defn dp
  "2d dot-product"
  [[ux uy] [vx vy]]
  (+ (* ux vx) (* uy vy)))

(defn- intersect? [[x y dx dy] [x1 y1 x2 y2]]
  (and
   					; cross-products with different sign indicate line has been crossed
					; but not necessarily within the segment drawn
   (neg? (* (xp (- x x1) (- y y1) (- x2 x1) (- y2 y1))
	    (xp (- (+ x dx) x1) (- (+ y dy) y1) (- x2 x1) (- y2 y1))))
   
					; so check that the ball is within the bounds of the line, too
   (and (<= x1 x x2) (<= y1 y y2))))

(defn- len [[x1 y1 x2 y2]]
  (Math/sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))

(defn- speed [[_ _ dx dy]]
  (len [0 0 dx dy]))

(defn- panpos [x]
  (- (* 2 (/ x w)) 1))

(defn- bounce-single [b line]
  (if (intersect? b line)
    (do
      (l/ks1 (/ 100000 (len line)) (min 1 (* 0.1 (speed b))) (panpos (first b)))
      
					; eqn from http://en.wikipedia.org/wiki/Reflection_%28mathematics%29
      (let [[x y dx dy] b
	    [x1 y1 x2 y2] line
	    l [(- x2 x1) (- y2 y1)]
	    mul (* 2 (/ (dp [dx dy] l) (dp l l)))]
	
	[(* resti (- (* mul (first l)) dx dx)) (* resti (- (* mul (second l)) dy dy))]))
    [0 0]))

(defn- bounce-off [b lines]
  (reduce
   (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
   (map #(bounce-single b %) lines)))

(defn show-good-looks []
  (let [radius 50
	delay 25
	grav 0.1

	balls (ref [])
	lines (take 10 (repeatedly create-line))

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
		      
					; remove balls that aren't onscreen any more
		      (ref-set balls (filter #(let [[x y _ _] @%] (and (< 0 x w) (< y h))) @balls))

		      
					; update all positions
		      (doseq [b @balls]
			(let [[x y dx dy] @b
			      [ddx ddy] (bounce-off [x y dx dy] lines)
			      dx (+ dx ddx)
			      dy (+ dy ddy)]
			  
			  (ref-set b [(+ dx x) (+ dy y) dx (+ dy grav)]))))

		     
					; clear background
		     (doto this
		       (background 0)
		       (fill 0))

					; draw lines
		     (doseq [[x1 y1 x2 y2] lines]
		       (doto this
			 (stroke 255)
			 (line x1 y1 x2 y2)))
		     
					; draw balls
		     (doseq [b @balls]
		       (let [[x y _ _] @b]
			 (doto this
			   (stroke 255 0 0)
			   (ellipse x y 5 5)))))
	       
	       (mousePressed [e]
			     (dosync
			      (ref-set balls (concat @balls (new-balls (mouse-x e)
								       (mouse-y e)))))))]
    
    ;; use the view function to display the sketch
    (view sktch :title "BINK" :size [w h] :exit-on-close true)))