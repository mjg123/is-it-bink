(ns bink.2d.processing
  (:use incanter.core)
  (:use incanter.processing)

  (:import org.jbox2d.dynamics.Fixture)
  (:import org.jbox2d.dynamics.Body)
  (:import org.jbox2d.collision.shapes.CircleShape)
  (:import org.jbox2d.collision.shapes.PolygonShape)
  (:import org.jbox2d.common.Vec2)

  (:use [bink.2d.clj-box-2d :only [transform-fn step fixtures user-data everything]]))

(set! *warn-on-reflection* true)

(def w 600)
(def h 800)
(def rate 60)

(defn prepare-sketch [sktch]
  (doto sktch
    (stroke 255)))


(def box->world identity)
(def scl 1)

;; drawing stuff

(defmulti draw-shape (fn [_ shape _ _ _] (class shape)))

(defmethod draw-shape CircleShape [sktch ^CircleShape shape [x y] a body]
	   
	   (let [r (* (. shape m_radius) scl)
		 dia (* 2 r)]

	     (prepare-sketch sktch)
	     
	     (when-let [pfn ((user-data body) :processing-fn)]
	       (pfn sktch))
	     
	     (doto sktch	       
	       (ellipse x y dia dia)
	       (line x y (+ x (* r (Math/sin a))) (+ y (* r (Math/cos a)))))))



(defmethod draw-shape PolygonShape [sktch ^PolygonShape shape _ _ ^Body body]

	   (let [vxs (.getVertices shape)
		 vxc (.getVertexCount shape)]
	     
	     (doseq [i (range 0 vxc)]
	       (let [j (mod (inc i) vxc)
		     ^Vec2 s (.getWorldPoint body (get vxs i))
		     ^Vec2 e (.getWorldPoint body (get vxs j))
		     [sx sy] (box->world [(. s x) (. s y)])
		     [ex ey] (box->world [(. e x) (. e y)])]
		 (stroke sktch 255)
		 (line sktch sx sy ex ey)))))

(defn draw-body [sktch ^Body body]
  (let [px (. (.getPosition body) x)
	py (. (.getPosition body) y)]
    
    (doseq [^Fixture f (fixtures body)]
      (draw-shape sktch (.getShape f) (box->world [px py]) (.getAngle body) body))))

;;; 

(defn make-sketch
  "Pass a map with keys of :world and :things"
  [everything]
     
     (sketch

      (setup []
	     (doto this
	       (size w h)
	       (framerate rate)
	       (smooth)))
      
      (draw []

	    (step (everything :world) (/ 1 rate))

	    (doto this
	      (background 0)
	      (fill 0))

	    (doseq [b (everything :things)]
	      (draw-body this b)))))

(defn show-sketch

  "Starts the simulation and visualisation.  Takes optional arguments:
     :title - the title of the windowed app
     :size - vector of [w h] for window size (default 800 600)
     :scale - number of pixels to represent one metre along the axis (default 5)
     :origin - pixel position to show world-location (0, 0) (default window-centre)"
  
  [& {:keys [title size scale origin]
      :or {title "Box2d + Processing"
	   size [800 600]
	   scale 5
	   origin nil}}]

  (let [[sx sy] size
	[ox oy] (if (nil? origin) [(/ sx 2) (/ sy 2)] origin)]
    
    (def w sx)
    (def h sy)
    (def scl scale)
    (def box->world (transform-fn scale [ox oy]))
    
    (view (make-sketch @everything) :title title :size [w h] :exit-on-close true)
    (str "Showing " title)))

