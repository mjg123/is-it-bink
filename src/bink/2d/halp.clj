(ns bink.2d.halp
  (:import org.jbox2d.dynamics.Body)
  (:import org.jbox2d.collision.shapes.CircleShape)
  (:import org.jbox2d.collision.shapes.PolygonShape)
  (:import org.jbox2d.common.Vec2)

  (:use bink.2d.clj-box-2d)
  
  (:use incanter.core)
  (:use incanter.processing))

(set! *warn-on-reflection* true)

;; processing for my app

(def everything (atom {}))

(def w 600)
(def h 800)
(def rate 60)
(def scl 20)

(def box->world
  (transform-fn scl [(/ w 2) (- h 100)]))

(defmulti draw-body (fn [sktch body] (class (get-shape body))))

(defmethod draw-body CircleShape [sktch ^Body body]
	   (let [px (. (.getPosition body) x)
		 py (. (.getPosition body) y)
		 [x y] (box->world [px py])
		 a (.getAngle body)
		 ^CircleShape shape (get-shape body)
		 r (* (. shape m_radius) scl)
		 dia (* 2 r)]
	     (doto sktch
	       (stroke (:colour (.getUserData body)))
	       (ellipse x y dia dia)
	       (line x y (+ x (* r (Math/sin a))) (+ y (* r (Math/cos a)))))))

(defmethod draw-body PolygonShape [sktch ^Body body]
	   (let [^PolygonShape shape (get-shape body)
		 vxs (.getVertices shape)
		 vxc (.getVertexCount shape)]
	     (doseq [i (range 0 vxc)]
	       (let [j (mod (inc i) vxc)
		     ^Vec2 s (.getWorldPoint body (get vxs i))
		     ^Vec2 e (.getWorldPoint body (get vxs j))
		     [sx sy] (box->world [(. s x) (. s y)])
		     [ex ey] (box->world [(. e x) (. e y)])]
		 (stroke sktch 255)
		 (line sktch sx sy ex ey)))))

(def sktch
     
     (sketch

      (setup []
	     (doto this
	       (size w h)
	       (framerate rate)
	       (comment (smooth))))
      
      (draw []
	    (when (@everything :world)

	      (step (@everything :world) (/ 1 rate))

	      (doto this
		(background 0)
		(fill 0))

	      (doseq [b (@everything :things)]
		(draw-body this b))))))


;; my-app stuff

(def ball-props {:restitution 0.5 :user-data {:colour 128}})
(def wall-props {:type :static})

(defn no-fn [& n]
  nil)

(defn create-listener [fns]
  (reify org.jbox2d.callbacks.ContactListener
	 (beginContact [_ c] ((fns :begin-contact no-fn) c))
	 (endContact [_ c] ((fns :end-contact no-fn) c))
	 (preSolve [_ c m] ((fns :pre-solve no-fn) c m))
	 (postSolve [_ c m] ((fns :post-solve no-fn) c m))))

(defn setup-world []
  (let [w (create-world)]
    (.setContactListener w (create-listener {:begin-contact println}))
    (reset! everything
	    {:world w
	     :things [(create-ball w 0 10 0.1 ball-props)
		      (create-ball w 0.05 8 0.2 ball-props)
		      (create-ball w 0.05 6 0.3 ball-props)
		      (create-ball w 0.05 4 0.4 ball-props)
		      (create-ball w -0.05 12 0.5 ball-props)
		      (create-ball w -0.05 14 0.6 ball-props)
		      (create-ball w -0.05 16 0.7 ball-props)
		      (create-ball w -0.05 18 0.8 ball-props)
		      (create-ball w -0.05 20 0.9 ball-props)
		      (create-rect w -1.5 12 5 0.1 (assoc wall-props :angle (deg->rad -45)))
		      (create-rect w 0 -1 5.5 0.1 {:type :static})]})))

(defn -main []
  (setup-world)
  (view sktch :title "BINK2d" :size [w h] :exit-on-close true))