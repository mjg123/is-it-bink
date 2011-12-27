(ns bink.2d.halp
  (:import org.jbox2d.dynamics.World)
  (:import org.jbox2d.dynamics.BodyDef)
  (:import org.jbox2d.dynamics.BodyType)
  (:import org.jbox2d.dynamics.FixtureDef)
  (:import org.jbox2d.collision.shapes.CircleShape)
  (:import org.jbox2d.collision.shapes.PolygonShape)
  (:import org.jbox2d.common.Vec2)

  (:use incanter.core)
  (:use incanter.processing))


;; cljBox2d stuff

(defn vec2 [x y]
  (Vec2. x y))

(def earth-gravity (vec2 0 -9.81))

(defn pos [o]
  [(. (.getPosition o) x)
   (. (.getPosition o) y)])

;;; my app box2d stuff

(defn create-ball [world x y]

  (let [bd (BodyDef.)
	cs (CircleShape.)
	fd (FixtureDef.)]
    
    (.set (. bd position) x y)
    (set! (. bd type) BodyType/DYNAMIC)

    (set! (. cs m_radius) 0.5)

    (set! (. fd shape) cs)
    (set! (. fd density) 0.9)
    (set! (. fd friction) 0.3)
    (set! (. fd restitution) 0.81)

    (let [body (.createBody world bd)
	  _ (.createFixture body fd)]
      
      body)))

(defn create-floor [world x y w h]
					;          PolygonShape ps = new PolygonShape();
					;        ps.setAsBox(100,1);
					;        
					;        FixtureDef fd2 = new FixtureDef();
					;        fd2.shape = ps;;;
					;
					;        BodyDef bd2 = new BodyDef();
					;        bd2.position= new Vec2(0,0);
					;
					;        world.createBody(bd2).createFixture(fd2);
  (let [ps (PolygonShape.)
	fd (FixtureDef.)
	bd (BodyDef.)]

    (.setAsBox ps w h (vec2 x y) 0.1)
    (set! (. fd shape) ps)
    (let [body (.createBody world bd)
	  _ (.createFixture body fd)]
      body)))

(defn step [w step]
  (let [velocity-iterations 8
	position-iterations 3]
    (.step w step velocity-iterations position-iterations)))


;; processing for my app

(def everything (atom {}))

(def w 600)
(def h 800)
(def rate 60)
(def scl 20)

(defn box->world [[x y]]
  [(+ (* x scl) (/ w 2)) (-  600 (* scl y))])

(def sktch
     
     (sketch

      (setup []
	     (doto this
	       (size w h)
	       (framerate rate)
	       (smooth)))
      
      (draw []
	    (when (@everything :world)

	      (step (@everything :world) (/ 1 rate))

	      (doto this
		(background 0)
		(fill 0)
		(stroke 255)

		(line 100 560 500 520))

	      (doseq [b (@everything :balls)]
		(let [[x y] (box->world (pos b))]
		  
		  (doto this
		    (stroke 255)
		    (ellipse x y (* scl 1) (* scl 1))
		    (line x y (+ x (* (/ scl 2) (Math/sin (.getAngle b)))) (+ y (* (/ scl 2) (Math/cos (.getAngle b))))))))))))


;; my-app stuff

(defn setup-world []
  (let [world (World. earth-gravity true)
	b1 (create-ball world 0 10)
	b2 (create-ball world 0.05 8)
	b3 (create-ball world 0.05 6)
	b4 (create-ball world 0.05 4)
	b5 (create-ball world -0.05 12)
	b6 (create-ball world -0.05 14)
	b7 (create-ball world -0.05 16)
	b8 (create-ball world -0.05 18)
	b9 (create-ball world -0.05 20)
	_ (create-floor world 0 3 10 0.1)]
    (reset! everything {:world world :balls [b1 b2 b3 b4 b5 b6 b7 b8 b9]})
    (view sktch :title "BINK2d" :size [w h] :exit-on-close true)))

(defn -main []
  (println "MAINLY MAIN")
  (setup-world))