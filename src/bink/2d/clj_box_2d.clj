(ns bink.2d.clj-box-2d
  (:import org.jbox2d.dynamics.Body)
  (:import org.jbox2d.dynamics.World)
  (:import org.jbox2d.dynamics.BodyDef)
  (:import org.jbox2d.dynamics.BodyType)
  (:import org.jbox2d.dynamics.FixtureDef)
  (:import org.jbox2d.collision.shapes.CircleShape)
  (:import org.jbox2d.collision.shapes.PolygonShape)
  (:import org.jbox2d.common.Vec2))

(set! *warn-on-reflection* true)


;; utils

(def body-type {:dynamic BodyType/DYNAMIC
		:static BodyType/STATIC})

(def defaults {:type :dynamic
	       :density 0.9
	       :friction 0.3
	       :restitution 0.94
	       :angle 0})

(defn- vec2 [x y] (Vec2. x y)) 

(defn- prop [props k]
  (get props k (get defaults k)))


(def earth-gravity (vec2 0 -9.81))

;; ctors

(defn create-world
  ([] (create-world earth-gravity))
  ([gravity] (create-world gravity true))
  ([gravity sleep] (World. gravity sleep)))

(defn create-ball [^World world x y r & [p]]

  (let [bd (BodyDef.)
	cs (CircleShape.)
	fd (FixtureDef.)]
    
    (set! (. bd position) (vec2 x y))
    (set! (. cs m_radius) r)

    (set! (. bd type) (body-type (prop p :type)))
    (set! (. fd density) (prop p :density))
    (set! (. fd friction) (prop p :friction))
    (set! (. fd restitution) (prop p :restitution))

    (set! (. fd shape) cs)

    (doto (.createBody world bd)
      (.createFixture fd))))


(defn create-rect [^World world x y w h & [p]]
  
  (let [ps (PolygonShape.)
	fd (FixtureDef.)
	bd (BodyDef.)]

    (set! (. bd type) (body-type (prop p :type)))
    
    (.setAsBox ps w h (vec2 x y) (prop p :angle))
    (set! (. fd shape) ps)

    (doto (.createBody world bd)
      (.createFixture fd))))


;; general

(defn step [^World world step]
  (let [velocity-iterations 8
	position-iterations 3]
    (.step world step velocity-iterations position-iterations)))

(defn get-shape [^Body body]
  (.getShape (.getFixtureList body)))

(defn transform-fn
  "Given a scale (in pixels-per-metre) and an origin position (in pixels)
   returns a function that turns box coords (metres) into screen
   coordinates (pixels)"
  [scl [ox oy]]
  (fn [[x y]]
    [(+ (* x scl) ox)
     (- oy (* y scl))]))

(defn deg->rad [deg]
  (/ (* 2 Math/PI deg) 360))