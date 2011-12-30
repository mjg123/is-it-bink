(ns bink.2d.clj-box-2d
  (:import org.jbox2d.dynamics.Body)
  (:import org.jbox2d.dynamics.World)
  (:import org.jbox2d.dynamics.BodyDef)
  (:import org.jbox2d.dynamics.BodyType)
  (:import org.jbox2d.dynamics.Fixture)
  (:import org.jbox2d.dynamics.FixtureDef)
  (:import org.jbox2d.dynamics.contacts.Contact)
  (:import org.jbox2d.dynamics.joints.RevoluteJointDef)
  (:import org.jbox2d.dynamics.joints.WeldJointDef)
  (:import org.jbox2d.collision.shapes.CircleShape)
  (:import org.jbox2d.collision.shapes.PolygonShape)
  (:import org.jbox2d.common.Vec2))

(set! *warn-on-reflection* true)


;; My world fns

(defn- vec2 [x y] (Vec2. x y)) 

(def earth-gravity (vec2 0 -9.81))

(defn create-world
  ([] (create-world earth-gravity))
  ([gravity] (create-world gravity true))
  ([gravity sleep] (World. gravity sleep)))

(def everything (atom {:world (create-world) :things []}))

(defn- add-thing! [body]
  (swap! everything assoc :things (conj (@everything :things) body)))

(defn get-world ^World []
  (@everything :world))

;; utils

(def bodytypes {:dynamic BodyType/DYNAMIC
		:static BodyType/STATIC
		:kinematic BodyType/KINEMATIC})

(def defaults {:type :dynamic
	       :density 0.9
	       :friction 0.3
	       :restitution 0.81
	       :angle 0
	       :user-data nil})


(defn- prop [props k]
  (get props k (get defaults k)))



;; ctors


;; new-style

(defn circle
  "Creates a circle with given radius around the origin."
  [r]
  (let [cs (CircleShape.)]
    (set! (. cs m_radius) r)
    cs))

(defn polygon
  "Creates a convex polygon.  Takes (<= 3 n 8) points."
  [& pts]
  (let [ps (PolygonShape.)
	vec-pts (map (fn [[x y]] (vec2 x y)) pts)]

    (.set ps (into-array vec-pts) (count vec-pts))
    ps))

(defn rectangle
  "Creates a rectangle centred on the origin.  Takes halfwidth and halfheight."
  [hx hy]
  (let [ps (PolygonShape.)]
    (.setAsBox ps hx hy)
    ps))

(defn compound
  "Specifies a compound shape.  Takes a number of other shapes."
  [& shapes]
  shapes)

(defn body

  "Adds a body to the world.  Takes a vector for position, a shape (or a seq of shapes,
   which will all be part of the same body) and optional named arguments:
     :bodytype - can be :static, :dynamic or :kinematic (default :dynamic)
     :user-data - any arbitrary data to be associated with this body (default nil)"
  
  [[px py] shape & {:keys [bodytype user-data]
		    :or {bodytype :dynamic
			 user-data nil}}]

  (let [bd (BodyDef.)
	shapes (if (seq? shape) shape (list shape))]
    
    (set! (. bd position) (vec2 px py))
    (set! (. bd type) (bodytypes bodytype))
    
    (let [body (.createBody (get-world) bd)]
      (doseq [s shapes]
	(let [fd (FixtureDef.)]
	  (set! (. fd shape) s)
	  (set! (. fd density) (defaults :density))
	  (set! (. fd friction) (defaults :friction))
	  (set! (. fd restitution) (defaults :restitution))
	  (.createFixture body fd)))
      (.setUserData body user-data)
      (add-thing! body)
      body)))


(defn- unroll-fixtures
  ".getFixtureList returns a fixture with a .getNext method that might return null!!  what is this??"
  [^Fixture f l]
  (if (nil? f) l
      (recur (.getNext f) (conj l f))))

(defn fixtures
  "Takes a body and returns a seq of all the fixtures associated with it."
  [^Body b]
  (unroll-fixtures (.getFixtureList b) '()))



;; joints


(defn weld-joint [^Body body-a body-b]
  (let [joint-def (WeldJointDef.)]
    (.initialize joint-def body-a body-b (.getWorldCenter body-a))
    (.createJoint (get-world) joint-def)))

(defn revolute-joint [body-a body-b [jx jy]]
  (let [joint-def (RevoluteJointDef.)]
    (.initialize joint-def body-a body-b (vec2 jx jy))
    (.createJoint (get-world) joint-def)))

;; general

(defn step [^World world step]
  (let [velocity-iterations 8
	position-iterations 3]
    (.step world step velocity-iterations position-iterations)))

(defn transform-fn
  "Given a scale (in pixels-per-metre) and an origin position (ie where to put the world origin, in pixels)
   returns a function that turns box coords (metres) into screen coordinates (pixels)"
  [scl [ox oy]]
  (fn [[x y]]
    [(+ (* x scl) ox)
     (- oy (* y scl))]))

(defn deg->rad [deg]
  (/ (* 2 Math/PI deg) 360))


;; used for the unimplemented methods when an interface is reified
(def no-fn (constantly nil))

(defn add-contact-listener! [& {:keys [begin-contact end-contact pre-solve post-solve]
			       :or {begin-contact no-fn
				    end-contact no-fn
				    pre-solve no-fn
				    post-solve no-fn}}]
  (.setContactListener (get-world)
		       (reify org.jbox2d.callbacks.ContactListener
			      (beginContact [_ c] (begin-contact c))
			      (endContact [_ c] (end-contact c))
			      (preSolve [_ c m] (pre-solve c m))
			      (postSolve [_ c m] (post-solve c m)))))


(defn get-bodies [^Contact c]
  [(.getBody (.getFixtureA c))
   (.getBody (.getFixtureB c))])

(defn user-data [^Body b]
  (.getUserData b))