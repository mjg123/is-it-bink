(ns bink.2d.halp
  (:import org.jbox2d.dynamics.World)
  (:import org.jbox2d.dynamics.BodyDef)
  (:import org.jbox2d.dynamics.BodyType)
  (:import org.jbox2d.dynamics.FixtureDef)
  (:import org.jbox2d.collision.shapes.CircleShape)
  (:import org.jbox2d.common.Vec2))


;; cljBox2d stuff

(defn vec2 [x y]
  (Vec2. x y))

(def earth-gravity (vec2 0 -9.81))

;;; my app stuff

(defn create-ball [w x y]

  (let [bd (BodyDef.)
	cs (CircleShape.)
	fd (FixtureDef.)]
    
    (.set (. bd position) x y)
    (set! (. bd type) BodyType/DYNAMIC)

    (set! (. cs m_radius) 0.1)

    (set! (. fd shape) cs)
    (set! (. fd density) 0.9)
    (set! (. fd friction) 0.3)
    (set! (. fd restitution) 0.81)

    (let [body (.createBody w bd)
	  _ (.createFixture body fd)]
      
      body)))

(defn do-looping [w b]
  (let [step (/ 1 60)
	velocity-steps 8
	position-steps 3]
    (for [i (range 0 100)]
      (do (.step w step velocity-steps position-steps)
	  (println i " : " (.getPosition b))))))

;; my app now

(defn setup-world []
  (let [w (World. earth-gravity true)
	b (create-ball w 0 10)]
    (do-looping w b)))

(defn -main []
  (println "MAINLY MAIN")
  (setup-world))