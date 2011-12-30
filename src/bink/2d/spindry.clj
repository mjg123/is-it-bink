(ns bink.2d.spindry
  (:use bink.2d.clj-box-2d)
  (:use bink.2d.processing)
  (:use [incanter.processing :only [stroke]]))

(defn setup-world []

  (comment "Y U NO WORK?!"
    (body [0 10]
	  (compound
	   (rectangle 5 0.5 :center [0 -2.5])
	   (rectangle 5 0.5 :center [0 2.5])
	   (rectangle 0.5 0 :center [-2.5 0])
	   (rectangle 0.5 0 :center [2.5 0]))))

  (body [0 0]
	(compound
	 (polygon [-6 -6] [6 -6] [6 -5] [-6 -5])
	 (polygon [-6 -6] [-5 -6] [-5 6] [-6 6])
	 (polygon [5 -6] [6 -6] [6 6] [5 6])
	 (polygon [-6 5] [6 5] [6 6] [-6 6]))
	:bodytype :kinematic
	:restitution 0.5
	:name "boxy")

  (.setAngularVelocity (named-body "boxy") -0.25)

  (body [0 -0.5]
	(circle 0.75)
	:restitution 0)

  (body [-1 2] (circle 0.5) :restitution 0)
  (body [1 2] (circle 0.5) :restitution 0)

  (body [-1 0] (circle 0.25) :restitution 0)
  (body [-2 0] (circle 0.25) :restitution 0)
  (body [2.5 0] (circle 0.25) :restitution 0)
  (body [1 0] (circle 0.25) :restitution 0)
  
  (comment
    (add-contact-listener!
     :begin-contact (fn [c] (println (map user-data (get-bodies c)))))))


(defn -main []
  (setup-world)
  (show-sketch
   :size [600 800]
   :scale 20
   :title "spindry"))
