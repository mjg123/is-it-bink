(ns bink.2d.spindry
  (:use bink.2d.clj-box-2d)
  (:use bink.2d.processing)
  (:use [incanter.processing :only [stroke]])
  (:use [overtone.live]))

(def base-pch-l 55)
(def base-pch-r 82.5)

(defsynth boe [cps 110 amp 1 pan 0 dur 2]
  (let [sig (klank [[1 6.267 17.55 34.39] ; Ratios from "Designing Sound", Farnell
		    [0.5 0.25 0.125 0.06125]
		    [dur dur dur dur]]
		   (impulse:ar 0.1)
		   cps)
	env (env-gen:kr (envelope [0 1 1 0] [0.01 (- dur 0.02) 0.01]) :action FREE)]
    (out 0 (pan2 (* env sig amp) pan))))


(defn setup-world []

  (body [-7.5 0]
	(compound
	 (polygon [-6 -6] [6 -6] [6 -5] [-6 -5])
	 (polygon [-6 -6] [-5 -6] [-5 6] [-6 6])
	 (polygon [5 -6] [6 -6] [6 6] [5 6])
	 (polygon [-6 5] [6 5] [6 6] [-6 6]))
	:bodytype :kinematic
	:restitution 1
	:angle (deg->rad 45)
	:angular-velocity -0.25)
  
  (body [-5 -0.5] (circle 0.75) :restitution 0 :user-data {:pch base-pch-l})
  
  (body [-6 2] (circle 0.5) :restitution 0 :user-data {:pch (* 1.99 base-pch-l)})
  (body [-4 2] (circle 0.5) :restitution 0 :user-data {:pch (* 2.01 base-pch-l)})
  
  (body [-6 0] (circle 0.25) :restitution 0 :user-data {:pch (* 4.01 base-pch-l)})
  (body [-7 0] (circle 0.25) :restitution 0 :user-data {:pch (* 3.99 base-pch-l)})
  (body [-2.5 0] (circle 0.25) :restitution 0 :user-data {:pch (* 4.03 base-pch-l)})
  (body [-4 0] (circle 0.25) :restitution 0 :user-data {:pch (* 3.97 base-pch-l)})
  

  (body [7.5 0]
	(compound
	 (polygon [-6 -6] [6 -6] [6 -5] [-6 -5])
	 (polygon [-6 -6] [-5 -6] [-5 6] [-6 6])
	 (polygon [5 -6] [6 -6] [6 6] [5 6])
	 (polygon [-6 5] [6 5] [6 6] [-6 6]))
	:bodytype :kinematic
	:restitution 1
	:angular-velocity 0.25)

  (body [5 -0.5] (circle 0.75) :restitution 0 :user-data {:pch base-pch-r})
  
  (body [6 2] (circle 0.5) :restitution 0 :user-data {:pch (* 1.99 base-pch-r)})
  (body [4 2] (circle 0.5) :restitution 0 :user-data {:pch (* 2.01 base-pch-r)})
  
  (body [6 0] (circle 0.25) :restitution 0 :user-data {:pch (* 4.01 base-pch-r)})
  (body [7 0] (circle 0.25) :restitution 0 :user-data {:pch (* 3.99 base-pch-r)})
  (body [3 0] (circle 0.25) :restitution 0 :user-data {:pch (* 4.03 base-pch-r)})
  (body [4 0] (circle 0.25) :restitution 0 :user-data {:pch (* 3.97 base-pch-r)})


  (add-contact-listener!
   :begin-contact (fn [c]
		    (doseq [b (get-bodies c)]
		      (when-let [pch ((user-data b) :pch)]
			(let [amp (* 0.2 (contact-velocity c))
			      pan (* (. (.getPosition b) x) 0.066)]
			  (boe pch amp pan)))))))


(defn -main []
  (setup-world)
  (show-sketch :size [800 600] :scale 20 :title "spindry"))
