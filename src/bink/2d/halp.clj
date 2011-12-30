(ns bink.2d.halp

  (:use bink.2d.clj-box-2d)
  (:use bink.2d.processing)
  
  (:use incanter.core)
  (:use incanter.processing))

;; my-app stuff

(def ball-props {:restitution 0.5 :user-data {:colour 200}})
(def wall-props {:type :static})

(defn setup-world []
  (let [w (the-world)
	part1 (create-rect w [1.5 2.75] 1.5 0.25 wall-props)
	part2 (create-rect w [4 2.75] 1.5 0.25 {:type :dynamic})
	_ (revolute-joint w part1 part2 [2.75 2.75])]
    
    (add-contact-listener
     w
     :begin-contact (fn [c] (println (map #(.getUserData %) (get-bodies c)))))
    
    (add-thing! (create-ball w 0 10 0.1 ball-props))
    (add-thing! (create-ball w 0.05 8 0.2 ball-props))
    (add-thing! (create-ball w 0.05 6 0.3 ball-props))
    (add-thing! (create-ball w 0.05 4 0.4 ball-props))
    (add-thing! (create-ball w -0.05 12 0.5 ball-props))
    (add-thing! (create-ball w -0.05 14 0.6 ball-props))
    (add-thing! (create-ball w -0.05 16 0.7 ball-props))
    (add-thing! (create-ball w -0.05 18 0.8 ball-props))
    (add-thing! (create-ball w -0.051 20 0.9 ball-props))
    (add-thing! (create-rect w [0 -1] 5.5 0.1 {:type :static}))
    (add-thing! part1)
    (add-thing! part2)))

(defn -main []
  (setup-world)
  (show-sketch (make-sketch @everything) :title "HALP"))
