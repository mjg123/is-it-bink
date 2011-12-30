(ns bink.2d.washing

  (:use bink.2d.clj-box-2d)
  (:use bink.2d.processing)
  
  (:use incanter.core)
  (:use incanter.processing))

;; my-app stuff

(def ball-props {:restitution 0.5 :user-data {:colour 200}})
(def wall-props {:type :static})

(defn setup-world []
  
  (add-thing! (body [0 -1]
		    (rectangle 5.5 0.1)
		    :bodytype :static))
  
  (add-thing! (body [0 5]
		    (circle 1)
		    :user-data {:color 128}))
  
  (add-thing! (body [0 6.2]
		    (polygon [0 0] [1 1] [0 2] [-1 1.2] [-0.75 0.8])))

  (add-thing! (body [0 10]
		    (rectangle 0.5 0.5)))

  
  (add-thing! (body [-3 3]
		    (compound
		     (polygon [0 0] [0.5 0] [0.5 0.1] [0 0.1])
		     (polygon [0 0] [0.1 0] [0.1 0.5] [0 0.5]))))

  (add-contact-listener!
   :begin-contact (fn [c] (println (map user-data (get-bodies c))))))

(defn -main []
  (setup-world)
  (show-sketch (make-sketch @everything) :title "HALP"))
