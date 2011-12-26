(ns bink.listen
  (:use [overtone.live]))


(definst bing [cps 440]
  (let [dur 4
	sig (sin-osc cps)
	kdclck (env-gen (envelope [0 1 1 0] [0.01 (- dur 0.02) 0.01]))
	env (env-gen:kr (envelope [1 0.00001] [dur] :exponential))]
    (* sig env kdclck)))


					; following inst taken from one of the examples in the source:
					; Experimenting with Karplus Strong synthesis...
(defsynth ks1 [note 440 amp 0.8 pan 0 dur 2 decay 30 coef 0.3 gate 1]
  (let [noize (* 0.8 (white-noise))
        dly (/ 1.0 note)
        plk   (pluck noize gate (/ 1.0 note) dly
                     decay
                     coef)
        dist (distort plk)
        filt (rlpf dist (* 12 note) 0.6)
        clp (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (out 0 (pan2 (* amp (env-gen (perc 0.0001 dur) :action FREE) reverb) pan))))
