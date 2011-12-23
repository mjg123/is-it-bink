(ns bink.listen
  (:use [overtone.live]))


(definst bing [cps 440]
  (let [dur 4
	sig (sin-osc cps)
	kdclck (env-gen (envelope [0 1 1 0] [0.01 (- dur 0.02) 0.01]))
	env (env-gen:kr (envelope [1 0.00001] [dur] :exponential))]
    (* sig env kdclck)))

