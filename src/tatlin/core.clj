(ns tatlin.core
  (:import [remixlab.proscene Scene]
           [saito.objloader OBJModel BoundingBox]
           [processing.core PConstants])
  (:require [clojure.java.io :as io]
            [quil.core :refer :all]
            [quil.applet :as ap]))


(defn setup 
  []
  (let [applet (ap/current-applet)
        scene (new remixlab.proscene.Scene applet)
        frame (.frame applet)]

    (when frame
      (.setResizable frame true))

    ;; when damping friction = 0 -> spin
    (.setDampingFriction (.frame (.eye scene)) 0.5)

    (let [path (io/resource "monkey3.obj")
          model (new OBJModel applet (.getPath path) "relative" PConstants/QUADS)
          bbox (new BoundingBox applet model)]
      (doto model 
        (.enableDebug)
        ;;(.scale 1)
        (.translateToCenter))
      (set-state! :model model :bbox bbox :scene scene))))

(defn draw 
  []
  (background 0)
  (lights)

  (let [model (state :model)
        bbox (state :bbox)]
    (if model
      (do 
        (push-matrix)
        ;;translate(0,0,i*bbox.getWHD().z)
        (translate 0 0 (-> bbox (.getWHD) .z))
        (.draw model)
        (pop-matrix))
      (println "no model")))

  #_(let [scene (state :scene)]
    (if (not (nil? scene))
      (.drawTorusSolenoid scene)
      (println "null scene"))))



(defsketch core
  :title "3D Sphere"
  :setup setup
  :draw draw
  :size [800 600]
  :renderer :opengl)

(defn -main [& args])
