(ns tatlin.core
  (:import [remixlab.proscene Scene])
  (:require [quil.core :refer :all]
            [quil.applet :as ap]))

;; (defn setup []
;;   (smooth)
;;   ;;(sphere-detail 100)
;;   (translate (/ (width) 2) (/ (height) 2) 0)
;;   (sphere 100)
;;   )


(defn setup 
  []
  (let [scene (new remixlab.proscene.Scene (current-applet))
        frame (.frame (current-applet))]
    (set-state! :scene scene)
    (when frame
      (.setResizable frame true))
    ;; when damping friction = 0 -> spin
    (.setDampingFriction (.frame (.eye scene)) 0.5)))

(defn draw 
  []
  (background 0)
  (fill 204, 102, 0, 150)
  (let [scene (state :scene)]
    (if (not (nil? scene))
      (.drawTorusSolenoid scene)
      (println "null scene"))))

;; void keyPressed() {
;;   if(scene.eye().frame().dampingFriction() == 0)
;;     scene.eye().frame().setDampingFriction(0.5);
;;   else
;;     scene.eye().frame().setDampingFriction(0);
;;   println("Camera damping friction now is " + scene.eye().frame().dampingFriction());
;; }



(defsketch core
  :title "3D Sphere"
  :setup setup
  :draw draw
  :size [640 360]
  :renderer :opengl)

(defn -main [& args])
