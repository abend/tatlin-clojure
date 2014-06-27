(ns tatlin.slice
  (:use [slingshot.slingshot :only [throw+ try+]]))


(defmacro defrecord+
  [record-name fields-and-values & record-body]
  (let [fields-and-values (map #(if (vector? %) % [% nil]) fields-and-values)
        fields            (vec (map first fields-and-values))
        default-map       (into {} fields-and-values)]
    `(do
       (defrecord ~record-name
         ~fields
         ~@record-body)
       (defn ~(symbol (str "make-" (name record-name)))
         [& {:keys ~fields :or ~default-map}]
         (new ~record-name ~@fields)))))


(defrecord+ Vertex [x y z [value nil] [index nil]])



(defrecord+ Edge [vert-p vert-q [face-1 nil] [face-2 nil] [visited false]])

(defn edge-add-face
  [edge face]
  (cond (nil? (:face-1 edge)) (merge edge {:face-1 face})
        (nil? (:face-2 edge)) (merge edge {:face-2 face})
        :else (throw+ {:type ::add-face :message "too many faces already"})))

(defn edge-next-face
  [edge previous]
  (cond (= previous (:face-1 edge)) (:face-2 edge)
        (= previous (:face-2 edge)) (:face-1 edge)
        :else (throw+ {:type ::next-face :message "unknown previous face"})))

(defn edge-intersection
  [edge test]
  (let [p       (:vert-p edge)
        p_x     (:x p)
        p_y     (:y p)
        p_z     (:z p)
        p_value (:value p)
        
        q       (:vert-q edge)
        q_x     (:x q)
        q_y     (:y q)
        q_z     (:z q)
        q_value (:value q)
        
        d (/ (- test p_value) (- q_value p_value))]
    
    (make-Vertex :x (+ p_x (* d (- q_x p_x)))
                 :y (+ p_y (* d (- q_y p_y)))
                 :z (+ p_z (* d (- q_z p_z))))))

(defn edge-intersects
  [edge test]
  (let [p_value (:value (:vert-p edge))
        q_value (:value (:vert-q edge))]
    (and
     ;; we don't want to intersect with an edge that lies parallel
     ;; to the intersection plane
     (not (= p_value q_value))
     (or (and (<= p_value test) (<= test q_value))
         (and (<= q_value test) (<= test p_value))))))

(defn edge-degenerate? 
  [edge]
  (or (nil? (:face-1 edge)) (nil? (:face-2 edge))))


(defrecord Face [edges])

(defn make-Face
  [edges]
  (let [f (Face. edges)]
    (doseq [e edges]
      (edge-add-face e f))
    f))

(defn face-next-edge
  [face previous z]
  (take-while #(and (not (= % previous))
                    (edge-intersects % z))) (:edges face))



(defn find-edge-loop
  [edge test visited]
  "find an unvisited edge loop reachable from the start edge"
    (loop [e edge
           face (:face-1 edge)
           loop-verts []
           new-visited visited]
      (if (not (contains? new-visited e))
        (recur (face-next-edge face e test)
               (edge-next-face e face) 
               (cons (edge-intersection e test) loop-verts)
               (cons e new-visited))
        (list loop-verts new-visited))))

(defn find-intersection [edges test]
  (loop [my-edges edges
         visited  []
         results  []]

    (letfn [(edge-valid? [edge]
              (and edge
                   (not (:visited edge))
                   (edge-intersects edge test)))]

      (if (edge-valid? (first edges))
        (let [[new-loop new-visited] (find-edge-loop (first edges) test visited)]
          (recur (rest edges)
                 (assoc visited new-visited)
                 (cons new-loop results)))
        results))))

(defn find-x-plane-intersection 
  [verts x]
  ;; TODO use simple value, not whole edge structure?
  (let [values (map #(:x %) verts)]
    (find-intersection values x)))
