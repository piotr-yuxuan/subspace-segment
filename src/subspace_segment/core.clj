(ns subspace-segment.core
  (:require [net.cgrand.xforms :as x]))

(defn- project-item
  [behavior projectors item]
  ;; FIXME finer grain flow control
  (let [projection (reduce (fn [acc [kw projector-parameters]]
                             (let [projector-fn (or (-> behavior kw :reifier/projector-fn)
                                                    (-> behavior :projector-fn/identity-fn (apply [kw])))]
                               (assoc acc kw (-> projector-fn
                                                 (apply [projector-parameters])
                                                 (apply [item])))))
                           {}
                           projectors)]
    (when (every? some? (vals projection))
      projection)))

(defn- transpose [m]
  (apply mapv vector m))

(defn- order
  [direction coll]
  (let [direction-fn (case direction
                       :asc identity
                       :desc reverse)]
    (direction-fn coll)))

(defn- comparable-pair
  [behavior orientation [x y] kw]
  (let [comparable-fn (or (-> behavior kw :reifier/comparable-fn)
                          (-> behavior :comparable-fn/identity))]
    (->> [x y]
         (map :projection)
         (map kw)
         (map #(comparable-fn % (orientation kw)))
         (order (orientation kw)))))

(defn- projected-sort-fn
  [behavior precedence orientation]
  (fn [x y]
    (->> precedence
         (map #(comparable-pair behavior orientation [x y] %))
         transpose
         (apply compare))))

(defn- coll->linear-segment
  [reifier coll]
  (let [precedence (map first (:reifier/ordering reifier))
        orientation (into {} (:reifier/ordering reifier))
        projectors (:reifier/projectors reifier)
        behavior (:reifier/behavior reifier)]
    (eduction
      (map #(do {:projection (project-item behavior projectors %)
                 :item %}))
      (filter (comp some? :projection))
      (x/sort (projected-sort-fn behavior precedence orientation))
      (map :item)
      coll)))
