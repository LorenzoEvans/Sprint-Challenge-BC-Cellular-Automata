(ns app-sketch.conway
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))


; Define size of row in grid.
(def grid 20)
; Generate initial grid state by filling a vector ([]) with 400 (grid * grid) instances
; of 0's, 1's and 2's, and putting that vector in a map,
(def state {:matrix (vec
                     (repeatedly (* grid grid) #(rand-int 3)))})

; Generates a sequence of 200 random rgb255 integer color values.
(defn interval []
  (let [vec
        (vec (repeatedly (* grid (/ grid 2))  #(rand-int 255)))]
    vec))

; Interleaves a list of 1's and 0's with random integer values for coloring via 
; numbers other than 1 or 0.
(defn interstate [s-vec num-vec]
  (interleave num-vec s-vec))

; Set up function for quil processing at ten frames/sec.
(defn setup []
  (q/frame-rate 10)
  state)

; Checks the neighbors of a given cell, finding them by the value of their index,
; relative to the vector in the map (Clojure maps are of type Seq so the elements are indexed and the index
; can be used as the key for the element), using increments, decrements and addition/subtraction to target
; the upper, lower and diagonal points relative to said cell.

(defn neighbors [index mtrx]
  [
   (get mtrx (dec (- index grid)))
   (get mtrx (- index grid))
   (get mtrx (inc (- index grid)))
   (get mtrx (dec index))
   (get mtrx (inc index))
   (get mtrx (dec (+ grid index)))
   (get mtrx (+ grid index))
   (get mtrx (inc (+ grid index)))])

; Targets all of the neighboring instances of 1's and 0's in grid relative to a cell,
; checks if they're alive conditionally, and returns flipped value accordingly.
(defn new-cells [index val mtrx]
  (let [alive-n (get (frequencies (neighbors index mtrx)) 1 0)]
    (if (= 1 val)
      (if (or (> alive-n 3) (< alive-n 2)) 0 1)
      (if (= 3 alive-n) 1 0))))

; Applies the new cell values to state correctly, by mapping them out back onto the state
; variable, associated with the matrix keyword, which, since keywords are also pure functions,
; in clojure,  can return the value associated with it if it exists, giving us access to the grid.
(defn update-cells [state]
  (assoc state :matrix
    (vec
      (map-indexed
       (fn [index val] (new-cells index val (:matrix state)))
       (:matrix state)))))

; Defines the color of the background, initiates a cell size of the width (20) divided by the grid,
; (400), creating a (do(seq))uence of 20 cell indexes and values, broken into singular cells by,
; desired x y dimension, based on the grid space not covered by the value of dividing the cell,
; index by the grid size, and then finally coloring them based on their value which can be 1, 0, 2,
; or a random rgb255 integer value to add some color.
(defn animate [state]
  (q/background 0)
  (q/fill (rand-nth (interval)))
  (let [cell (quot (q/width) grid)]
    (doseq [[i v] (map-indexed vector (:matrix state))]
      (let [multiplier (quot i grid)
            x (* cell (- i (* multiplier grid)))
            y (* cell multiplier)]
        (q/fill
         (if (= 1 v) (rand-nth (interval)) 0)
         (if (= 2 v) 0 255))
        (q/ellipse x y cell cell)))))



(q/defsketch conway
  :name "Conways Go At Life"
  :size [900 900]
  :setup setup
  :update update-cells
  :draw animate
  :middleware [m/fun-mode])

