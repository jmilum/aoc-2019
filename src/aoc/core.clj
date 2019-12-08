(ns aoc.core
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.string :as str]
   [aoc.util :as util]
   [clojure.edn :as edn]
   [clojure.set :as set]))

;; 1-1
#_(def data (edn/read-string (slurp "resources/1.edn")))
(defn fuel [mass] (int (- (math/floor (/ mass 3)) 2)))

#_ (reduce + (map fuel data))

;; 1-2
(defn fuel* [mass]
  (loop [x (fuel mass)
         y (fuel x)]
    (if (pos? y)
      (recur (+ x y) (fuel y))
      x)))

#_ (reduce + (map fuel* data))

;; 2-1
#_(def data  (into [] (edn/read-string (slurp "resources/2.edn"))))
#_(def opcodes {1 + 2 * 99 :end})

(defn execute-rf [state ptr]
  (let [[opcode in1 in2 out] (subvec state ptr (+ 4 ptr))
        op (get opcodes opcode)]
    (if (= op :end)
      (reduced state)
      (assoc state out (op (get state in1) (get state in2))))))

(defn run-program [state noun verb]
  (-> state
      (assoc 1 noun)
      (assoc 2 verb)
      (as-> $ (reduce execute-rf $ (range 0 (count state) 4)))
      (first)))

#_ (run-program data 12 2)

;; 2-2
#_ (->> (for [noun (range 0 100)
              verb (range 0 100)
              :let [x (run-program data noun verb)]]
          [x noun verb])
        (filter #(= 19690720 (first %))))

;; 3-1
#_(def data (edn/read-string (slurp "resources/3.edn")))

(defn right [loc len]
  (->> (range 1 (inc len))
       (map (fn [n] [(+ (first loc) n) (second loc)]))))

(defn left [loc len]
  (->> (range 1 (inc len))
       (map (fn [n] [(- (first loc) n) (second loc)]))))

(defn up [loc len]
  (->> (range 1 (inc len))
       (map (fn [n] [(first loc) (+ (second loc) n)]))))

(defn down [loc len]
  (->> (range 1 (inc len))
       (map (fn [n] [(first loc) (- (second loc) n)]))))

#_(def ops {:U up :D down :R right :L left})

(defn parse-instr [instr] [(keyword (subs instr 0 1)) (edn/read-string (subs instr 1))])

(defn execute-instr [loc instr]
  (let [[dir len] instr
        op (get ops dir)]
    (op loc len)))

(defn update-state [state instr]
  (let [loc (last state)]
    (into state (execute-instr loc instr))))

(defn run-path [path]
  (->> path
       (map str)
       (map parse-instr)
       (reduce update-state [[0 0]])
       (rest)))

#_(def path1 (into #{} (run-path (first data))))
#_(def path2 (into #{} (run-path (second data))))

(defn distance [[x y]] [(+ (math/abs x) (math/abs y)) [x y]])

#_(def crosses (set/intersection path1 path2))

#_ (first (sort (map distance crosses)))

;;3-2
#_ (def data (edn/read-string (slurp "resources/3.edn")))

(defn right [[x y z] len]
  (->> (range 1 (inc len))
       (map (fn [n] [(+ x n) y (+ z n)]))))

(defn left [[x y z] len]
  (->> (range 1 (inc len))
       (map (fn [n] [(- x n) y (+ z n)]))))

(defn up [[x y z] len]
  (->> (range 1 (inc len))
       (map (fn [n] [x (+ y n) (+ z n)]))))

(defn down [[x y z] len]
  (->> (range 1 (inc len))
       (map (fn [n] [x (- y n) (+ z n)]))))

(def ops {:U up :D down :R right :L left})

(defn parse-instr [instr] [(keyword (subs instr 0 1)) (edn/read-string (subs instr 1))])

(defn execute-instr [loc instr]
  (let [[dir len] instr
        op (get ops dir)]
    (op loc len)))

(defn update-state [state instr]
  (let [loc (last state)]
    (into state (execute-instr loc instr))))

(defn run-path [path]
  (->> path
       (map str)
       (map parse-instr)
       (reduce update-state [[0 0 0]])
       (rest)))

(defn crosses [path1 path2]
  (let [path1* (into #{} (map (juxt first second) path1))
        path2* (into #{} (map (juxt first second) path2))]
    (set/intersection path1* path2*)))

(defn path-merge [path crosses]
  (->> path
       (map (fn [[x y z]] {[x y] z}))
       (apply merge-with (fn [a _] a))
       (filter #(contains? crosses (key %1)))
       (into {})))

(defn cross-steps [data]
  (let [path1 (run-path (first data))
        path2 (run-path (second data))
        crosses (crosses path1 path2)
        path1-steps (path-merge path1 crosses)
        path2-steps (path-merge path2 crosses)]
    (->> (merge-with + path1-steps path2-steps)
         (sort-by val)
         (first)
         (last))))

#_ (cross-steps data)

;; 4-1

