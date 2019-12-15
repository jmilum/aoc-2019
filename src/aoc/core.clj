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

(defn execute-rf [state ptr]
  (let [[opcode in1 in2 out] (subvec state ptr (+ 4 ptr))
        opcodes {1 + 2 * 99 :end}
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

(defn parse-instr [instr] [(keyword (subs instr 0 1)) (edn/read-string (subs instr 1))])

(defn execute-instr [loc instr]
  (let [ops {:U up :D down :R right :L left}
        [dir len] instr
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
#_ (def data (range 171309 643604))

(defn digits [n] (->> n str (map (comp read-string str))))

(defn adjacent-equal? [n]
  (->> (digits n)
       (partition 2 1)
       (some #(= (first %) (second %)))
       ((complement nil?))))

(defn never-decreasing? [n]
  (->> (digits n)
       (partition 2 1)
       (every? #(<= (first %) (second %)))))

#_ (->> data
        (filter adjacent-equal?)
        (filter never-decreasing?)
        (count))

;; 4-2

(defn adjacent-equal? [n]
  (->> (digits n)
       (partition 2 1)
       (filter #(= (first %) (second %)))
       (frequencies)
       (some #(= 1 (second %)))
       ((complement nil?))))

#_(->> data
       (filter adjacent-equal?)
       (filter never-decreasing?)
       (count))

;; 5-1

#_(def data (into [] (edn/read-string (slurp "resources/5-1.edn"))))

(defn get-val [state args modes arg-pos]
  (let [arg  (get args arg-pos)
        mode (get modes arg-pos)]
    (condp = mode
      0 (get state arg)
      1 arg)))

(defn op-1 [state args modes]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)
        arg3 (get args 2)]
    (assoc state arg3 (+ arg1 arg2))))

(defn op-2 [state args modes]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)
        arg3 (get args 2)]
    (assoc state arg3 (* arg1 arg2))))

(defn op-3 [state args _]
  (let [arg (first args)]
    (println "please input value? ")
    (assoc state arg (edn/read-string (read-line)))))

(defn op-4 [state args modes]
  (let [value (get-val state args modes 0)]
    (println (str "output: " value))
    state))

(def opcodes
  {1  {:fn op-1 :arg-num 3}
   2  {:fn op-2 :arg-num 3}
   3  {:fn op-3 :arg-num 1}
   4  {:fn op-4 :arg-num 1}
   99 {:fn nil :arg-num 0}})

(defn parse-instr [instr]
  (let [instr-str  (str instr)
        instr-len  (count instr-str)
        opcode-str (condp = instr-len
                     1 instr-str
                     2 instr-str
                     (subs instr-str (- instr-len 2)))
        opcode     (edn/read-string opcode-str)
        arg-num    (get-in opcodes [opcode :arg-num])
        mode-len   (- instr-len (count opcode-str))
        mode-str   (subs instr-str 0 mode-len)
        mode       (-> (if (empty? mode-str) "0" mode-str)
                       (str/split #"")
                       reverse
                       (->> (map edn/read-string))
                       (as-> coll (mapv #(nth coll % 0) (range 0 arg-num))))]
    {:opcode opcode :mode mode}))

(defn execute-opcode [m _]
  (let [{:keys [state instr-ptr]} m
        instr   (get state instr-ptr)
        {:keys [opcode mode]} (parse-instr instr)
        arg-num (get-in opcodes [opcode :arg-num])
        args    (subvec state (inc instr-ptr) (+ 1 arg-num instr-ptr))
        f       (get-in opcodes [opcode :fn])]
    (if (not= 99 opcode)
      {:state (f state args mode) :instr-ptr (+ 1 instr-ptr arg-num)}
      (reduced {}))))

#_(reduce execute-opcode {:state data :instr-ptr 0} (range 0 (count data)))

;; 5-2
(defn op-1 [state args modes instr-ptr]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)
        arg3 (get args 2)]
    {:state     (assoc state arg3 (+ arg1 arg2))
     :instr-ptr (+ 4 instr-ptr)}))

(defn op-2 [state args modes instr-ptr]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)
        arg3 (get args 2)]
    {:state     (assoc state arg3 (* arg1 arg2))
     :instr-ptr (+ 4 instr-ptr)}))

(defn op-3 [state args _ instr-ptr]
  (let [arg (first args)]
    (println "please input value? ")
    {:state     (assoc state arg (edn/read-string (read-line)))
     :instr-ptr (+ 2 instr-ptr)}))

(defn op-4 [state args modes instr-ptr]
  (let [value (get-val state args modes 0)]
    (println (str "output: " value))
    {:state     state
     :instr-ptr (+ 2 instr-ptr)}))

(defn op-5 [state args modes instr-ptr]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)]
    (if (zero? arg1)
      {:state state :instr-ptr (+ 3 instr-ptr)}
      {:state state :instr-ptr arg2})))

(defn op-6 [state args modes instr-ptr]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)]
    (if (zero? arg1)
      {:state state :instr-ptr arg2}
      {:state state :instr-ptr (+ 3 instr-ptr)})))

(defn op-7 [state args modes instr-ptr]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)
        arg3 (nth args 2)]
    (if (< arg1 arg2)
      {:state (assoc state arg3 1) :instr-ptr (+ 4 instr-ptr)}
      {:state (assoc state arg3 0) :instr-ptr (+ 4 instr-ptr)})))

(defn op-8 [state args modes instr-ptr]
  (let [arg1 (get-val state args modes 0)
        arg2 (get-val state args modes 1)
        arg3 (nth args 2)]
    (if (= arg1 arg2)
      {:state (assoc state arg3 1) :instr-ptr (+ 4 instr-ptr)}
      {:state (assoc state arg3 0) :instr-ptr (+ 4 instr-ptr)})))

(def opcodes
  {1  {:fn op-1 :arg-num 3}
   2  {:fn op-2 :arg-num 3}
   3  {:fn op-3 :arg-num 1}
   4  {:fn op-4 :arg-num 1}
   5  {:fn op-5 :arg-num 2}
   6  {:fn op-6 :arg-num 2}
   7  {:fn op-7 :arg-num 3}
   8  {:fn op-8 :arg-num 3}
   99 {:fn nil :arg-num 0}})

(defn parse-instr [instr]
  (let [instr-str  (str instr)
        instr-len  (count instr-str)
        opcode-str (condp = instr-len
                     1 instr-str
                     2 instr-str
                     (subs instr-str (- instr-len 2)))
        opcode     (Integer/parseInt opcode-str)
        arg-num    (get-in opcodes [opcode :arg-num])
        mode-len   (- instr-len (count opcode-str))
        mode-str   (subs instr-str 0 mode-len)
        mode       (-> (if (empty? mode-str) "0" mode-str)
                       (str/split #"")
                       reverse
                       (->> (map edn/read-string))
                       (as-> coll (mapv #(nth coll % 0) (range 0 arg-num))))]
    {:opcode opcode :mode mode}))

(defn execute-opcode [m _]
  (let [{:keys [state instr-ptr]} m
        instr   (get state instr-ptr)
        {:keys [opcode mode]} (parse-instr instr)
        arg-num (get-in opcodes [opcode :arg-num])
        args    (subvec state (inc instr-ptr) (+ 1 arg-num instr-ptr))
        f       (get-in opcodes [opcode :fn])]
    (if (not= 99 opcode)
      (f state args mode instr-ptr)
      (reduced {}))))

#_(def data (into [] (edn/read-string (slurp "resources/5-1.edn"))))
#_(reduce execute-opcode {:state data :instr-ptr 0} (range 0 (count data)))

;; 6-1
