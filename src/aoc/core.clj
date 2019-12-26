(ns aoc.core
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]
   [aoc.util :as util]
   [clojure.edn :as edn]
   [clojure.set :as set]
   [ubergraph.core :as uber]
   [ubergraph.alg :as uber-alg]
   [clojure.core.async :as a :refer [>! <! <!! >!!]]))

;;; 1-1
;#_(def data (edn/read-string (slurp "resources/1.edn")))
;(defn fuel [mass] (int (- (math/floor (/ mass 3)) 2)))
;
;#_ (reduce + (map fuel data))
;
;;; 1-2
;(defn fuel* [mass]
;  (loop [x (fuel mass)
;         y (fuel x)]
;    (if (pos? y)
;      (recur (+ x y) (fuel y))
;      x)))
;
;#_ (reduce + (map fuel* data))
;
;;; 2-1
;#_(def data  (into [] (edn/read-string (slurp "resources/2.edn"))))
;
;(defn execute-rf [state ptr]
;  (let [[opcode in1 in2 out] (subvec state ptr (+ 4 ptr))
;        opcodes {1 + 2 * 99 :end}
;        op (get opcodes opcode)]
;    (if (= op :end)
;      (reduced state)
;      (assoc state out (op (get state in1) (get state in2))))))
;
;(defn run-program [state noun verb]
;  (-> state
;      (assoc 1 noun)
;      (assoc 2 verb)
;      (as-> $ (reduce execute-rf $ (range 0 (count state) 4)))
;      (first)))
;
;#_ (run-program data 12 2)
;
;;; 2-2
;#_ (->> (for [noun (range 0 100)
;              verb (range 0 100)
;              :let [x (run-program data noun verb)]]
;          [x noun verb])
;        (filter #(= 19690720 (first %))))
;
;;; 3-1
;#_(def data (edn/read-string (slurp "resources/3.edn")))
;
;(defn right [loc len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [(+ (first loc) n) (second loc)]))))
;
;(defn left [loc len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [(- (first loc) n) (second loc)]))))
;
;(defn up [loc len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [(first loc) (+ (second loc) n)]))))
;
;(defn down [loc len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [(first loc) (- (second loc) n)]))))
;
;(defn parse-instr [instr] [(keyword (subs instr 0 1)) (edn/read-string (subs instr 1))])
;
;(defn execute-instr [loc instr]
;  (let [ops {:U up :D down :R right :L left}
;        [dir len] instr
;        op (get ops dir)]
;    (op loc len)))
;
;(defn update-state [state instr]
;  (let [loc (last state)]
;    (into state (execute-instr loc instr))))
;
;(defn run-path [path]
;  (->> path
;       (map str)
;       (map parse-instr)
;       (reduce update-state [[0 0]])
;       (rest)))
;
;#_(def path1 (into #{} (run-path (first data))))
;#_(def path2 (into #{} (run-path (second data))))
;
;(defn distance [[x y]] [(+ (math/abs x) (math/abs y)) [x y]])
;
;#_(def crosses (set/intersection path1 path2))
;
;#_ (first (sort (map distance crosses)))
;
;;;3-2
;#_ (def data (edn/read-string (slurp "resources/3.edn")))
;
;(defn right [[x y z] len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [(+ x n) y (+ z n)]))))
;
;(defn left [[x y z] len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [(- x n) y (+ z n)]))))
;
;(defn up [[x y z] len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [x (+ y n) (+ z n)]))))
;
;(defn down [[x y z] len]
;  (->> (range 1 (inc len))
;       (map (fn [n] [x (- y n) (+ z n)]))))
;
;(def ops {:U up :D down :R right :L left})
;
;(defn parse-instr [instr] [(keyword (subs instr 0 1)) (edn/read-string (subs instr 1))])
;
;(defn execute-instr [loc instr]
;  (let [[dir len] instr
;        op (get ops dir)]
;    (op loc len)))
;
;(defn update-state [state instr]
;  (let [loc (last state)]
;    (into state (execute-instr loc instr))))
;
;(defn run-path [path]
;  (->> path
;       (map str)
;       (map parse-instr)
;       (reduce update-state [[0 0 0]])
;       (rest)))
;
;(defn crosses [path1 path2]
;  (let [path1* (into #{} (map (juxt first second) path1))
;        path2* (into #{} (map (juxt first second) path2))]
;    (set/intersection path1* path2*)))
;
;(defn path-merge [path crosses]
;  (->> path
;       (map (fn [[x y z]] {[x y] z}))
;       (apply merge-with (fn [a _] a))
;       (filter #(contains? crosses (key %1)))
;       (into {})))
;
;(defn cross-steps [data]
;  (let [path1 (run-path (first data))
;        path2 (run-path (second data))
;        crosses (crosses path1 path2)
;        path1-steps (path-merge path1 crosses)
;        path2-steps (path-merge path2 crosses)]
;    (->> (merge-with + path1-steps path2-steps)
;         (sort-by val)
;         (first)
;         (last))))
;
;#_ (cross-steps data)
;
;;; 4-1
;#_ (def data (range 171309 643604))
;
(defn digits [n] (->> n str (map (comp read-string str))))
;
;(defn adjacent-equal? [n]
;  (->> (digits n)
;       (partition 2 1)
;       (some #(= (first %) (second %)))
;       ((complement nil?))))
;
;(defn never-decreasing? [n]
;  (->> (digits n)
;       (partition 2 1)
;       (every? #(<= (first %) (second %)))))
;
;#_ (->> data
;        (filter adjacent-equal?)
;        (filter never-decreasing?)
;        (count))
;
;;; 4-2
;
;(defn adjacent-equal? [n]
;  (->> (digits n)
;       (partition 2 1)
;       (filter #(= (first %) (second %)))
;       (frequencies)
;       (some #(= 1 (second %)))
;       ((complement nil?))))
;
;#_(->> data
;       (filter adjacent-equal?)
;       (filter never-decreasing?)
;       (count))
;
;;; 5-1
;
;#_(def data (into [] (edn/read-string (slurp "resources/5-1.edn"))))
;
;(defn get-val [state args modes arg-pos]
;  (let [arg  (get args arg-pos)
;        mode (get modes arg-pos)]
;    (condp = mode
;      0 (get state arg)
;      1 arg)))
;
;(defn op-1 [state args modes]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (get args 2)]
;    (assoc state arg3 (+ arg1 arg2))))
;
;(defn op-2 [state args modes]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (get args 2)]
;    (assoc state arg3 (* arg1 arg2))))
;
;(defn op-3 [state args _]
;  (let [arg (first args)]
;    (println "please input value? ")
;    (assoc state arg (edn/read-string (read-line)))))
;
;(defn op-4 [state args modes]
;  (let [value (get-val state args modes 0)]
;    (println (str "output: " value))
;    state))
;
;(def opcodes
;  {1  {:fn op-1 :arg-num 3}
;   2  {:fn op-2 :arg-num 3}
;   3  {:fn op-3 :arg-num 1}
;   4  {:fn op-4 :arg-num 1}
;   99 {:fn nil :arg-num 0}})
;
;(defn parse-instr [instr]
;  (let [instr-str  (str instr)
;        instr-len  (count instr-str)
;        opcode-str (condp = instr-len
;                     1 instr-str
;                     2 instr-str
;                     (subs instr-str (- instr-len 2)))
;        opcode     (edn/read-string opcode-str)
;        arg-num    (get-in opcodes [opcode :arg-num])
;        mode-len   (- instr-len (count opcode-str))
;        mode-str   (subs instr-str 0 mode-len)
;        mode       (-> (if (empty? mode-str) "0" mode-str)
;                       (str/split #"")
;                       reverse
;                       (->> (map edn/read-string))
;                       (as-> coll (mapv #(nth coll % 0) (range 0 arg-num))))]
;    {:opcode opcode :mode mode}))
;
;(defn execute-opcode [m _]
;  (let [{:keys [state instr-ptr]} m
;        instr   (get state instr-ptr)
;        {:keys [opcode mode]} (parse-instr instr)
;        arg-num (get-in opcodes [opcode :arg-num])
;        args    (subvec state (inc instr-ptr) (+ 1 arg-num instr-ptr))
;        f       (get-in opcodes [opcode :fn])]
;    (if (not= 99 opcode)
;      {:state (f state args mode) :instr-ptr (+ 1 instr-ptr arg-num)}
;      (reduced {}))))
;
;#_(reduce execute-opcode {:state data :instr-ptr 0} (range 0 (count data)))
;
;;; 5-2
;(defn op-1 [state args modes instr-ptr]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (get args 2)]
;    {:state     (assoc state arg3 (+ arg1 arg2))
;     :instr-ptr (+ 4 instr-ptr)}))
;
;(defn op-2 [state args modes instr-ptr]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (get args 2)]
;    {:state     (assoc state arg3 (* arg1 arg2))
;     :instr-ptr (+ 4 instr-ptr)}))
;
;(defn op-3 [state args _ instr-ptr]
;  (let [arg (first args)]
;    (println "please input value? ")
;    {:state     (assoc state arg (edn/read-string (read-line)))
;     :instr-ptr (+ 2 instr-ptr)}))
;
;(defn op-4 [state args modes instr-ptr]
;  (let [value (get-val state args modes 0)]
;    (println (str "output: " value))
;    {:state     state
;     :instr-ptr (+ 2 instr-ptr)}))
;
;(defn op-5 [state args modes instr-ptr]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)]
;    (if (zero? arg1)
;      {:state state :instr-ptr (+ 3 instr-ptr)}
;      {:state state :instr-ptr arg2})))
;
;(defn op-6 [state args modes instr-ptr]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)]
;    (if (zero? arg1)
;      {:state state :instr-ptr arg2}
;      {:state state :instr-ptr (+ 3 instr-ptr)})))
;
;(defn op-7 [state args modes instr-ptr]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (nth args 2)]
;    (if (< arg1 arg2)
;      {:state (assoc state arg3 1) :instr-ptr (+ 4 instr-ptr)}
;      {:state (assoc state arg3 0) :instr-ptr (+ 4 instr-ptr)})))
;
;(defn op-8 [state args modes instr-ptr]
;  (let [arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (nth args 2)]
;    (if (= arg1 arg2)
;      {:state (assoc state arg3 1) :instr-ptr (+ 4 instr-ptr)}
;      {:state (assoc state arg3 0) :instr-ptr (+ 4 instr-ptr)})))
;
;(def opcodes
;  {1  {:fn op-1 :arg-num 3}
;   2  {:fn op-2 :arg-num 3}
;   3  {:fn op-3 :arg-num 1}
;   4  {:fn op-4 :arg-num 1}
;   5  {:fn op-5 :arg-num 2}
;   6  {:fn op-6 :arg-num 2}
;   7  {:fn op-7 :arg-num 3}
;   8  {:fn op-8 :arg-num 3}
;   99 {:fn nil :arg-num 0}})
;
;(defn parse-instr [instr]
;  (let [instr-str  (str instr)
;        instr-len  (count instr-str)
;        opcode-str (condp = instr-len
;                     1 instr-str
;                     2 instr-str
;                     (subs instr-str (- instr-len 2)))
;        opcode     (Integer/parseInt opcode-str)
;        arg-num    (get-in opcodes [opcode :arg-num])
;        mode-len   (- instr-len (count opcode-str))
;        mode-str   (subs instr-str 0 mode-len)
;        modes       (-> (if (empty? mode-str) "0" mode-str)
;                        (str/split #"")
;                        reverse
;                        (->> (map edn/read-string))
;                        (as-> coll (mapv #(nth coll % 0) (range 0 arg-num))))]
;    {:opcode opcode :modes modes}))
;
;(defn execute-opcode [m _]
;  (let [{:keys [state instr-ptr phase input]} m
;        instr   (get state instr-ptr)
;        {:keys [opcode mode]} (parse-instr instr)
;        arg-num (get-in opcodes [opcode :arg-num])
;        args    (subvec state (inc instr-ptr) (+ 1 arg-num instr-ptr))
;        f       (get-in opcodes [opcode :fn])]
;    (if (not= 99 opcode)
;      (f state args mode instr-ptr)
;      (reduced {}))))
;
;#_(def data (into [] (edn/read-string (slurp "resources/5-1.edn"))))
;#_(reduce execute-opcode {:state data :instr-ptr 0} (range 0 (count data)))
;
;;; 6-1
;#_(def data (str/split-lines (slurp "resources/6-1.edn")))
;
;(defn parse-orbit [s]
;  (let [nodes (str/split s #"\)")]
;    [(keyword (first nodes)) (keyword (second nodes))]))
;
;(defn build-graph [g data]
;  (->> data
;       (map parse-orbit)
;       (uber/add-edges* g)))
;
;(defn count-orbits [data]
;  (let [orbits (->> data (map parse-orbit) (uber/add-edges* (uber/graph)))]
;    (->> orbits
;         (uber/nodes)
;         (map #(uber-alg/shortest-path orbits :COM %))
;         (map :cost)
;         (apply +))))
;
;#_(count-orbits data)
;
;;; 6-2
;(defn transfers [data start end]
;  (let [orbits (->> data (map parse-orbit) (uber/add-edges* (uber/graph)))]
;    (- (:cost (uber-alg/shortest-path orbits start end)) 2)))
;
;#_(transfers data :YOU :SAN)
;
;;; 7-1
;
;(defn get-val [state args modes arg-pos]
;  (let [arg  (get args arg-pos)
;        mode (get modes arg-pos)]
;    (condp = mode
;      0 (get state arg)
;      1 arg)))
;
;(defn op-1 [m]
;  (let [{:keys [state args modes]} m
;        arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (get args 2)]
;    (-> m
;        (assoc-in [:state arg3](+ arg1 arg2))
;        (update :instr-ptr #(+ 4 %)))))
;
;(defn op-2 [m]
;  (let [{:keys [state args modes]} m
;        arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (get args 2)]
;    (-> m
;        (assoc-in [:state arg3](* arg1 arg2))
;        (update :instr-ptr #(+ 4 %)))))
;
;(defn op-3 [m]
;  (let [{:keys [args phase io phase-flag]} m
;        arg (first args)]
;    (-> m
;        (assoc-in [:state arg] (if (nil? phase-flag) (first phase) io))
;        (assoc :phase (if (nil? phase-flag) (rest phase) phase))
;        (assoc :phase-flag (if (nil? phase-flag) true phase-flag))
;        (update :instr-ptr #(+ 2 %)))))
;
;(defn op-4 [m]
;  (let [{:keys [state args modes]} m
;        value (get-val state args modes 0)]
;    (-> m
;        (assoc :io value)
;        (update :instr-ptr #(+ 2 %)))))
;
;(defn op-5 [m]
;  (let [{:keys [state args modes]} m
;        arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)]
;    (if (zero? arg1)
;      (update m :instr-ptr #(+ 3 %))
;      (assoc m :instr-ptr arg2))))
;
;(defn op-6 [m]
;  (let [{:keys [state args modes]} m
;        arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)]
;    (if (zero? arg1)
;      (assoc m :instr-ptr arg2)
;      (update m :instr-ptr #(+ 3 %)))))
;
;(defn op-7 [m]
;  (let [{:keys [state args modes]} m
;        arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (nth args 2)
;        value  (if (< arg1 arg2) 1 0)]
;    (-> m
;        (assoc-in [:state arg3] value)
;        (update :instr-ptr #(+ 4 %)))))
;
;(defn op-8 [m]
;  (let [{:keys [state args modes]} m
;        arg1 (get-val state args modes 0)
;        arg2 (get-val state args modes 1)
;        arg3 (nth args 2)
;        value (if (= arg1 arg2) 1 0)]
;    (-> m
;        (assoc-in [:state arg3] value)
;        (update :instr-ptr #(+ 4 %)))))
;
;(def opcodes
;  {1  {:fn op-1 :arg-num 3}
;   2  {:fn op-2 :arg-num 3}
;   3  {:fn op-3 :arg-num 1}
;   4  {:fn op-4 :arg-num 1}
;   5  {:fn op-5 :arg-num 2}
;   6  {:fn op-6 :arg-num 2}
;   7  {:fn op-7 :arg-num 3}
;   8  {:fn op-8 :arg-num 3}
;   99 {:fn op-99 :arg-num 0}})
;
;(defn execute-opcode [m _]
;  (let [{:keys [state instr-ptr]} m
;        instr   (get state instr-ptr)
;        {:keys [opcode modes]} (parse-instr instr)
;        arg-num (get-in opcodes [opcode :arg-num])
;        args    (subvec state (inc instr-ptr) (+ 1 arg-num instr-ptr))
;        info (merge m (util/->map args modes))
;        f       (get-in opcodes [opcode :fn])]
;    (if (not= 99 opcode)
;      (f info)
;      (reduced info))))
;
;
;
;#_ (def data [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])
;
;(defn thrust [program phase]
; (as-> {:state program :instr-ptr 0 :phase phase :io 0} $
;       (reduce execute-opcode $ (repeat 0))
;       (assoc $ :state program :instr-ptr 0 :phase-flag nil)
;       (reduce execute-opcode $ (repeat 0))
;       (assoc $ :state program :instr-ptr 0 :phase-flag nil)
;       (reduce execute-opcode $ (repeat 0))
;       (assoc $ :state program :instr-ptr 0 :phase-flag nil)
;       (reduce execute-opcode $ (repeat 0))
;       (assoc $ :state program :instr-ptr 0 :phase-flag nil)
;       (reduce execute-opcode $ (repeat 0))
;       (:io $)))

#_(def program (into [] (edn/read-string (slurp "resources/7-1.edn"))))
#_(apply max (map (partial thrust program) (combo/permutations [0 1 2 3 4])))


;;7-2
(defn get-val [program args modes arg-pos]
  (let [arg  (get args arg-pos)
        mode (get modes arg-pos)]
    (condp = mode
      0 (get program arg)
      1 arg)))

(defn op-1 [m]
  (let [{:keys [program args modes]} m
        arg1 (get-val program args modes 0)
        arg2 (get-val program args modes 1)
        arg3 (get args 2)]
    (-> m
        (assoc-in [:program arg3] (+ arg1 arg2))
        (update :instr-ptr + 4))))

(defn op-2 [m]
  (let [{:keys [program args modes]} m
        arg1 (get-val program args modes 0)
        arg2 (get-val program args modes 1)
        arg3 (get args 2)]
    (-> m
        (assoc-in [:program arg3] (* arg1 arg2))
        (update :instr-ptr + 4))))

(defn op-5 [m]
  (let [{:keys [program args modes]} m
        arg1 (get-val program args modes 0)
        arg2 (get-val program args modes 1)]
    (if (zero? arg1)
      (update m :instr-ptr + 3)
      (assoc m :instr-ptr arg2))))

(defn op-6 [m]
  (let [{:keys [program args modes]} m
        arg1 (get-val program args modes 0)
        arg2 (get-val program args modes 1)]
    (if (zero? arg1)
      (assoc m :instr-ptr arg2)
      (update m :instr-ptr + 3))))

(defn op-7 [m]
  (let [{:keys [program args modes]} m
        arg1  (get-val program args modes 0)
        arg2  (get-val program args modes 1)
        arg3  (nth args 2)
        value (if (< arg1 arg2) 1 0)]
    (-> m
        (assoc-in [:program arg3] value)
        (update :instr-ptr + 4))))

(defn op-8 [m]
  (let [{:keys [program args modes]} m
        arg1  (get-val program args modes 0)
        arg2  (get-val program args modes 1)
        arg3  (nth args 2)
        value (if (= arg1 arg2) 1 0)]
    (-> m
        (assoc-in [:program arg3] value)
        (update :instr-ptr + 4))))

(defn op-99 [m] (assoc m :halt true))

(def arg-nums
  {1  3
   2  3
   3  1
   4  1
   5  2
   6  2
   7  3
   8  3
   99 0})

(defn parse-instr [instr]
  (let [instr-str  (str instr)
        instr-len  (count instr-str)
        opcode-str (condp = instr-len
                     1 instr-str
                     2 instr-str
                     (subs instr-str (- instr-len 2)))
        opcode     (Integer/parseInt opcode-str)
        arg-num    (get arg-nums opcode)
        mode-len   (- instr-len (count opcode-str))
        mode-str   (subs instr-str 0 mode-len)
        modes      (-> (if (empty? mode-str) "0" mode-str)
                       (str/split #"")
                       reverse
                       (->> (map edn/read-string))
                       (as-> coll (mapv #(nth coll % 0) (range 0 arg-num))))]
    {:opcode opcode :modes modes}))

(defn init-program [program phase]
  {:program   program
   :instr-ptr 0
   :in-ch     (a/chan 2)
   :out-ch    (a/chan 2)
   :phase     phase})

(defn execute-opcode [state]
  (a/put! (:in-ch state) (:phase state))
  (a/go-loop [m state]
    (let [{:keys [program instr-ptr]} m
          instr   (get program instr-ptr)
          {:keys [opcode modes]} (parse-instr instr)
          arg-num (get arg-nums opcode)
          args    (subvec program (inc instr-ptr) (+ 1 arg-num instr-ptr))
          info    (merge m (util/->map args modes))
          rtn     (condp = opcode
                    1 (op-1 info)
                    2 (op-2 info)
                    5 (op-5 info)
                    6 (op-6 info)
                    7 (op-7 info)
                    8 (op-8 info)
                    99 (op-99 info)
                    3 (let [{:keys [args in-ch]} info
                            arg (first args)]
                        (-> m
                           (assoc-in [:program arg] (<! in-ch))
                           (update :instr-ptr + 2)))
                    4 (let [{:keys [program args modes out-ch]} info
                            value (get-val program args modes 0)]
                        (>! out-ch value)
                        (-> (update m :instr-ptr + 2))))]
      (if (:halt rtn)
        rtn
        (recur rtn)))))

(defn calculate [states amps]
  (doseq [[out in] (partition 2 1 (into states [(first states)]))]
    (a/pipe (:out-ch out) (:in-ch in)))
  (a/put! (:in-ch (first states)) 0)
  (a/alts!! [(last amps) (a/timeout 1000)])
  (a/poll! (:in-ch (first states))))

(defn run-prog [prog phases]
  (let [states (mapv #(init-program prog %) phases)
        amps   (mapv execute-opcode states)
        val    (calculate states amps)]
    (doseq [[in out] (map (juxt :in-ch :out-ch) states)]
      (a/close! in)
      (a/close! out))
    val))

#_(def program (into [] (edn/read-string (slurp "resources/7-1.edn"))))
#_(apply max (map (partial run-prog program) (combo/permutations [5 6 7 8 9])))

;; 8-1
#_(->> (slurp "resources/8-1.txt")
       (map str)
       (butlast)
       (partition 150)
       (map frequencies)
       (sort-by #(get % "0"))
       (first)
       (#(* (get % "2") (get % "1"))))

;; 8-2
#_(->> (slurp "resources/8-1.txt")
       (map str)
       (butlast)
       (map read-string)
       (partition 150)
       (apply interleave)
       (partition 100)
       (map (fn [x] (drop-while #{2} x)))
       (map first)
       (partition 25))


