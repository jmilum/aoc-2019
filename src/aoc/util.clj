(ns aoc.util
  (:require
   [clojure.java.io :as jio]
   [clojure.set :as set]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.algo.generic.functor :as f]
   [com.hypirion.clj-xchart :as chart])
  (:import
   (java.io FileNotFoundException BufferedReader)
   (java.util Random)
   (clojure.lang Reflector)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defmacro doseq-indexed
  "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
   `(loop [vals# (seq ~values)
           ~index-sym (long 0)]
      (if vals#
        (let [~val-sym (first vals#)]
          ~@code
          (recur (next vals#) (inc ~index-sym)))
        nil))))

(defmacro defmethod*
  "closure over defmethod to bind the dispatch-val with :as"
  [multifn dispatch-val & fn-tail]
  (let [[kw n & body] fn-tail]
    (if (= :as kw)
      `(let [~n ~dispatch-val]
         (defmethod ~multifn ~dispatch-val ~body))
      `(defmethod ~dispatch-val ~fn-tail))))

(defmacro when-let*
  "allow multiple bindings in when-let"
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))

(defmacro if-let*
  "allow multiple bindings in if-let"
  ([bindings then]
   `(if-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~then ~else)
        ~(if-not (second bindings) else))
     then)))

(defmacro ->map
  "create a map of the values with the names as keywords"
  [& ks]
  (zipmap (map keyword ks) ks))

(defn map-kv [f coll] (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn str->int [s] (when s (Integer/parseInt s)))

(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit ^Character %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit ^Character %) s)]
      (empty? s))))

(defn lazy-file-lines [file]
  (letfn [(helper [^BufferedReader rdr]
            (lazy-seq
             (if-let [line (.readLine ^BufferedReader rdr)]
               (cons line (helper rdr))
               (do (.close rdr) nil))))]
    (helper (jio/reader file))))

(defn load-csv-data
  [file-name separator]
  (with-open [in (jio/reader file-name)]
    (doall (csv/read-csv in :separator separator))))

(defn save-csv-data
  [file-name data]
  (with-open [out (jio/writer file-name)]
    (csv/write-csv out data)))

(defn save-coll
  [file coll]
  (when (seq coll)
    (->> coll
         (interpose \newline)
         (apply str)
         (spit file))))

(defn third [coll] (nth coll 2))
(defn fourth [coll] (nth coll 3))
(defn fifth [coll] (nth coll 4))
(defn sixth [coll] (nth coll 5))
(defn seventh [coll] (nth coll 6))
(defn eighth [coll] (nth coll 7))
(defn ninth [coll] (nth coll 8))
(defn tenth [coll] (nth coll 9))

(defn slurp-from-classpath
  "Slurps a file from the classpath."
  [path]
  (or (some-> path
              jio/resource
              slurp)
      (throw (FileNotFoundException. path))))

(defn get-edn-data
  [file]
  (try
    (read-string (slurp file))
    (catch FileNotFoundException e#
      nil)))

(defn replace-several
  [str & replacements]
  (reduce (fn [s [a b]]
            (str/replace s a b))
          str
          (partition 2 replacements)))

(defn round-double [n x] (->> x (double) (format (str "%." n "g")) (Double/parseDouble)))

(defn unchunk [s]
  (when (seq s)
    (lazy-seq
     (cons (first s)
           (unchunk (next s))))))

(defn print-bits [b]
  (let [class-name    (.getName (class b))
        is-byte       (= "java.lang.Byte" class-name)
        num-bits      (Reflector/getStaticField class-name "SIZE")
        format-string (str "~" num-bits "'0b")
        bin-str-fn    #(Reflector/invokeStaticMethod
                        (if is-byte "java.lang.Integer" class-name)
                        "toBinaryString"
                        (to-array [%]))
        bit-string    (if is-byte
                        (str/join (take-last 8 (bin-str-fn (Byte/toUnsignedInt b))))
                        (bin-str-fn b))]
    (str (str/join (repeat (- num-bits (count bit-string)) \0)) bit-string)))

(defn drop-take [coll m]
  (let [start  (get m :start 0)
        amount (get m :amount Integer/MAX_VALUE)
        stop   (min amount (get m :stop Integer/MAX_VALUE))]
    (->> coll (drop start) (take stop))))

(def fib-seq-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))

(defn pad-to-n [n x v]
  (into v (repeat (- n (count v)) x)))

(defn plotx [x]
  (chart/view
   (chart/xy-chart
    {"x" {:y x :style {:line-color :red :marker-type :none}}}
    {:title  "Response"
     :x-axis {:title "Time (steps)"}
     :y-axis {:title "% Change"}
     :theme  :matlab})))

(defn plotxy [x y]
  (let [points (min (count x) (count y))
        x'     (take points x)
        y'     (take points y)]
    (chart/view
     (chart/xy-chart
      {"value" {:y y' :x x' :style {:line-color :red :marker-type :none}}}
      {:title  "Response"
       :x-axis {:title "Time (steps)"}
       :y-axis {:title "% Change"}
       :theme  :matlab}))))

(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(defn lump [& colls]
  "Column major to row major"
  (partition
   (count colls)
   (apply interleave colls)))

(defn unlump [coll]
  "Row major to column major"
  (map
   #(map
     (fn [i] (nth coll i) %)
     (range (count (first coll))))
   coll))
