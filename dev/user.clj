(ns user
  (:require
   [clojure.main]
   [clojure.pprint :refer [pprint pp]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.algo.generic.functor :as f]
   [clojure.math.numeric-tower :as math]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]
   [criterium.core :refer [bench quick-bench]]
   [clojure.edn :as edn]
   [aoc.core :refer :all]
   [aoc.util :as util]
   [ubergraph.core :as uber]
   [ubergraph.alg :as uber-alg]))
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


