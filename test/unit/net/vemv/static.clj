(ns unit.net.vemv.static
  (:require
   [clojure.test :refer :all]
   [net.vemv.static :as sut]))

(deftest analyze
  (are [i e] (= e
                (sut/analyze i))
    `(defn foo [x]
       (str x)),                #{String}

    `(defn foo [x]
       42
       (str x)),                #{String}

    `(defn foo [x]
       (do
         (str x))),             #{String}

    `(defn foo [x]
       (do
         (do
           (str x)))),          #{String}

    `(defn foo [x]
       (do
         (if true
           (str x)
           (int (str x))))),    #{String Long}

    `(defn foo [x]
       (do
         (if true
           (str x)
           (if true
             (int x)
             (if true
               (boolean x)
               (double x)))))), #{Long Boolean Double String}

    ;; testing: `if` content is excluded, because (str) wraps the `if`, so whatever content produced is by `if`, will be `str`ed
    `(defn foo [x]
       (do
         (str (if true
                (byte x)
                (double x))))), #{String}

    ;; testing: `byte` is excluded, because it is wrapped in `boolean`
    `(defn foo [x]
       (do
         (if true
           (str x)
           (if true
             (int x)
             (if true
               (boolean (if true
                          (byte x)
                          (byte x)))
               (double)))))),   #{Long Boolean Double String}

    ;; https://github.com/athos/symbol-analyzer ?
    #_`(defn foo [x]
         (let [y (str x)]
           y)),                 #_String))
