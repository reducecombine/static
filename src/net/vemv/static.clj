(ns net.vemv.static
  (:require
   [clojure.walk :as walk]))

(defn type-of [x]
  (-> {`do       ::identity
       `identity ::identity
       `str      String
       `boolean  Boolean
       `double   Double
       `byte     Byte
       `int      Long}
      (get x ::unknown)))

(defn derive-type
  ([x]
   x)

  ([x y]
   (cond
     (= y ::identity)
     x

     true
     y)))

(defmulti process (fn [[x] f acc]
                    x))

(defmethod process :default [[head & tail] f acc]
  (into (f head acc)
        (if (seq tail)
          (f tail [])
          [])))

(defmethod process `if [[_ _ then else] f acc]
  (into acc
        [[::if
          (f then [])
          (f else [])]]))

(defmulti normalize (fn [x]
                      (when (sequential? x)
                        (first x))))

(defmethod normalize :default [x]
  x)

(defmethod normalize ::if [x]
  (->> x
       rest
       (map reverse)
       (map (partial reduce derive-type))
       (map normalize)
       (set)))

(defn wrap [x]
  (if (coll? x)
    x
    #{x}))

(defn leaves [coll]
  (let [r (atom #{})]
    (->> coll (walk/postwalk (fn [x]
                               (when-not (coll? x)
                                 (swap! r conj x))
                               x)))
    @r))

(defn analyze
  "Returns the possible return types of `simple-defn-form`.

  `simple-defn-form` means it has a single, unwrapped arity, no docstring, etc"
  [simple-defn-form]
  (let [[_defn _name _argv & body] simple-defn-form
        target (last body)]
    (letfn [(do-process [x acc]
              (if-not (sequential? x)
                (conj acc (type-of x))
                (process x do-process acc)))]
      (->> (do-process target [])
           (reverse)
           (map normalize)
           (reduce derive-type)
           (wrap)
           (leaves)))))
