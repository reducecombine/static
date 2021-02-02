(ns net.vemv.static
  (:require
   [clojure.walk :as walk]
   [nedap.speced.def :as speced])
  (:import
   (clojure.lang IObj)))

;; https://github.com/athos/symbol-analyzer

;; XXX this is the most important case:
;; (-> x .foo .bar .baz) (involves analyzing the return values of these methods)

(defn process-tag [x]
  (cond
    (string? x) x
    (class? x)  (-> x pr-str symbol)
    (symbol? x) x
    true        nil))

(defn ctor-sym->class-sym [x]
  (->> x str butlast (apply str) symbol))

(defn ctor-sym? [x]
  (and (symbol? x)
       (-> x str last #{\.})))

(defn type-of [x]
  (or (and (instance? IObj x)
           (or (some-> x meta :tag process-tag resolve)
               (and (symbol? x)
                    (some-> x resolve meta :tag process-tag resolve))
               (and (symbol? x)
                    (some->> x resolve meta :arglists (map meta) (keep :tag) (keep process-tag) (some resolve)))))
      (-> {`do       ::identity
           `identity ::identity
           `double   Double
           `boolean  Boolean
           `int      Long}
          (get x (cond
                   (symbol? x)
                   ::unknown

                   (and (seq? x)
                        (-> x first ctor-sym?))
                   (-> x first ctor-sym->class-sym resolve)

                   (not (seq? x)) ;; allow processing of `do`, `if`
                   (class x)

                   true
                   nil)))))

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

(defn unthread [form]
  (->> form
       (walk/postwalk (fn [x]
                        (cond-> x
                          (and (seq? x)
                               (-> x first #{'-> `-> '->> `->>}))
                          macroexpand-1)))))

(defn value-of-local-binding
  "Assuming `bindings` in a surrounding `let`, what would `local-binding` evaluate to?"
  [bindings local-binding]
  (let [b (->> bindings
               (partition 2)
               (mapcat (fn [[l r]]
                         [l (list 'quote r)]))
               vec)
        x (eval (list 'let b local-binding))
        y (->> bindings (partition 2) (map first) (some #{x}))]
    (if-not y
      x
      (recur bindings y))))

(defn unlet [form]
  (->> form
       (walk/postwalk (fn [x]
                        (if-not (and x
                                     (-> x seq?)
                                     (-> x first #{'let}))
                          x
                          (let [bindings (second x)
                                syms (->> bindings (partition 2) (map first))
                                all-syms (atom #{})]
                            (->> syms
                                 (walk/postwalk (fn [s]
                                                  (when (symbol? s)
                                                    (swap! all-syms conj s))
                                                  s)))
                            (let [body (->> x
                                            last
                                            (walk/postwalk (fn [i]
                                                             (if-not (symbol? i)
                                                               i
                                                               (let [s (try
                                                                         (value-of-local-binding bindings i)
                                                                         ;; XXX works but not very efficient:
                                                                         (catch clojure.lang.Compiler$CompilerException _
                                                                           i))
                                                                     tag (->> @all-syms
                                                                              (filter #{i})
                                                                              first
                                                                              meta
                                                                              :tag)]
                                                                 (if (and (not (-> s meta :tag))
                                                                          tag)
                                                                   (vary-meta s assoc :tag tag)
                                                                   s))))))]
                              (list 'do body))))))))

(speced/defn analyze* [target]
  (letfn [(do-process [x acc]
            (let [t (type-of x)]
              (if-not (contains? #{::unknown nil} t)
                (conj acc t)
                (if (sequential? x)
                  (process x do-process acc)
                  (conj acc ::unknown)))))]
    (let [final-target (-> target unthread unlet)]
      (->> (do-process final-target [])
           (reverse)
           (map normalize)
           (reduce derive-type)
           (wrap)
           (leaves)))))

(defn analyze
  "Returns the possible return types of `simple-defn-form`.

  `simple-defn-form` means it has a single, unwrapped arity, no docstring, etc"
  [simple-defn-form]
  (let [[_defn _name _argv & body] simple-defn-form
        target (last body)]
    (analyze* target)))

(comment
  (value-of-local-binding '[b (Thread.) a b] 'a)

  (value-of-local-binding '[x (-> 1)] 'x)

  (value-of-local-binding '[x (-> 1)] 'foo))

(comment
  (unlet '(let [a + x a] (x 1 1) (x 1 1) x)))

(comment
  (analyze* '(let [^Thread x (str)] ;; NOK; meta should take precedence
               x))

  (unlet  '(let [^Thread x (str)] ;; NOK; meta should take precedence
             x)))
