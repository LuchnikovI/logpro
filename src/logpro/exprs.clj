(ns logpro.exprs)

(defn variable? [expr]
  (and (symbol? expr) (= (first (str expr)) \?)))

