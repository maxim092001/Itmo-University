(defn operation [f]
  (fn [& operands]
    (fn [variables] (apply f ((apply juxt operands) variables)))))

(def add (operation +))
(def subtract (operation -))
(def multiply (operation *))
(def divide (operation (fn
                         ([args] (/ (double args)))
                         ([x & args] (/ x (double (apply * args)))))))
(def negate subtract)
(def med (operation #(nth (sort %&) (quot (count %&) 2))))
(def avg (operation #(/ (apply + %&) (count %&))))
(defn constant [x] (constantly x))
(defn variable [name] (fn [arguments] (get arguments name)))


(def operations {
                 '+ add
                 '- subtract
                 '* multiply
                 '/ divide
                 'negate negate
                 'med med
                 'avg avg
                 }
  )

(defn parseExpression [expression]
  (cond
    (seq? expression) (apply (operations (first expression)) (map parseExpression (rest expression)))
    (number? expression) (constant expression)
    (symbol? expression) (variable (str expression))))

(defn parseFunction
  [expression]
  (parseExpression (read-string expression)))
