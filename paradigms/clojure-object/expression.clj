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

(def function-operations {
                          '+ add
                          '- subtract
                          '* multiply
                          '/ divide
                          'negate negate
                          'med med
                          'avg avg
                          })

(defn parse-expression [operations constant variable expression]
  (cond
    (seq? expression) (apply (operations (first expression))
                             (mapv (partial parse-expression operations constant variable) (rest expression)))
    (number? expression) (constant expression)
    (symbol? expression) (variable (str expression))))

(defn common-parse
  [operations constant variable]
  (fn [expression]
    (parse-expression operations constant variable (read-string expression))))

(def parseFunction (common-parse function-operations constant variable))


; ---------------------------- 11 HW -----------------------------------


(defn prot-get
  [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (prot-get (obj :prototype) key)
    :else nil))

(defn prot-call
  [this key & args]
  (apply (prot-get this key) this args))

;-------------------

(defn field
  [key]
  (fn [this] (prot-get this key)))

(defn method
  [key]
  (fn [this & args] (apply prot-call this key args)))

;-------------------

(defn constructor
  [consruct prototype]
  (fn [& args] (apply consruct {:prototype prototype} args)))

(defn associated-constructor
  [prototype & fields]
  (constructor (fn [this & values]
                 (reduce (fn [this [field value]]
                           (assoc this
                             field value))
                         this
                         (mapv vector fields values)))
               prototype))

;-------------------

(def evaluate (method :evaluate))
(def toString (method :toString))
(def to-string-suffix (method :to-string-suffix))
(def diff (method :diff))

(declare ZERO)

(def Constant-prototype
  (let [val (field :value)]
    {:evaluate         (fn [this _] (val this))
     :toString         (fn [this] (format "%.1f" (double (val this))))
     :to-string-suffix (fn [this] (format "%.1f" (double (val this))))
     :diff             (fn [_ _] ZERO)}))

(def Constant (associated-constructor Constant-prototype :value))

(def ZERO (Constant 0))
(def ONE (Constant 1))
(def TWO (Constant 2))

(def Variable-prototype
  (let [name (field :name)]
    {:evaluate (fn [this values] (values (name this)))
     :toString (fn [this] (name this))
     :diff (fn [this diffVarName]
             (if (= (name this) diffVarName)
               ONE
               ZERO))}))

(def Variable (associated-constructor Variable-prototype :name))

(def Operator-prototype
  (let [f (field :f)
        diffRule (field :diff-rule)
        name (field :name)
        expressions (field :expressions)]
    {:evaluate (fn [this values] (apply (f this) (mapv #(evaluate % values) (expressions this))))
     :toString (fn [this] (str "(" (name this) " "
                               (clojure.string/join " " (mapv toString (expressions this)))
                               ")"))
     :diff (fn [this diffVarName] ((diffRule this)
                                   (expressions this)
                                   (mapv #(diff % diffVarName) (expressions this))))}))


;-------------------
(defn oper-constructor
  [f name diffRule]
  (constructor
    (fn [this & values]
      (assoc this
        :expressions values))
    ((associated-constructor Operator-prototype :f :name :diff-rule)
     f name diffRule)))

(def Add (oper-constructor
           +
           '+
           (fn [_ dfs] (apply Add dfs))))

(def Subtract (oper-constructor
                -
                '-
                (fn [_ dfs] (apply Subtract dfs))))

(def Negate (oper-constructor
              -
              'negate
              (fn [_ [dfs]] (apply Negate dfs))))

(declare Multiply)

(defn multiply-diff-rule
  [fs dfs]
  (second (reduce (fn [[f df] [g dg]]
                    [(Multiply f g)
                     (Add (Multiply f dg)
                          (Multiply df g))])
                  [ONE ZERO]
                  (mapv vector fs dfs))))

(def Multiply (oper-constructor
                *
                '*
                multiply-diff-rule))

(def Divide (oper-constructor
              (fn
                ([x] (/ (double x)))
                ([x & other] (/ x (double (apply * other)))))
              '/
              (fn [[f & fs] [df & dfs]]
                (if (== (count fs) 0)
                  (Divide (Negate df)
                          (Multiply f f))
                  (let [g (apply Multiply fs)
                        dg (multiply-diff-rule fs dfs)]
                    (Divide
                      (Subtract (Multiply df g)
                                (Multiply f dg))
                      (Multiply g g)))))
              ))

(def Sum (oper-constructor
           +
           'sum
           (fn [_ dfs] (apply Sum dfs))))

(def Avg (oper-constructor
           #(/ (apply + %&) (count %&))
           'avg
           (fn [fs dfs] (Divide (apply Add dfs)
                                (Constant (count fs))))))


(def object-operations {
                        '+ Add
                        '- Subtract
                        '* Multiply
                        '/ Divide
                        'negate Negate
                        'sum Sum
                        'avg Avg
                        })

;-------------------


(def parseObject (common-parse object-operations Constant Variable))
