(defn equal-vectors-size [& v] (apply = (mapv count v)))
(defn equal-matrices-size [& m] (equal-vectors-size (mapv first m)))

(defn is-vector [v] (and (vector? v) (every? number? v)))
(defn is-matrix [m] (and (every? is-vector m) (vector? m) (apply equal-vectors-size m)))


(defn v [f] (fn [& vs] {
                        :pre  [(every? is-vector vs) (apply equal-vectors-size vs)]
                        :post [(is-vector %) (equal-vectors-size (first vs) %)]
                        }
              (apply (partial mapv f) vs))
  )

(def v+ (v +))
(def v- (v -))
(def v* (v *))

(defn v*s [v & s] {
                   :pre [(is-vector v) (every? number? s)]
                   :post [(is-vector %) (equal-vectors-size % v)]
                   }
  (mapv (partial * (apply * s)) v))

(defn scalar [& vs] {
                     :pre [(every? is-vector vs) (apply equal-vectors-size vs)]
                     :post [(number? %)]
                     }
  (apply + (apply v* vs)))

(defn vect [& vs] {
                   :pre  [(every? is-vector vs) (apply equal-vectors-size vs) (= 3 (count (first vs)))]
                   :post [(is-vector %) (= 3 (count %))]
                   }
  (reduce (fn [a b] (vector (- (* (nth a 1) (nth b 2)) (* (nth a 2) (nth b 1)))
                             (- (* (nth a 2) (nth b 0)) (* (nth a 0) (nth b 2)))
                             (- (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0))))) vs))

(defn m [f] (fn [& ms] {
                        :pre  [(every? is-matrix ms) (apply equal-matrices-size ms)]
                        :post [(is-matrix %) (equal-matrices-size % (first ms))]
                        }
              (apply (partial mapv f) ms)
              )
  )

(def m+ (m v+))
(def m- (m v-))
(def m* (m v*))
(defn m*s [m & s] {
                   :pre [(every? number? s) (is-matrix m)]
                   :post [(is-matrix %) (equal-matrices-size % m)]
                   }
  (mapv (fn [v] (apply (partial v*s v) s)) m))

(defn transpose [m] {
                     :pre  [(is-matrix m)]
                     :post [(is-matrix %) (= (count m) (count (nth % 0))) (= (count %) (count (nth m 0)))]
                     }
  (apply mapv vector m))

(defn m*v [m & v] {
                   :pre  [(is-matrix m) (every? is-vector v) (mapv (fn [x] (equal-vectors-size v x)) m)]
                   :post [(is-vector %) (equal-vectors-size % m)]
                   }
  (mapv (fn [a] (apply scalar a v)) m))

(defn m*m [& ms] {
                  :pre  [every? is-matrix ms]
                  :post [(= (count %) (count (first ms))) (= (count (nth % 0)) (count (nth (last ms) 0)))]
                  }
  (reduce (fn [a b] (mapv (fn [v] (mapv (fn [w] (apply + (v* v w))) (transpose b))) a)) ms))

(defn s [f] (fn result [& vs] {
                               :pre  [(apply equal-vectors-size vs)]
                               :post [(equal-vectors-size % (first vs))]
                               }
              (if (vector? (first vs))
                (apply (partial mapv result) vs)
                (apply f vs)))
  )

(def s+ (s +))
(def s- (s -))
(def s* (s *))

(defn equal-tensors-size [ts] (apply = (mapv count ts)))
(defn is-tensor [& ts] (or (every? number? ts) (and (every? vector? ts) (equal-tensors-size ts)
                                                      (apply is-tensor (apply concat [] ts)))))
(defn t [f]
  (letfn [(get-result [& ts]
            {
             :pre [(and (apply is-tensor ts) (or (every? number? ts)
                                                  (and (every? vector? ts) (equal-tensors-size ts))))]
             }
            (cond (every? number? ts)
                  (apply f ts)
                  :else
                  (apply mapv get-result ts))
            )
          ]
    :return get-result)
  )


(def t+ (t +))
(def t- (t -))
(def t* (t *))
