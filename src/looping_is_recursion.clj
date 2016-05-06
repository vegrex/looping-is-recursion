(ns looping-is-recursion)

(defn rec-power [n k]
  (if (zero? n)
    0
    (if (zero? k)
      1
      (* n (rec-power n (dec k))))))

(defn power-helper [n acck k]

  (if (zero? n)
    0

    (if (zero? k)
      acck

      (power-helper n (* n acck) (dec k)))))

(defn power1 [base exp]
  (power-helper base 1 exp))


(defn power [base exp]
  (let [helper (fn [n acc k]
                 (if (zero? n)
                   0
                   (if (zero? k)
                     acc
                     (recur n (* n acc) (dec k)))
                   ))]
    (helper base 1 exp)))

;(power 2 2)  ;=> 4
;(power 5 3)  ;=> 125
;(power 7 0)  ;=> 1
;(power 0 10) ;=> 0

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (= (count a-seq) 1)
      (first a-seq)
      (last-element (rest a-seq)))))

;(last-element [])      ;=> nil
;(last-element [1 2 3]) ;=> 3
;(last-element [2 5])   ;=> 5


(defn seq= [seq1 seq2]
  (let [a (seq seq1)
        b (seq seq2)]
    (cond
      (and (empty? a) (empty? b))
        true
      (or (empty? a) (empty? b))
        false
      (= (first a) (first b))
        (seq= (rest a) (rest b))
      :else
        false)))

;(seq= [1 2 4] '(1 2 4))  ;=> true
;(seq= [1 2 3] [1 2 3 4]) ;=> false
;(seq= [1 3 5] [])        ;=> false


(defn find-first-index [pred a-seq]
  (loop [the-seq a-seq
         acc 0
         n (count the-seq)]
    (if (zero? n)
      nil
      (if (pred (first the-seq))
        acc
        (recur (rest the-seq) (inc acc) (dec n))))))

;(find-first-index nil? [])
;(find-first-index zero? [1 1 3 7 2])
;(find-first-index zero? [7 8 2 3 1 1 1 0 3 7 0 2])
;(find-first-index false? [true true true true true false])

(defn avg [a-seq]
  (loop [the-seq a-seq
         some-seq a-seq
         elements 0
         summs 0
         n (count the-seq)]
    (if (zero? n)
      nil
      (if (= n 1)
        (/ (+ summs (first the-seq)) elements)
        (recur (rest the-seq) some-seq (count some-seq) (+ summs (first the-seq)) (dec n))
))))

;(avg [1 2 3])   ;=> 2
;(avg [0 0 0 4]) ;=> 1
;(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5



(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn parity [a-seq]

  (loop [the-seq a-seq
         odds #{}
         n (count the-seq)]

    ; kill on empty
    (if (empty? the-seq)
      nil

      ; remove if exists
      ;(if (contains? odds (first the-seq))
      (if (= (first the-seq) (some #{(first the-seq)} odds))
        (disj odds (first the-seq))

        ; exit on last
        (if (= n 1)
          (conj odds (first the-seq))

          ; recur if not last
          (recur (rest the-seq) (conj odds (first the-seq)) (dec n)))))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn fib [n]
  (if (zero? n)
    0
    (if (= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2))))))

(defn fast-fibo [n]
  (loop [first 0
         second 1
         acc 0]
    (if (= acc n)
      first
      (recur second (+ first second) (inc acc)))))

(defn cut-at-repetition [a-seq]

  (loop [reps #{}
         veccy []
         the-seq a-seq
         n (count the-seq)]

    (if (empty? the-seq)
      nil
      (if (contains? reps (first the-seq))
        veccy
        (if (= n 1)
          (and (conj reps (first the-seq)) (conj veccy (first the-seq)))
          (recur (conj reps (first the-seq)) (conj veccy (first the-seq)) (rest the-seq) (dec n))
)))))
;(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
;(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
;(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]
