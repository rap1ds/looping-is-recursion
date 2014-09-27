(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (= k 0)
                   acc
                   (recur (* n acc) n (- k 1))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   :else (if (= (first seq1) (first seq2))
           (recur (rest seq1) (rest seq2))
           false)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         xs a-seq]
    (cond
     (empty? xs) nil
     (pred (first xs)) idx
     :else (recur (+ idx 1) (rest xs)))))

(defn avg [a-seq]
  (loop [xs a-seq
         sum 0
         count 0]
    (if (empty? xs)
      (/ sum count)
      (recur (rest xs) (+ sum (first xs)) (+ 1 count)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         xs a-seq]
    (if (empty? xs)
      acc
      (recur (toggle acc (first xs)) (rest xs)))))

(defn fast-fibo [n]
  (loop [n-2 1
         n-1 1
         i 3]
    (cond 
     (= n 0) 0
     (= n 1) 1
     (= n 2) 1
     (= i n) (+ n-2 n-1)
     :else (recur n-1 (+ n-2 n-1) (+ i 1)))))

(defn cut-at-repetition [a-seq]
  (loop [found #{}
         seq a-seq
         memo []]
    (if (empty? seq)
      memo
      (if (contains? found (first seq))
        memo
        (recur (conj found (first seq)) (rest seq) (conj memo (first seq)))))))

