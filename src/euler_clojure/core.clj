(ns euler-clojure.core
  (:require [clojure.string :as s]))

(defmacro defproblem [id & body]
  `(defn ~(symbol (str 'p- id)) []
     ~@body))

(defmacro runproblem [id]
  `(~(symbol (str 'p- id))))

(defproblem 1
  (reduce + (filter #(or (zero? (mod % 3))
                         (zero? (mod % 5)))
                    (range 1 1000))))


(defn fib
  ([] (fib 0 1))
  ([a b] (cons a (lazy-seq (fib b (+ a b))))))

(defproblem 2
  (reduce + (filter #(and (> 4000000 %) (even? %)) (take 90 (fib)))))

(defn prime? [n]
      (.isProbablePrime (BigInteger/valueOf n) 5))

(defproblem 3
  (let [n 600851475143
        x (Math/sqrt n)]
    (->> x
         (range 0)
         reverse
         (filter prime?)
         (filter #(zero? (rem n %)))
         first)))

(defn palindrome? [x]
  (= x (Integer/parseInt (s/join (reverse (str x))))))

(defproblem 4
  (let [is (range 999 900 -1)]
    (first (filter palindrome? (for [i is j is] (* i j))))))

(defn answer? [x]
  (every? #(zero? (rem x %)) (range 3 21)))

(def p-5-seq (filter answer? (iterate #(+ 2 %) 2500)))
(def p-5-seq-p (filter first (pmap (fn [x] [(answer? x), x]) (iterate #(+ 2 %) 2500))))

(defproblem 5
  (time (println (first p-5-seq-p)))
  (time (println (first p-5-seq))))

(defproblem 6
  (let [numbers (range 1 101)]
    (- (int (Math/pow (reduce + numbers) 2))
       (int (reduce + (map #(Math/pow % 2) numbers))))))

(defproblem 7
  (let [n 10001]
    (last (take n (filter prime? (iterate inc 0))))))

(defproblem 8
  (let [number 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450]
    (->> number
         str
         (partition 5 1)
         (map (fn [slice] (map str slice)))
         (map (fn [slice] (map #(Integer/parseInt %) slice)))
         (map (fn [slice] (reduce * slice)))
         (apply max))))

(defproblem 9
  (let [nums (range 1 501)
        triplets (for [a nums
                       b nums
                       c nums] [a b c])
        pow (fn [a b] (Math/pow a b))
        pyth-triplet? (fn [[a b c]] (and (= (+ (pow a 2) (pow b 2)) (pow c 2))
                                         (= (+ a b c) 1000)))]
    (reduce * (first (filter pyth-triplet? triplets)))))

(defproblem 10
  (let [nums (range 2000000 0 -1)
        primes (filter prime? nums)]
    (println (reduce + primes))))

(defproblem 11
  (let [string "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
                49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
                81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
                52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
                22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
                24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
                32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
                67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
                24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
                21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
                78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
                16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
                86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
                19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
                04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
                88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
                04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
                20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
                20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
                01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
        grid (->> (s/split string #"\s")
                  (filter (partial re-find #"\d+"))
                  (map #(Integer/parseInt %))
                  (partition 20)
                  (map vec)
                  vec)
        all-indexes (for [i (range 0 20) j (range 0 20)] [i j])
        get-in-grid (fn [[x y]] (-> grid
                                    (get x)
                                    (get y 1)))
        deltas [[1 1] [1 -1] [1 0] [0 1]]
        neighbors (fn [pos] (map (fn [[dx dy]]
                                   (take 4 (iterate (fn [[x y]]
                                                      [(+ dx x) (+ dy y)])
                                                    pos)))
                                 deltas))
        vals-of-neighbors    (fn [neighbors] (map (fn [line] (map get-in-grid line)) neighbors))
        product-of-neighbors (fn [neighbors] (map (fn [line] (reduce * line)) neighbors))
        max-of-neighbors     (fn [neighbors] (reduce max neighbors))]
    (->> all-indexes
         (map neighbors)
         (map vals-of-neighbors)
         (map product-of-neighbors)
         (map max-of-neighbors)
         (reduce max))))

(defproblem 12
  (letfn [(trianlge-number [n] (reduce + (range 1 (inc n))))
          (factors-count [n] (* 2 (count (filter (comp zero? (partial rem n)) (range 1 (Math/sqrt n))))))]
    (let [numbers (iterate inc 1)
          t-numbers (pmap (fn [n]
                            (let [num (trianlge-number n)
                                  cnt (factors-count num)]
                              [cnt num]))
                          numbers)]
      (last (first (filter (fn [[cnt num]] (>= cnt 500)) t-numbers))))))
