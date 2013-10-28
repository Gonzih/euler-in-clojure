(ns euler-clojure.util
  (:require [clj-webdriver.taxi :refer :all]))

(def problems (atom {}))

(defmacro defproblem [id & body]
  `(let [fun# (fn [] ~@body)]
     (swap! problems assoc ~id fun#)))

(defn run-problem [id]
  ((get @problems id)))

(defn euler-login []
  (System/getenv "EULER_LOGIN"))

(defn euler-pass []
  (System/getenv "EULER_PASS"))

(defn login [driver]
  (to driver "https://projecteuler.net/login")
  (input-text driver "#username" (euler-login))
  (input-text driver
              (find-element driver {:name "password"})
              (euler-pass))
  (click driver (find-element driver {:name "login"})))

(defn wait-fn [driver]
  (or (find-element driver {:tag :img :title "Wrong"})
      (find-element driver {:tag :img :title "Correct"})))

(defn show-problem [id]
  (to (new-driver {:browser :firefox})
      (str "https://projecteuler.net/problem=" id)))

(defn submit-problem [id]
  (let [driver-atom (future (new-driver {:browser :firefox}))
        answer (run-problem id)
        driver @driver-atom]
    (login driver)
    (to driver (str "https://projecteuler.net/problem=" id))
    (if (find-element driver {:id "guess"})
      (do (input-text driver "#guess" answer)
          (input-text driver (find-element driver {:name "confirm"}) ""))
      (do (quit driver)
          (throw (Exception. "Can't find input for answer. Already solved?"))))
    (wait-until driver wait-fn 600000 1000)
    (condp = (attribute driver (wait-fn driver) "title")
      "Wrong" (println "Wrong answer.")
      "Correct" (println "Good job!"))
    (quit driver)))
