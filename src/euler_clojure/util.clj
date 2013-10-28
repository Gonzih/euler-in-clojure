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

(defn login []
  (to "https://projecteuler.net/login")
  (input-text "#username" (euler-login))
  (-> (find-element {:name "password"})
      (input-text (euler-pass)))
  (-> (find-element {:name "login"})
      click))

(defn wait-fn []
  (or (find-element {:tag :img :title "Wrong"})
      (find-element {:tag :img :title "Correct"})))

(defn submit-problem [id]
  (let [driver (future (new-driver {:browser :firefox}))
        answer (run-problem id)]
    (set-driver! @driver)
    (login)
    (to (str "https://projecteuler.net/problem=" id))
    (if (find-element {:id "guess"})
      (do (input-text "#guess" answer)
          (input-text (find-element {:name "confirm"}) ""))
      (do (quit)
          (throw (Exception. "Can't find input for answer. Already solved?"))))
    (wait-until wait-fn 600000)
    (condp = (attribute (wait-fn) "title")
      "Wrong" (println "Wrong answer.")
      "Correct" (println "Good job!"))
    (quit)))
