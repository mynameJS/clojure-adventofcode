(ns clojure-adventofcode.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; adventofcode 2018 

;; 1. 주파수 계산

;; 축약 버전
(def frequency-change-value [+10
-9
+12
+5
+13
-6
-19
-4
-17
2])

;; result
(def final-frequency (reduce + 0 frequency-change-value)) ; 592

;; 2. 인벤토리 관리 시스템

(defn convert-to-vector 
  "입력 데이터 가공 및 벡터화"
  [text]
  (vec (clojure.string/split text #"\n")))

;; 축약 버전
(def input-data "ymdrcyapvwfloiuktanxzjsieb
ymdrwhgznwfloiuktanxzjsqeb
ymdrchguvwfloiuktanxmjsleb
pmdrchgmvwfdoiuktanxzjsqeb
ymdrfegpvwfloiukjanxzjsqeb
ymdrchgpvwfloiukmanazjsdeb
ymdsnhgpvwflciuktanxzjsqeb
lmdrbhrpvwfloiuktanxzjsqeb
")


(def box-ids (convert-to-vector input-data))

(defn count-alphabet 
  "상자 id 를 순회하면서 각각의 알파벳의 갯수를 세는 함수"
  [box-id]
  (reduce (fn [count-map alphabet] (update count-map alphabet (fnil inc 0))) {} box-id))

(defn count-pair
  "알파벳이 2개씩 들어있는 경우 카운트 함수"
  [box-ids]
  (count (filter #(= 2 %) (mapcat #(set(vals (count-alphabet %))) box-ids))))

(defn count-triple
  "알파벳이 3개씩 들어있는 경우 카운트 함수"
  [box-ids]
  (count (filter #(= 3 %) (mapcat #(set(vals (count-alphabet %))) box-ids))))

(defn check-sum
  "2개씩 나온 알파벳의 개수와 3개씩 나온 알파벳의 개수를 곱하는 함수"
  [box-ids]
  (* (count-pair box-ids) (count-triple box-ids)))

;; result

(def chek-sum-boxs (check-sum box-ids)) ; 5000