(ns day-1
  (:require [clojure.java.io]))


(defn parse-frequency-to-long
  "문자열을 Long으로 변환하는 함수"
  [freq-str]
  (Long/parseLong freq-str))


(defn calculate-frequency-change-list
  "lazy sequence를 이용하여 주파수 변동 리스트를 읽고 최종 주파수를 합산하는 함수"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [frequency-change-list (line-seq rdr)
          parsed-frequency-change-list (map parse-frequency-to-long frequency-change-list)]
      (reduce + 0 parsed-frequency-change-list))))


;; result
(def result (calculate-frequency-change-list "input_data/adventofcode1.txt")) ; 592
