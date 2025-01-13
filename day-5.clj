(ns day-5
  (:require [clojure.string :as str])
  (:require [clojure.java.io]))



(defn react-polymer
  "반응하는 폴리머를 제거한 후 남은 폴리머의 길이를 계산하는 함수"
  [polymer]
  (loop [remaining polymer
         stack []]
    (if (empty? remaining)
      (count stack)  ;; 스택에 남은 문자들의 개수를 반환
      (let [current (first remaining)
            last (first stack)]
        (if (and last (not= current last) ;; "a", "A" 비교시 둘다 true
                 (= (str/lower-case current) (str/lower-case last)))
          (recur (rest remaining) (rest stack))  ;; "a" "A"
          (recur (rest remaining) (cons current stack)))))))  ;; "a" "B"


(defn read-polymer-file
  "파일에서 폴리머 데이터를 lazy sequence로 읽어와 함수를 실행"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [polymer (str (line-seq rdr))]
      (react-polymer polymer))))



(def result (read-polymer-file "input_data/adventofcode5.txt")) ;; 10890
