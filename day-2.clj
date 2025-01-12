(ns day-2
  (:require [clojure.java.io]))


(defn filter-by-count
  "frequencies 결과에서 주어진 count 조건을 만족하는 문자만 필터링"
  [freqs target-count]
  (filter (fn [[_ count]]
            (= count target-count))
          freqs))


(defn count-by-condition
  "파일의 각 줄에서 주어진 condition 에 맞게 출현한 알파벳을 각각 카운트"
  [filename, condition1, condition2]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [lines (line-seq rdr)]
      (reduce
       (fn [[condition1-count condition2-count] line]
         (let [freqs (frequencies line)
               has-condition1 (seq (filter-by-count freqs condition1))
               has-condition2 (seq (filter-by-count freqs condition2))]
           [(if has-condition1 (inc condition1-count) condition1-count)
            (if has-condition2 (inc condition2-count) condition2-count)]))
       [0 0]
       lines))))

(def count-pairs-and-triples (count-by-condition "input_data/adventofcode2.txt" 2 3))


(def result  (* (first count-pairs-and-triples) (second count-pairs-and-triples))) ;; 5000
