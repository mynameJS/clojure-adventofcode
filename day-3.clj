(ns day-3
  (:require [clojure.java.io]))


(defn parse-claim
  "문자열 claim을 파싱하여 좌표와 크기를 반환"
  [claim]
  (let [[_ x y w h] (re-matches #"#\d+ @ (\d+),(\d+): (\d+)x(\d+)" claim)]
    {:x (parse-long x)
     :y (parse-long y)
     :w (parse-long w)
     :h (parse-long h)}))



(defn create-square-coordinates
  "사각형 영역의 모든 좌표를 생성"
  [{:keys [x y w h]}]
  (for [i (range x (+ x w))
        j (range y (+ y h))]
    [i j]))



(defn count-overlap-square-inches
  "겹치는 원단의 인치를 계산하는 함수."
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [claims (line-seq rdr)
          coordinates (mapcat #(create-square-coordinates (parse-claim %)) claims)
          frequencies (frequencies coordinates)]
      (count (filter #(>= % 2) (vals frequencies))))))


(def result (count-overlap-square-inches "input_data/adventofcode3.txt")) ;; 1011196


