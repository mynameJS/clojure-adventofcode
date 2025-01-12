(ns day-4
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter])
  (:require [clojure.string :as str])
  (:require [clojure.java.io]))


(defn extract-datetime
  "로그에서 첫 번째 괄호와 공백을 제외하고 'yyyy-MM-dd HH:mm'만 추출"
  [log]
  (subs log 1 17))


(defn sort-log-list
  "근무 로그를 날짜 순으로 정렬해주는 함수"
  [log-list]
  (sort (fn [log1 log2] (compare (extract-datetime log1) (extract-datetime log2))) log-list))


(defn parse-log [log]
  "로그 문자열에서 타임스탬프와 이벤트를 추출"
  (let [timestamp (subs log 1 17)
        event (str/trim (subs log 19))]
    [timestamp event]))


(defn parse-timestamp [timestamp]
  "타임스탬프 문자열을 LocalDateTime 객체로 변환"
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")]
    (LocalDateTime/parse timestamp formatter)))

(defn minutes-difference [timestamp1 timestamp2]
  "두 타임스탬프 사이의 분 차이를 계산"
  (let [time1 (parse-timestamp timestamp1)
        time2 (parse-timestamp timestamp2)]
    (-> (.until time1 time2 java.time.temporal.ChronoUnit/MINUTES)
        int)))


(defn get-minute [timestamp]
  "타임스탬프에서 분(minute)을 추출"
  (let [parsed-time (parse-timestamp timestamp)]
    (.getMinute parsed-time)))

(defn calculate-sleeping-log
  "Guard id 별로 잠을 잔 누적시간 계산"
  [log-list]
  (-> (reduce (fn [acc log]
                (let [[timestamp event] (parse-log log)]
                  (cond
                    ;; 'Guard'가 근무를 시작하면, 해당 가드 ID로 새로운 항목을 추가
                    (re-find #"Guard #(\d+)" event)
                    (let [guard-id (second (re-find #"Guard #(\d+)" event))]
                      (assoc acc :current-guard guard-id))
                    ;; 'falls asleep'이면 현재 가드 ID의 잠든 시간을 누적
                    (= "falls asleep" event)
                    (let [sleep-start timestamp]  ;; 잠든 시작 시간 저장
                      (assoc acc :sleep-start sleep-start))
                    ;; 'wakes up'이면 깨어난 시간을 반영하여 누적
                    (= "wakes up" event)
                    (let [sleep-end timestamp
                          sleep-duration (minutes-difference (acc :sleep-start) sleep-end)]
                      (update acc :sleep-times
                              (fn [times]
                                (update times (acc :current-guard)
                                        (fnil + 0) sleep-duration))))

                    :else acc)))
              {:sleep-times {}}
              log-list)
      :sleep-times))


(defn get-most-sleeping-guard-id
  "잠을 가장 많이 잔 Guard id 계산"
  [sleep-times-log]
  (let [max-sleep (apply max (vals sleep-times-log))]
    (first (filter #(= (second %) max-sleep) sleep-times-log))))


(defn find-most-sleeping-guard-id
  "lazy sequence 를 이용하여 로그를 읽고 가장 많이 잔 경비원 ID를 반환하는 함수"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [log-list (line-seq rdr)  ;; 파일에서 로그 읽기
          sorted-log-list (sort-log-list log-list)  ;; 로그를 날짜 순으로 정렬
          sleeping-log (calculate-sleeping-log sorted-log-list)  ;; 잠자는 시간 계산
          most-sleeping-guard-id (first (get-most-sleeping-guard-id sleeping-log))]  ;; 가장 많이 잔 경비원 ID
      most-sleeping-guard-id)))



;; 가장 많이 잔 id
(def most-sleeping-guard-id (find-most-sleeping-guard-id "input_data/adventofcode4.txt"))

(defn calculate-sleep-times-for-guard
  "특정 Guard id 의 수면 패턴 획득 함수"
  [log-list guard-id]
  (loop [logs log-list
         current-guard nil
         sleep-start nil
         result []]
    (if (empty? logs)
      result
      (let [[timestamp event] (parse-log (first logs))
            next-logs (rest logs)]
        (cond
          ;; 근무 시작 -> current-guard 업데이트
          (re-find #"Guard #(\d+) begins shift" event)
          (let [new-guard-id (second (re-find #"Guard #(\d+) begins shift" event))]
            (recur next-logs new-guard-id nil result))

          ;; 잠들었을 때 -> current-guard가 target guard일 경우만 처리
          (and (= "falls asleep" event) (= current-guard guard-id))
          (recur next-logs current-guard timestamp result)

          ;; 깨어났을 때 -> current-guard가 target guard일 경우만 처리
          (and (= "wakes up" event) (= current-guard guard-id))
          (recur next-logs current-guard nil
                 (conj result [(get-minute sleep-start) (get-minute timestamp)]))

          :else
          (recur next-logs current-guard sleep-start result))))))



(defn get-most-slept-minute
  "가장 많이 잠든 분 빈도 계산 함수"
  [sleep-ranges]
  (let [all-minutes (mapcat #(range (first %) (second %)) sleep-ranges)
        minute-freq (frequencies all-minutes)]
    (apply max-key val minute-freq)))



(defn find-sleep-times-for-guard
  "lazy sequence 를 이용하여 로그를 읽고 특정 Guard id 의 수면 패턴을 반환하는 함수 "
  [filename guard-id]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [log-list (line-seq rdr) ;; 파일에서 로그 읽기
          sorted-log-list (sort-log-list log-list) ;; 로그를 날짜 순으로 정렬
          sleep-times (calculate-sleep-times-for-guard sorted-log-list guard-id)] ;; 특정 Guard의 수면 패턴 계산
      sleep-times)))


(def most-slept-minute (get-most-slept-minute (find-sleep-times-for-guard "input_data/adventofcode4.txt" most-sleeping-guard-id)))


(def result (* (Long/parseLong most-sleeping-guard-id) (first most-slept-minute))) ;; 87681





;; ctrl+op+c -> enter 칼바리로드

