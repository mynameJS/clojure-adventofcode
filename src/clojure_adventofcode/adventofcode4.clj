(ns clojure-adventofcode.adventofcode4
  (:require [clojure.java.io]))
  
  
;; (:require [clojure.java.io])
;; (:require [clojure.string :as str])
;; (clojure.string/split log-list #"\n")
;; adventofcode
;; 4. 휴식 기록


(defn read-log-list
  "log list 를 lazy sequence로 읽어오는 함수"
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (line-seq rdr)))

;; (def log-list (read-log-list "src/clojure_adventofcode/input_data/adventofcode4.txt"))
;; (def log-list (slurp "src/clojure_adventofcode/input_data/adventofcode4.txt"))
  

(def log-list
  ["[1518-11-01 00:00] Guard #10 begins shift"
   "[1518-11-01 00:05] falls asleep"
   "[1518-11-01 00:25] wakes up"
   "[1518-11-01 00:30] falls asleep"
   "[1518-11-01 00:55] wakes up"
   "[1518-11-02 00:00] Guard #99 begins shift"
   "[1518-11-02 00:40] falls asleep"
   "[1518-11-02 00:50] wakes up"
   "[1518-11-03 00:05] Guard #10 begins shift"
   "[1518-11-03 00:25] falls asleep"
   "[1518-11-03 00:55] wakes up"])



(defn extract-datetime 
  "로그에서 첫 번째 괄호와 공백을 제외하고 'yyyy-MM-dd HH:mm'만 추출"
  [log]
  (subs log 1 17))

;; 근무 로그를 날짜 순으로 정렬
(defn sort-log-list
  "근무 로그를 날짜 순으로 정렬해주는 함수"
  [log-list]
  (sort (fn [log1 log2] (compare (extract-datetime log1) (extract-datetime log2))) log-list))

    
(defn parse-log
  "로그 데이터를 파싱해서 날짜, 시간, Guard ID, 이벤트를 추출"
  [log]
  (let [[_ datetime event] (re-matches #"\[(.*)\] (.*)" log) 
        guard-id (when-let [match (re-find #"#(\d+)" event)]  
                    (second match))]
    {:datetime datetime
     :guard-id (if guard-id guard-id nil)  
     :event (if guard-id "shift" event)}))  



(defn parse-time-to-minutes
  "시간을 'HH:mm' 형식으로 받아서 분 단위로 변환"
  [time-str]
  (if time-str
    (let [[_ hour minute] (re-matches #"(\d{2}):(\d{2})" time-str)]
      (+ (* (Long/parseLong hour) 60) (Long/parseLong minute)))
    0))  ;; time-str이 nil일 경우 0으로 처리



(defn calculate-duration
  "두 시간을 받아서 분 단위로 차이를 계산. 시간대가 넘어가면 처리"
  [start-time end-time]
  (let [start-minutes (parse-time-to-minutes start-time)
        end-minutes (parse-time-to-minutes end-time)]
    (if (> end-minutes start-minutes)
      (- end-minutes start-minutes)  ;; 시간이 같은 날짜 내에 있을 경우
      (+ (- end-minutes start-minutes) 1440))))  ;; 1440분 = 24시간


(defn most-asleep-guard
  "가장 많이 잠을 잔 경비원의 ID를 구하는 함수"
  [log-list]
  (let [sorted-logs (sort (fn [log1 log2] (compare (extract-datetime log1) (extract-datetime log2))) log-list)  ;; 날짜 순 정렬
        guard-sleep-times (atom {})  ;; 경비원별 잠을 잔 시간 기록
        current-guard-id (atom nil)
        sleep-start-time (atom nil)]  ;; 현재 잠 시작 시간

    ;; 로그를 순차적으로 처리하면서 잠을 자고 일어난 시간을 계산
    (doseq [log sorted-logs]
      (let [{:keys [datetime guard-id event]} (parse-log log)]
        (cond
          ;; 경비원 시작 시
          (= event "shift") (reset! current-guard-id guard-id)

          ;; 잠을 자는 중
          (= event "falls asleep") (reset! sleep-start-time datetime)

          ;; 잠에서 깨어날 때
          (= event "wakes up") 
          (let [duration (calculate-duration @sleep-start-time datetime)]
            (swap! guard-sleep-times update @current-guard-id (fnil + 0) duration)))))

    ;; 가장 많이 잠을 잔 경비원 찾기
    (let [max-sleep-guard (apply max-key val @guard-sleep-times)]  ;; apply 부분 수정
      (key max-sleep-guard))))  ;; 마지막 괄호 수정



;; (defn group-logs-by-guard
;;   "정렬된 로그를 순차적으로 처리하여 경비원 별로 잠을 잔 시간을 기록"
;;   [logs]
;;   (loop [logs logs
;;          guard-sleep-data {}  ;; 각 경비원 별 잠자는 시간을 기록
;;          current-guard nil
;;          sleep-start nil]
;;     (if (empty? logs)
;;       guard-sleep-data
;;       (let [log (first logs)
;;             {guard-id :guard-id event :event datetime :datetime} log]
;;         (cond
;;           (= event "shift")  ;; 새로운 경비원 근무 시작
;;           (recur (rest logs) guard-sleep-data guard-id nil)

;;           (= event "falls asleep")  ;; 잠든 시간 기록
;;           (recur (rest logs) guard-sleep-data current-guard datetime)

;;           (= event "wakes up")  ;; 깬 시간 기록 후 잠 잔 시간 계산
;;           (let [duration (calculate-duration sleep-start datetime)
;;                 updated-guard-sleep (update guard-sleep-data current-guard
;;                                              (fn [sleep-time]
;;                                                (+ (or sleep-time 0) duration)))]
;;             (recur (rest logs) updated-guard-sleep current-guard nil)))))))
