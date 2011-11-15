(ns luhnybin.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn luhn-check
  "Takes a sequence of digits and determines whether they pass the Luhn test.
   The sequence must be in order from right to left."
  [digits]
  (->> digits
       (partition 2 2 (repeat 0))
       (mapcat #(vector (first %) (* 2 (second %))))
       (reduce #(+ %1 (int (/ %2 10)) (int (mod %2 10))))
       (#(mod % 10))
       (= 0)))

(defn digit? [char]
  (contains? (set "0123456789") char))

(defn separator? [char]
  (contains? (set " -") char))

(defn digit->int [char]
  (Integer/parseInt (str char)))

(defn replace-digits
  "Takes a (reversed) string, an index, and a number of digits to replace.
   Returns the string with num-digits digits replaced by Xs starting at idx."
  [s [first-idx last-idx]]
  (str (apply str (vec (take first-idx s))) ; take returns a lazy sequence, so force it with vec
       (apply str
              (reduce (fn [s letter]
                        (if (digit? letter)
                          (conj s \X)
                          (conj s letter)))
                      []
                      (vec (subs s first-idx (inc last-idx)))))
       (subs s (inc last-idx))))

(defn handle-string-at-index
  "Takes a (reversed) string and an index.  If the next 14, 15, or 16 digits
   (possibly separated by spaces or dashes) pass the Luhn check, return a vector
   of the index in the string and the last index in the string which contains a
   digit that passes the Luhn check.  Otherwise, return nil."
  [s idx]
  (let [digits
        (->> s
             (#(subs % idx))
             (map-indexed #(vector %2 (+ idx %1)))
             (take-while #(or (digit? (first %)) (separator? (first %))))
             (remove (comp separator? first))
             (take 16)
             (map (fn [[dig i]] [(digit->int dig) i])))]
    (if (< (count digits) 14)
      nil
      (cond
       (and (>= (count digits) 16) (luhn-check (map first (take 16 digits))))
       [idx (second (last digits))]
       (and (>= (count digits) 15) (luhn-check (map first (take 15 digits))))
       [idx (second (last (take 15 digits)))]
       (luhn-check (map first (take 14 digits)))
       [idx (second (last (take 14 digits)))]
       :else
       nil))))

(defn handle-input-line
  "Takes a line of input and replaces the possible credit-card numbers with Xs."
  [line]
  (let [line (apply str (reverse line))
        replace-between-indexes (remove nil? (map handle-string-at-index (repeat line) (range (count line))))]
    (reduce replace-digits line replace-between-indexes)))

(defn -main [& args]
  (let [not-empty (atom true)]
    (while @not-empty
      (let [line (read-line)]
        (if (empty? line)
          (swap! not-empty not)
          (println (apply str (reverse (handle-input-line line)))))))))

