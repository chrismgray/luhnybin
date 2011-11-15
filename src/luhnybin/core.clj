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

(luhn-check (reverse [5 6 7 8]))

(defn digit? [char]
  (contains? (set "0123456789") char))

(defn separator? [char]
  (contains? (set " -") char))

(defn digit->int [char]
  (Integer/parseInt (str char)))

(defn replace-digits
  "Takes a (reversed) string, an index, and a number of digits to replace.
   Returns the string with num-digits digits replaced by Xs starting at idx."
  [s idx num-digits]
  (str (apply str (vec (take idx s))) ; take returns a lazy sequence, so force it with vec
       (apply str
        (first (reduce (fn [[s num-found] idx]
                         (if (= num-found num-digits)
                           [s num-found]
                           [(assoc s idx (if (digit? (s idx))
                                           \X
                                           (s idx)))
                            (if (digit? (s idx))
                              (inc num-found)
                              num-found)]))
                       [(vec (subs s idx)) 0]
                       (range (count (subs s idx))))))))

(defn handle-string-at-index
  "Takes a (reversed) string and an index.  If the next 14, 15, or 16 digits
   (possibly separated by spaces or dashes) pass the Luhn check, replace the
   digits by Xs and return the string."
  [s idx]
  (let [digits
        (->> s
             (#(subs % idx))
             (take-while #(or (digit? %) (separator? %)))
             (remove separator?)
             (take 16)
             (map digit->int))]
    (if (< (count digits) 14)
      s
      (cond
       (luhn-check (take 14 digits))
       (replace-digits s idx 14)
       (luhn-check (take 15 digits))
       (replace-digits s idx 15)
       (luhn-check (take 16 digits))
       (replace-digits s idx 16)
       :else
       s))))

(defn handle-input-line
  "Takes a line of input and replaces the possible credit-card numbers with Xs."
  [line]
  (reduce handle-string-at-index (apply str (reverse line)) (range (count line))))

(defn handle-input-lines
  "Takes multiple lines of input and replaces all the possible credit-card numbers with Xs"
  [lines]
  (map #(apply str (reverse %)) (map handle-input-line (string/split-lines lines))))

(defn -main [& args]
  (doall (map println (handle-input-lines (slurp *in*)))))