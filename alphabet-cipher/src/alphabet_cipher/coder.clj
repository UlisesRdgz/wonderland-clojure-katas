;; Author: Ulises Rodriguez Garcia

(ns alphabet-cipher.coder)

;; Define the alphabet.
(def alphabet "abcdefghijklmnopqrstuvwxyz")

;; Function to repeat the keyword the number of times until the message 
;; size is reached.
(defn repeat-key [keyword message]
  (let [n (count message)]
    (map (fn [i] (nth keyword (mod i (count keyword)))) (range n))))

;; Obtain the position of the character in the alphabet, transforming 
;; it to ACSII code.
(defn character-position [c]
  (- (int c) 97))

;; Obtain the position of each character of the string s.
(defn string-position [s]
  (map character-position s))

;; Function to increase the character of the string according to the position.
(defn increase [s message]
  (map (fn [c p] (str (nth alphabet (mod (+ p (character-position c)) (count alphabet))))) s (string-position message)))

;; Function to decrease the character of the string according to the position.
(defn decrease [s message]
  (map (fn [c p] (str (nth alphabet (mod (- p (character-position c)) (count alphabet))))) s (string-position message)))

;; Find the difference in position of each character in the string.
(defn difference [s1 s2]
  (map (fn [c1 c2] (mod (- (character-position c1) (character-position c2)) 26)) s1 s2))

;; Find the keyword, with the possibility of being repeated.
(defn find-key [n]
  (apply str (map (fn [n] (nth alphabet n)) n)))

(defn encode [keyword message]
  (apply str (increase (repeat-key keyword message) message)))

(defn decode [keyword message]
  (apply str (decrease (repeat-key keyword message) message)))

;; Verify which substring is correct.
(defn substring [keyword message cipher index]
  (if (= cipher (encode (subs keyword 0 index) message))
    (subs keyword 0 index)
    (substring keyword message cipher (+ index 1))))

(defn decipher [cipher message]
  (substring (find-key (difference cipher message)) message cipher 1))

