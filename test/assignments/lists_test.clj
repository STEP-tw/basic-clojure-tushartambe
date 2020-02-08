(ns assignments.lists-test
  (:require [clojure.test :refer :all]
            [assignments.lists :refer :all]))

(deftest map-test
  (testing "identity with single coll"
    (is (= [1 2 3] (map' identity [1 2 3]))))
  (testing "with multiple coll"
    (is (= [2 4 6] (map' + [1 2 3] [1 2 3])))))

(deftest reduce-test
  (testing "arity 2"
    (is (= 15 (reduce' + [1 2 3 4 5]))))
  (testing "arity 3"
    (is (= 15 (reduce' + 1 [2 3 4 5])))))

(deftest filter-test
  (testing "even? predicate"
    (is (= [2 4 6] (filter' even? [1 2 3 4 5 6 9])))))

(deftest count-test
  (testing "sequence length"
    (is (= 5 (count' [1 2 3 4 5]))))
  (testing "nil"
    (is (= 0 (count' nil))))
  (testing "[]"
    (is (= 0 (count' []))))
  (testing "map"
    (is (= 2 (count {:one 1 :two 2}))))
  (testing "string"
    (is (= 6 (count "abcdef")))))

(deftest reverse-test
  (testing "seqable collection"
    (is (= [5 4 3 2 1] (reverse' [1 2 3 4 5])))))

(deftest every-test
  (testing "true condition"
    (is (true? (every?' even? [2 4 6 8]))))
  (testing "false condition"
    (is (false? (every?' even? [2 4 9 6 8])))))

(deftest some-test
  (testing "true condition"
    (is (true? (some?' even? [1 5 3 4]))))
  (testing "false condition"
    (is (false? (some?' even? [1 5 7 9])))))

(deftest ascending-test
  (testing "true condition"
    (is (true? (ascending? [1 2 3 4 5 6]))))
  (testing "false condition"
    (is (false? (ascending? [1 2 3 2 1 0])))))

(deftest index-of-test
  (testing "element present in collection"
    (is (= 4 (index-of [1 2 3 4 5 6] 5))))
  (testing "element not-present in collection"
    (is (= -1 (index-of [1 2 3 4 5 6] 9)))))

(deftest palindrome?-test
  (testing "true condition"
    (is (true? (palindrome? ["n" "a" "m" "a" "n"]))))
  (testing "false condition"
    (is (false? (palindrome? ["k" "a" "n" "n" "u"])))))

(deftest sqr-of-the-first-test
  (testing "non-empty collection"
    (is (= [16 16 16] (sqr-of-the-first [4 5 6])))))

(deftest double-up-test
  (testing "single dimensional collection"
    (is (= [1 1 2 2 3 3] (double-up [1 2 3]))))
  (testing "two dimensional collection"
    (is (= [[1 2] [1 2] [3 4] [3 4]] (double-up [[1 2] [3 4]])))))

(deftest transpose-test
  (testing "non-empty collection"
    (is (= [[\a \d] [\b \e] [\c \f]] (transpose [[\a \b \c] [\d \e \f]])))))

(deftest cross-product-test
  (testing "non-empty collection"
    (is (= [[1 4] [1 3] [1 5] [2 4] [2 3] [2 5] [3 4]] (cross-product [1 2 3] [4 3 5])))))

(deftest third-or-fifth-test
  (testing "non-empty collection"
    (is (= [1 4 6 7] (third-or-fifth [1 2 3 4 5 6 7 8 9])))))

(deftest union-test
  (testing "non-empty collection"
    (is (= [1 2 3 4 5 6 7 8] (union [1 2 3 4] [5 6 1 2 7 8])))))

(deftest difference-test
  (testing "non-empty collection"
    (is (= [5 6 7 8] (difference [1 2 3 4] [5 6 1 2 7 8])))))

(deftest muted-thirds-test
  (testing "non-empty collection"
    (is (= [1 2 0 4 5 0 7 8 0] (muted-thirds [1 2 3 4 5 6 7 8 9])))))

(deftest points-around-origin-test
  (testing "non-empty collection"
    (is (= [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] (points-around-origin)))))

(deftest split-comb-test
  (testing "odd length collection"
    (is (= '(1 3 2 4 5) (split-comb [1 2 3 4 5]))))
  (testing "even length collection"
    (is (= '(1 4 2 5 3 6) (split-comb [1 2 3 4 5 6])))))

(deftest russian-dolls-test
  (testing "non-empty collection"
    (is (= [[[1]] [[2]] [[3]]] (russian-dolls [1 2 3] 3)))))

(deftest max-three-digit-sequence-test
  (testing "middle term max"
    (is (= [2 -1 2] (max-three-digit-sequence [1 2 -1 2 0]))))
  (testing "end term max"
    (is (= [-1 2 3] (max-three-digit-sequence [1 2 -1 2 3])))))

(deftest sum-of-adjacent-digits-test
  (testing "non-empty collection"
    (is (= [3 5 7] (sum-of-adjacent-digits [1 2 3 4])))))

(deftest dedupe'-test
  (testing "non-empty collection"
    (is (= [1 2 3 1 3] (dedupe' [1 1 2 3 1 1 3 3])))))

(deftest distinct-test
  (testing "non-empty collection"
    (is (= [1 2 4 5 3] (distinct' [1 2 1 2 4 5 3 5])))))

(deftest validate-sudoku-grid-test
  (testing "correct grid"
    (is (true? (validate-sudoku-grid
                 [[4 3 5 2 6 9 7 8 1]
                  [6 8 2 5 7 1 4 9 3]
                  [1 9 7 8 3 4 5 6 2]
                  [8 2 6 1 9 5 3 4 7]
                  [3 7 4 6 8 2 9 1 5]
                  [9 5 1 7 4 3 6 2 8]
                  [5 1 9 3 2 6 8 7 4]
                  [2 4 8 9 5 7 1 3 6]
                  [7 6 3 4 1 8 2 5 9]]))))
  (testing "incorrect grid"
    (is (false? (validate-sudoku-grid
                  [[4 4 5 2 6 9 7 8 1]
                   [6 8 2 5 7 1 4 9 3]
                   [1 9 7 8 3 4 5 6 2]
                   [8 2 6 1 9 5 3 4 7]
                   [3 7 4 6 8 2 9 1 5]
                   [9 5 1 7 4 3 6 2 8]
                   [5 1 9 3 2 6 8 7 4]
                   [2 4 8 9 5 7 1 3 6]
                   [7 6 3 4 1 8 2 5 9]])))))