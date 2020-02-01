(ns assignments.conditions-test
  (:require [clojure.test :refer :all]
            [assignments.conditions :refer :all]))

(deftest safe-division
  (testing "non zero denominator"
    (is (= 2 (safe-divide 4 2))))
  (testing "zero denominator"
    (is (nil? (safe-divide 3 0)))))

(deftest informative-division
  (testing "non zero denominator"
    (is (= 2 (informative-divide 4 2))))
  (testing "zero denominator"
    (is (= :infinite (informative-divide 3 0)))))

(deftest test-harishchandra
  (testing "testing truthy values"
    (is (= 3 (harishchandra 3))))
  (testing "testing falsy values"
    (is (= nil (harishchandra false)))))

(deftest test-yudishtira
  (testing "testing truthy values"
    (is (= "truthy-value" (yudishtira "truthy-value"))))
  (testing "testing falsy values"
    (is (= :ashwathama (yudishtira false)))))

(deftest test-duplicate-first
  (testing "should return nil if collection is empty"
    (is (= nil (duplicate-first []))))
  (testing "should return duplicated first element"
    (is (= [1 1 2 3] (duplicate-first [1 2 3])))))

(deftest test-five-point-someone
  (testing "testing y = 5"
    (is (= :chetan-bhagat (five-point-someone 6 5))))
  (testing "testing x = 5"
    (is (= :satan-bhagat (five-point-someone 5 0))))
  (testing "testing x > y"
    (is (= :greece (five-point-someone 45 32))))
  (testing "universe test"
    (is (= :universe (five-point-someone 2 2)))))

(deftest test-repeat-and-truncate
  (testing "should return empty sequence when given empty list"
    (is (= '() (repeat-and-truncate '() true true 0))))
  (testing "repeat and truncate"
    (is (= '(0 1 2 3 0 1) (repeat-and-truncate (range 4) true true 6)))))

(deftest test-order-in-words
  (testing "when x is y and y is greater than z"
    (is (= [:x-greater-than-y :y-greater-than-z] (order-in-words 4 3 2))))
  (testing "when x is y and z is greater than x"
    (is (= [:x-greater-than-y :z-greater-than-x] (order-in-words 4 3 5))))
  (testing "when z is greater than x"
    (is (= [:z-greater-than-x] (order-in-words 2 3 4)))))

(deftest test-zero-aliases
  (testing "when zero is provided"
    (is (= :zero (zero-aliases 0))))
  (testing "when [] is provided"
    (is (= :empty (zero-aliases []))))
  (testing "when {} is provided"
    (is (= :empty-map (zero-aliases {}))))
  (testing "when empty string is provided"
    (is (= :empty-string (zero-aliases ""))))
  (testing "when non zero value is provided"
    (is (= :not-zero (zero-aliases 1))))
  (testing "when #{} is provided"
    (is (= :empty-set (zero-aliases #{})))))

(deftest test-zero-separated-palindrome
  (testing "when collection is provided"
    (is (= '(4 3 2 0 2 3 4) (zero-separated-palindrome [1 2 3])))))