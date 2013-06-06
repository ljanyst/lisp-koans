;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


"you need to write the triangle method"

(define-condition triangle-error  (error)
  ((reason :initarg :reason :initform :no-reason :reader reason)))

; check if the line segments are valid
(defun triangle-is-valid (a b c)
  (let* ((segments (list a b c))
	 (sorted-segments (sort segments #'<)))

    ; the line segments need to be of positive length
    (loop for x in segments do
	 (if (>= 0 x)
	     (return-from triangle-is-valid
	       (values nil :invalid-segment-length))))

    ; check if the segments are of the right size to make a triangle
    (if (<= (+ (first sorted-segments) (second sorted-segments))
	    (third sorted-segments))
	(return-from triangle-is-valid
	  (values nil :invalid-segment-set)))
    (values t :none)))

; count same segments
(defun triangle-count-unique-sides (a b c)
  (let ((sides (list a b c))
	(sides-hash (make-hash-table)))
    (loop for s in sides do
	 (setf (gethash s sides-hash) t))
    (hash-table-count sides-hash)))

; check triangle
(defun triangle (a b c)
  (let ((valid nil)
	(reason nil)
	(same-sides nil))
    (multiple-value-setq (valid reason) (triangle-is-valid a b c))
    (if (not valid)
	(error 'triangle-error :reason reason))
    (setf same-sides (triangle-count-unique-sides a b c))
    (cond ((= same-sides 3) :scalene)
	  ((= same-sides 2) :isosceles)
	  (t :equilateral))))

(define-test test-equilateral-triangles-have-equal-sides
    (assert-equal :equilateral (triangle 2 2 2))
    (assert-equal :equilateral (triangle 10 10 10)))


(define-test test-isosceles-triangles-have-two-equal-sides
    (assert-equal :isosceles (triangle 3 4 4))
    (assert-equal :isosceles (triangle 4 3 4))
    (assert-equal :isosceles (triangle 4 4 3))
    (assert-equal :isosceles (triangle 10 10 2)))


(define-test test-scalene-triangles-have-no-equal-sides
    (assert-equal :scalene (triangle 3 4 5))
    (assert-equal :scalene (triangle 10 11 12))
    (assert-equal :scalene (triangle 5 4 2)))


(define-test test-illegal-triangles-throw-exceptions
    (assert-error 'triangle-error (triangle 0 0 0))
    (assert-error 'triangle-error (triangle 3 4 -5))
    (assert-error 'triangle-error (triangle 1 1 3))
    (assert-error 'triangle-error (triangle 2 4 2)))
