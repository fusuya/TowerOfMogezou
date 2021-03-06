(in-package :caske2021au)
;; 7:start 8:goal
;;(+kara+ 1+l-r+ 2+u-d+ 3+u-r+ 4+l-u+ 5+d-r+ 6+l-d+ 7:rlud 8:r-ud 9:l-ud 10:u-lr 11:d-rl 12:d-l)
;;(my-enum 0+brigand+ 1+dragon+ 2+hydra+ 3+yote1+  4+orc+ 5+slime+ 6+bubble+ 7+skelton+)

(defparameter *path-dict*
  '((1 . (1 4 6)) (2 . (2 3 10)) (3 . (1 4 6)) (4 . (8 5 12)) (5 . (1 4 6)) (6 . (2 3 10))
    (7 . (7 9 11)) (8 . (8 5 12)) (9 . (8 5 12)) (10 . (7 9 11)) (11 . (2 3 10)) (12 . (7 9 11))))

(defun wave-list (lst)
  (case lst
    (:slime-main-list '((5 . 100) (6 . 10) (7 . 5) (0 . 5)
			(4 . 5) (2 . 5) (1 . 1) (3 . 1)))
    (:bubble-brigand-list '((5 . 10) (6 . 80) (7 . 5) (0 . 50)
			(4 . 5) (2 . 5) (1 . 1) (3 . 1)))
    (:orc-main-list '((5 . 15) (6 . 10) (7 . 5) (0 . 5)
		      (4 . 88) (2 . 5) (1 . 1) (3 . 1)))
    (:skelton-hydra-list '((5 . 15) (6 . 10) (7 . 50) (0 . 5)
			   (4 . 5) (2 . 50) (1 . 1) (3 . 1)))
    (:all '((5 . 50) (6 . 50) (7 . 50) (0 . 50)
	    (4 . 50) (2 . 50) (1 . 50) (3 . 1)))))

(defparameter *stage1*
  ;;(make-array (list 14 15) :initial-contents
  '(:arr  ((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (1 1 1 1 1 1 1 6 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 2 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 2 0 0 0 0 0 0 0)
	   (0 0 5 1 1 1 1 4 0 0 0 0 0 0 0)
	   (0 0 2 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 3 1 1 1 1 1 1 1 1 1 1 1 1)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    :lv 1
    :startx 0
    :starty 3
    :max-monster-num 10
    :min-monster-num 5
    :wave-num 6
    :wave-type :slime-main-list))

(defparameter *stage2*
  ;;(make-array (list 14 15) :initial-contents
  '(:arr  ((0 0  0 0 0 0 0 0 0 0 0 5 1 1 1)
	   (0 0  0 0 0 0 0 0 0 0 0 2 0 0 0)
	   (0 0  5 1 1 1 1 1 6 0 0 2 0 0 0)
	   (0 0  2 0 0 0 0 0 8 1 1 4 0 0 0)
	   (1 1  9 0 0 0 0 0 2 0 0 0 0 0 0)
	   (0 0  2 0 0 0 0 0 2 0 0 0 0 0 0)
	   (0 0  2 0 0 5 1 1 4 0 0 0 0 0 0)
	   (0 0  2 0 0 2 0 0 0 0 0 0 0 0 0)
	   (0 0  2 0 0 2 0 0 0 0 0 0 0 0 0)
	   (0 0  3 1 1 4 0 0 0 0 0 0 0 0 0)
	   (0 0  0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0  0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0  0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0  0 0 0 0 0 0 0 0 0 0 0 0 0))
    :lv 2
    :startx 0
    :starty 4
    :max-monster-num 12
    :min-monster-num 6
    :wave-num 8
    :wave-type :bubble-brigand-list))

(defparameter *stage3*
  '(:arr  ((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 5 1 1 1 1 1 1 1 1 1 1)
	   (0 0 0 0 2 0 0 0 0 0 0 0 0 0 0)
	   (1 1 6 0 3 1 6 0 0 0 0 0 0 0 0)
	   (0 0 2 0 0 0 2 0 0 0 0 0 0 0 0)
	   (0 0 2 0 0 0 2 0 0 0 0 0 0 0 0)
	   (0 0 3 1 6 0 2 0 0 0 0 0 0 0 0)
	   (0 0 0 0 2 0 2 0 0 0 0 0 0 0 0)
	   (0 0 0 0 2 0 2 0 0 0 0 0 0 0 0)
	   (0 0 0 0 2 0 2 0 0 0 0 0 0 0 0)
	   (0 0 0 0 3 1 4 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    :lv 3
    :startx 0
    :starty 4
    :max-monster-num 13
    :min-monster-num 7
    :wave-num 8
    :wave-type :orc-main-list))

(defparameter *stage4*
  '(:arr  ((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 5 1 1 1 1 1 1 1 1)
	   (0 0 5 1 1 1 4 0 0 0 0 0 0 0 0)
	   (0 0 2 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 2 0 0 0 0 0 0 0 0 0 0 0 0)
	   (1 1 9 0 0 0 0 5 1 1 1 1 1 1 1)
	   (0 0 2 0 0 0 0 2 0 0 0 0 0 0 0)
	   (0 0 2 0 0 0 0 2 0 0 0 0 0 0 0)
	   (0 0 3 1 1 1 1 9 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 2 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 3 1 1 1 1 1 1 1)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    :lv 4
    :startx 0
    :starty 6
    :max-monster-num 14
    :min-monster-num 8
    :wave-num 9
    :wave-type :skelton-hydra-list))

(defparameter *stage5*
  '(:arr  ((0 0  0 0 0 0 0  0 0 0 0  0 0 0 0)
	   (0 0  0 0 0 0 0  0 0 0 0  0 0 0 0)
	   (0 0  0 0 0 0 5  1 1 1 1 11 1 1 1)
	   (1 1 11 1 1 1 9  0 0 0 0  2 0 0 0)
	   (0 0  2 0 0 0 2  0 0 0 0  2 0 0 0)
	   (0 0  2 0 0 0 2  0 0 0 0  2 0 0 0)
	   (0 0  2 0 0 0 3 11 1 1 1  4 0 0 0)
	   (0 0  2 0 0 0 0  2 0 0 0  0 0 0 0)
	   (0 0  2 0 0 0 0  2 0 0 0  0 0 0 0)
	   (0 0  3 1 1 1 1  9 0 0 0  0 0 0 0)
	   (0 0  0 0 0 0 0  2 0 0 0  0 0 0 0)
	   (0 0  0 0 0 0 0  3 1 1 1  1 1 1 1)
	   (0 0  0 0 0 0 0  0 0 0 0  0 0 0 0)
	   (0 0  0 0 0 0 0  0 0 0 0  0 0 0 0))
    :lv 5
    :startx 0
    :starty 3
    :max-monster-num 16
    :min-monster-num 9
    :wave-num 10
    :wave-type :all))

(defparameter *stage-data-list*
  (list  *stage1* *stage2* *stage3* *stage4* *stage5*))


(defun assocdr (path)
  (cdr (assoc path *path-dict*)))

(defun get-next-path-list (now-path)
  (assocdr now-path))

;;(+kara+ 1+l-r+ 2+u-d+ 3+u-r+ 4+l-u+ 5+d-r+ 6+l-d+ 7:r-l 8:d-u 9:r-u 10:u-l 11:r-d 12:d-l)

