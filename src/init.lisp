(in-package :caske2021au)

(defun init-game  ()
  (setf *game* (make-game :state :playing :stage 0 :all-stage (copy-tree *stage-data-list*))
	*player* (make-player :money 500)
	*enemy* (make-enemy :money 600 :act-cd 180 :act-c 0))
  (create-stage-data (car (game-all-stage *game*))))


(defun init-mouse ()
  (setf *mouse* (make-mouse)))

(defun init-mouse-state ()
  (with-slots (left right) *mouse*
    (setf left nil right nil)))


(defun init-samples ()
  (let ((posx (+ *field-w* 5))
	(x-interval (+ *tower-w* 5))
	(y (+ *player-field-h-min* 20))
	(y2 (+ *player-field-h-min* 45)))
    (setf *tower-samples* (list (make-canon :posx posx :posy y :hp 3 :dmg 1 :lv 1 :point 30
					    :cd 160 :blt-spd 2 :cost 100 :atk-c 150
					    :w 32 :h 16 :w/2 16 :h/2 8 :img +canon+)
				(make-beam :posx (+ posx x-interval) :posy y :point 40
					   :hp 3 :dmg 1 :cd 120 :atk-c 110 :blt-spd 2
					   :penetrate 2 :lv 1 :cost 150
					   :w 32 :h 16 :w/2 16 :h/2 8 :img +beam+)
				(make-missile :posx (+ posx (* x-interval 2)) :posy y
					      :lv 1 :hp 5 :dmg 2 :blt-spd 1 :point 50
					      :explosion-r 32 :cd 220 :atk-c 210 :cost 250
					      :w 32 :h 16 :w/2 16 :h/2 8 :img +missile+)
				(make-factory :posx (+ posx (* x-interval 3)) :posy y
					      :cd 260 :inc-money 10 :point 30
					      :lv 1 :hp 5 :cost 250
					      :w 32 :h 16 :w/2 16 :h/2 8 :img +factory+)
				(make-wall :posx (+ posx (* x-interval 4)) :posy y
					   :lv 1 :hp 5 :cost 50
					   :w 32 :h 16 :w/2 16 :h/2 8 :img +wall+)
				(make-guided :posx posx :posy y2
					      :lv 1 :hp 5 :dmg 2 :blt-spd 3 :point 50
					      :explosion-r 32 :cd 220 :atk-c 210 :cost 600
					     :w 32 :h 16 :w/2 16 :h/2 8 :img +guided+)
				(make-ice :posx (+ posx x-interval) :posy y2 :hp 3 :dmg 0 :lv 1 :point 30
					    :cd 180 :blt-spd 2 :cost 140 :atk-c 160 :stop-time 120
					    :w 32 :h 16 :w/2 16 :h/2 8 :img +ice+)))))


    

(defun create-monster (monster)
  (with-slots (hp point spd img lv) monster
    (cond
      ((= img +slime+)
       (setf hp 4 point 20))
      ((= img +bubble+)
       (setf hp 5 point 25))
      ((= img +brigand+)
       (setf hp 6 point 30))
      ((= img +orc+)
       (setf hp 7 point 35))
      ((= img +hydra+)
       (setf hp 8 point 40))
      ((= img +skelton+)
       (setf hp 9 point 45))
      ((= img +dragon+)
       (setf hp 11 point 70))
      ((= img +yote1+)
       (setf hp 3 point 200 spd 8)))
    (setf hp (* hp (+ 1 (/ lv 10)))
	  point (floor (* point (+ 1 (/ lv 10)))))
    monster))
     

(defun create-monsters ()
  (with-slots (stage-data) *game*
    (with-slots (wave-type wave-num startx starty monsters lv
		 max-monster-num min-monster-num) stage-data
      (let ((wave (loop :repeat (max min-monster-num (random max-monster-num))
			:collect (weightpick (wave-list wave-type)))))
      (setf monsters (loop :for x :from -32 :downto -999 :by 48
			   :for y = (* (+ starty 4) 32)
			   :for w :in wave
			   :collect (let ((monster (make-monster :w 32 :h 32 :w/2 16 :h/2 16 :lv lv
								 :state :wait
								 :img w :posx x :posy y :dir +right+ :spd 1)))
				      (create-monster monster)))
	    wave-num (- wave-num 1))))))

(defun create-stage-data (data)
  (with-slots (stage-data) *game*
    (setf stage-data (make-stage :stage-arr (make-array (list 14 15) :initial-contents
							(getf data :arr))
				 :wave-num (getf data :wave-num)
				 :wave-type (getf data :wave-type)
				 :max-monster-num (getf data :max-monster-num)
				 :min-monster-num (getf data :min-monster-num)
				 :lv (getf data :lv)
				 :startx (getf data :startx)
				 :starty (getf data :starty)))
    (create-monsters)))


;;クライアント領域を*client-w* *client-h*に設定
(defun set-client-size (hwnd)
  (let* ((rc (get-client-rect hwnd))
         (rw (get-window-rect hwnd))
         (new-w (+ *client-w* (- (- (rect-right rw) (rect-left rw))
                               (- (rect-right rc) (rect-left rc)))))
         (new-h (+ *client-h* (- (- (rect-bottom rw) (rect-top rw))
                               (- (rect-bottom rc) (rect-top rc))))))
    (set-window-pos hwnd nil 0 0 new-w new-h '(:no-move :no-zorder))))
