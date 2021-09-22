(in-package :caske2021au)


;;キー押したとき
(defun moge-keydown (hwnd wparam)
  (with-slots (state) *game*
  (let ((key (virtual-code-key wparam)))
    (case key
      (:key1
       (when (eq state :end)
	 (init-game)))
      (:escape  (send-message hwnd (const +wm-close+) nil nil))))))



(defun delete-images ()
  )

(defun delete-monster (monster stage-data)
  (with-slots (monsters) stage-data
    (setf monsters (remove monster monsters :test #'equal))))

(defun delete-tower (tower team)
  (with-slots (towers) team
    (setf towers (remove tower towers :test #'equal))))

(defun hit? (a b) ;;a bullet b: 
  (with-slots ((aposx posx) (aposy posy) (aw/2 w/2) (ah/2 h/2)) a
    (with-slots ((bposx posx) (bposy posy) (bw/2 w/2) (bh/2 h/2)) b
      (cond
	((and (eq (type-of a) 'beam)
	      (find b (tower-hitted a) :test #'equal))
	 nil)
	(t
	 (let* ((a-px (+ aposx aw/2)) (a-py (+ aposy ah/2))
		(b-px (+ bposx bw/2)) (b-py (+ bposy bh/2))) 
	   (and (<= (abs (- a-px b-px)) (+ aw/2 bw/2))
		(<= (abs (- a-py b-py)) (+ ah/2 bh/2)))))))))
;;-----------------------------------------------------------------------------

(defun update-explosion (ex team)
  (with-slots (explosion) team
    (with-slots (render-c render-time ) ex
      (incf render-c)
      (when (= render-c render-time)
	(setf explosion (remove ex explosion :test #'equal))))))


(defun hit-explosion-monster (team ex stage-data)
  (with-slots (monsters) stage-data
    (with-slots (dmg hitted) ex
      (loop :for monster :in monsters
	    :when (and (null hitted)
		       (hit? ex monster))
	      :do (with-slots (hp point) monster
		    (decf hp dmg)
		    (when (>= 0 hp)
		      (incf (p-score team) point)
		      (incf (p-money team) point)
		      (delete-monster monster stage-data)))))))

;;爆発とタワー
(defun hit-explosion-tower (team e-team ex)
  (with-slots (dmg hitted) ex
    (loop :for tower :in (p-towers e-team)
	  :when (and (null hitted)
		     (hit? ex tower))
	    :do (with-slots (hp point) tower
		  (decf hp dmg)
		  (when (>= 0 hp)
		    (incf (p-score team) point)
		    (incf (p-money team) point)
		    (delete-tower tower e-team))))))

(defun update-explosions (team e-team stage-data)
  (with-slots (explosion) team
    (loop :for ex :in explosion
	  :do (with-slots (hitted) ex
		(update-explosion ex team)
		(hit-explosion-monster team ex stage-data)
		(hit-explosion-tower team e-team ex)
		(setf hitted t)))))




;;-----------------------------------------------------------------------------
(defun delete-bullet (blt team)
  (with-slots (bullets) team
    (setf bullets (remove blt bullets :test #'equal))))

(defmethod hit-bullet-action (blt team collided)
  :delete)
  ;;(delete-bullet blt team))

(defmethod hit-bullet-action ((blt beam) team collided)
  (with-slots (bullets) team
    (with-slots (penetrate hitted) blt
      (decf penetrate)
      (push collided hitted)
      (if (>= 0 penetrate)
	  :delete
	  :hoge))))
	  ;;(delete-bullet blt team)))))

;;ミサイル
(defmethod hit-bullet-action ((blt missile) team collided)
  (with-slots (bullets explosion) team
    (with-slots (explosion-r dmg) blt
      (with-slots (posx posy w/2 h/2) collided
	(push 
	 (make-explosion :posx (- (+ posx w/2) explosion-r) :posy (- (+ posy h/2) explosion-r)
			 :render-time 40 :dmg dmg
			 :w (* explosion-r 2) :h (* explosion-r 2)
			 :w/2 explosion-r :h/2 explosion-r :img +explosion+)
	 explosion)
	:delete))))
;;(delete-bullet blt team)))))
;;ミサイル
(defmethod hit-bullet-action ((blt guided) team collided)
  (with-slots (bullets explosion) team
    (with-slots (explosion-r dmg) blt
      (with-slots (posx posy w/2 h/2) collided
	(push 
	 (make-explosion :posx (- (+ posx w/2) explosion-r) :posy (- (+ posy h/2) explosion-r)
			 :render-time 40 :dmg dmg
			 :w (* explosion-r 2) :h (* explosion-r 2)
			 :w/2 explosion-r :h/2 explosion-r :img +explosion+)
	 explosion)
	:delete))))
  
;;弾とモンスター当たった時
(defmethod hit-bullet-monster (team blt monster stage-data)
  (with-slots (dmg) blt
    (with-slots (hp point) monster
      (decf hp dmg)
      (when (>= 0 hp)
	(incf (p-score team) point)
	(incf (p-money team) point)
	(delete-monster monster stage-data)))))

;;弾とモンスター当たった時
(defmethod hit-bullet-monster (team (blt ice) monster stage-data)
  (with-slots (stop-time) blt
    (with-slots (state stop-c) monster
      (setf stop-c stop-time
	    state :ice))))

;;e-team=当てられた方 
(defmethod hit-bullet-tower (e-team blt tower)
  (with-slots (dmg) blt
    (with-slots (hp point) tower
      (decf hp dmg)
      (when (>= 0 hp)
	(setf (p-towers e-team) (remove tower (p-towers e-team) :test #'equal))
	(decf (p-score e-team) point)))))

(defmethod hit-bullet-tower (e-team (blt ice) tower)
  (with-slots (stop-time) blt
    (with-slots (state stop-c) tower
      (setf stop-c stop-time
	    state :ice))))


(defmethod update-bullet (bullet)
  (with-slots (blt-spd posx posy) bullet
    (decf posy blt-spd)))


(defun get-monster-target (bullet)
  (with-slots (stage-data) *game*
    (with-slots (monsters) stage-data
      (when monsters
	(with-slots (posx posy w/2 h/2) bullet
	  (let ((blt-px (+ posx w/2))
		(blt-py (+ posy h/2))
		(temp 9999)
		(monmon nil))
	    (loop :for monster :in monsters
		  :do (with-slots ((mposx posx) (mposy posy) (mw/2 w/2) (mh/2 h/2)) monster
			(let* ((mon-px (+ mposx mw/2))
			       (mon-py (+ mposy mh/2))
			       (x (expt (- blt-px mon-px) 2))
			       (y (expt (- blt-py mon-py) 2))
			       (diff (sqrt (+ x y))))
			  (when (> temp diff)
			    (setf temp diff
				  monmon monster)))))
	    monmon))))))

;;追尾団
(defun update-guided-bullet (bullet)
  (with-slots (blt-spd x y posx posy target w/2 h/2 vx vy) bullet
    (with-slots ((mposx posx) (mposy posy) (mw/2 w/2) (mh/2 h/2)) target
      (let* ((blt-px (+ posx w/2))
	     (blt-py (+ posy h/2))
	     (mon-px (+ mposx mw/2))
	     (mon-py (+ mposy mh/2))
	     (dx (- mon-px blt-px))
	     (dy (- blt-py mon-py))
	     (rad (atan dy dx)))
	(setf vx (* blt-spd (cos rad))
	      vy (* blt-spd (sin rad))
	      x (+ posx vx)
	      y (- posy vy)
	      posx (floor x)
	      posy (floor y))))))
;;追尾団
(defun update-guided-bullet-target-disappears (bullet)
  (with-slots (blt-spd x y posx posy w/2 h/2 vx vy) bullet
    (setf x (+ posx vx)
	  y (- posy vy)
	  posx (floor x)
	  posy (floor y))))


(defmethod update-bullet ((bullet guided))
  (with-slots (stage-data) *game*
    (with-slots (blt-spd posx posy target) bullet
      (cond
	((null target)
	 (setf target (get-monster-target bullet)))
	((find target (stage-monsters stage-data) :test #'equal)
	 (update-guided-bullet bullet))
	(t
	 (update-guided-bullet-target-disappears bullet))
	))))

(defun bullet-gamen-gai? (bullet)
  (with-slots (posx posy w h) bullet
    (or (> 0 (+ posx w))
	(> posx *field-w*)
	(> posy *player-field-h-max*)
	(> 0 (+ posy h)))))
  

(defun update-bullets (my-team enemy-team stage-data)
  (with-slots (bullets) my-team
    (loop :for bullet :in bullets
	  :do (let ((hit nil))
		(update-bullet bullet)
		(if (bullet-gamen-gai? bullet)
		    (delete-bullet bullet my-team)
		    (progn
		      (loop :for monster :in (stage-monsters stage-data)
			    :when (hit? bullet monster)
			      :do (let ((act (hit-bullet-action bullet my-team monster)))
				    (hit-bullet-monster my-team bullet monster stage-data)
				    (when (eq :delete act)
				      (delete-bullet bullet my-team))
				    (setf hit t)
				    (return)))
		      (when (null hit)
			(loop :for e-bullet :in (p-bullets enemy-team)
			      :when (hit? bullet e-bullet)
				:do (let ((act (hit-bullet-action bullet my-team e-bullet))
					  (e-act (hit-bullet-action e-bullet enemy-team bullet)))
				      (format nil "hoge")
				      (when (eq act :delete)
					(delete-bullet bullet my-team))
				      (when (eq e-act :delete)
					(delete-bullet e-bullet enemy-team))
				      (setf hit t)
				      (return))))
		      (when (null hit)
			(loop :for tower :in (p-towers enemy-team)
			      :when (hit? bullet tower)
				:do (let ((act (hit-bullet-action bullet my-team tower)))
				      (hit-bullet-tower enemy-team bullet tower)
				      (when (eq act :delete)
					(delete-bullet bullet my-team))
				      (return))))))))))

;;-----------------------------------------------------------------------------
(defmethod create-bullet ((tower canon) (team player))
  (with-slots (blt-spd dmg posx posy w) tower
    (let* ((bltw 8) (bltw/2 4)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-canon :blt-spd blt-spd :dmg dmg :color (encode-rgb 140 255 251)
		  :posx bltx
		  :posy (- posy blth)
		  :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower ice) (team player))
  (with-slots (blt-spd  posx posy w stop-time) tower
    (let* ((bltw 8) (bltw/2 4)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-ice :blt-spd blt-spd :color (encode-rgb 63 72 204)
		:posx bltx :stop-time stop-time 
		:posy (- posy blth)
		:w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower beam) (team player))
  (with-slots (blt-spd dmg posx posy w penetrate) tower
    (let* ((bltw 6) (bltw/2 3)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-beam :blt-spd blt-spd :dmg dmg :color (encode-rgb 0 168 243)
		 :posx bltx :penetrate penetrate
		 :posy (- posy blth)
		 :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower missile) (team player))
  (with-slots (blt-spd dmg posx posy w explosion-r) tower
    (let* ((bltw 10) (bltw/2 5)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-missile :blt-spd blt-spd :dmg dmg :color (encode-rgb 255 202 24)
		    :posx bltx :explosion-r explosion-r
		    :posy (- posy blth)
		    :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower wall) team)
  )

(defmethod create-bullet ((tower guided) (team player))
  (with-slots (blt-spd dmg posx posy w explosion-r) tower
    (let* ((bltw 12) (bltw/2 6)
	   (blth 12) (blth/2 6)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-guided :blt-spd blt-spd :dmg dmg :color (encode-rgb 236 28 36)
		    :posx bltx :explosion-r explosion-r
		    :posy (- posy blth)
		    :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower canon) (team enemy))
  (with-slots (blt-spd dmg posx posy w h) tower
    (let* ((bltw 8) (bltw/2 4)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-canon :blt-spd (- blt-spd) :dmg dmg :color (encode-rgb 140 255 251)
		  :posx bltx
		  :posy (+ posy h)
		  :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower beam) (team enemy))
  (with-slots (blt-spd dmg posx posy w h penetrate) tower
    (let* ((bltw 6) (bltw/2 3)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-beam :blt-spd (- blt-spd) :dmg dmg :color (encode-rgb 0 168 243)
		 :posx bltx :penetrate penetrate
		 :posy (+ posy h)
		 :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower missile) (team enemy))
  (with-slots (blt-spd dmg posx posy w h explosion-r) tower
    (let* ((bltw 10) (bltw/2 5)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-missile :blt-spd (- blt-spd) :dmg dmg :color (encode-rgb 255 202 24)
		    :posx bltx :explosion-r explosion-r
		    :posy (+ posy h)
		    :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower guided) (team enemy))
  (with-slots (blt-spd dmg posx posy w explosion-r) tower
    (let* ((bltw 12) (bltw/2 6)
	   (blth 12) (blth/2 6)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-guided :blt-spd blt-spd :dmg dmg :color (encode-rgb 236 28 36)
		    :posx bltx :explosion-r explosion-r
		    :posy (+ posy blth)
		   :w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defmethod create-bullet ((tower ice) (team enemy))
  (with-slots (blt-spd  posx posy w stop-time) tower
    (let* ((bltw 8) (bltw/2 4)
	   (blth 18) (blth/2 9)
	   (bltx (- (+ posx (floor w 2)) bltw/2)))
      (make-ice :blt-spd (- blt-spd) :color (encode-rgb 63 72 204)
		:posx bltx :stop-time stop-time 
		:posy (+ posy blth)
		:w bltw :h blth :w/2 bltw/2 :h/2 blth/2))))

(defun set-bullet (tower team)
  (with-slots (bullets) team
    (push (create-bullet tower team)
	  bullets)))


(defun inc-factory-money (tower team)
  (with-slots (inc-money) tower
    (incf (p-money team) inc-money)))

;;タワー更新
(defun update-tower (tower team)
  (with-slots (cd atk-c stop-c state) tower
    (cond
      ((eq state :ice)
       (decf stop-c)
       (when (>= 0 stop-c)
	 (setf state :alive
	       atk-c 0)))
      (t
       (incf atk-c)
       (when (<= cd atk-c)
	 (case (type-of tower)
	   (factory  (inc-factory-money tower team))
	   (wall nil)
	   (otherwise
	    (set-bullet tower team)))
	 (setf atk-c 0))))))

(defun update-towers (team)
  (with-slots (towers) team
    (loop :for tower :in towers
	  :do (update-tower tower team))))


(defun update-position (monster)
  (with-slots (posx posy dir spd) monster
    (cond
      ((= dir +right+)
       (incf posx spd))
      ((= dir +left+)
       (decf posx spd))
      ((= dir +up+)
       (decf posy spd))
      ((= dir +down+)
       (incf posy spd)))))

;;方向転換
(defun update-direction (monster stage-data)
  (with-slots (stage-arr) stage-data
    (with-slots (y x dir) monster
      (let ((cell (aref stage-arr y x)))
	(cond
	  ((= cell +d-rl+)
	   (cond
	     ((= dir +right+)
	      (if (= (random 2) 0)
		  (setf dir +right+)
		  (setf dir +down+)))
	     ((= dir +left+)
	      (if (= (random 2) 0)
		  (setf dir +left+)
		  (setf dir +down+)))
	     ((= dir +up+)
	      (if (= (random 2) 0)
		  (setf dir +right+)
		  (setf dir +left+)))))
	  ((= cell +l-ud+)
	   (cond
	     ((= dir +right+)
	      (if (= (random 2) 0)
		  (setf dir +up+)
		  (setf dir +down+)))
	     ((= dir +down+)
	      (if (= (random 2) 0)
		  (setf dir +left+)
		  (setf dir +down+)))
	     ((= dir +up+)
	      (if (= (random 2) 0)
		  (setf dir +left+)
		  (setf dir +up+)))))
	  ((= cell +r-ud+)
	   (cond
	     ((= dir +left+)
	      (if (= (random 2) 0)
		  (setf dir +up+)
		  (setf dir +down+)))
	     ((= dir +down+)
	      (if (= (random 2) 0)
		  (setf dir +right+)
		  (setf dir +down+)))
	     ((= dir +up+)
	      (if (= (random 2) 0)
		  (setf dir +right+)
		  (setf dir +up+)))))
	  ((= cell +u-r+)
	   (if (= dir +down+)
	       (setf dir +right+)
	       (setf dir +up+)))
	  ((= cell +l-u+)
	   (if (= dir +right+)
	       (setf dir +up+)
	       (setf dir +left+)))
	  ((= cell +d-r+)
	   (if (= dir +up+)
	       (setf dir +right+)
	       (setf dir +down+)))
	  ((= cell +l-d+)
	   (if (= dir +right+)
	       (setf dir +down+)
	       (setf dir +left+))))))))

;;モンスター削除
(defun delete-monster (monster stage-data)
  (with-slots (monsters) stage-data
    (setf monsters (remove monster monsters :test #'equal))))

;;アニメ
(defun update-monster-anime (monster)
  (with-slots (anime-c anime-img) monster
    (incf anime-c)
    (when (> anime-c 15)
      (incf anime-img)
      (when (> anime-img *monster-anime-max*)
	(setf anime-img 0))
      (setf anime-c 0))))

(defun update-monster (monster stage-data)
  (with-slots (x y posx posy dir spd state stop-c point) monster
    (cond
      ((eq state :ice)
       (decf stop-c)
       (when (<= stop-c 0)
	 (setf state :alive)))
      (t
       (update-position monster)
       (when (and (eq state :wait)
		  (>= posx 0))
	 (setf state :alive))
       (if (and (eq state :alive)
		(< posx 0))
	   (progn
	     (decf (p-score *player*) point)
	     (delete-monster monster stage-data))
		  
	   (progn
	     (update-monster-anime monster)
	     
	     (when (and (>= posx 0)
			(>= posy 0)
			(zerop (mod posx 32))
			(zerop (mod posy 32)))
	       (setf x (floor posx 32)
		     y (floor (- posy *enemy-field-h-max*) 32))
	       (if (and (> *x-max* x -1)
			(> *y-max* y -1))
		   (update-direction monster stage-data)
		   (progn
		     (decf (p-score *player*) point)
		     (delete-monster monster stage-data))))))))))

(defun update-monsters (stage-data)
  (with-slots (monsters) stage-data
    (loop :for monster :in monsters
	  :do (update-monster monster stage-data))))


;;--------------------------------------------------------------------
(defun random-select-enemy-tower ()
  (let ((tower (random 110)))
    (cond
      ((>= 20 tower 0) (make-canon :posx 0 :posy 0 :hp 3 :dmg 1 :lv 1 :point 30
				     :cd 160 :blt-spd 2 :cost 100 :atk-c 150
				     :w 32 :h 16 :w/2 16 :h/2 8 :img +canon+))
      ((>= 30 tower 21) (make-ice :posx 0 :posy 0 :hp 3 :dmg 0 :lv 1 :point 30
				 :cd 180 :blt-spd 2 :cost 140 :atk-c 170 :stop-time 120
				 :w 32 :h 16 :w/2 16 :h/2 8 :img +ice+))
      ((>= 50 tower 31) (make-beam :posx 0 :posy 0
				   :hp 3 :dmg 1 :cd 120 :atk-c 110 :blt-spd 2
				   :penetrate 2 :lv 1 :cost 150 :point 40
				   :w 32 :h 16 :w/2 16 :h/2 8 :img +beam+))
      ((>= 70 tower 51) (make-missile :posx 0 :posy 0 :point 50
					 :lv 1 :hp 5 :dmg 2 :blt-spd 1
					 :explosion-r 32 :cd 220 :atk-c 210 :cost 200
					 :w 32 :h 16 :w/2 16 :h/2 8 :img +missile+))
      ((>= 90 tower 71) (make-factory :posx 0 :posy 0 :point 30
					 :cd 260 :inc-money 10
					 :lv 1 :hp 5 :cost 180
					 :w 32 :h 16 :w/2 16 :h/2 8 :img +factory+))
      ((>= 95 tower 91) (make-wall :posx 0 :posy 0 :hp 5 :cost 10 :lv 1
				   :w 32 :h 16 :w/2 16 :h/2 8 :img +wall+))
      ((>= 110 tower 96) (make-guided :posx 0 :posy 0 :point 50
				       :lv 1 :hp 5 :dmg 2 :blt-spd 3
				       :explosion-r 32 :cd 250 :atk-c 230 :cost 300
				       :w 32 :h 16 :w/2 16 :h/2 8 :img +guided+)))))

;;タワー被ってないか				
(defun check-over-enemy-tower-pos (new-tower team)
  (with-slots (towers) team
    (loop :for tower :in towers
	  :do (with-slots (posx posy w h) tower
		(when (hit? tower new-tower)
		  (return t))))))

(defun set-tower-common (tower)
  (with-slots (money towers) *enemy*
    (decf money (tower-cost tower))
    (push tower towers)))

(defmethod set-tower (tower)
  (with-slots (money towers) *enemy*
    (setf (tower-posx tower) (random (- *field-w* 32))
	  (tower-posy tower) (random (- *enemy-field-h-max* 16)))
    (when (null (check-over-enemy-tower-pos tower *enemy*))
      (set-tower-common tower))))

(defmethod set-tower ((tower wall))
  (with-slots (money towers) *enemy*
    (when towers
      (let* ((hoge (remove-if #'(lambda (moge) (or (<= (- *enemy-field-h-max* 32) (tower-posy moge))
						   (eq (type-of moge) 'wall)))
			      towers)))
	(when hoge
	  (let* ((cover-tower (nth (random (length hoge)) hoge)))
	    (setf (tower-posx tower) (tower-posx cover-tower)
		  (tower-posy tower)  (+ (tower-posy cover-tower) 17))
	    (when (null (check-over-enemy-tower-pos tower *enemy*))
	      (set-tower-common tower))))))))
  


(defun enemy-put-tower ()
  (with-slots (money towers) *enemy*
    (let* ((tower (random-select-enemy-tower)))
      (when (and (>= money (tower-cost tower)))
		 ;;(and (null towers) (eq (type-of tower) 'wall)))
	(set-tower tower)))))

(defun enemy-action ()
  (enemy-put-tower))

(defun update-enemy-action ()
  (with-slots (act-cd act-c) *enemy*
    (incf act-c)
    (when (= act-cd act-c)
      (setf act-c 0)
      (enemy-action))))

;;wave追加
(defun update-wave (stage-data)
  (with-slots (monsters wave-num ) stage-data
    (cond
      ((and (> wave-num 0)
	    (null monsters))
       (create-monsters))
      ((= wave-num 0)
       (with-slots (all-stage state) *game*
	 (setf all-stage (cdr all-stage))
	 (if all-stage
	     (create-stage-data (car all-stage))
	     (setf state :end)))))))

(defmethod upgrade-tower-individual ((tower canon))
  )

(defmethod upgrade-tower-individual ((tower beam))
  (with-slots (lv penetrate) tower
    (when (zerop (mod lv 3))
      (incf penetrate))))

(defmethod upgrade-tower-individual ((tower missile))
  (with-slots (lv explosion-r) tower
    ;;(when (evenp lv)
    (incf explosion-r 12)))

(defmethod upgrade-tower-individual ((tower guided))
  (with-slots (lv explosion-r) tower
    ;;(when (evenp lv)
      (incf explosion-r 12)))

(defmethod upgrade-tower-individual ((tower factory))
  (with-slots (lv inc-money cd) tower
    (incf inc-money 10)
    (if (evenp lv)
	(decf cd 10)
	(decf cd 10))))

(defmethod upgrade-tower-individual ((tower ice))
  (with-slots (stop-time lv) tower
    (when (oddp lv)
      (incf stop-time 20))))

;;タワーアップグレード
(defmethod upgrade-tower-base (tower team)
  (with-slots (lv hp dmg cost cd blt-spd) tower
    (when (>= (p-money team) cost)
      (upgrade-tower-individual tower)
      (when (zerop (mod lv 3))
	(incf blt-spd))
      (cond
	((oddp lv)
	 (incf dmg))
	((evenp lv)
	 (incf hp)
	 (decf cd 10)))
      (incf lv)
      (decf (p-money team) cost)
      (incf cost (floor (* cost 0.2))))))

;;壁アップグレード
(defmethod upgrade-tower-base ((tower wall) team)
  (with-slots (lv hp cost) tower
    (when (>= (p-money team) cost)
      (incf hp 2)
      (incf lv)
      (decf (p-money team) cost)
      (incf cost (floor (* cost 1.2))))))

(defun update-money ()
  (when (>= (game-frame *game*) 60)
    (incf (p-money *player*))
    (incf (p-money *enemy*) 10)
    (setf (game-frame *game*) 0)))


(defun update-game ()
  (with-slots (stage-data) *game*
    (update-monsters stage-data)
    (update-enemy-action)
    (update-towers *player*)
    (update-towers *enemy*)
    (update-bullets *player* *enemy*  stage-data)
    (update-bullets *enemy* *player* stage-data)
    (update-explosions *player* *enemy* stage-data)
    (update-explosions *enemy* *player* stage-data)
    (update-wave stage-data)
    (update-money)))


;;マウス操作
(defun update-mouse-action ()
  (with-slots (left right selected smpl-selected (mposx posx) (mposy posy)) *mouse*
    (when (and right
	       (or smpl-selected selected)) 
      (setf smpl-selected nil selected nil))
    (when (and left selected
	       (null smpl-selected)
	       (and (>= *upgrade-btn-x2* mposx *upgrade-btn-x1*)
		    (>= *upgrade-btn-y2* mposy *upgrade-btn-y1*))
	       (find selected (p-towers *player*) :test #'equal))
      (upgrade-tower-base selected *player*))
    (when (and left
	       (null smpl-selected))
      (loop :for tower :in *tower-samples*
	    :do (with-slots (posx posy w h) tower
		  (when (and (>= (+ posx w) mposx posx)
    			     (>= (+ posy h) mposy posy)
			     (>= (p-money *player*) (tower-cost tower)))
		    (setf smpl-selected
			  (case (type-of tower)
			    (canon (copy-canon tower))
			    (beam (copy-beam tower))
			    (missile (copy-missile tower))
			    (factory (copy-factory tower))
			    (wall (copy-wall tower))
			    (ice (copy-ice tower))
			    (guided (copy-guided tower))))
		    (return-from update-mouse-action))))
      (loop :for tower :in (append (player-towers *player*) (enemy-towers *enemy*))
	    :do ;;(when tower
		(with-slots (posx posy w h) tower
		  (when (and (>= (+ posx w) (mouse-posx *mouse*) posx)
			     (>= (+ posy h) (mouse-posy *mouse*) posy))
		    (setf selected tower)
		    (return-from update-mouse-action)))))
    
    (when (and left smpl-selected
	       
	       (and (>= (- *field-w* 14) (mouse-posx *mouse*) 14)
		    (>= *player-field-h-max* (mouse-posy *mouse*) *player-field-h-min*))
	       (null (check-over-enemy-tower-pos smpl-selected *player*)))
      (push smpl-selected (player-towers *player*))
      (decf (p-money *player*) (tower-cost smpl-selected))
      (setf smpl-selected nil))))


(defun main-game-loop (hwnd)
  (with-slots (state) *game*
    (cond
      ((eq state :playing)
       (update-game)
       (update-mouse-action)
       (init-mouse-state)
       (incf (game-frame *game*))
       (invalidate-rect hwnd nil nil)))))

(defun moge-timer (hwnd)
  (main-game-loop hwnd))

(defun moge-paint (hwnd)
  (with-double-buffering-2 (hdc hwnd)
    (with-compatible-dc (hmemdc hdc)
      (set-bk-mode hdc :transparent)
      (render-game hdc hmemdc))))





(defun moge-create (hwnd)
  (set-timer :hwnd hwnd
	     :elapse 17
	     :replace-timer 1)
  (set-font)
  (setf *c-rect* (get-client-rect hwnd))
  (set-client-size hwnd)
  (setf *c-rect* (get-client-rect hwnd))
  (init-mouse)
  (load-images)
  (init-samples)
  (init-game)
  )



;;ウィンドウサイズ変更時に画像拡大縮小する
(defun change-screen-size (lp)
  (let* ((change-w (loword lp))
	 (change-h (hiword lp)))
    (setf *change-screen-w* change-w
	  *change-screen-h* change-h
	  *mouse-hosei-x* (/ *change-screen-w*  (rect-right *c-rect*))
	  *mouse-hosei-y* (/ *change-screen-h*  (rect-bottom *c-rect*)))))

;;マウス移動
(defun mouse-move (lparam)
  (with-slots (posx posy smpl-selected) *mouse*
    (setf posy (/ (hiword lparam) *mouse-hosei-y*)
          posx (/ (loword lparam) *mouse-hosei-x*))
    (when smpl-selected
      (setf (tower-posx smpl-selected) (- (floor posx) (floor (tower-w smpl-selected) 2))
	    (tower-posy smpl-selected) (- (floor posy) (floor (tower-h smpl-selected) 2))))))

;;proc
(defwndproc moge-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (moge-create hwnd))
    ((const +wm-paint+)
     (moge-paint hwnd))
    ((const +wm-size+)
     (change-screen-size lparam))
    ((const +wm-close+)
     (destroy-window hwnd))
    ((const +wm-timer+)
     (moge-timer hwnd))
    ((const +wm-keydown+)
     (moge-keydown hwnd wparam))
    ;; ((const +wm-keyup+)
    ;;  (moge-keyup wparam))
    ((const +wm-lbuttondown+)
     (setf (mouse-left *mouse*) t))
    ((const +wm-rbuttondown+)
     (setf (mouse-right *mouse*) t))
    ((const +wm-lbuttonup+)
     (setf (mouse-left *mouse*) nil))
    ((const +wm-rbuttonup+)
     (setf (mouse-right *mouse*) nil))
    ((const +wm-mousemove+)
     (mouse-move lparam))
    ((const +wm-destroy+)
     (delete-images)
     (delete-font)
     (post-quit-message)))
  (default-window-proc hwnd msg wparam lparam))


;;メイン
(defun moge ()
  (setf *random-state* (make-random-state t))
  (register-class "MOGE" (callback moge-wndproc)
		  :styles (logior-consts +cs-hredraw+ +cs-vredraw+)
                  :cursor (load-cursor :arrow)
                  :background (create-solid-brush (encode-rgb 0 0 0)))
  (let ((hwnd (create-window "MOGE"
                             :window-name "もげぞうの迷宮"
                             :ex-styles  (logior-consts +ws-ex-composited+) ;;透明
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width 700 :height 800))
        (msg (make-msg)))
    ;;(init-game)
    (show-window hwnd)
    (update-window hwnd)
    (do ((done nil))
        (done)
      (let ((r (get-message msg)))
        (cond
          ((zerop r) (setf done t))
          (t
           (translate-message msg)
           (dispatch-message msg)))))))
