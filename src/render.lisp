(in-package :caske2021au)


(defun new-trans-blt (x y x-src y-src w-src h-src w-dest h-dest hdc hmemdc)
  (transparent-blt hdc x y hmemdc x-src y-src :width-source w-src
		   :height-source h-src
		   :width-dest w-dest :height-dest h-dest
		   :transparent-color (encode-rgb 0 255 0)))


(defun render-monster (monster hdc hmemdc)
  (with-slots (posx posy img anime-img) monster
    (select-object hmemdc *monsters-img*)
    (new-trans-blt posx posy (* anime-img 32) (* img 32) 32 32 32 32 hdc hmemdc)))


;;枠付きボタン
(defun create-render-button (x1 x2 y1 y2 str strx stry hdc hmemdc &key (font *font20*))
  (with-slots (posx posy) *mouse*
    (select-object hdc font)
    (if (and (>= x2 posx x1)
	     (>= y2 posy y1))
	(progn (select-object hdc (get-stock-object :white-brush))
	       (rectangle hdc x1 y1 x2 y2)
	       (set-text-color hdc (encode-rgb 0 0 0)))
	(progn (select-object hmemdc *waku-img*)
	       (new-trans-blt x1 y1 0 0 128 128 (- x2 x1) (- y2 y1) hdc hmemdc)
	       (set-text-color hdc (encode-rgb 255 255 255))))
    (text-out hdc (format nil "~a" str) strx stry)))




(defun render-monsters (monsters hdc hmemdc)
  (loop :for monster :in monsters
	:do (render-monster monster hdc hmemdc)))


(defun render-stage (hdc hmemdc stage)
  (select-object hmemdc *road-img*)
  (loop :for y :from 0 :below 14
	:do (loop :for x :from 0 :below 15
		  :do (let ((cell (aref stage y x)))
			(new-trans-blt (* x 32) (+ (* y 32) *enemy-field-h-max*)
				       (* cell 32) 0 32 32 32 32 hdc hmemdc)))))


(defun render-border-line (hdc)
  (let ((pen (get-stock-object :white-pen)))
    (select-object hdc pen)
    (move-to hdc (1+ *enemy-field-w*) 0)
    (line-to hdc (1+ *enemy-field-w*) *screen-h*)
    ;;tower-explain
    (move-to hdc (1+ *enemy-field-w*) 165)
    (line-to hdc *screen-w* 165)
    ;;sample
    (move-to hdc (1+ *enemy-field-w*) *player-field-h-min*)
    (line-to hdc *screen-w* *player-field-h-min*)
    ;;status
    (move-to hdc (1+ *enemy-field-w*) 495)
    (line-to hdc *screen-w* 495)
    (move-to hdc (1+ *enemy-field-w*) 420)
    (line-to hdc *screen-w* 420)))

(defun fuchidori (x y str hdc color)
  (set-text-color hdc color)
  (text-out hdc str (+ x 2) y)
  (text-out hdc str (- x 2) y)
  (text-out hdc str x (+ y 2))
  (text-out hdc str x (- y 2)))

(defun render-status (hdc team str team-y score-y money-y color fuchicolor)
  (with-slots (score money) team
    (let* ((x (+ *field-w* 10)))
      (select-object hdc *font20*)
      (fuchidori x team-y str hdc fuchicolor)
      (fuchidori x score-y (format nil "SCORE : ~d" score) hdc fuchicolor)
      (fuchidori x money-y (format nil "MONEY : ~d" money) hdc fuchicolor)
      (set-text-color hdc color)
      (text-out hdc str x team-y)
      (text-out hdc (format nil "SCORE : ~d" score) x score-y)
      (text-out hdc (format nil "MONEY : ~d" money) x money-y))))


(defun render-tower (hdc hmemdc tower img-src)
  (with-slots (posx posy img w h) tower
    (select-object hmemdc img-src)
    (new-trans-blt posx posy (* img w) 0 w h w h hdc hmemdc)))


;;タワーの説明
(defmethod tower-explain-text ((tower canon) hdc)
  (with-slots (lv blt-spd hp dmg cd cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 5))
	(text-out hdc "種類 : キャノン" x y)
	(text-out hdc (format nil "  Lv  : ~d" lv) x (hoge y))
	(text-out hdc (format nil "  HP : ~d" hp) x (hoge y))
	(text-out hdc (format nil "威力 : ~d" dmg) x (hoge y))
	(text-out hdc (format nil "  CD : ~d" cd) x (hoge y))
	(text-out hdc (format nil "値段 : ~d" cost) x (hoge y))
	))))

;;iceの説明
(defmethod tower-explain-text ((tower ice) hdc)
  (with-slots (lv blt-spd hp cd cost stop-time) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 5))
	(text-out hdc "種類 : アイス" x y)
	(text-out hdc (format nil "   Lv   : ~d" lv) x (hoge y))
	(text-out hdc (format nil "   HP  : ~d" hp) x (hoge y))
	(text-out hdc (format nil "停止時間 : ~d" stop-time) x (hoge y))
	(text-out hdc (format nil "   CD  : ~d" cd) x (hoge y))
	(text-out hdc (format nil " 値段  : ~d" cost) x (hoge y))
	))))

(defmethod tower-explain-text ((tower beam) hdc)
  (with-slots (lv blt-spd hp dmg cd penetrate cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 5))
      (text-out hdc "種類 : ビーム" x y)
      (text-out hdc (format nil " Lv  : ~d" lv) x (hoge y))
      (text-out hdc (format nil " HP : ~d" hp) x (hoge y))
      (text-out hdc (format nil "威力 : ~d" dmg) x (hoge y))
      (text-out hdc (format nil "貫通 : ~d" penetrate) x (hoge y))
      (text-out hdc (format nil " CD : ~d" cd) x (hoge y))
      (text-out hdc (format nil "値段 : ~d" cost) x (hoge y))
      ))))

(defmethod tower-explain-text ((tower missile) hdc)
  (with-slots (lv blt-spd hp dmg cd explosion-r cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 5))
      (text-out hdc "   種類   : ミサイル" x y)
      (text-out hdc (format nil "   Lv      : ~d" lv) x (hoge y))
      (text-out hdc (format nil "   HP     : ~d" hp) x (hoge y))
      (text-out hdc (format nil "   威力   : ~d" dmg) x (hoge y))
      (text-out hdc (format nil "爆発範囲: ~d" explosion-r) x (hoge y))
      (text-out hdc (format nil "    CD    : ~d" cd) x (hoge y))
      (text-out hdc (format nil "   値段    : ~d" cost) x (hoge y))
      ))))

(defmethod tower-explain-text ((tower factory) hdc)
  (with-slots (lv blt-spd hp dmg cd inc-money cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 5))
      (text-out hdc "  種類 : 工場" x y)
      (text-out hdc (format nil "   Lv   : ~d" lv) x (hoge y))
      (text-out hdc (format nil "   HP  : ~d" hp) x (hoge y))
      (text-out hdc (format nil "増加金: ~d" inc-money) x (hoge y))
      (text-out hdc (format nil "   CD  : ~d" cd) x (hoge y))
      (text-out hdc (format nil "  値段  : ~d" cost) x (hoge y))
	))))

(defmethod tower-explain-text ((tower wall) hdc)
  (with-slots (lv hp cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 5))
      (text-out hdc "  種類 : 壁" x y)
      (text-out hdc (format nil "   Lv   : ~d" lv) x (hoge y))
      (text-out hdc (format nil "   HP  : ~d" hp) x (hoge y))
      (text-out hdc (format nil "  値段  : ~d" cost) x (hoge y))
	))))

(defmethod tower-explain-text ((tower guided) hdc)
  (with-slots (lv blt-spd hp dmg cd explosion-r cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 5))
      (text-out hdc " 種類 : 誘導ミサイル" x y)
      (text-out hdc (format nil "   Lv       : ~d" lv) x (hoge y))
      (text-out hdc (format nil "   HP      : ~d" hp) x (hoge y))
      (text-out hdc (format nil "   威力    : ~d" dmg) x (hoge y))
      (text-out hdc (format nil "爆発範囲: ~d" explosion-r) x (hoge y))
      (text-out hdc (format nil "    CD     : ~d" cd) x (hoge y))
      (text-out hdc (format nil "   値段     : ~d" cost) x (hoge y))
      ))))



(defun render-tower-explain (tower hdc)
  (select-object hdc *font20*)
  (set-text-color hdc (encode-rgb 255 255 255))
  (tower-explain-text tower hdc))

;;共通部分
(defun tower-upgrade-explain-text-common (x y tower hdc)
  (with-slots (lv hp dmg cd cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (cond
	((oddp lv)
	 (text-out hdc (format nil "  HP : ~d" hp) x (hoge y))
	 (text-out hdc (format nil "威力 : ~d → ~d" dmg (1+ dmg)) x (hoge y))
	 (text-out hdc (format nil "  CD : ~d" cd) x (hoge y)))
	((evenp lv)
	 (text-out hdc (format nil "  HP : ~d → ~d" hp (1+ hp)) x (hoge y))
	 (text-out hdc (format nil "威力 : ~d" dmg) x (hoge y))
	 (text-out hdc (format nil "  CD : ~d → ~d" cd (- cd 10)) x (hoge y))))
      (text-out hdc (format nil "値段 : ~d → ~d" cost (floor (* cost 1.2))) x (hoge y)))))

;;タワーの説明
(defmethod tower-upgrade-explain-text ((tower canon) hdc)
  (with-slots (lv blt-spd hp dmg cd cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 190))
      (text-out hdc "種類 : キャノン" x y)
      (text-out hdc (format nil "  Lv  : ~d → ~d" lv (1+ lv)) x (hoge y))
      (tower-upgrade-explain-text-common x y tower hdc)))))

(defmethod tower-upgrade-explain-text ((tower beam) hdc)
  (with-slots (lv blt-spd hp dmg cd cost penetrate) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 190))
	(text-out hdc "種類 : ビーム" x y)
	(text-out hdc (format nil "  Lv  : ~d → ~d" lv (1+ lv)) x (hoge y))
	(if (zerop (mod lv 3))
	    (text-out hdc (format nil "貫通 : ~d → ~d" penetrate (1+ penetrate)) x (hoge y))
	    (text-out hdc (format nil "貫通 : ~d" penetrate) x (hoge y)))
        (tower-upgrade-explain-text-common x y tower hdc)))))
;;工場
(defmethod tower-upgrade-explain-text ((tower factory) hdc)
  (with-slots (lv hp cd cost inc-money) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 190))
	(text-out hdc "種類 : 工場" x y)
	(text-out hdc (format nil "  Lv  : ~d → ~d" lv (1+ lv)) x (hoge y))
	(text-out hdc (format nil "増加金 : ~d→~d" inc-money (+ inc-money 10)) x (hoge y))
	(text-out hdc (format nil " CD : ~d→~d" cd (- cd 20)) x (hoge y))
	(text-out hdc (format nil "値段 : ~d→~d" cost (floor (* cost 1.2))) x (hoge y))))))
	
;;ミサイル
(defmethod tower-upgrade-explain-text ((tower missile) hdc)
  (with-slots (lv blt-spd hp dmg cd cost explosion-r) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 190))
	(text-out hdc "種類 : ミサイル" x y)
	(text-out hdc (format nil "  Lv  : ~d→~d" lv (1+ lv)) x (hoge y))
	(if (zerop (mod lv 3))
	    (text-out hdc (format nil "爆発範囲: ~d→~d" explosion-r (+ explosion-r 12))
		      x (hoge y))
	    (text-out hdc (format nil "爆発範囲: ~d" explosion-r) x (hoge y)))
        (tower-upgrade-explain-text-common x y tower hdc)))))

;;誘導ミサイル
(defmethod tower-upgrade-explain-text ((tower guided) hdc)
  (with-slots (lv blt-spd hp dmg cd cost explosion-r) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 190))
	(text-out hdc "種類 : 誘導ミサイル" x y)
	(text-out hdc (format nil "  Lv  : ~d→~d" lv (1+ lv)) x (hoge y))
	(if (zerop (mod lv 3))
	    (text-out hdc (format nil "爆発範囲: ~d→~d" explosion-r (+ explosion-r 12))
		      x (hoge y))
	    (text-out hdc (format nil "爆発範囲: ~d" explosion-r) x (hoge y)))
        (tower-upgrade-explain-text-common x y tower hdc)))))

;;壁
(defmethod tower-upgrade-explain-text ((tower wall) hdc)
  (with-slots (lv hp cost) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 190))
	(text-out hdc "種類 : 壁" x y)
	(text-out hdc (format nil "  Lv : ~d→~d" lv (1+ lv)) x (hoge y))
	(text-out hdc (format nil "  HP : ~d→~d" hp (+ hp 2)) x (hoge y))
	(text-out hdc (format nil " cost : ~d→~d" cost (floor (* cost 1.2))) x (hoge y))))))

;;ice
(defmethod tower-upgrade-explain-text ((tower ice) hdc)
  (with-slots (lv hp cost stop-time) tower
    (macrolet ((hoge (n) `(incf ,n 22)))
      (let ((x (+ *field-w* 5))
	    (y 190))
	(text-out hdc "種類 : アイス" x y)
	(text-out hdc (format nil "   Lv  : ~d→~d" lv (1+ lv)) x (hoge y))
	(if (evenp lv)
	    (text-out hdc (format nil "   HP  : ~d→~d" hp (+ hp 1)) x (hoge y))
	    (text-out hdc (format nil "   HP  : ~d" hp) x (hoge y)))
	(if (oddp lv)
	    (text-out hdc (format nil "停止時間 : ~d→~d" stop-time (+ stop-time 20)) x (hoge y))
	    (text-out hdc (format nil "停止時間 : ~d" stop-time) x (hoge y)))
	(text-out hdc (format nil "  cost  : ~d→~d" cost (floor (* cost 1.2))) x (hoge y))))))


;;選択中のタワー　アップグレード用説明
(defun render-tower-upgrade-explain (tower hdc hmemdc)
  (select-object hdc *font20*)
  (set-text-color hdc (encode-rgb 255 255 255))
  (text-out hdc "選択中" (+ *field-w* 50) 165)
  (tower-upgrade-explain-text tower hdc)
  (when (find tower (p-towers *player*))
    (create-render-button *upgrade-btn-x1* *upgrade-btn-x2*
			  *upgrade-btn-y1* *upgrade-btn-y2*
			  "UPGRADE" (+ *upgrade-btn-x1* 5) *upgrade-btn-y1* hdc hmemdc :font *font20*)))

;;マウス重なったタワーに枠つける
(defun render-mouse-over-tower-waku (hdc hmemdc tower)
  (with-slots (posx posy w h) tower
    ;;(with-slots ((mposx posx) (mposy posy)) *mouse*
      (when (and (>= (+ posx w) (mouse-posx *mouse*) posx)
    		 (>= (+ posy h) (mouse-posy *mouse*) posy))
	(select-object hmemdc *waku-img*)
	(when (null (mouse-selected *mouse*))
	  (render-tower-explain tower hdc))
	(new-trans-blt posx  (- posy 2) 0 0 128 128 32 26 hdc hmemdc))))

;;
(defun render-selected-tower-waku (hdc hmemdc)
  (with-slots (selected) *mouse*
    (when selected
      (with-slots (posx posy) selected
	(select-object hmemdc *waku-img*)
	(render-tower-explain selected hdc)
	(render-tower-upgrade-explain selected hdc hmemdc)
	(new-trans-blt posx  (- posy 2) 0 0 128 128 32 26 hdc hmemdc)))))
;;
(defun render-towers (hdc hmemdc towers img-src)
  (loop :for tower :in towers
	:do (render-tower hdc hmemdc tower img-src)
	(render-mouse-over-tower-waku hdc hmemdc tower)))


(defun render-all-towers (hdc hmemdc)
  (render-towers hdc hmemdc *tower-samples* *towers-img*)
  (render-towers hdc hmemdc (player-towers *player*) *towers-img*)
  (render-towers hdc hmemdc (enemy-towers *enemy*) *enemy-towers-img*))
		 

(defun render-bullet (hdc bullet)
  (with-slots (posx posy w h color) bullet
    (let* ((pen (create-pen :null color 1))
	   (c (create-solid-brush color)))
      (rectangle hdc posx posy (+ posx w) (+ posy h))
      ;;(select-object hdc hold-brush)
     ;; (select-object hdc hold-pen)
      (delete-object pen)
      (delete-object c))))

(defun render-bullets (hdc team)
  (with-slots (bullets) team
    (loop :for bullet :in bullets 
	  :do (render-bullet hdc bullet))))

;;選択中のたわー描画 マウスカーソル追従
(defun render-sample-selected-tower (hdc hmemdc)
  (with-slots (smpl-selected) *mouse*
    (when smpl-selected
      (render-tower hdc hmemdc smpl-selected *towers-img*))))


(defun render-explosion (ex hdc hmemdc)
  (with-slots (posx posy w h) ex
    (select-object hmemdc *explosion-img*)
    (new-trans-blt posx posy 0 0 96 96 w h hdc hmemdc)))

(defun render-explosions (team hdc hmemdc)
  (with-slots (explosion) team
    (loop :for ex :in explosion 
	  :do (render-explosion ex hdc hmemdc))))



(defun render-test (hdc)
  (select-object hdc *font20*)
  (set-text-color hdc (encode-rgb 255 255 255)))
  ;; (text-out hdc (format nil "mousex ~d" (mouse-posx *mouse*))
  ;; 	      *field-w* 300)
  ;; (text-out hdc (format nil "mousey ~d" (mouse-posy *mouse*))
  ;; 	      *field-w* 330)
  ;;(select-object hdc font40)
  ;;(text-out hdc "enemy field" 100 30)
  ;;(text-out hdc "player field" 100 630)
(defun render-end-gamen (hdc)
  (let ((p-score (p-score *player*))
	(e-score  (p-score *enemy*)))
    (select-object hdc *font70*)
    (set-text-color hdc (encode-rgb 55 55 255))
    (text-out hdc (format nil "あなたのスコア : ~d" p-score)  10 20)
    (set-text-color hdc (encode-rgb 255 55 55))
    (text-out hdc (format nil "相手のスコア　 : ~d" e-score) 10 110)
    (set-text-color hdc (encode-rgb 255 255 255))
    (cond
      ((> p-score e-score)
       (text-out hdc "あなたの勝ちです" 10 300))
      ((< p-score e-score)
       (text-out hdc "あなたの負けです" 10 300))
      (t (text-out hdc "引き分けです" 10 300)))
    (set-text-color hdc (encode-rgb 55 255 55))
    (text-out hdc "  1    もう一度やる" 100 500) 
    (text-out hdc "esc  おわる" 100 600)))
  


(defun render-game (hdc hmemdc)
  (with-slots (stage-data state) *game*
    (cond
      ((eq state :playing)
       (with-slots (stage-arr monsters) stage-data
	 (render-stage hdc hmemdc stage-arr)
	 (render-status hdc *player* "PLAYER" 500 525 550 (encode-rgb 255 255 255) (encode-rgb 0 0 255))
	 (render-status hdc *enemy* "ENEMY" 420 445 470 (encode-rgb 255 255 255) (encode-rgb 255 0 0))
	 (render-border-line hdc)
	 (render-all-towers hdc hmemdc)
	 (render-selected-tower-waku hdc hmemdc)
	 (render-sample-selected-tower hdc hmemdc)
	 (render-bullets hdc *player*)
	 (render-bullets hdc *enemy*)
	 (render-explosions *player* hdc hmemdc)
	 (render-explosions *enemy* hdc hmemdc)
	 ;;(render-test hdc)
	 (render-monsters monsters hdc hmemdc)))
      ((eq state :end)
       (render-end-gamen hdc)))))
