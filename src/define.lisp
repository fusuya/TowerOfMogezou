(in-package :caske2021au)


(defmacro my-enum (&rest names)
  `(progn
     ,@(loop for i from 0
             for name in names
        collect `(defparameter ,name ,i))))

;; (defparameter *data-root* (asdf:system-source-directory 'caske2021au))
;; (defparameter *img-root* (merge-pathnames "img/" *data-root*))
;; (defparameter *sound-root* (merge-pathnames "sound/" *data-root*))


(defvar *x-max* 15)
(defvar *y-max* 14)
(defvar *field-w* (* *x-max* 32))
(defvar *field-h* (* 4 32))
(defvar *enemy-field-h-min* 0)
(defvar *enemy-field-h-max* *field-h*)
(defvar *enemy-field-w* *field-w*)
(defvar *player-field-w* *field-w*)
(defvar *player-field-h-min* (+ *enemy-field-h-max* (* 32 *y-max*)))
(defvar *player-field-h-max* (+ *player-field-h-min* *field-h*))
(defvar *tower-w* 32)
(defvar *tower-h* 16)

(defparameter *upgrade-btn-x1* (+ *field-w* 40))
(defparameter *upgrade-btn-x2* (+ *field-w* 145))
(defparameter *upgrade-btn-y1* 390)
(defparameter *upgrade-btn-y2* 416)

(defvar *status-w* 200)

(defvar *client-w* (+ *field-w* *status-w*))
(defvar *client-h* *player-field-h-max*)

(defparameter *screen-w* 800)
(defparameter *screen-h* 800)

(defparameter *change-screen-w* *screen-w*)
(defparameter *change-screen-h* *screen-h*)

(defparameter *c-rect* nil) ;;クライアント領域

(defmacro with-double-buffering-2 ((var hwnd) &body body)
  "Evaluate body in a WITH-PAINT context where VAR is bound to an in-memory HDC
which is blitted onto the hwnd's DC as the final step. This prevents flickering 
when drawing lots of small items on the screen."
  (alexandria:with-gensyms (gbm gold gwidth gheight ghdc gps)
    `(with-paint (,hwnd ,ghdc ,gps)
       (let ((,gwidth (rect-right *c-rect*))
		       ;;(paintstruct-paint ,gps)))
	     (,gheight (rect-bottom *c-rect*)))
			;;(paintstruct-paint ,gps))))
	 (with-compatible-dc (,var ,ghdc)
	   (let* ((,gbm (create-compatible-bitmap ,ghdc ,gwidth ,gheight))
		  (,gold (select-object ,var ,gbm)))
	     (unwind-protect (progn ,@body)
	       (stretch-blt ,ghdc 0 0 ,var 0 0
			:width-dest *change-screen-w*
			:height-dest *change-screen-h*
			:width-source (rect-right *c-rect*)
			:height-source (rect-bottom *c-rect*)
			:raster-op :srccopy)
	       ;; (transparent-blt ,ghdc 0 0 ,var 0 0 
	       ;; 			:width-dest *change-screen-w*
	       ;; 			:height-dest *change-screen-h*
	       ;; 			:width-source (rect-right *c-rect*)
	       ;; 			:height-source (rect-bottom *c-rect*)
	       ;; 			:transparent-color (encode-rgb 0 255 0))
	       
	       (select-object ,var ,gold)
	       (delete-object ,gbm))))))))

(defvar *player* nil)
(defvar *enemy* nil)
(defvar *game* nil)
(defvar *p-img* nil)
(defvar *waku-img* nil)
(defvar *mouse* nil)
(defvar *mouse-hosei-x* 1)
(defvar *mouse-hosei-y* 1)
(defvar *road-img* nil)
(defvar *monsters-img* nil)
(defvar *towers-img* nil)
(defvar *enemy-towers-img* nil)
(defvar *explosion-img* niL)
(defvar *monster-anime-max* 3)


(my-enum +kara+ +l-r+ +u-d+ +u-r+ +l-u+ +d-r+ +l-d+ +rlud+ +r-ud+ +l-ud+ +u-lr+ +d-rl+)
(my-enum +brigand+ +dragon+ +hydra+ +yote1+  +orc+ +slime+ +bubble+ +skelton+ +fire+)
(my-enum +up+ +down+ +left+ +right+)
(my-enum +canon+ +beam+ +missile+ +factory+ +wall+ +guided+ +ice+ +max-tower+)
(my-enum +explosion+)

(defun load-images ()
  (setf *p-img*              (load-image  (namestring (merge-pathnames "p-ido-anime.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))
	*monsters-img*       (load-image  (namestring (merge-pathnames "monsters.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))
	*towers-img*         (load-image  (namestring (merge-pathnames "towers.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))
	*enemy-towers-img*   (load-image  (namestring (merge-pathnames "enemy-towers.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))
	*waku-img*           (load-image  (namestring (merge-pathnames "waku.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))
	*explosion-img*      (load-image  (namestring (merge-pathnames "ms-explosion.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))
	*road-img*           (load-image  (namestring (merge-pathnames "road.bmp" *img-root*))
					  :type :bitmap
					  :flags '(:load-from-file :create-dib-section))))

(defstruct game
  (state nil)
  (stage 0)
  (all-stage nil)
  (stage-data nil)
  (frame 0)
  )

(defstruct p
  (towers nil)
  (bullets nil)
  (explosion nil)
  (effect nil)
  (money 0)
  (score 0))

(defstruct rectan
  )

(defstruct (player (:include p))
  )

(defstruct (enemy (:include p))
  (act-c 0)
  (act-cd 0))

(defstruct stage
  (stage-arr nil)
  (lv 0)
  (wave nil)
  (wave-type nil)
  (wave-num 0)
  (max-monster-num 0)
  (min-monster-num 0)
  (startx 0)
  (starty 0)
  (monsters nil))

(defstruct mouse
  (posx 0)
  (posy 0)
  (selected nil)
  (smpl-selected nil)
  (right nil)
  (left nil))

(defstruct obj
  (w 0)
  (h 0)
  (w/2 0)
  (h/2 0)
  (x 0)
  (y 0) ;;x y配列の座標
  (posx 0)
  (posy 0) ;; posx posy 描画用の座標
  (img nil))

(defstruct (explosion (:include obj))
  (render-c 0)
  (dmg 0)
  (render-time 100)
  (hitted nil))
	   
(defstruct (slime (:include obj))
  (hoge 0))

(defstruct (monster (:include obj))
  (lv 0)
  (state nil)
  (anime-c 0)
  (anime-img 0)
  (stop-c 0)
  (point 0)
  (spd 0)
  (dir nil)
  (hp 0))

(defstruct (tower (:include obj))
  (atk-c 0)
  (stop-c 0)
  (state nil)
  (cd 0)
  (blt-spd 0)
  (hitted nil)
  (dmg 0)
  (hp 0)
  (lv 0)
  (cost 0)
  (color nil)
  (point 0)
  )

(defstruct (wall (:include tower))
  )

(defstruct (guided (:include tower))
  (vx 0)
  (vy 0)
  (target nil)
  (explosion-r 0))

(defstruct (canon (:include tower))
  )

(defstruct (beam (:include tower))
  (penetrate 0)
  )

(defstruct (missile (:include tower))
  (explosion-r 0)
  )

(defstruct (factory (:include tower))
  (inc-money 0)
  (money-c 0))

(defstruct (ice (:include tower))
  (stop-time 0))


(defstruct (bullet  (:include obj))
  (spd 0)
  (dmg 0)
  (hitted nil)
  (color nil))


(defparameter *tower-samples* nil)


				
;;font----------------------------------------------------------
(defparameter *font140* nil)
(defparameter *font90* nil)
(defparameter *font70* nil)
(defparameter *font40* nil)
(defparameter *font30* nil)
(defparameter *font20* nil)
(defparameter *font2* nil)

(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
        *font90* (create-font "MSゴシック" :height 90)
        *font70* (create-font "MSゴシック" :height 70)
        *font40* (create-font "MSゴシック" :height 40)
        *font30* (create-font "MSゴシック" :height 30)
        *font20* (create-font "MSゴシック" :height 24)
	*font2* (create-font "MSゴシック" :height 15)));; :width 12 :weight (const +fw-bold+))))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font90*)
  (delete-object *font70*)
  (delete-object *font40*)
  (delete-object *font30*)
  (delete-object *font20*)
  (delete-object *font2*))


;;重み付け抽選-----------------------------------------------
(defun rnd-pick (i rnd lst len)
  (if (= i len)
      (1- i)
      (if (< rnd (nth i lst))
	  i
	  (rnd-pick (1+ i) (- rnd (nth i lst)) lst len))))
;;lst = *copy-buki*
(defun weightpick (lst)
  (let* ((lst1 (mapcar #'cdr lst))
	 (total-weight (apply #'+ lst1))
	 (len (length lst1))
	 (rnd (random total-weight)))
    (car (nth (rnd-pick 0 rnd lst1 len) lst))))
;;------------------------------------------------------------
