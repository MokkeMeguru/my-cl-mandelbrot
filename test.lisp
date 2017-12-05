(ql:quickload :cl-opengl)
(ql:quickload :cl-glu)
(ql:quickload :cl-glut)

;; my-variable
(defparameter *iterate* 30)
(defparameter *width* 900)
(defparameter *height* 600)

;; my-functions
(defun calc-mandelbrot (c)
  (labels ((f (c z acc)
             (cond ((= acc *iterate*) 0)
                   ((< 2 (abs z)) (* 0.2  acc))
                   (t (f c (+ c (expt z 2)) (1+ acc))))))
    (f c 0 0)))

(defun generate-points (sx sy ex ey x-len y-len)
  (apply #'append
         (loop for x
               from 1
                 to *width*
               collect
               (loop for y
                     from 1
                       to *height*
                     collect
                     (complex (coerce (+ sx (* x (/ (- ex sx) x-len))) 'short-float)
                              (coerce (+ sy (* y (/ (- ey sy) y-len))) 'short-float))))))

(defun set-point (color point)
  (%gl:color-3f (1- color) 0 0)
  (gl:vertex (- (first point) (/ *width* 2))  (- (second point) (/ *height* 2))  0))

(defun set-points (mandelbrot)
  (let* ((colors mandelbrot)
        (points (apply #'append
                       (loop for x
                             from 0
                               below *width*
                             by 1
                             collect (loop for y
                                           from 0
                                             below *height*
                                           by 1
                                           collect (list x y))))))
    (loop for color in colors
          for point in points
          collect (set-point
                   color
                   point)))
  )

;; utilities
(defparameter *mandelbrot* (mapcar #'calc-mandelbrot (generate-points -2 -1 1 1 *width* *height*)))
(defclass my-mandelbrot-window (glut:window)
  ((start-x :initform -2.0
            :accessor start-x)
   (end-x :initform 1.0
          :accessor end-x)
   (start-y :initform -1.0
            :accessor start-y)
   (end-y :initform 1.0
          :accessor end-y))
  (:default-initargs :width *width* :height *height* :mode '(:depth :rgb)
                     :title "my-mandelbrot-with-common-lisp"
   ))

(defun calc-mandelbrots (w)
  (let* ((sx (start-x w))
         (sy (start-y w))
         (ex (end-x w))
         (ey (end-y w)))
    (mapcar #'calc-mandelbrot (generate-points sx sy ex ey *width* *height*))))

(defmethod glut:display-window :before ((w my-mandelbrot-window))
  (gl:clear-color 1 1 1 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 *width* *height* 0 -1 1))

(defmethod glut:display ((w my-mandelbrot-window))
  (gl:clear :color-buffer-bit)
  (%gl:color-3f 0 0 0)
  (gl:push-matrix)
  (gl:translate (/ *width* 2) (/ *height* 2) 0)
  (gl:begin :points)
  (set-points *mandelbrot*)
  (gl:end)
  (gl:pop-matrix)
  (gl:flush))

(defmethod glut:mouse ((w my-mandelbrot-window) button state x y)
  (let ((sx (start-x w))
        (sy (start-y w))
        (ex (end-x w))
        (ey (end-y w)))
    (case button
      (:left-button
       (when (eq state :down)
         (let* ((pix-size (/ (- ex sx) 900))
                (real-x (+ (* pix-size x) sx))
                (real-y (+ (* pix-size y) sy))
                (new-x-len (/ (- ex sx) 4))
                (new-sx (- real-x new-x-len))
                (new-ex (+ real-x new-x-len))
                (new-y-len (/ (- ey sy) 4))
                (new-sy (- real-y new-y-len))
                (new-ey (+ real-y new-y-len)))
           (setf (start-x w) new-sx)
           (setf (end-x w) new-ex)
           (setf (start-y w) new-sy)
           (setf (end-y w) new-ey)
           (setq *mandelbrot*
                 (mapcar #'calc-mandelbrot
                         (generate-points new-sx new-sy new-ex new-ey *width* *height*)))
           (sleep 1.0))
         ))
      (:right-button
       (when (eq state :down)
         (let* ((new-x-len (/ (- ex sx) 2))
                (new-sx (- sx new-x-len))
                (new-ex (+ ex new-x-len))
                (new-y-len (/ (- ey sy) 2))
                (new-sy (- sy new-y-len))
                (new-ey (+ ey new-y-len)))
           (setf (start-x w) new-sx)
           (setf (end-x w) new-ex) 
           (setf (start-y w) new-sy)
           (setf (end-y w) new-ey)
           (setq *mandelbrot*
                 (mapcar #'calc-mandelbrot
                         (generate-points new-sx new-sy new-ex new-ey *width* *height*)))
           (sleep 1.0)
           )
         ))
      )
    (glut:post-redisplay)))

(defun my-mandelbrot ()
  (glut:display-window (make-instance 'my-mandelbrot-window)))

;; (my-mandelbrot)

(sb-ext:save-lisp-and-die "mandelbrot-sbcl"
                          :toplevel #'my-mandelbrot
                          :executable t)
