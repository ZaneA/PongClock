;; Pong Clock
;; HTML5 + JS version by The Silvervest Group Labs
;; Ported to OpenGL by Zane Ashby (http://github.com/ZaneA/PongClock

(use gl glu glut posix)

; Puck Direction
(define dx -4)
(define dy 3)
(define dx-reset dx)
(define dy-reset dy)

; Canvas
(define canvas-w 800.0)
(define canvas-h 450.0)

; Puck
(define x 0)
(define y 0)
(define draw-speed 11)
(define puck-size 20)

; Timers
(define draw-timer '())
(define clock-timer '())
(define last-seconds 0)

; Paddles
(define paddle-ly '())
(define paddle-lx '())
(define paddle-ry '())
(define paddle-rx '())
(define paddle-w 20)
(define paddle-h 100)

; Scores
(define score-l 0)
(define score-r 0)
(define inc-h #f)
(define inc-m #f)


(define (init)
  (glut:InitWindowSize canvas-w canvas-h)
  (glut:CreateWindow "Pong Clock - Original HTML5 version by The Silvervest Group Labs - Ported by Zane Ashby")

  (glu:Ortho2D 0 canvas-w canvas-h 0)

  (set! draw-timer (glut:TimerFunc draw-speed redraw-board 0))

  (let* ((time (seconds->local-time))
         (time-h (vector-ref time 2))
         (time-m (vector-ref time 1)))
    (when (> time-h 12)
          (set! time-h (- time-h 12)))
    (set! score-l time-h)
    (set! score-r time-m))

  (update-score)

  (set! paddle-ly (- (/ canvas-h 2) (/ paddle-h 2)))
  (set! paddle-ry (- (/ canvas-h 2) (/ paddle-h 2)))

  (reset-puck "r"))


; Clock Tick
(define (clock-tick user)
  (let* ((time (seconds->local-time))
         (time-m (vector-ref time 1))
         (time-s (vector-ref time 0)))
    (if (and (= time-m 0) (= time-s 0))
        (set! inc-h #t)
        (when (= time-s 0)
              (set! inc-m #t)))))

(define (redraw-board user)
  ; Bounce off the roof/floor
  (when (or (> (+ y dy) (- canvas-h puck-size)) (<= (+ y dy) 0))
        (set! dy (- 0 dy)))

  ; Hits the left side
  (when (<= (+ x dx) paddle-w)
        (if (and (> y (- paddle-ly puck-size)) (< y (+ paddle-ly paddle-h)))
            (set! dx (- 0 dx))
            (when (< x puck-size)
                  (reset-puck "l")
                  (set! score-r (+ score-r 1))
                  (update-score))))

  ; Hits the right side
  (when (>= (+ x dx) (- (- canvas-w paddle-w) puck-size))
        (if (and (> y (- paddle-ry puck-size)) (< y (+ paddle-ry paddle-h)))
            (set! dx (- 0 dx))
            (when (> x (- canvas-w puck-size))
                  (reset-puck "r")
                  (set! score-l (+ score-l 1))
                  (set! score-r 0)
                  (update-score))))

  ; Move the puck
  (set! x (+ x dx))
  (set! y (+ y dy))

  ; Smoothly move paddles, missing if appropriate
  (let ((fuzz 1)
        (move-to (- y (/ paddle-h 2))))
    (if (and (< x (/ canvas-w 2.6)) (< dx 0))
        (begin
          (when inc-m (set! fuzz 4))
          (if (> move-to paddle-ly)
              (set! paddle-ly (- paddle-ly (/ (- paddle-ly move-to) (* 10 fuzz))))
              (set! paddle-ly (+ paddle-ly (/ (- move-to paddle-ly) (* 10 fuzz))))))
        (when (and (> x (/ canvas-w 1.6)) (> dx 1))
              (when inc-h (set! fuzz 4))
              (if (> move-to paddle-ry)
                  (set! paddle-ry (- paddle-ry (/ (- paddle-ry move-to) (* 10 fuzz))))
                  (set! paddle-ry (+ paddle-ry (/ (- move-to paddle-ry) (* 10 fuzz))))))))

  ; Make sure to not go over the edges
  (when (<= paddle-ly 0) (set! paddle-ly 1))
  (when (<= paddle-ry 0) (set! paddle-ry 1))
  (when (>= paddle-ly (- canvas-h paddle-h)) (set! paddle-ly (- canvas-h paddle-h 1)))
  (when (>= paddle-ry (- canvas-h paddle-h)) (set! paddle-ry (- canvas-h paddle-h 1)))

  (when (not (= (current-seconds) last-seconds))
        (clock-tick 0)
        (set! last-seconds (current-seconds)))
  
  (glut:PostRedisplay)

  (set! draw-timer (glut:TimerFunc draw-speed redraw-board 0)))


; Reset puck onto winners paddle
(define (reset-puck side)
  (set! inc-h #f)
  (set! inc-m #f)
  (set! dx dx-reset)
  (if (string=? side "r")
      (begin
        (set! x (+ paddle-w puck-size))
        (set! y (+ paddle-ly (/ paddle-h 2))))
      (begin
        (set! x (- canvas-w (+ paddle-w puck-size)))
        (set! y (+ paddle-ry (/ paddle-h 2))))))
  

; Update score text
(define (update-score)
  #t) ; Oops, this is a no-op


; Draw rectangle
(define (draw-rect x y w h)
  (gl:Color3f 1 1 1)
  (gl:Begin gl:POLYGON)
  (gl:Vertex2f x y)
  (gl:Vertex2f x (+ y h))
  (gl:Vertex2f (+ x w) (+ y h))
  (gl:Vertex2f (+ x w) y)
  (gl:End))


(init)


(glut:DisplayFunc
 (lambda ()
  (gl:Clear gl:COLOR_BUFFER_BIT)

  ; Draw paddles
  (draw-rect (- canvas-w (+ paddle-w 1)) paddle-ry paddle-w paddle-h)
  (draw-rect 1 paddle-ly paddle-w paddle-h)

  ; Draw puck
  (draw-rect x y puck-size puck-size)

  ; Text rendering
  (let ((text-l (format "~a" score-l))
        (text-r (format "~a" score-r)))
    (when (= (string-length text-l) 1)
          (set! text-l (format "0~a" text-l)))
    (when (= (string-length text-r) 1)
          (set! text-r (format "0~a" text-r)))

    (gl:RasterPos2f (- (/ canvas-w 2) 30)  20)
    (glut:BitmapCharacter glut:BITMAP_HELVETICA_18 (string-ref text-l 0))
    (glut:BitmapCharacter glut:BITMAP_HELVETICA_18 (string-ref text-l 1))

    (gl:RasterPos2f (- (/ canvas-w 2) 0)  20)
    (glut:BitmapCharacter glut:BITMAP_HELVETICA_18 (string-ref text-r 0))
    (glut:BitmapCharacter glut:BITMAP_HELVETICA_18 (string-ref text-r 1)))
  
  (gl:Flush)))


(gl:ClearColor 0 0 0 0)
(glut:MainLoop)
