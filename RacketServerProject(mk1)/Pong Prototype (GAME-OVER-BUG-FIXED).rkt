#lang racket

(require 2htdp/universe 2htdp/image)

;; Pong prototype - One player versus the machine.

;;----- Constants definitions

;; Numerical constants

(define ZERO 0)
(define ONE 1)
(define TWO 2)
(define THREE 3)

;; Physical constants

(define-values (WIDTH HEIGHT) (values 1000 650))
(define-values (CANVAS-WIDTH CANVAS-HEIGHT) (values 1000 650))

(define BALL-IMAGE (bitmap "PixelArt (1).png"))
(define BALL (scale 0.1 BALL-IMAGE))
(define-values (BALL-WIDTH BALL-HEIGHT) (values 55 55))

(define-values (WIDTH-BAR HEIGHT-BAR) (values 40 100))
(define BAR (rectangle WIDTH-BAR HEIGHT-BAR "solid" "black"))

(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "darkgreen"))

(define-values (DISP TICK PAUSED?) (values 10 0.003 #f))

(define-values (BOUNDARY LEFT RIGHT) (values 10 5 (- CANVAS-WIDTH 5)))

(define GAME-OVER (bitmap "PixelArt (2).png"))

;; A state is a list formed by the following members:

(define (ball-x state) (list-ref state 0))
(define (ball-y state) (list-ref state 1))
(define (ball-x-vec state) (list-ref state 2))
(define (ball-y-vec state) (list-ref state 3))
(define (left-bar-y state) (list-ref state 4))
(define (right-bar-y state) (list-ref state 5))

;; Updating the list at a tracker with a new value
;; Given a list, a tracker and a value, returns the updated list formed by the new values.
;; Replacing the firs occurence of the variable tracker by the given variable value.

(define (replace lst tracker value)
  (append (take lst tracker)
          (list value)
          (list-tail lst (add1 tracker))))

;; Constants for tests
(define LST-1 (list 0 1 2 3 4 5))
(define LST-2 (list 0 2 0 3))

;; Test
;;(check-expect (replace LST-1 2 3) (list 0 1 3 3 4 5)) #t
;;(check-expect (replace LST-2 0 4) (list 4 2 0 3)) #t

;; Did the ball hit one of the bars ?


;;***********************************************************************
;;****THIS FUNCTION WAS THE BUGGY ONE, THE FIXED VERSION IS DOWN BELOW
;;(define (collision? state)
;;  (define (within? n target dist)
;;    (and (>= n (- target dist)) (<= n (+ target dist))))
;;  (or (and (equal? (ball-x state) 20)
;;           (within? (ball-y state) (left-bar-y state) 25))
;;      (and (equal? (ball-x state) (- WIDTH 20))
;;          (within? (ball-y state) (right-bar-y state) 25))))
;;***********************************************************************



;;******************************************************************************************************************
;;****THIS FUNCTION DOWN BELOW IS THE FUNCTION I IMPLEMENTED FOR THE GAME-OVER BUG FIX****

;;This is the error margin definition
(define ERROR-MARGIN 10)
;;This function will check if the ball hit the left moving platform
(define (PLATFORM-HIT? state)
  (cond
    [(and
      (<= (ball-x state) (- (+ (image-width BAR) (/ (image-width BALL) 2)) (* 0.5 (image-width BAR)))) 
       (<= (cond
        [( >= (left-bar-y state) (ball-y state)) ( - (left-bar-y state) (ball-y state))]
        [else ( - (ball-y state) (left-bar-y state))]) (+ (/ (image-height BAR) 2) ERROR-MARGIN))) #t]
    [(and
      (<= (ball-x state) (- (+ (image-width BAR) (/ (image-width BALL) 2)) (* 0.5 (image-width BAR))))
      (equal? (cond
        [( >= (left-bar-y state) (ball-y state)) ( - (left-bar-y state) (ball-y state))]
        [else ( - (ball-y state) (left-bar-y state))]) (/ (image-height BAR) 2))) #t]
    [(and
      (<= (ball-x state) (- (+ (image-width BAR) (/ (image-width BALL) 2)) (* 0.5 (image-width BAR)))) 
       (<= (cond
        [( >= (left-bar-y state) (ball-y state)) ( - (left-bar-y state) (ball-y state))]
        [else ( - (ball-y state) (left-bar-y state))]) (- (/ (image-height BAR) 2) ERROR-MARGIN))) #t]
    [(and
      (>=  (ball-x state) (- WIDTH (- (+ (image-width BAR) (/ (image-width BALL) 2)) (/ (image-width BAR) 2))))) 
      (>= (cond
        [( >= (right-bar-y state) (ball-y state)) ( - (right-bar-y state) (ball-y state))]
        [else ( - (ball-y state) (right-bar-y state))]) (- (/ (image-width BALL) 2) 20)) #t]
    [else #f]))
;;******************************************************************************************************************




;; Constants for tests:
(define STATE-1 (list 772 53 1 -1 325 53))
(define STATE-2 (list 1200 100 1 1 429 12))
(define STATE-3 (list 1000 600 -1 -1 220 96))

;; Given a state,

(define (off-limits? state)
  (or (>= BOUNDARY (ball-x state))
      (<= (- WIDTH BOUNDARY) (ball-x state))))

;; Test
#|
> (off-limits? STATE-1)
#f
> (off-limits? STATE-2)
#t
> (off-limits? STATE-3)
#f
|#

;; Moving the ball one step

(define (move-ball state)
  (replace (replace state ZERO (+ (ball-x state) (ball-x-vec state)))
           ONE
           (+ (ball-y state) (ball-y-vec state))))

;; Tests:
#|
> (move-ball STATE-1)
'(773 52 1 -1 325 53)
> STATE-1
'(772 53 1 -1 325 53)
> (move-ball STATE-2)
'(1201 101 1 1 429 12)
> (move-ball STATE-3)
'(999 599 -1 -1 220 96)
|#

;; Next step of the ball

(define (next-ball state)
  (if PAUSED?
      state
      (move-ball
       (cond
         [(PLATFORM-HIT? state)
          (replace state TWO (* (sub1 ZERO) (ball-x-vec state)))]
         [(or (equal? (ball-y state) BOUNDARY)
              (equal? (ball-y state) (- HEIGHT BOUNDARY)))
          (replace state 3 (* (sub1 ZERO) (ball-y-vec state)))]
         [else state]))))

;; Tests:
#|
> (next-ball STATE-1)
'(772 53 1 -1 325 53)
> (next-ball STATE-2)
'(1200 100 1 1 429 12)
> (next-ball STATE-3)
'(1000 600 -1 -1 220 96)
|#

(define (next-player state vec)
  (define new-y
    (max 40  (min (- HEIGHT 25) (+ vec (left-bar-y state)))))
  (if PAUSED?
      state
      (replace state 4 new-y)))

;; Tests:
#|
> (next-player STATE-1 10)
'(772 53 1 -1 325 53)
> STATE-1
'(772 53 1 -1 325 53)
> (next-player STATE-2 1)
'(1200 100 1 1 429 12)
> STATE-2
'(1200 100 1 1 429 12)
> (next-player STATE-3 23)
'(1000 600 -1 -1 220 96)
> STATE-3
'(1000 600 -1 -1 220 96)
|#

;; Best part of the code: by replacing the state, it matches the ball's height. (y coordinate)

(define (arti-intel state)
  (replace state 5 (ball-y state)))

;; Tests:
#|
> (arti-intel STATE-1)
'(772 53 1 -1 325 53)
> STATE-1
'(772 53 1 -1 325 53)
> (arti-intel STATE-2)
'(1200 100 1 1 429 100)
> STATE-2
'(1200 100 1 1 429 12)
> (arti-intel STATE-3)
'(1000 600 -1 -1 220 600)
> STATE-3
'(1000 600 -1 -1 220 96)
|#

;; Pause the game using a toggle

(define (toggle-pause state)
  (set! PAUSED? (not PAUSED?))
  state)

;; Tests:
#|
> (toggle-pause STATE-1)
'(772 53 1 -1 325 53)
> (toggle-pause STATE-2)
'(1200 100 1 1 429 12)
> (toggle-pause STATE-3)
'(1000 600 -1 -1 220 96)
|#

;; Event handlers

(define (event-handler state a-key)
  (cond
    [(key=?  a-key "up") (next-player state (- DISP))]
    [(key=?  a-key "down") (next-player state (+ DISP))]
    [(key=?  a-key "p") (toggle-pause state)]
    [else state]))

;; Tests:
#|
> (event-handler STATE-1 "up")
'(772 53 1 -1 315 53)
> STATE-1
'(772 53 1 -1 325 53)
> (event-handler STATE-1 "down")
'(772 53 1 -1 335 53)
> (event-handler STATE-3 "a")
'(1000 600 -1 -1 220 96)
> STATE-3
'(1000 600 -1 -1 220 96)
|#

;; The universe library can move object with ticks. 
;; After every tick ,  It will find new ai position, every time the ball moves
(define (world-step state)
  (arti-intel (next-ball state)))

;; Tests:
#|
> (world-step STATE-2)
'(1201 101 1 1 429 101)
> STATE-2
'(1200 100 1 1 429 12)
> (world-step STATE-3)
'(999 599 -1 -1 220 599)
> STATE-3
'(1000 600 -1 -1 220 96)
|#

(define original-state
  (let ([random-vector (λ () (list-ref (list -1 1) (random 2)))]
        [half-width (/ WIDTH TWO)] 
        [half-height (/ HEIGHT TWO)])
    (list half-width half-height                    ; ball's position
          (random-vector) (random-vector)  ; Randomly chosing the ball's direction
          half-height half-height)))                 ; bar's heights

;; Drawing the game area
;; The only pieces are the ball and 2 bars (representing each player).
(define-values (CENTER-X CENTER-Y) (values (/ WIDTH TWO) (/ HEIGHT TWO)))

(define (display-sprites state)
  (place-image
   BALL (ball-x state) (ball-y state)
   (place-image
    BAR LEFT (left-bar-y state)
    (place-image 
     BAR RIGHT (right-bar-y state)
     (add-line(empty-scene WIDTH HEIGHT "darkgreen" )  CENTER-X ZERO CENTER-X HEIGHT (make-pen "white" 7 "long-dash" "round" "round") )))))

;; Displays a game over text, when the game is over
;; Given the final state, returns a new image.

(define (game-over-sit w)
  (overlay GAME-OVER (display-sprites w)))

;; Starting the game - calling the big-bang function.

(big-bang original-state
          (to-draw display-sprites)
          (on-key event-handler)
          (on-tick world-step TICK)
          (stop-when off-limits? game-over-sit))