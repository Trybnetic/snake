;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct game-state (snake target speed))
(define-struct snake-elem (position color))
(define-struct snake (head direction))

(define WIDTH 600)
(define HEIGHT 400)
(define SNAKE-ELEM-SIZE 10)

; A Speed is a Number
; interp. speed of the game
(define STANDARD-SPEED 3)

; A Direction is one of:
;- "left"
;- "right"
;- "up"
;- "down"
(define UP "up")
(define DOWN "down")
(define RIGHT "right")
(define LEFT "left")

; A Color is a String
; interp. a Color
(define RED "red")
(define YELLOW "yellow")

; A Location is a structure: (make-posn Number Number)
; interp. x and y coordinate of a location on screen.
(define CENTER (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
(define CORNER (make-posn 0 0))

; A Snake-elem is a structure: (make-snake-elem Location Color)
; interp. the snake in the game, currently just the head of the snake
(define SNAKE-HEAD (make-snake-elem CENTER RED))

; A Snake is a structure: (make-snake Snake-elem)
; interp. the snake in the game, currently just the head of the snake
(define SNAKE (make-snake SNAKE-HEAD RIGHT))

; A State is a structure: (make-game-state Snake Location)
; interp. current position of the snake, position of the target
(define INITIAL-STATE (make-game-state SNAKE CORNER STANDARD-SPEED))

; A Direction-key is a Direction
; interp. Key belonging to a Direction
(define KEY-UP "up")
(define KEY-DOWN "down")
(define KEY-RIGHT "right")
(define KEY-LEFT "left")




; State -> Image
; render the state to the screen
(define (draw state)
  (place-image (square SNAKE-ELEM-SIZE "solid" (snake-elem-color (snake-head (game-state-snake state))))
               (posn-x (snake-elem-position (snake-head (game-state-snake state))))
               (posn-y (snake-elem-position (snake-head (game-state-snake state))))
               (place-image (square SNAKE-ELEM-SIZE "solid" YELLOW)
                            (posn-x (game-state-target state))
                            (posn-y (game-state-target state))
                            (empty-scene WIDTH HEIGHT "white"))))
  
  ; State Direction -> State
  ; changes the direction in of a State
  (define (change-direction-to state direction)
    (make-game-state (make-snake (snake-head (game-state-snake state))
                                 direction)
                     (game-state-target state)
                     (game-state-speed state)))
  
  ; State Direction-key -> State
  ; react to key
  (define (key state a-key)
    (cond ((string=? a-key KEY-UP) (change-direction-to state UP))
          ((string=? a-key KEY-DOWN) (change-direction-to state DOWN))
          ((string=? a-key KEY-RIGHT) (change-direction-to state RIGHT))
          ((string=? a-key KEY-LEFT) (change-direction-to state LEFT))
          (else state)))
  
  ; State Location -> State
  ; changes the position of the snakes head
  (define (change-head-position state new-posn)
    (make-game-state (make-snake (make-snake-elem new-posn (snake-elem-color (snake-head (game-state-snake state))))
                                 (snake-direction (game-state-snake state)))
                     (game-state-target state)
                     (game-state-speed state)))
  
  ; State -> State
  ; update the state when time passes
  (define (tick state)
    (cond ((string=? (snake-direction (game-state-snake state)) UP) (change-head-position state (make-posn (posn-x (snake-elem-position (snake-head (game-state-snake state))))
                                                                                                           (- (posn-y (snake-elem-position (snake-head (game-state-snake state)))) (game-state-speed state)))))
          ((string=? (snake-direction (game-state-snake state)) DOWN) (change-head-position state (make-posn (posn-x (snake-elem-position (snake-head (game-state-snake state))))
                                                                                                             (+ (posn-y (snake-elem-position (snake-head (game-state-snake state)))) (game-state-speed state)))))
          ((string=? (snake-direction (game-state-snake state)) LEFT) (change-head-position state (make-posn (- (posn-x (snake-elem-position (snake-head (game-state-snake state)))) (game-state-speed state))
                                                                                                             (posn-y (snake-elem-position (snake-head (game-state-snake state)))))))
          ((string=? (snake-direction (game-state-snake state)) RIGHT) (change-head-position state (make-posn (+ (posn-x (snake-elem-position (snake-head (game-state-snake state)))) (game-state-speed state))
                                                                                                              (posn-y (snake-elem-position (snake-head (game-state-snake state)))))))))
  
  ; State -> State
  ; runs the circle game
  (define (circle-game initial-state)
    (big-bang initial-state
              [to-draw draw]
              [on-tick tick]
              [on-key key]))
  
  
  (circle-game INITIAL-STATE)