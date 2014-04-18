;; Copyright (c) 2014 Nikolay Puzanov <punzik@gmail.com>
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang racket/gui

(require racket/gui/base)

(define grid-step 20)
(define board-width 30)
(define board-height 30)
(define zero-x (round (/ board-width 2)))
(define zero-y (round (/ board-height 2)))
(define default-file-name "turtle.prog")

(struct point (draw x y))

(define poly (list (point #t 0 0)))
(define draw #t)

(define frame (new frame%
                   [label "Turtle"]
                   [width (* board-width grid-step)]
                   [height (* board-height grid-step)]))

(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (λ (canvas dc)
                       (send dc set-smoothing 'unsmoothed)
                       (send dc set-pen "gray" 1 'solid)
                       (let-values (((w h) (send canvas get-size)))
                         (begin
                           (do ((x 0 (+ x grid-step))) ((>= x w) #t)
                             (send dc draw-line x 0 x h))
                           (do ((y 0 (+ y grid-step))) ((>= y h) #t)
                             (send dc draw-line 0 y w y))))
                       (send dc set-smoothing 'smoothed)
                       (send dc set-pen "blue" 2 'solid)
                       (send dc set-brush "blue" 'transparent)
                       (send dc draw-ellipse
                             (- (* grid-step zero-x) 4)
                             (- (* grid-step zero-y) 4)
                             8 8)
                       (let ((points (map (λ (pt)
                                            (point (point-draw pt)
                                                   (* grid-step (+ zero-x (point-x pt)))
                                                   (* grid-step (+ zero-y (point-y pt)))))
                                          poly)))
                         (let loop ((pt0 (car points))
                                    (points (cdr points)))
                           (if (null? points)
                               #f
                               (let ((pt (car points)))
                                 (begin
                                   (if (point-draw pt0)
                                       (send dc draw-line
                                             (point-x pt0)
                                             (point-y pt0)
                                             (point-x pt)
                                             (point-y pt))
                                       #f)
                                   (loop pt (cdr points)))))))

                       (send dc set-brush "blue" (if draw 'solid 'transparent))
                       (let* ((last-pt (car poly))
                              (last-pt-x (* grid-step (+ zero-x (point-x last-pt))))
                              (last-pt-y (* grid-step (+ zero-y (point-y last-pt)))))
                         (send dc draw-rectangle (- last-pt-x 4) (- last-pt-y 4) 8 8)))]))

(define (parse-commands str)
  (if (= (string-length str) 0)
      (cons 0 0)
      (let parse ((cmds (string-split str))
                  (x 0)
                  (y 0))
        (if (null? cmds)
            (cons x y)
            (let* ((cmd (string-upcase (car cmds)))
                   (cmd-char (string-ref cmd 0))
                   (cmds-tail (cdr cmds))
                   (argument (λ (dx dy)
                               (if (null? cmds-tail)
                                   (begin
                                     (printf "Need argument for '~a'\n" cmd)
                                     (parse cmds-tail x y))
                                   (let ((val (string->number (car cmds-tail))))
                                     (if (equal? val #f)
                                         (begin
                                           (printf "'~a' is not valid argument for '~a'\n" (car cmds-tail) cmd)
                                           (parse cmds-tail x y))
                                         (parse (cdr cmds-tail)
                                                (+ x (* val dx))
                                                (+ y (* val dy)))))))))
              (cond
               ((or (equal? cmd "EXIT")
                    (equal? cmd-char #\Q)) 'quit)
               ((equal? cmd-char #\L) (argument -1 0))
               ((equal? cmd-char #\R) (argument 1 0))
               ((equal? cmd-char #\U) (argument 0 -1))
               ((equal? cmd-char #\D) (argument 0 1))
               ((equal? cmd-char #\C) 'clear)
               ((equal? cmd-char #\P) 'pen)
               ((equal? cmd-char #\S) 'save)
               ((equal? cmd-char #\B) 'back)
               (else
                (begin
                  (printf "Unknown command '~a'\n" cmd)
                  (cons 0 0)))))))))

(define (process poly port interactive)
  (let ((cmd-line (begin
                    (if interactive
                        (display ">> ")
                        #f)
                    (read-line port))))
    (if (eof-object? cmd-line)
        #f
        (let ((ret (parse-commands cmd-line)))
          (cond
           ((equal? ret 'quit) #f)
           ((equal? ret 'clear) (list (point #t 0 0)))
           ((equal? ret 'back)
            (if (> (length poly) 1)
                (cdr poly)
                poly))
           ((equal? ret 'pen)
            (begin
              (set! draw (not draw))
              poly))
           ((equal? ret 'save)
            (begin
              (call-with-output-file (if (equal? file-name #f)
                                         default-file-name
                                         file-name)
                (λ (port)
                  (foldr (lambda (pt pz)
                           (let ((dx (- (point-x pt) (point-x pz)))
                                 (dy (- (point-y pt) (point-y pz))))
                             (begin
                               (if (not (eq? (point-draw pt) (point-draw pz)))
                                   (fprintf port "pen\n")
                                   #f)
                               (if (not (= dx 0))
                                   (if (> dx 0)
                                       (fprintf port "right ~a " dx)
                                       (fprintf port "left ~a " (- dx)))
                                   #f)
                               (if (not (= dy 0))
                                   (if (> dy 0)
                                       (fprintf port "down ~a " dy)
                                       (fprintf port "up ~a " (- dy)))
                                   #f)
                               (newline port))
                             pt))
                         (point #t 0 0) poly))
                #:exists 'truncate)
              poly))
           (else
            (let ((dx (car ret))
                  (dy (cdr ret))
                  (prev-x (point-x (car poly)))
                  (prev-y (point-y (car poly))))
              (if (and (= dx 0) (= dy 0))
                  poly
                  (cons (point draw (+ prev-x dx) (+ prev-y dy)) poly)))))))))

(define file-name
  (if (= (vector-length (current-command-line-arguments)) 0)
      #f
      (vector-ref (current-command-line-arguments) 0)))   

(send frame show #t)

(thread (λ ()
          (let ((func (λ (port interactive)
                        (begin
                          (let loop ()
                            (let ((ret (process poly port interactive)))
                              (if (equal? ret #f)
                                  #f
                                  (begin
                                    (set! poly ret)
                                    (send canvas refresh)
                                    (loop)))))))))
            (if (not (equal? file-name #f))
                (if (file-exists? file-name)
                    (call-with-input-file file-name
                      (λ (port) (func port #f))) #f) #f)
            (func (current-input-port) #t))
          (send frame show #f)))
