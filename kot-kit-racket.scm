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

#lang racket

(require rnrs/io/ports-6)     ;; Racket

;; Helper
(define (answer . params)
  (for-each (lambda (x) (display x)) params)
  (flush-output-port (current-output-port))
  (get-line (current-input-port)))

(define (yes? . params)
  (let ((ans (apply answer (append params '("? ")))))
    (if (= (string-length ans) 0)
        #f
        (or
         (char-ci=? (string-ref ans 0) #\y)
         (char-ci=? (string-ref ans 0) #\д)))))

;; Copy tree
(define (copy-pet-tree pet-tree)
  (if (null? pet-tree)
      '()
      (list (car pet-tree)
            (cadr pet-tree)
            (copy-pet-tree (caddr pet-tree))
            (copy-pet-tree (cadddr pet-tree)))))

;; Main function
(define (game-round pet-tree top-pet-name)
  (let walk-pet-tree ((pet-tree pet-tree)
                      (y-pet (list top-pet-name)))
    (if (null? pet-tree)
        (if (yes? "Это " (car y-pet))
            (begin
              (display "Ура, я угадал!")
              (newline)
              '())
            (let* ((name (answer "Сдаюсь! Кто это? "))
                   (feature (answer "Чем " name " отличается от " (car y-pet) "? ")))
              (list name feature '() '()))) ;; return new pet
        (append
         (list (car pet-tree)
               (cadr pet-tree))
         (if (yes? (cadr pet-tree))
             (list (walk-pet-tree (caddr pet-tree) pet-tree)
                   (copy-pet-tree (cadddr pet-tree)))
             (list (copy-pet-tree (caddr pet-tree))
                   (walk-pet-tree (cadddr pet-tree) y-pet)))))))

(define TREE-FILE-NAME "data.sexp")

;; Main LOOP
(let loop ((pet-tree
            (if (file-exists? TREE-FILE-NAME)
                (call-with-input-file TREE-FILE-NAME read)
                '())))
  (let ((tree (game-round pet-tree "Кот")))
    (newline)
    (if (yes? "Играем еще")
        (loop tree)
        (begin
          ;; Racket
          (if (file-exists? TREE-FILE-NAME) (delete-file TREE-FILE-NAME) #f)

          (call-with-output-file TREE-FILE-NAME (lambda (p) (write tree p)))
          ))))
