(import (rnrs io ports (6)))  ;; Guile
;;(require rnrs/io/ports-6)     ;; Racket

(setlocale LC_ALL "") ;; Guile

(define (println . params)
  (for-each (lambda (x) (display x)) params)
  (newline))

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
              (println "Ура, я угадал!")
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
          ;; (if (file-exists? TREE-FILE-NAME) (delete-file TREE-FILE-NAME) #f)

          (call-with-output-file TREE-FILE-NAME (lambda (p) (write tree p)))
          ))))

;; vim: set ts=2 sts=2 sw=2 expandtab:
