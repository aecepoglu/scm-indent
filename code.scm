#!/usr/bin/guile \
-e main -s
!#

(import (ice-9 match))

(define (replace-car x l)
        (cons x (cdr l)))

(define (safe-cdr l)
        (if (null? l)
            l
            (cdr l)))

(define (read-chars-until k)
        (define (aux acc c)
                (cond ((eof-object? c) (error (string-append
                                               "EOF found on a non-empty line."
                                               " I assumed this couldn't happen")))
                      ((eq? c k) (list (eof-object? (peek-char))
                                       (reverse acc)))
                      (#t (aux (cons c acc)
                               (read-char)))))
        (aux '() (read-char)))

(define (fold-stdin-lines f acc0)
        (define (aux acc cur i)
                (let ((acc-1 (f acc (cadr cur) i (car cur))))
                     (if (car cur)
                         (reverse acc)
                         (aux acc-1
                              (read-chars-until #\newline)
                              (+ 1 i)))))
        (aux acc0 (read-chars-until #\newline) 0))

(define (foo refmt? stack0 l0)
        (define (aux col stack state acc l)
                (match (cons state l)
                       (('indent #\space tl ...)(if refmt?
                                                    (aux col stack state acc tl)
                                                    (aux (+ 1 col) stack state (cons #\space acc) tl)))
                       (('indent tl ...)        (if refmt?
                                                    (aux (car stack)
                                                         stack
                                                         'code
                                                         (make-list (car stack) #\space)
                                                         tl)
                                                    (aux col stack 'code acc tl)))
                       (('code #\" tl ...)       (aux (+ 1 col) stack                         'string (cons #\" acc)          tl))
                       (('string #\\ #\" tl ...) (aux (+ 2 col) stack                         'string (append '(#\\ #\") acc) tl))
                       (('string #\" tl ...)     (aux (+ 1 col) stack                         'code   (cons #\" acc)          tl))
                       (('string hd tl ...)      (aux (+ 1 col) stack                         'string (cons hd acc)           tl))
                       ((_ #\( tl ...)           (aux (+ 1 col) (cons (+ 1 col) stack)        'code   (cons #\( acc)          tl))
                       ((_ #\) tl ...)           (aux (+ 1 col) (safe-cdr stack)              'code   (cons #\) acc)          tl))
                       ((_ #\space tl ...)       (aux (+ 1 col) (replace-car (+ 1 col) stack) 'code   (cons #\space acc)      tl))
                       ((_ hd tl ...)            (aux (+ 1 col) stack                         'code   (cons hd acc)           tl))
                       ((_)                      (list (reverse acc) stack))))
        (aux 0 stack0 'indent '() l0))


(define (run! fmt-start)
        (define refmt-line? (if (< fmt-start 0)
                                (lambda (_ last?) last?)
                                (lambda (i _) (>= i fmt-start))))
        (define (foo! refmt? stack0 l0)
                (let ((r (foo refmt? stack0 l0)))
                     (if refmt-line?
                         (begin
                          (display (list->string (car r)))
                          (newline)))
                     (cadr r)))
        (fold-stdin-lines (lambda (acc cur line-i last?)
                                  ;(display (list "DEBUG" (list->string cur) acc line-i last?)) (newline)
                                  (foo! (refmt-line? line-i last?)
                                        acc
                                        cur))
                          '(0)))

(define (main args)
        (define (usage-and-exit err-msg)
                (display "Error: ")
                (display err-msg)
                (display "\nUsage: ")
                (display (car args))
                (display " [skip-lines]\n")
                (display "  * skip-lines: Number of lines to not reindent. ")
                (display "Can be set to -1 to mean \"all lines but the last.\"")
                (exit 1))
        (let ((skip-i (cond ((<= (length args) 1) 0)
                            ((= (length args) 2) (string->number (cadr args)))
                            (#t (usage-and-exit "wrong number of arguments")))))
             (cond ((not skip-i) (usage-and-exit "skip-lines must be a number"))
                   ((< skip-i -1) (usage-and-exit "skip-lines must be >= -1"))
                   (#t (run! skip-i)))))

; test helper
(define (assert-eql? quot expected)
        (let ((received (primitive-eval quot)))
             (if (equal? expected received)
                 (display (list "OK" expected "=" quot))
                 (display (list "FAIL" expected "=" quot "\n GOT " received))))
        (display "\n"))

;tests
(define (run-tests)
        (assert-eql? '(replace-car 'newone '(one two three)) '(newone two three))
        (assert-eql? '(replace-car 'newone '(one))           '(newone))
        (assert-eql? '(foo #f '(0) (string->list "  ("))         (list (string->list "  (") '(3 0))); )
        (assert-eql? '(foo #f '(0) (string->list "  (foo"))      (list (string->list "  (foo") '(3 0))) ;)
        (assert-eql? '(foo #f '(0) (string->list "  (foo bar"))  (list (string->list "  (foo bar") '(7 0))) ;)
        (assert-eql? '(foo #t '(5) (string->list "x"))           (list (string->list "     x") '(5)))
        (assert-eql? '(foo #f '(0) '())                          (list '() '(0)))
        )
