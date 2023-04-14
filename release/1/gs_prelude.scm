;; Basic imports
;(import (srfi 1))
;(import (srfi 13))
;(import (srfi 69))
;(import (srfi 132))

;; Disabling some stuff
(define-macro (import . X) #t)
(define-macro (export . X) #t)

;; Lambda
(define-macro (=> LST . CODE)
  `(lambda ,LST . ,CODE))

;; While
(define-macro (while COND . BODY)
  `(do ()
       ((not ,COND))
       .
       ,BODY))

;; Exceptions & error
(define (exit2)
  (display "\n" (current-error-port))
  (exit))

(define (catch FLAG FUNC ERR)
  (with-exception-handler
    ERR
    FUNC))

(define (_error)
  (exit 1))

;; Atoms
(define (unspecified? X)
  (eq? X ((lambda () (if #f 1234)))))

;; Strings
(define (string-contains S SS I0)
  (define LS (string-length S))
  (define LSS (string-length SS))
  (define RES #t)
  (define I 0)
  (if (> (+ I0 LSS) LS)
    #f
    (begin
      (while (< I LSS)
        (if (not (eq? (string-ref SS I) (string-ref S (+ I0 I))))
          (set! RES #f))
        (set! I (+ I 1)))
      (if RES
        RES
        (string-contains S SS (+ I0 1))))))

(define (%substring/shared s start end)
  (if (and (zero? start) (= end (string-length s))) s
      (substring s start end)))

(define (substring/shared s start . end)
  (let ((slen (string-length s)))
    (set! end (if (empty? end) slen (car end)))
    (%substring/shared s start end)))

(define (string-split str delim)
  (let ((add (lambda (current output)
               (if (not (list? output))
                 (set! output '()))
               (if (not (list? current))
                 output
                 (cons (list->string (reverse current)) output)))))
    (let loop ((input (string->list str))
               (output '())
               (current '()))
      (if (null? input)
          (if output
              (reverse (if current
                           (add current output)
                           output))
              '())
          (let ((char (car input))
                (input (cdr input)))
            (if (char=? char delim)
                (loop input (add current output) '())
                (loop input output (cons char current))))))))

(define (string-trim-both S . CH) ;; SRFI-13
  (define RES '())
  (define CS (if (null? CH)
               '(#\newline #\return #\space)
               `(,(car CH))))
  (define I 0)
  (while (< I (string-length S))
    (set! CH (string-ref S I))
    (if (null? (filter (lambda (C) (eq? C CH))
                       CS))
      (set! RES (cons CH RES)))
    (set! I (+ I 1)))
  (list->string (reverse RES)))

(define (string-join strings . delim)
  (define grammar 'infix)
  (set! delim (if (null? delim) " " (car delim)))
  (let ((buildit (lambda (lis final)
                   (let recur ((lis lis))
                     (if (pair? lis)
                         (cons delim (cons (car lis) (recur (cdr lis))))
                         final)))))
    (cond ((pair? strings)
           (string-concatenate
            (case grammar
              ((infix strict-infix)
               (cons (car strings) (buildit (cdr strings) '())))
              ((prefix) (buildit strings '()))
              ((suffix)
               (cons (car strings) (buildit (cdr strings) (list delim))))
              (else (error "Illegal join grammar"
                           grammar string-join)))))
           ((not (null? strings))
            (error "STRINGS parameter not list." strings string-join))
           ;; STRINGS is ()
           ((eq? grammar 'strict-infix)
            (error "Empty list cannot be joined with STRICT-INFIX grammar."
                   string-join))
           (else ""))))

;; Lists
(define (list-head L I)
  (take L I))

;(define (list-copy L) ;; SRFI-1
;  #f)

(define (copy-tree L)
  (if (pair? L)
      (cons (copy-tree (car L)) (copy-tree (cdr L)))
      L))

;(define (append! X Y) ;; SRFI-1
;  #f)
(define (last-pair lis)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))

(define (append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists) (prev '()))
    (if (not (pair? lists)) prev
        (let ((first (car lists))
              (rest (cdr lists)))
          (if (not (pair? first)) (lp rest first)

              ;; Now, do the splicing.
              (let lp2 ((tail-cons (last-pair first))
                        (rest rest))
                (if (pair? rest)
                    (let ((next (car rest))
                          (rest (cdr rest)))
                      (set-cdr! tail-cons next)
                      (lp2 (if (pair? next) (last-pair next) tail-cons)
                           rest))
                    first)))))))

(define (string< S1 S2)
  (define N1 (string-length S1))
  (define N2 (string-length S2))
  (define I 0)
  (define RES #f)
  (define DONE #f)
  (while (and (< I N1) (< I N2))
    (set! C1 (string-ref S1 I))
    (set! C2 (string-ref S2 I))
    (if (not (eq? C1 C2))
      (begin
        (set! DONE #t)
        (set! RES (char<? C1 C2))
        (set! I N1)))
    (set! I (+ I 1)))
  (if (and (not DONE)
           (not (equal? N1 N2)))
    (set! RES (< N1 N2)))
  RES)

(define (sort LST CMP)
  (list-sort CMP LST))

;; Hash tables
(define (make-hashq-table)
  (make-table test: eq?))

(define (make-hashv-table)
  (make-table test: equal?))

(define (hash-length HT)
  (table-length HT))

(define (hash-ref HT KEY)
  (table-ref HT KEY #f))

(define (hash-set! HT KEY VAL)
  (table-set! HT KEY VAL))

(define (hashq-ref HT KEY NOVAL)
  (table-ref HT KEY NOVAL))

(define (hashq-set! HT KEY VAL)
  (hash-set! HT KEY VAL))

(define (hash-remove! HT KEY)
  (table-set! HT KEY))

(define (hash-table-for-each FUNC HT)
  (table-for-each FUNC HT))
(define hash-for-each hash-table-for-each)

;(define (hash-map->list FUNC HT)
;  (hash-table-map->list FUNC HT))
(define (hash-map->list FUNC HT)
  (define RES '())
  (hash-for-each (=> (KEY VAL)
                   (set! RES (cons (FUNC KEY VAL) RES)))
                 HT)
  (reverse RES))

;; Procedures
(define (procedure-name F)
  (error "procedure-name: !Yet"))
