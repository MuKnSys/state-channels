(use-modules (ice-9 popen)
             (ice-9 rdelim))

(define (sh-cmd CMD) ;; Code extracted from basics.ss
  (let* ((PORT (open-input-pipe CMD)) ;; FIXME: seems (open-input-pipe) doesn't exists in Gerbil ; find a way
         (S  (read-line PORT))
         (RES '()))
    (while (not (eof-object? S))
      (set! RES (cons S RES))
      (set! S (read-line PORT)))
    (close-pipe PORT)
    (reverse RES)))

(define L (command-line))
(if (< (length L) 2)
(begin
  (display (car (reverse (string-split (list-ref L 0) #\/))))
  (display " <ENV> expected\n")
  (exit 1)))

(define CWD (getcwd))
(define CMD (list-ref L 0))
(define BACKENDS #f)
(if (not (equal? (string-ref CMD 0) #\.))
  (set! BACKENDS (string-append CWD "/" CMD))
  (error "Calc full path !Yet"))
(set! BACKENDS (string-join (reverse (cdr (reverse (string-split BACKENDS #\/)))) "/"))

(define DIR (string-append BACKENDS "/" (list-ref L 1)))
(if (not (access? DIR F_OK))
  (error (string-append "Directory " DIR " not found")))

(define (fext FNAME)
  (define L (string-split FNAME #\.))
  (if (eq? (length L) 1)
      ""
      (car (reverse L))))

(define (destdir FNAME)
  (define EXT (fext FNAME))
  (cond ((equal? EXT "")
         "bin")
        ((equal? EXT "ss")
         "src")
        (else
          (error "destdir"))))

(define (diff FNAME)
  (let* ((SRC (string-append DIR "/" FNAME))
         (DEST (string-append "../" (destdir FNAME) "/" FNAME))
        )
   ;(display (string-append "Diffing " SRC " => " DEST "\n"))
    (sh-cmd (string-append "diff -q " SRC " " DEST))))
             
(define (diff* . LF)
  (map (lambda (FN)
         (diff FN))
       LF))

(define (cp FNAME) ;; TODO: keep file attributes (?)
  (let* ((SRC (string-append DIR "/" FNAME))
         (DEST (string-append "../" (destdir FNAME) "/" FNAME))
        )
    (display (string-append "Copying " SRC " => " DEST "\n"))
    (copy-file SRC DEST)))
             
(define (cp* . LF)
  (for-each (lambda (FN)
              (cp FN))
            LF))

(define (diff*! . LF)
  (define RDIFF (apply diff* LF))
  (set! RDIFF (filter (lambda (X) (not (null? X))) RDIFF))
  (if (not (null? RDIFF)) ;; TODO: do that only if we know that the currently installed version is the one to build
    (for-each (lambda (X)
                (display (car X))
                (display "\n")) RDIFF)))

(define (build . LF)
 ;(define RDIFF (apply diff* LF))
 ;(set! RDIFF (filter (lambda (X) (not (null? X))) RDIFF))
  (if #f ;;(not (null? RDIFF)) ;; TODO: do that only if we know that the currently installed version is the one to build
    (for-each (lambda (X)
                (display (car X))
                (display "\n")) RDIFF)
    (apply cp* LF)))

(chdir BACKENDS)
