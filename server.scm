(use tcp6 srfi-18 srfi-69)

(define values (make-hash-table))
(define change-mutex (make-mutex))
(define read-mutex (make-mutex))

(define (set-value! key value)
  (mutex-lock! change-mutex)
  (hash-table-set! values key value)
  (format #t "Settings ~a to ~a~%" key value)
  (print (hash-table->alist values))
  (mutex-unlock! change-mutex))

(define (get-value! key)
  (mutex-lock! read-mutex)
  (let ((r (if (hash-table-exists? values key)
               (hash-table-ref values key)
               #f)))
    (mutex-unlock! read-mutex)
    r))

(define l (tcp-listen 4242))

(define (close-client i o)
  (print "closing client")
  (close-input-port i)
  (close-output-port o))

(define (variable-set-name s)
  (if (and (> (string-length s) 0)
           (eq? (string-ref s 0) #\>))
      (substring s 1)
      #f))

(define (variable-follow-name s)
  (if (and (> (string-length s) 0)
           (eq? (string-ref s 0) #\<))
      (substring s 1)
      #f))

(define (handle-client i o)
  (print "accepted client")
  (write-line "hello client!" o)
  (condition-case
   ((lambda ()
      (let loop ((line (read-line i))
                 (current-set "void")
                 (current-follow #f))
        (unless (eof-object? line)
                (print line)
                (let ((get-name (variable-set-name line))
                      (follow-name (variable-follow-name line)))
                  (when (cond
                         ;; command was a variable name to set to
                         (get-name (format #t "current name is ~a~%" get-name))
                         ;; command was a variable name to follow
                         (follow-name
                          (format #t "following ~a~%" follow-name)
                          (let ((value (get-value! follow-name)))
                            (if (string? value) (write-line value o))))
                         ;; command was exit
                         ((equal? "" line)
                          (close-client i o)
                          #f) ;; exit
                         ;; else command is a value ...
                         (else (set-value! current-set line)))
                        (thread-sleep! 0.01)
                        (loop (read-line i)
                              (or get-name current-set)
                              (or follow-name current-follow))))))))
    ;; client timed out
    ((exn timeout) (close-client i o))))

(let loop ()
  (when (tcp-accept-ready? l)
        (print "client is waiting ...")
        (let-values (((i o) (tcp-accept l)))
          (thread-start! (lambda () (handle-client i o)))))
  (thread-sleep! 0.01)
  (loop))
