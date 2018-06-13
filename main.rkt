#lang racket

(require web-server/servlet)
(provide/contract (start (request? . -> . response?)))
(require web-server/servlet-env)

(struct post
  (name
   content
   id
   post_time)
  #:mutable)

(struct thread
  (posts id topic)
  #:mutable)

(struct board
  (threads name)
  #:mutable)

(define last-num -1)

(define /b/
  (board (list ;; The diffrent threads in a board
          (thread (list ;; The diffrent posts in the thread
                   (post "Anomymous"
                         "Hello this is a test"
                         1
                         "04:20")
                   (post "Anomymous2"
                         "Hello this is a test"
                         2
                         "04:22"))
                  1
                  "testing")
          (thread (list ;; The diffrent posts in the thread
                   (post "Anomymous1"
                         "Hello this is a test1"
                         1
                         "04:21")
                   (post "Anomymous2"
                         "Hello this is a test"
                         2
                         "04:22"))
                  2
                  "testing1")
          )
         "/b/"))

(define (render-board B)
  (quasiquote (div ((class "board"))
                   (unquote-splicing (map render-thread-as-list (board-threads B))))))

(define (get-thread-by-id board thread_number f)
  "Get or apply a function on a given thread"
  (let ((answer ;; Either '() if out of bounds or '(thing i want)
         (filter (lambda (x) (or (number? x) (string? x) (thread? x)))
                 (map (lambda (current_thread)
                        (if (equal?  (thread-id current_thread) thread_number)
                            (f current_thread)
                            '()))
                      (board-threads board)))))
    (if (null? answer) #f (car answer))))

(define (get-post-by-id board thread_id post_id f)
  "Get or apply a function on a given post"
  (let ((answer ;; Either '() if out of bounds or '(thing i want)
         (filter (lambda (x) (or (number? x) (string? x) (post? x) (thread? x)))
                (map (lambda (current_post)
                       (if (equal? (post-id current_post) post_id)
                           (f current_post)
                           '()))
                     (thread-posts (get-thread-by-id board thread_id (lambda (x) x)))))))
     (if (null? answer) #f (car answer))))

(define (render-thread-as-list T)
  (quasiquote (div ((class "thread"))
                   (div (unquote (thread-topic T)))
                   (div (unquote (number->string (thread-id T))))
                   (ul (unquote-splicing
                        (map render-post-as-list-item (thread-posts T))))
                   (div (unquote (render-post-form T)))
              (br))))

(define (render-post-as-list-item P)
  (if (empty? P) (quote (p "Empty thread"))
      (quasiquote (li ((class "post"))
                      (ul (li (unquote (post-name P)))
                          (li (unquote (number->string (post-id P))))
                          (li (unquote (post-post_time P)))
                          (li (unquote (post-content P))))))))


;; Creating a post ;;
(define (render-post-form T)
  (quasiquote
   (form (input ((name (unquote (string-append "name:"
                                               (number->string (thread-id T)))))))
         (input ((name (unquote (string-append "content:"
                                               (number->string (thread-id T)))))))
         (input ((name (unquote (string-append "where:"
                                               (number->string (thread-id T)))))))
         (input ((type "submit"))))))

(define (can-parse-post? bindings thread-number)
  (and (exists-binding? (string->symbol (string-append "name:"  (number->string thread-number))) bindings)
       (exists-binding? (string->symbol (string-append "content:"  (number->string thread-number))) bindings)))

(define (parse-post bindings thread-number)
  (post (extract-binding/single (string->symbol (string-append "name:"  (number->string thread-number))) bindings)
        (extract-binding/single (string->symbol (string-append "content:" (number->string thread-number))) bindings)
        69
        "13:37"))

(define (modify-single-thread used-board thread-number function)
  (map (lambda (T) (if (= (thread-id T) thread-number)
                       (function T)
                       T))
        (board-threads used-board)))

(define (insert-post! used-board thread-number new-post)
  (set-board-threads! used-board
                      (modify-single-thread used-board
                                            thread-number
                                            (lambda (T) ;; Append a given post to the thread: Thread -> Thread
                                              (thread (append (thread-posts T) (list new-post))
                                                      (thread-id T)
                                                      (thread-topic T))))))

                       
;; Creating a thread ;;
(define (render-thread-form)
  (quasiquote
   (form (input ((name "topic")))
         (input ((type "submit"))))))

(define (can-parse-thread? bindings)
  (exists-binding? 'topic bindings))

(define (parse-thread bindings)
  (thread '()
          420
          (extract-binding/single 'topic bindings)))

;; board, quoted thread -> None
(define (insert-thread! used-board new-thread)
  (set-board-threads! used-board
                     (append (board-threads used-board)
                             (list new-thread))))

;; board, request -> response/xexpr
(define (render-entire-page chosen-board request)
  (response/xexpr
   (quasiquote
    (html (head (title "testchan"))
          (body (h1 (unquote (string-append "Welcome! to " (board-name chosen-board))))
                (unquote (render-board chosen-board))
                (unquote (render-thread-form)))))))

;; bindings -> number
(define (extract-thread-number bindings)
  (let* ((final-number 1)
         (n (map (lambda (symbol)
                   (let ((id? (cdr (string-split (symbol->string (car symbol)) ":"))))
                     (cond ((number? id?) (set! final-number id?)))))
                 (car (list bindings)))))
    final-number))

(define (extract-id bindings)
  (string->number (car (cdr (string-split (symbol->string (car (car bindings))) ":")))))

;; request -> None
(define (start request)
  (let* ((bindings (request-bindings request))
         (thread-number (if (not (can-parse-thread? bindings)) (extract-id bindings) 1)))
  (cond ((can-parse-thread? bindings)
         (insert-thread! /b/ (parse-thread bindings)))
        ((can-parse-post? bindings thread-number)
         (begin (set! last-num thread-number)
                (insert-post! /b/ thread-number (parse-post bindings thread-number)))))
  (render-entire-page /b/ request)))

(define (run)
  (serve/servlet start
                 #:launch-browser? #f
                 #:quit? #f
                 #:listen-ip #f
                 #:port 8000
                 #:servlet-path
                 "/space"
                 ))
