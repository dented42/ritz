#lang racket

(require libuuid
         rackunit
         typed/untyped-utils
         (except-in typed/racket #%top-interaction #%module-begin)
         "uuid-storage.rkt"
         "../uuid-box.rkt"
         "../uuid-storage.rkt"
         )

(module+ test
  
  (test-case "make-uuid-box"
    (fail "write test"))
  
  (test-case "fetch-uuid-box"
    (let* ([uuid0 (uuid-generate)]
                      [store (make-simple-store (make-hash `((,uuid0 . thing))))]
                      #;[thing-box (fetch-uuid-box:hs store uuid0)])
      (fail "write test")))
  
  (test-case "uuid-unbox"
    (fail "write test"))
  
  (test-case "uuid-unbox fetched box"
    (fail "write test"))
  
  (test-case "uuid-unbox new box"
    (fail "write test"))
  
  (test-case "uuid-unbox/refresh"
    (fail "write test"))
  
  (test-case "set-uuid-box!"
    (fail "write test"))
  
  (test-case "uuid-boxes reset during garbage collection"
    (fail "write test"))
  
  (test-case "uuid-boxes don't reset if there isn't a garbage collection"
    (fail "write-test")))
