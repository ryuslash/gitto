;; -*- coding: utf-8; -*-
;; gitto -- Keep track of your git repositories
;; Copyright (C) 2012 Tom Willemsen <tom at ryuslash dot org>

;; This file is part of gitto.

;; gitto is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; gitto is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with gitto.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gitto config)
  #:use-module (ice-9 rdelim)
  #:export (read-config write-config))

(define (parse-setting line)
  (let ((idx (string-index line #\=)))
    (list (cons (string-trim-both (substring line 0 idx))
                (string-trim-both (substring line (1+ idx)))))))

(define (read-config repo-location)
  (let ((port (open-input-file
               (string-append repo-location "/.git/config")))
        (config '())
        (current-section #f)
        (assign-pos #f))
    (do ((line (read-line port) (read-line port)))
        ((eof-object? line))
      (cond ((string= line "[" 0 1)
             (let ((section (cons (string-trim-both
                                   line (char-set #\[ #\])) '())))
               (set! config (append config (list section)))
               (set! current-section section)))
            ((string-contains line "=")
             (set-cdr! current-section
                       (append (cdr current-section)
                               (parse-setting line))))))
    (close-port port)
    config))

(define* (write-config config #:optional (file #f))
  (let ((thunk (lambda () (for-each write-section config))))
    (if file
        (with-output-to-file file thunk)
        (thunk))))

(define (write-section section)
  (format #t "[~a]~%" (car section))
  (for-each write-setting (cdr section)))

(define (write-setting setting)
  (format #t "~8t~a = ~a~%" (car setting) (cdr setting)))
