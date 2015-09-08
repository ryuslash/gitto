;; -*- coding: utf-8; -*-
;; gitto -- Keep track of your git repositories
;; Copyright (C) 2012 Tom Willemse <tom at ryuslash dot org>

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

(define-module (gitto command)
  #:export (define-command
            command?
            command-documentation
            command-function
            for-each-command))

(define-syntax define-command
  (syntax-rules ()
    ((_ (name . args)
        usage
        longdoc
        exp exp* ...)
     (begin
       (set! command-list
             (cons
              (list (symbol->string (quote name))
                    (cons
                     #:function
                     (case-lambda*
                      (args
                       exp exp* ...)
                      (lst (format #t "Wrong number of arguments.~%"))))
                    (cons #:usage usage)
                    (cons #:documentation longdoc))
              command-list))))))

(define command-list '())

(define (command? name)
  "Check if NAME corresponds to a command."
  (and (assoc-ref command-list name) #t))

(define (command-documentation command)
  "Get the documentation for COMMAND."
  (assq-ref (assoc-ref command-list command) #:documentation))

(define (command-function command)
  "Get the function for COMMAND."
  (assq-ref (assoc-ref command-list command) #:function))

(define (for-each-command function)
  "Execute FUNCTION for each known command."
  (for-each function command-list))
