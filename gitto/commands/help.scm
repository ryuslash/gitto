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

(define-module (gitto commands help)
  #:use-module (gitto command)
  #:use-module (ice-9 format))

(define (print-command-help command)
  "Print the help message for COMMAND."
  (if (command? command)
      (format #t "~a~%" (command-documentation command))
      (format #t "Unknown command: ~a~%" command)))

(define (print-short-command-help command)
  "Print COMMAND's name and its short description."
  (format #t "  ~a~15t~a~%" (car command) (assq-ref command #:usage)))

(define (print-general-help)
  "Print the general help message for gitto."
  (display "gitto [command [arguments ...]]")
  (newline)
  (for-each-command print-short-command-help))

(define-command (help #:optional command)
  "Display this help."
  "Usage: gitto help [COMMAND]

Display a help message. If COMMAND is not specified, print some
information about gitto, otherwise print some information about
COMMAND."
  (if command
      (print-command-help command)
      (print-general-help)))
