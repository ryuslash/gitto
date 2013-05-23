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

(define-module (gitto ui)
  #:use-module (ice-9 rdelim)
  #:export (y-or-n?))

(define* (y-or-n? prompt #:key (default #f))
  (format #t "~a [~a] " prompt (if default "Y/n" "y/N"))
  (let ((char (read-char)))

    ;; Clear the rest of the input buffer.
    (unless (eq? char #\newline)
      (read-line))

    (case char
      ((#\y #\Y #\newline) #t)
      ((#\n #\N) #f)
      (else
       (display "Invalid response, please use `y' or `n'.")
       (newline)
       (y-or-n? prompt #:default default)))))
