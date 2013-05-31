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

(define-module (gitto config)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:export (global-config
            hook-alist

            install-hooks
            merge-config
            read-config
            write-config))

(define global-config '())
(define hook-alist '())

(define (install-hooks repo-location)
  "Install each hook in `hook-alist' into REPO-LOCATION."
  (for-each
   (lambda (hook)
     (let ((new-name (string-append repo-location "/.git/hooks/"
                                    (car hook))))
       (unless (file-exists? new-name)
         (symlink (cdr hook) new-name))))
   hook-alist))

(define (merge-config repo-name x y)
  "Merge configuration X with configuration Y.

The values in configuration Y will have `%a' substituted with
REPO-NAME."
  (let ((lst (if x (list-copy x) '())))
    (for-each
     (lambda (s)
       (let ((b-sec (assoc (car s) lst)))
        (set! lst (assoc-set!
                   lst (car s) (merge-settings
                                repo-name (if b-sec (cdr b-sec) #f) (cdr s))))))
     y)
    lst))

(define (merge-setting repo-name lst var val)
  "Merge VAL into LST under VAR, substituting `%a' with REPO-NAME.

In case val is a list, all values in it have `%a' substituted with
REPO-NAME."
  (if (list? val)
      (assoc-set! lst var (map (lambda (v) (format #f v repo-name)) val))
      (assoc-set! lst var (format #f val repo-name))))

(define (merge-settings repo-name x y)
  "Merge the settings in X with those in Y.

During merging values in Y will have `%a' substituted with REPO-NAME."
  (let ((lst (if x (list-copy x) '())))
    (for-each
     (lambda (v)
       (set! lst (merge-setting repo-name lst (car v) (cdr v))))
     y)
    lst))

(define (split-setting line)
  "Split LINE into a cons cell.

LINE should be a string which looks like `key=value'. The result is a
cons cell with `(key . value)'."
  (let ((idx (string-index line #\=)))
    (cons (string-trim-both (substring line 0 idx))
          (string-trim-both (substring line (1+ idx))))))

(define (read-setting settings line)
  "Read and put the setting in LINE into SETTINGS.

LINE should be a string which looks like `key=value'. The result is
SETTINGS with either the setting in LINE added to it, the current
value of the variable specified in LINE overwritten with the value in
LINE or the value in LINE appended to the existing variable in
SETTINGS."
  (let* ((new-setting (split-setting line))
         (var (car new-setting)) (val (cdr new-setting))
         (current-value (assoc-ref settings var)))
    (if current-value
        (if (list? current-value)
            (assoc-set! settings var (append current-value (list val)))
            (assoc-set! settings var (list current-value val)))
        (assoc-set! settings var val))))

(define (read-config repo-location)
  "Read the configuration for the git repository at REPO-LOCATION.

This procedure returns an alist of `(SECTION-TITLE . SETTINGS)' cells
where SETTINGS is an alist of `(VARIABLE . VALUE)' cells."
  (let ((port (open-input-file
               (string-append repo-location "/.git/config")))
        (config '())
        (current-section #f))
    (do ((line (read-line port) (read-line port)))
        ((eof-object? line))
      (cond ((string= line "[" 0 1)
             (let ((section (cons (string-trim-both
                                   line (char-set #\[ #\])) '())))
               (set! config (append config (list section)))
               (set! current-section section)))
            ((string-contains line "=")
             (set-cdr! current-section
                       (read-setting (cdr current-section) line)))))
    (close-port port)
    config))

(define* (write-config config #:optional (file #f))
  (let ((thunk (lambda () (for-each write-section config))))
    (if file
        (with-output-to-file file thunk)
        (thunk))))

(define (write-section section)
  "Output SECTION as a git config section.

This prints SECTION's car as a section header and prints all the
settings in SECTION's cdr."
  (format #t "[~a]~%" (car section))
  (for-each write-setting (cdr section)))

(define (write-setting setting)
  "Write SETTING to `standard-output'.

SETTING should be a cons cell or a proper list. In the case of it
being a proper list a key=value line will be printed for each item in
the cdr of SETTING. In case of a cons cell where the cdr isn't a list
just one line will be printed."
  (let ((value (cdr setting)))
    (if (list? value)
        (map (lambda (v)
               (format #t "~8t~a = ~a~%" (car setting) v)) value)
        (format #t "~8t~a = ~a~%" (car setting) (cdr setting)))))
