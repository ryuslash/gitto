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

(define-module (gitto main)
  #:use-module (gitto path)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (main))

(define-generic same-repository?)
(define-generic print)

(define-class <repository> ()
  (name #:getter repo-name)
  (location #:getter repo-location)
  (clean? #:getter repo-clean?)
  (pushable #:getter repo-pushable)
  (pullable #:getter repo-pullable)
  (updated #:getter repo-updated))

(define-method (initialize (repo <repository>) args)
  (let ((dir (car args)))
    (slot-set! repo 'name (basename dir))
    (slot-set! repo 'location dir)
    (slot-set! repo 'clean? (delay (git-clean? dir)))
    (slot-set! repo 'pushable (delay (git-revs-to-push dir)))
    (slot-set! repo 'pullable (delay (git-revs-to-pull dir)))
    (slot-set! repo 'updated (delay (git-last-update dir)))))

(define-method (repo-clean? (repo <repository>))
  (force (slot-ref repo 'clean?)))
(define-method (repo-pushable (repo <repository>))
  (force (slot-ref repo 'pushable)))
(define-method (repo-pullable (repo <repository>))
  (force (slot-ref repo 'pullable)))
(define-method (repo-updated (repo <repository>))
  (force (slot-ref repo 'updated)))

(define-method (print (repo <repository>))
  (if (file-exists? (repo-location repo))
      (format
       #t "~a:~15t~d to push, ~d to pull and is ~a. Last update: ~a\n"
       (repo-name repo) (repo-pushable repo) (repo-pullable repo)
       (if (repo-clean? repo) "clean" "dirty") (repo-updated repo))
      (format #t "~a:~15tnot found at ~s\n"
              (repo-name repo) (repo-location repo))))

(define (storage-dir xdg-env fallback)
  (let ((xdg (getenv xdg-env)))
    (string-append
     (or xdg (getenv "HOME")) (unless xdg "/" fallback) "/gitto")))

(define (config-dir) (storage-dir "XDG_CONFIG_HOME" "/.config"))
(define (config-file file) (string-append (config-dir) "/" file))
(define (data-dir) (storage-dir "XDG_DATA_HOME" "/.local/share"))
(define (data-file file) (string-append (data-dir) "/" file))

(define (version)
  "Display version information."
  (display "gitto 0.1.0") (newline)
  (display "Copyright (C) 2012 Tom Willemsen") (newline)
  (display "This program comes with ABSOLUTELY NO WARRANTY.") (newline)
  (display "You may redistribute copies of this program") (newline)
  (display "under the terms of the GNU General Public License.") (newline)
  (display "For more information about these matters, see the file named COPYING.") (newline))

(define (help)
  "Display some help."
  (display "\
gitto [options]
  -r, --register REPO  Register a new repository directory
  -R, --remove REPO    Remove a repository directory
  -l, --repositories   List all registered repositories' locations
  -p, --purge          Remove all repositories that don't exist
  -v, --version        Display version
  -h, --help           Display this help
"))

(define (git-dir? dir)
  "Check whether or not DIR/.git exists."
  (let ((dir (string-append dir "/.git")))
    (if (file-exists? dir)
        (let ((dirstat (stat dir)))
          (eq? (stat:type dirstat) 'directory))
        #f)))

(define-method (same-repository? (x <repository>) (y <repository>))
  (string= (repo-location x) (repo-location y)))
(define-method (same-repository? (x <string>) (y <repository>))
  (string= x (repo-location y)))
(define-method (same-repository? (x <repository>) (y <string>))
  (string= (repo-location x) y))

(define (known? repo)
  "Do we know REPO?"
  (or (member repo repositories same-repository?)
      (member (realpath (if (string? repo) repo (repo-location repo)))
              repositories same-repository?)))

(define (save-repositories-list)
  "Save the list of repositories."
  (let ((dir (data-dir)))
    (unless (file-exists? dir)
      (mkdir dir)))

  ;; Sort first
  (set! repositories
        (sort repositories
              (lambda (s1 s2)
                (string<? (repo-name s1) (repo-name s2)))))

  (let ((port (open-output-file repositories-file))
        (repos (map repo-location repositories)))
    (write repos port)
    (close-port port)))

(define (repository-registered? repository)
  "Check to see if REPOSITORY has been registered."
  (format #t "Repository is~a registered~%"
          (if (known? repository) "" " not")))

(define (register-repository repository)
  "Register REPOSITORY in the repository list."
  (set! repository (make <repository> (realpath repository)))
  (if (not (known? repository))
      (begin
        (set! repositories (append `(,repository) repositories))
        (save-repositories-list)
        (simple-format #t "Repository ~A registered."
                       (repository-name repository)))
      (display "Repository already registered."))
  (newline))

(define (remove-repository repository)
  "Remove/unregister REPOSITORY from the repository list."
  (unless (member repository repositories same-repository?)
    (set! repository (realpath repository)))

  (if (known? repository)
      (begin
        (set! repositories
              (delete repository repositories same-repository?))
        (save-repositories-list)
        (simple-format #t "Repository ~A removed." repository))
      (display "Not a registered repository."))
  (newline))

(define (git-revs-to-push dir)
  "Check how many commits should be pushed upstream."
  (let* ((pipe (start-git dir "log --pretty=oneline @{u}.." "| wc -l"))
         (num (string->number (read-line pipe))))
    (close-pipe pipe)
    num))

(define (git-revs-to-pull dir)
  "Check how many commits should be pulled/merged from upstream."
  (let* ((pipe (start-git dir "log --pretty=oneline ..@{u}" "| wc -l"))
         (num (string->number (read-line pipe))))
    (close-pipe pipe)
    num))

(define* (start-git dir args #:optional (extra ""))
  (open-input-pipe
   (format #f "git --work-tree=~s --git-dir=\"~a/.git\" ~a 2>/dev/null ~a"
           dir dir args extra)))

(define (git-clean? dir)
  "Check whether a repository is clean, meaning there are no changes
to the tracked files. Utracked files will not register."
  (let* ((pipe (start-git dir "status -suno"))
         (clean? (eof-object? (read-delimited "" pipe))))
    (close-pipe pipe)
    clean?))

(define (git-last-update dir)
  "Check when the last update upstream was."
  (let* ((pipe (start-git dir "log -1 --format=%ar @{u}"))
         (relative-last-update (read-line pipe)))
    (close-pipe pipe)
    (if (eof-object? relative-last-update)
        "never"
        relative-last-update)))

(define (list-repositories)
  "List information about every repository."
  (for-each print repositories))

(define (list-repository-locations)
  "List the registered locations of repositories."
  (for-each
   (lambda (repo)
     (display (repo-location repo))
     (newline))
   (sort repositories
         (lambda (s1 s2)
           (string<? (repo-location s1) (repo-location s2))))))

(define (purge)
  "Purge all items from the list that can no longer be found."
  (set! repositories  (filter file-exists? repositories))
  (save-repositories-list))

(define option-spec
  `((version      (single-char #\v))
    (help         (single-char #\h))
    (register     (single-char #\r) (value #t) (predicate ,git-dir?))
    (remove       (single-char #\R) (value #t) (predicate ,known?))
    (repositories (single-char #\l))
    (purge        (single-char #\p))
    (check        (single-char #\c) (value #t))))

(define (main args)
  "Parse the command line options and run the appropriate functions."
  (let* ((options (getopt-long args option-spec))
         (help-wanted?         (option-ref options 'help #f))
         (version-wanted?      (option-ref options 'version #f))
         (registration-needed? (option-ref options 'register #f))
         (removal?             (option-ref options 'remove #f))
         (list?                (option-ref options 'repositories #f))
         (purge?               (option-ref options 'purge #f))
         (check?               (option-ref options 'check #f))
         (cfg (config-file "rc.scm")))
    (when (file-exists? cfg)
      (save-module-excursion
       (lambda ()
         (set-current-module (resolve-module '(gitto main)))
         (primitive-load cfg))))

    (cond (version-wanted?         (version))
          (help-wanted?            (help))
          (registration-needed? => register-repository)
          (removal?             => remove-repository)
          (list?                   (list-repository-locations))
          (purge?                  (purge))
          (check?               => repository-registered?)
          (#t                      (list-repositories)))))

(define repositories-file (data-file "repos.scm"))

(define repositories
  (if (file-exists? repositories-file)
      (let* ((port (open-input-file repositories-file))
             (result (read port)))
        (close-port port)
        (map-in-order (lambda (repo) (make <repository> repo)) result))
      '()))
