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
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (main))

(define data-dir
  (let ((xdg (getenv "XGD_DATA_HOME"))
        (name "gitto"))
    (if xdg
        (string-append xdg "/" name)
        (string-append (getenv "HOME") "/." name))))

(define repositories-file
  (string-append data-dir "/repos.scm"))

(define repositories
  (if (file-exists? repositories-file)
      (let* ((port (open-input-file repositories-file))
             (result (read port)))
        (close-port port)
        result)
      '()))

(define (version)
  "Display version information"
  (display "gitto 0.1") (newline)
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
  -R, --remove REPO    Repmove a repository directory
  -l, --repositories   List all registered repositories' locations
  -v, --version        Display version
  -h, --help           Display this help
"))

(define (git-dir? dir)
  "Check whether or not DIR/.git exists"
  (let ((dir (string-append dir "/.git")))
    (if (file-exists? dir)
        (let ((dirstat (stat dir)))
          (eq? (stat:type dirstat) 'directory))
        #f)))

(define (save-repositories-list)
  "Save the list of repositories."
  (if (not (file-exists? data-dir))
      (mkdir data-dir))

  ;; Sort first
  (set! repositories
        (sort repositories
              (lambda (s1 s2)
                (string<? (basename s1) (basename s2)))))

  (let ((port (open-output-file repositories-file)))
    (write repositories port)
    (close-port port)))

(define (register-repository repository)
  (if (not (member repository repositories))
      (begin
        (set! repositories (append `(,repository) repositories))
        (save-repositories-list)
        (simple-format #t "Repository ~A registered." repository))
      (display "Repository already registered."))
  (newline))

(define (remove-repository repository)
  (if (member repository repositories)
      (begin
        (set! repositories (delete repository repositories))
        (save-repositories-list)
        (simple-format #t "Repository ~A removed." repository))
      (display "Not a registered repository."))
  (newline))

(define (git-revs-to-push)
  (let* ((pipe (open-input-pipe
                "git log --pretty=oneline @{u}.. 2>/dev/null | wc -l"))
         (num (string->number (read-line pipe))))
    (close-pipe pipe)
    num))

(define (git-revs-to-pull)
  (let* ((pipe (open-input-pipe
              "git log --pretty=oneline ..@{u} 2>/dev/null | wc -l"))
         (num (string->number (read-line pipe))))
    (close-pipe pipe)
    num))

(define (git-clean?)
  (let* ((pipe (open-input-pipe "git status -suno 2>/dev/null"))
         (clean? (eof-object? (read-delimited "" pipe))))
    (close-pipe pipe)
    clean?))

(define (git-last-update)
  (let* ((pipe (open-input-pipe
                "git log -1 --format=%ar @{u} 2>/dev/null"))
         (relative-last-update (read-line pipe)))
    (close-pipe pipe)
    (if (eof-object? relative-last-update)
        "never"
        relative-last-update)))

(define (list-repositories)
  (for-each
   (lambda (repo)
     (chdir repo)
     (let ((numup (git-revs-to-push))
           (numdown (git-revs-to-pull))
           (clean? (git-clean?))
           (lastupdate (git-last-update)))
       (format #t
        "~a:~15t~d to push, ~d to pull and is ~adirty. Last update: ~a\n"
        (basename repo) numup numdown (if clean? "not " "") lastupdate)))
   repositories))

(define (list-repository-locations)
  (for-each (lambda (repo)
              (display repo)
              (newline))
            (sort repositories string<?)))

(define option-spec
  `((version      (single-char #\v))
    (help         (single-char #\h))
    (register     (single-char #\r) (value #t) (predicate ,git-dir?))
    (remove       (single-char #\R) (value #t) (predicate ,git-dir?))
    (repositories (single-char #\l))))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted         (option-ref options 'help #f))
         (version-wanted      (option-ref options 'version #f))
         (registration-needed (option-ref options 'register #f))
         (removal             (option-ref options 'remove #f))
         (list                (option-ref options 'repositories #f)))
    (cond (version-wanted         (version))
          (help-wanted            (help))
          (registration-needed => register-repository)
          (removal             => remove-repository)
          (list                   (list-repository-locations))
          (#t                     (list-repositories)))))
