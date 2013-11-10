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

(define-module (gitto main)
  #:use-module (gitto config)
  #:use-module (gitto git)
  #:use-module (gitto path)
  #:use-module (gitto ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (main))

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
(define config-exclusion-list '())

(define (storage-dir xdg-env fallback)
  "Get the location where gitto stores information.

XDG-ENV specifies which XDG environment variable should be looked at
and FALLBACK specifies the directory to use if XDG-ENV has not been
set in the current environment."
  (let ((xdg (getenv xdg-env)))
    (string-append
     (or xdg (getenv "HOME")) (if xdg "" fallback) "/gitto")))

(define (config-dir) (storage-dir "XDG_CONFIG_HOME" "/.config"))
(define (config-file file) (string-append (config-dir) "/" file))
(define (data-dir) (storage-dir "XDG_DATA_HOME" "/.local/share"))
(define (data-file file) (string-append (data-dir) "/" file))

(define-command (version)
  "Display version information."
  "Usage: gitto version

Displays version and some copyright information."
  (display "gitto 0.1.0") (newline)
  (display "Copyright (C) 2012 Tom Willemse") (newline)
  (display "This program comes with ABSOLUTELY NO WARRANTY.") (newline)
  (display "You may redistribute copies of this program") (newline)
  (display "under the terms of the GNU General Public License.") (newline)
  (display "For more information about these matters, see the file named COPYING.") (newline))

(define-command (help #:optional command)
  "Display this help."
  "Usage: gitto help [COMMAND]

Display a help message. If COMMAND is not specified, print some
information about gitto, otherwise print some information about
COMMAND."
  (if command
      (let ((command-spec (assoc-ref command-list command)))
        (if command-spec
            (format #t "~a~%" (assq-ref command-spec #:documentation))
            (format #t "Unknown command: ~a~%" command)))
      (begin
        (display "gitto [command [arguments ...]]")
        (newline)
        (for-each
         (lambda (cmd)
           (format #t "  ~a~20t~a~%" (car cmd) (assq-ref cmd #:usage)))
         command-list))))

(define (known? repo)
  "Do we know REPO?"
  (and (or (repository? repo) (string? repo))
       (or (member repo repositories same-repository?)
           (member (realpath (if (string? repo)
                                 repo
                                 (repo-location repo)))
                   repositories same-repository?))))

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

(define-command (check repository)
  "Check to see if a repository has been registered."
  "Usage: gitto check REPO

Checks whether or not the git repository REPO has been registered with
gitto."
  (format #t "Repository is~a registered~%"
          (if (known? repository) "" " not")))

(define-command (add repository)
  "Register a repository."
  "Usage: gitto add REPO

Add REPO to the registered repository list. This command will fail if
REPO does not indicate a git repository or if it has already been
registered."
  (set! repository (make <repository> (realpath repository)))
  (if (not (known? repository))
      (begin
        (set! repositories (append `(,repository) repositories))
        (save-repositories-list)
        (simple-format #t "Repository ~A registered.~%"
                       (repo-name repository))

        ;; Ask the user if they would like to merge their config
        ;; template with the newly registered repository if they have
        ;; a configuration set-up and the current input port is a tty.
        (when (and (isatty? (current-input-port))
                   (not (eq? global-config '()))
                   (y-or-n? "Would you like to merge your settings?"
                            #:default #t))
          (update-repo-config repository)))
      (display "Repository already registered."))
  (newline))

(define-command (remove repository)
  "Unregister a repository."
  "Usage: gitto remove REPO

Removes REPO from the registered repository list. This command will
fail if REPO does not indicate a git repository of if it hasn't been
registered."
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

(define-command (list . args)
  "List information about every repository."
  "Usage: gitto list
       gitto list locations

The first form shows an overview of the status of your registered
repositories and their branches. By default branches without changer
aren't shown, but you can change this behaviour by setting the
`show-unchanged-branches?' variable in your init file.

The second form prints the location on your filesystem for each
registered repository as absolute paths."
  (if (and (not (eq? args '())) (equal? (car args) "locations"))
      (list-repository-locations)
      (for-each print repositories)))

(define (list-repository-locations)
  "List the registered locations of repositories."
  (for-each
   (lambda (repo)
     (display (repo-location repo))
     (newline))
   (sort repositories
         (lambda (s1 s2)
           (string<? (repo-location s1) (repo-location s2))))))

(define-command (purge)
  "Purge all registered repositories that can no longer be found."
  "Usage: gitto purge

Go through the list of registered repositories and remove all the ones
which no longer point to a git repository."
  (set! repositories
        (filter (lambda (repo)
                  (file-exists? (repo-location repo)))
                repositories))
  (save-repositories-list))

(define (show-global-config)
  "Show the template specified in `global-config'."
  (write-config global-config))

(define-command (config #:optional sub)
  "Manage your repositories' configurations."
  "Usage: gitto config
       gitto config global
       gitto config update
       gitto config hooks

The first form prints the configurations for each registered
repository.

The second form shows what your configured configuration template
looks like as a git configuration file. This does not expand the `%a'
format specifier which can be used to indicate the repository name.

The third form merges the template in and existing configurations,
overwriting settings when necessary. The repositories in the
`config-exclusion-list' will be skipped. *Note:* This is a destructive
operation, you should be mindful.

The fourth form installs the configured hooks into each repository."
  (cond
   ((not sub)
    (for-each (lambda (repo)
                (display (string-upcase (repo-name repo)))
                (newline)
                (write-config (read-config (repo-location repo)))
                (newline)
                (newline))
              repositories))
   ((equal? sub "global") (show-global-config))
   ((equal? sub "update") (update-config))
   ((equal? sub "hooks")
    (for-each (lambda (r)
                (unless (member (repo-name r) config-exclusion-list)
                  (install-hooks (repo-location r))))
              repositories))))

(define (update-repo-config repo)
  "Merge the configured configuration with REPO's configuration.

Don't do anything if REPO has been added to `config-exclusion-list'."
  (unless (member (repo-name repo) config-exclusion-list)
    (write-config
     (merge-config (repo-name repo)
                   (read-config (repo-location repo))
                   global-config)
     (string-append (repo-location repo) "/.git/config"))))

(define (update-config)
  "Merge the configured configuration with all repositories."
  (for-each update-repo-config repositories))

(define (main args)
  "Parse the command line options and run the appropriate functions."
  (let ((cfg (config-file "rc.scm")))
    (when (file-exists? cfg)
      (save-module-excursion
       (lambda ()
         (set-current-module (resolve-module '(gitto main)))
         (primitive-load cfg))))

    (let* ((command-spec (cdr (member "gitto" args string-suffix?)))
           (command? (not (eq? command-spec '())))
           (command
            (assoc-ref command-list
                       (car (if command? command-spec '("list"))))))
      (if command
          (apply (assq-ref command #:function)
                 (if command? (cdr command-spec) '()))
          (format #t "Unknown command: ~a~%" (car command-spec))))))

(define repositories-file (data-file "repos.scm"))

(define repositories
  (if (file-exists? repositories-file)
      (let* ((port (open-input-file repositories-file))
             (result (read port)))
        (close-port port)
        (map-in-order (lambda (repo) (make <repository> repo)) result))
      '()))
