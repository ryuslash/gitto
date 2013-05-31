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

(define (version)
  "Display version information."
  (display "gitto 0.1.0") (newline)
  (display "Copyright (C) 2012 Tom Willemse") (newline)
  (display "This program comes with ABSOLUTELY NO WARRANTY.") (newline)
  (display "You may redistribute copies of this program") (newline)
  (display "under the terms of the GNU General Public License.") (newline)
  (display "For more information about these matters, see the file named COPYING.") (newline))
(set! command-list (append command-list `(("version" . ,version))))

(define (help)
  "Display some help."
  (display "\
gitto [command [arguments ...]]
  add                  Register a new repository directory
  remove               Remove a repository directory
  check                Check if a repository has been registered
  list                 List all repositories and their status
  list locations       List all registered repositories' locations
  purge                Remove all repositories that don't exist
  config               Show each repository's configuration
  config global        Show template configuration
  config update        Merge template configuration with each
                       repository's configuration
  config hooks         Install configured hooks for repositories
  version              Display version
  help                 Display this help
"))
(set! command-list (append command-list `(("help" . ,help))))

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
(set! command-list (append command-list
                           `(("check" . ,repository-registered?))))

(define (register-repository repository)
  "Register REPOSITORY in the repository list."
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
(set! command-list (append command-list
                           `(("add" . ,register-repository))))

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
(set! command-list (append command-list
                           `(("remove" . ,remove-repository))))

(define (list-repositories . args)
  "List information about every repository."
  (if (and (not (eq? args '())) (equal? (car args) "locations"))
      (list-repository-locations)
      (for-each print repositories)))
(set! command-list (append command-list
                           `(("list" . ,list-repositories))))

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
  (set! repositories
        (filter (lambda (repo)
                  (file-exists? (repo-location repo)))
                repositories))
  (save-repositories-list))
(set! command-list (append command-list `(("purge" . ,purge))))

(define (show-global-config)
  "Show the template specified in `global-config'."
  (write-config global-config))

(define (show-config . args)
  "Do something with the config module.

If ARGS is an empty list, show each repository's current
configuration. If the car of ARGS is `global' show the template
specified in the user's init file. If the car of ARGS is `update'
merge the specified template and each repository's configuration,
excluding the repositories in `config-exclusion-list'. If the car of
ARGS is `hooks' install configured hooks in each repository, excluding
repositories in `config-exclusion-list'."
  (cond
   ((eq? args '())
    (for-each (lambda (repo)
                (display (string-upcase (repo-name repo)))
                (newline)
                (write-config (read-config (repo-location repo)))
                (newline)
                (newline))
              repositories))
   ((equal? (car args) "global") (show-global-config))
   ((equal? (car args) "update") (update-config))
   ((equal? (car args) "hooks")
    (for-each (lambda (r)
                (unless (member (repo-name r) config-exclusion-list)
                  (install-hooks (repo-location r))))
              repositories))))
(set! command-list (append command-list `(("config" . ,show-config))))

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
          (apply command (if command? (cdr command-spec) '()))
          (format #t "Unknown command: ~a~%" (car command-spec))))))

(define repositories-file (data-file "repos.scm"))

(define repositories
  (if (file-exists? repositories-file)
      (let* ((port (open-input-file repositories-file))
             (result (read port)))
        (close-port port)
        (map-in-order (lambda (repo) (make <repository> repo)) result))
      '()))
