#! /usr/guile-2.0/bin/guile \
-e main -s
!#
(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 popen)
             (ice-9 rdelim))

(define data-dir
  (string-append (or (getenv "XDG_DATA_HOME") "~/.local/share")
                 "/gitracker"))

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
  (display "gitto version 0.1\n"))

(define (help)
  "Display some help."
  (display "\
gitto [options]
  -r, --register REPO  Register a new repository directory
  -R, --remove REPO    Repmove a repository directory
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

  (let ((port (open-output-file repositories-file)))
    (write repositories port)
    (close-port port)))

(define (register-repository repository)
  (if (not (member repository repositories))
      (begin
        (set! repositories (append `(,repository)
                                   repositories))
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
  (let* ((response (open-input-pipe "git log --pretty=oneline @{u}.. | wc -l"))
         (num (string->number (read-line response))))
    (close-pipe response)
    num))

(define (git-revs-to-pull)
  (let* ((response (open-input-pipe "git log --pretty=oneline ..@{u} | wc -l"))
         (num (string->number (read-line response))))
    (close-pipe response)
    num))

(define (git-clean?)
  (let* ((response (open-input-pipe "git status -suno"))
         (clean? (eof-object? (read-delimited "" response))))
    (close-pipe response)
    clean?))

(define (list-repositories)
  (for-each (lambda (repo)
              (chdir repo)
              (let ((numup (git-revs-to-push))
                    (numdown (git-revs-to-pull))
                    (clean? (git-clean?)))
                (format #t "~a: ~d to push, ~d to pull and is ~adirty.\n"
                        (basename repo) numup numdown
                        (if clean? "not " ""))))
            repositories))

(define option-spec
  `((version  (single-char #\v) (value #f))
    (help     (single-char #\h) (value #f))
    (register (single-char #\r) (value #t)
              (predicate ,git-dir?))
    (remove   (single-char #\R) (value #t)
              (predicate ,git-dir?))))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f))
         (registration-needed (option-ref options 'register #f))
         (removal (option-ref options 'remove #f)))
    (cond (version-wanted (version))
          (help-wanted (help))
          (registration-needed => register-repository)
          (removal => remove-repository)
          (#t (list-repositories)))))
