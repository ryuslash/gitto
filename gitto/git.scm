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

(define-module (gitto git)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<branch>
            <repository>

            branch-name
            branch-pullable
            branch-pushable
            branch-updated
            git-dir?
            print
            repo-branches
            repo-clean?
            repo-location
            repo-name
            repository?
            repository<?
            same-repository?))

(define show-unchanged-branches? #f)

(define-generic print)
(define-generic same-repository?)

(define-class <branch> ()
  (name #:getter branch-name)
  (pushable #:getter branch-pushable)
  (pullable #:getter branch-pullable)
  (updated #:getter branch-updated))

(define-class <repository> ()
  (name #:getter repo-name)
  (location #:getter repo-location)
  (clean? #:getter repo-clean?)
  (branches #:getter repo-branches))

(define (repository? repo)
  (is-a? repo <repository>))

(define (repository<? repo1 repo2)
  "Compary REPO1 and REPO2 to see if REPO1 should be considered smaller."
  (string<? (repo-location repo1) (repo-location repo2)))

(define-method (branch-pullable (branch <branch>))
  (force (slot-ref branch 'pullable)))

(define-method (branch-pushable (branch <branch>))
  (force (slot-ref branch 'pushable)))

(define-method (branch-updated (branch <branch>))
  (force (slot-ref branch 'updated)))

(define (git-branches dir)
  "Call git-branch and parse its output."
  (let ((pipe (start-git dir "branch")))
    (map
     (lambda (b) (string-trim-both b (char-set #\* #\space)))
     (string-split (string-trim-right (read-string pipe)) #\newline))))

(define (git-clean? dir)
  "Check whether a repository is clean.

Clean means there are no changes to the tracked files. Untracked files
will not register."
  (let* ((pipe (start-git dir "status -suno"))
         (clean? (eof-object? (read-delimited "" pipe))))
    (close-pipe pipe)
    clean?))

(define (git-dir? dir)
  "Check whether or not DIR is a git repository.

DIR will be considered a git repository if it has a `.git'
sub-directory."
  (let ((dir (string-append dir "/.git")))
    (if (file-exists? dir)
        (let ((dirstat (stat dir)))
          (eq? (stat:type dirstat) 'directory))
        #f)))

(define (git-last-update dir branch)
  "Check when the last update in DIR of upstream for BRANCH was."
  (let* ((pipe (start-git
                dir (format #f "log -1 --format=%ar ~a@{u}" branch)))
         (relative-last-update (read-line pipe)))
    (close-pipe pipe)
    (if (eof-object? relative-last-update)
        "never"
        relative-last-update)))

(define (git-revs-to-pull dir branch)
  "Check how many commits should be pulled/merged from upstream."
  (let* ((pipe (start-git
                dir (format #f "log --pretty=oneline ~a..~:*~a@{u}" branch)
                "| wc -l"))
         (num (string->number (read-line pipe))))
    (close-pipe pipe)
    num))

(define (git-revs-to-push dir branch)
  "Check how many commits should be pushed upstream."
  (let* ((pipe (start-git
                dir (format #f "log --pretty=oneline ~a@{u}..~:*~a" branch)
                "| wc -l"))
         (num (string->number (read-line pipe))))
    (close-pipe pipe)
    num))

(define-method (initialize (branch <branch>) args)
  (let ((name (car args))
        (dir (cadr args)))
    (slot-set! branch 'name name)
    (slot-set! branch 'pushable (delay (git-revs-to-push dir name)))
    (slot-set! branch 'pullable (delay (git-revs-to-pull dir name)))
    (slot-set! branch 'updated (delay (git-last-update dir name)))))

(define-method (initialize (repo <repository>) args)
  (let ((dir (car args)))
    (slot-set! repo 'name (basename dir))
    (slot-set! repo 'location dir)
    (slot-set! repo 'clean? (delay (git-clean? dir)))

    (slot-set! repo 'branches
               (delay (map (lambda (b) (make <branch> b dir))
                           (git-branches dir))))))

(define-method (print (branch <branch>))
  (let ((pushable (branch-pushable branch))
        (pullable (branch-pullable branch)))
    (if (or show-unchanged-branches?
            (> (+ pushable pullable) 0))
        (format #t "  ~a:~15t~d to push and ~d to pull. Last update: ~a~%"
                (branch-name branch) pushable pullable
                (branch-updated branch))
        #f)))

(define (repo-state-description repo)
  "Return the state of REPO as either clean or dirty.

REPO should be of type `<repository>' and the result is a string."
  (if (repo-clean? repo) "clean" "dirty"))

(define-method (print (repo <repository>))
  (if (file-exists? (repo-location repo))
      (begin
        (format #t "~a: Worktree is ~a~%" (repo-name repo)
                (repo-state-description repo))
        (when (any identity (map-in-order print (repo-branches repo)))
          (newline))
        #t)
      (format #t "~a:~15tnot found at ~s\n"
              (repo-name repo) (repo-location repo))))

(define-method (repo-branches (repo <repository>))
  (force (slot-ref repo 'branches)))

(define-method (repo-clean? (repo <repository>))
  (force (slot-ref repo 'clean?)))

(define-method (same-repository? (x <repository>) (y <repository>))
  (string= (repo-location x) (repo-location y)))

(define-method (same-repository? (x <repository>) (y <string>))
  (string= (repo-location x) y))

(define-method (same-repository? (x <string>) (y <repository>))
  (string= x (repo-location y)))

(define-method (same-repository? x y)
  #f)

(define* (start-git dir args #:optional (extra ""))
  (open-input-pipe
   (format #f "git --work-tree=~s --git-dir=\"~a/.git\" ~a 2>/dev/null ~a"
           dir dir args extra)))
