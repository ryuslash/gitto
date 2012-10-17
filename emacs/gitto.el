;;; gitto.el --- Gitto in emacs

;; Copyright (C) 2012  Tom Willemsen

;; Author: Tom Willemsen <thomas@aethon.nl>
;; Keywords: convenience
;; Package-Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Small interface between gitto and Emacs.

;; Right now it only offers the `gitto-register' command which lets
;; you register repositories from within Emacs.  When called
;; interactively this command will try to register the git directory
;; of the file of the current buffer.

;;; Code:

(defgroup gitto nil
  "Gitto settings."
  :group 'applications)

(defcustom gitto-program "gitto"
  "The gitto executable."
  :group 'gitto
  :type 'string)

;;;###autoload
(defun gitto-registered-p (dir)
  "Check if DIR is a registered repository."
  (not (string-match-p "not registered"
                       (shell-command-to-string
                        (concat gitto-program " -c " dir)))))

;;;###autoload
(defun gitto-register (dir)
  "Register DIR with gitto."
  (interactive (list (locate-dominating-file (buffer-file-name) ".git")))
  (unless dir
    (error "Not a git repository"))

  (shell-command (concat gitto-program " -r " dir)))

;;;###autoload
(defun gitto-unregister (dir)
  "Unregister DIR with gitto."
  (interactive (list (locate-dominating-file (buffer-file-name) ".git")))
  (unless (and dir (gitto-registered-p dir))
    (error "Not a registered git repository"))

  (shell-command (concat gitto-program " -R " dir)))

(provide 'gitto)
;;; gitto.el ends here
