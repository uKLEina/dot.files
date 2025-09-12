;;; gptel-save-chat.el --- Save/load helpers for gptel buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kobayashi Naohiro

;; Author: Kobayashi Naohiro <xopilli@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (gptel "0.9.8.5")
;; Keywords: convenience, tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; Utility tool to save/load gptel chat
;; - gptel-save-chat-as-md
;; - gptel-save-chat-buffer-dwim
;; - gptel-open-saved-chat
;; Put this file on your load-path and (require 'gptel-save-chat).

;;; Code:

(require 'recentf nil t)
(require 'gptel)

(defgroup gptel-save-chat nil
  "Save/load helpers for gptel chat buffers."
  :group 'applications)

(defcustom gptel-save-chat-directory "~/gptel/"
  "Directory where gptel chat buffers are saved."
  :type 'directory
  :group 'gptel-save-chat)

;; safe-local-variable registrations
(put 'gptel-model 'safe-local-variable (lambda (v) (or (symbolp v) (stringp v))))
(put 'gptel--backend-name 'safe-local-variable #'stringp)
(put 'gptel--bounds 'safe-local-variable #'listp)

;; Implementations:
(defun gptel-save-chat--ensure-md-name (name)
    "Return NAME without directory/extension and ensure it ends with .md."
    (let ((base (file-name-base name)))
      (concat base ".md")))

;;;###autoload
(defun gptel-save-chat-as-md (name)
    "Prompt for NAME (basename) and save current buffer to `gptel-save-chat-directory'.
Saved filename is prefixed with YYYYMMDD__ and forced to have the .md extension."
    (interactive (list (read-string "Save as (basename): " "")))
    (unless (and name (not (string-empty-p name)))
      (user-error "No file name provided"))
    (let* ((dir (file-name-as-directory (expand-file-name gptel-save-chat-directory)))
           (mdname (gptel-save-chat--ensure-md-name name))
           (dated (concat (format-time-string "%Y%m%d__") mdname))
           (path (expand-file-name dated dir)))
      (unless (file-directory-p dir) (make-directory dir t))
      (write-file path)
      (message "Saved buffer to %s" path)))

(defun gptel-save-chat--order-files-by-recentf (files)
    "Return FILES reordered so that items appearing in `recentf-list' come first (preserving recentf order).
Remaining files are appended sorted by modification time (newest first)."
    (unless (and (boundp 'recentf-list) recentf-list)
      (recentf-mode 1)) ;; enable if necessary to get recentf-list
    (let* ((recent recentf-list)
           (in-recent (delq nil (mapcar (lambda (f) (and (member f files) f)) recent)))
           (remaining (cl-remove-if (lambda (f) (member f in-recent)) files)))
      (append in-recent
              (sort remaining
                    (lambda (a b)
                      (time-less-p (nth 5 (file-attributes b))
                                   (nth 5 (file-attributes a))))))))

;;;###autoload
(defun gptel-save-chat-dwim ()
    "If buffer visits a file, do `save-buffer'.  Otherwise call `gptel-save-chat-as-md'."
    (interactive)
    (when (and (buffer-modified-p) (not buffer-read-only))
      (delete-trailing-whitespace))
    (if (buffer-file-name)
        (call-interactively #'save-buffer)
      (call-interactively #'gptel-save-chat-as-md)))

;;;###autoload
(defun gptel-save-chat-open ()
    "Choose a saved gptel Markdown file (from `gptel-save-chat-directory') and open it with `gptel-mode'.
Files present in `recentf-list' are shown first (respecting recentf order).  Completion
is done via `completing-read' (works with Vertico/Consult/Orderless)."
    (interactive)
    (let* ((dir (file-name-as-directory (expand-file-name gptel-save-chat-directory)))
           (files (when (file-directory-p dir)
                    (directory-files dir t "\\.md\\'"))))
      (unless (and files (cl-remove-if #'null files))
        (user-error "No saved .md files in %s" gptel-save-chat-directory))
      (let* ((ordered (gptel-save-chat--order-files-by-recentf files))
             (names (mapcar #'file-name-nondirectory ordered))
             (choice (completing-read "Open saved chat: " names nil t)))
        (when (and choice (not (string-empty-p choice)))
          (let ((path (expand-file-name choice dir)))
            (find-file path)
            ;; enable gptel-mode
            (gptel-mode 1)
            (message "Opened %s" path))))))

;; Optional: define-key in gptel-mode-map after gptel is loaded
(with-eval-after-load 'gptel
  (when (boundp 'gptel-mode-map)
    (define-key gptel-mode-map (kbd "C-x C-s") #'gptel-save-chat-dwim)
    (define-key gptel-mode-map (kbd "C-l c o") #'gptel-save-chat-open)))

(provide 'gptel-save-chat)
;;; gptel-save-chat.el ends here
