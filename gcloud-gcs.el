;;; gcloud-gcs.el --- Google Cloud Storage browser

;; Author: Jethro Kuan <jethrokuan95@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'gcloud-utils)
(require 'tablist)

(defcustom gcloud-gcs-cache-file
  (expand-file-name "gcs.cache" user-emacs-directory)
  "The name of GCS's cache file."
  :group 'gcloud
  :type 'string)

(defvar gcloud-gcs-cache nil
  "A hashmap used to cache gcs entries to speed up viewing.")

(defcustom gcloud-gsutil-command "gsutil" "The command for the \\[gsutil] utility."
  :type 'string
  :group 'gcloud)

(defun gcloud-gsutil-ls (dir)
  "Lists a particular directory"
  (let ((command (format "%s ls gs://%s" gcloud-gsutil-command dir)))
    (shell-command-to-string command)))

(defun gcloud-gcs-get-dir (dir)
  "Return the actual directory given `DIR'."
  (s-chop-prefix "gs://" dir))

(defun gcloud-gcs-lines-parse (line)
  "Convert a LINE from `gsutil ls' to a `tabulated-list-entries' entry."
  (let* ((trimmed (s-trim line))
         (dir (gcloud-gcs-get-dir trimmed))
         (file-or-folder-name (s-chop-prefix gcloud-current-directory dir)))
    (list dir (vector file-or-folder-name))))

(defun gcloud-gcs-get-entries (dir &optional refresh)
  "Returns the items in google cloud storage for `tabulated-list-entries'."
  (let* ((data (if (or refresh
                       (not (gethash dir gcloud-gcs-cache)))
                   (progn
                     (gcloud-gsutil-ls dir))
                 (gethash dir gcloud-gcs-cache)))
         (lines (s-split "\n" data t)))
    (puthash dir data gcloud-gcs-cache)
    (-map #'gcloud-gcs-lines-parse lines)))

(defun gcloud-gcs-entries ()
  "List GCS entries."
  (interactive)
  (pop-to-buffer "*gcs*")
  (gcloud-gcs-mode)
  (tablist-revert))

(defun gcloud-gcs-entries-refresh (&optional refresh)
  "Refresh the entries shown in the tablist."
  (interactive)
  (setq tabulated-list-entries
        (gcloud-gcs-get-entries gcloud-current-directory refresh)))

(defun gcloud-gcs-select-directory ()
  "Update the selected gcloud directory to current entry."
  (interactive)
  (let* ((dir (tabulated-list-get-id)))
    (setq gcloud-current-directory dir)
    (gcloud-gcs-entries-refresh)
    (tabulated-list-revert)))

(defun gcloud-gcs-parent-directory ()
  "Update the selected gloud directory to parent directory."
  (interactive)
  (let* ((parent (or (file-name-directory (directory-file-name gcloud-current-directory))
                     "/")))
    (setq gcloud-current-directory parent)
    (gcloud-gcs-entries-refresh)
    (tabulated-list-revert)))

(defun gcloud-gcs-save-cache ()
  "Serialize cache for later usage."
  (interactive)
  (gcloud-serialize gcloud-gcs-cache gcloud-gcs-cache-file)
  (message "Cache saved!"))

(defun gcloud-gcs-add-to-killring ()
  "Add the current item to clipboard."
  (interactive)
  (let* ((path (tabulated-list-get-id))
         (gs-uri (format "gs://%s" path)))
    (kill-new gs-uri)
    (message (format "`%s' added to kill-ring." gs-uri))))

(defsubst tabulated-list-get-id (&optional pos)
  "Return the entry ID of the Tabulated List entry at POS.
The value is an ID object from `tabulated-list-entries', or nil.
POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'tabulated-list-id))

(defvar gcloud-gcs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'gcloud-gcs-select-directory)
    (define-key map (kbd "G") 'gcloud-gcs-entries-refresh)
    (define-key map (kbd "w") 'gcloud-gcs-add-to-killring)
    (define-key map (kbd "^") 'gcloud-gcs-parent-directory)
    (define-key map (kbd "s") 'gcloud-gcs-save-cache)
    map)
  "Keymap for `gcloud-gcs-mode'")

(define-derived-mode gcloud-gcs-mode tabulated-list-mode "GCS"
  "Major mode for handling GCS entries."
  (setq tabulated-list-format [("Entry" 70 t)])
  (setq tabulated-list-padding 2)
  (unless gcloud-gcs-cache
    (setq gcloud-gcs-cache
          (or (gcloud-unserialize gcloud-gcs-cache-file)
              (make-hash-table :test 'equal))))
  (defvar-local gcloud-current-directory "/"
    "Current bucket being browsed.")
  (tabulated-list-init-header)
  (tablist-minor-mode)
  (add-hook 'gcloud-gcs-mode-hook
            (lambda ()
              (let ((oldmap (cdr (assoc '<minor-mode> minor-mode-map-alist)))
                    (newmap (make-sparse-keymap)))
                (set-keymap-parent newmap oldmap)
                (define-key newmap (kbd "G") nil)
                (make-local-variable 'minor-mode-overriding-map-alist)
                (push `(<minor-mode> . ,newmap) minor-mode-overriding-map-alist)))))

(provide 'gcloud-gcs)

;; gcloud-gcs.el ends here
