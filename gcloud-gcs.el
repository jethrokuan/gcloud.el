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
(require 'tabulated-list)

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
  "List files in `DIR'."
  (let ((command (format "%s ls gs://%s" gcloud-gsutil-command dir)))
    (shell-command-to-string command)))

(defun gcloud-gsutil-cat (entry)
  "Get contents in `ENTRY'."
  (let ((command (format "%s cat gs://%s" gcloud-gsutil-command entry)))
    (shell-command-to-string command)))

(defun gcloud-gcs-get-dir (dir)
  "Return the actual directory given `DIR'."
  (s-chop-prefix "gs://" dir))

(defun gcloud-gcs-entry-is-directory (entry)
  "Return non-nil if `ENTRY' is a directory."
  (equal (aref entry (- (length entry) 1)) ?/))

(defun gcloud-gcs-lines-parse (line)
  "Convert a LINE from `gsutil ls' to a `tabulated-list-entries' entry."
  (let* ((trimmed (s-trim line))
         (dir (gcloud-gcs-get-dir trimmed))
         (file-or-folder-name (s-chop-prefix gcloud-current-directory dir)))
    (list dir (vector file-or-folder-name))))

(defun gcloud-gcs-get-entries (dir &optional refresh)
  "Returns the items in `DIR' for `tabulated-list-entries'."
  (let* ((data (if (or refresh
                       (not (gethash dir gcloud-gcs-cache)))
                   (progn
                     (gcloud-gsutil-ls dir))
                 (gethash dir gcloud-gcs-cache)))
         (lines (s-split "\n" data t)))
    (puthash dir data gcloud-gcs-cache)
    (-select (lambda (entry)
               (not (equal (aref (cadr entry) 0) "")))
             (-map #'gcloud-gcs-lines-parse lines))))

(defun gcloud-gcs-entries ()
  "List GCS entries."
  (interactive)
  (pop-to-buffer "*gcs*")
  (gcloud-gcs-mode)
  (gcloud-gcs-entries-refresh)
  (tabulated-list-revert))

(defun gcloud-gcs-entries-refresh ()
  "Refresh the entries shown in the tablist."
  (interactive)
  (setq tabulated-list-entries
        (gcloud-gcs-get-entries gcloud-current-directory))
  (tabulated-list-revert))

(defun gcloud-gcs-entries-reload ()
  "Reload the entries shown in the tablist."
  (interactive)
  (message "Reloading %s..." gcloud-current-directory)
  (setq tabulated-list-entries
        (gcloud-gcs-get-entries gcloud-current-directory t))
  (tabulated-list-revert))

(defun gcloud-gcs-select-file-or-directory ()
  "Open the file at entry, or the directory."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (if (gcloud-gcs-entry-is-directory entry)
        (gcloud-gcs-select-directory entry)
      (gcloud-gcs-open-file entry))))

(defun gcloud-gcs-open-file (entry)
  "Open the file at `ENTRY'."
  (interactive)
  (let* ((buffer-name (file-name-nondirectory entry))
         (buffer (generate-new-buffer buffer-name))
         (contents (gcloud-gsutil-cat entry)))
    (with-current-buffer buffer
      (insert contents)
      (beginning-of-buffer))
    (pop-to-buffer buffer)))

(defun gcloud-gcs-select-directory (dir)
  "Update the selected gcloud directory to `DIR'."
  (setq gcloud-current-directory dir)
  (gcloud-gcs-entries-refresh))

(defun gcloud-gcs-parent-directory ()
  "Update the selected gloud directory to parent directory."
  (interactive)
  (let* ((parent (or (file-name-directory (directory-file-name gcloud-current-directory))
                     "/")))
    (setq gcloud-current-directory parent)
    (gcloud-gcs-entries-refresh)))

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
    (define-key map (kbd "<return>") 'gcloud-gcs-select-file-or-directory)
    (define-key map (kbd "G") 'gcloud-gcs-entries-reload)
    (define-key map (kbd "w") 'gcloud-gcs-add-to-killring)
    (define-key map (kbd "^") 'gcloud-gcs-parent-directory)
    (define-key map (kbd "s") 'gcloud-gcs-save-cache)
    map)
  "Keymap for `gcloud-gcs-mode'")

(setq gcloud-gcs-mode-map (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "<return>") 'gcloud-gcs-select-file-or-directory)
                            (define-key map (kbd "G") 'gcloud-gcs-entries-reload)
                            (define-key map (kbd "w") 'gcloud-gcs-add-to-killring)
                            (define-key map (kbd "^") 'gcloud-gcs-parent-directory)
                            (define-key map (kbd "s") 'gcloud-gcs-save-cache)
                            map))

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
  (tabulated-list-init-header))

(provide 'gcloud-gcs)

;; gcloud-gcs.el ends here
