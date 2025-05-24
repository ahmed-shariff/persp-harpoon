;;; persp-harpoon.el --- Customizable harpoon -*- lexical-binding: t -*-

;; Copyright (C) 2025 Shariff AM Faleel

;; Author: Shariff AM Faleel
;; Package-Requires: ((emacs "29.1") (dash "2.0"))
;; Version: 0.1-pre
;; Homepage: https://github.com/ahmed-shariff/persp-harpoon
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This is a harpoon implementation that is inspired by harpoon for
;; vim (by ThePrimeagen).

;;; Code:
(require 'dash)
(require 'map)

;;; User options
(defgroup persp-harpoon nil
  "Customization for `persp-harpoon'."
  :group 'tools
  :group 'convenience
  :link '(url-link "https://github.com/ahmed-shariff/persp-harpoon"))

(defcustom persp-harpoon-current-persp-name-function nil
  "A function that returns the name of the current perspective.
This is used to identify which hapoon list should be used.  Can also
be configured using `persp-harpoon-configure'"
  :group 'persp-harpoon
  :type 'function)

(defcustom persp-harpoon-current-persp-buffers-list-function nil
  "A function that returns the list of buffers or buffer-names.
Can also be configured using `persp-harpoon-configure'"
  :group 'persp-harpoon
  :type 'function)

(defcustom persp-harpoon-keymap-prefix-key nil
  "Prefix key to activate persp-harpoon-keymap."
  :group 'persp-harpoon
  :set (lambda (sym value)
         (when (and (bound-and-true-p persp-harpoon-mode-map)
                    (bound-and-true-p persp-harpoon-keymap))
           (substitute-key-definition 'persp-harpoon-keymap nil persp-harpoon-mode-map)
           (when value
             (define-key persp-harpoon-mode-map (kbd value) 'persp-harpoon-keymap)))
         (set-default sym value))
  :type '(choice (const :tag "None" nil)
                 key-sequence))

;;; Variables
(defvar persp-harpoon--cache-file (expand-file-name ".cache/pers-harpoon.json" user-emacs-directory))
(defvar persp-harpoon--buffers nil)
(defvar persp-harpoon--buffers-list nil)
(defvar-local persp-harpoon-show--current-hashtable nil)

(defvar persp-harpoon-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'persp-harpoon-add-buffer)
    (define-key map (kbd "r") #'persp-harpoon-remove-buffer)
    (define-key map (kbd "m") #'persp-harpoon-show-list)
    (define-key map (kbd "c") #'persp-harpoon-clear-buffers)
    (define-key map (kbd "o") #'persp-harpoon-switch-other)
    (define-key map (kbd "k") #'persp-harpoon-kill-non-harpoon-buffers)
    (define-key map (kbd "1") #'persp-harpoon-jump-to-1)
    (define-key map (kbd "2") #'persp-harpoon-jump-to-2)
    (define-key map (kbd "3") #'persp-harpoon-jump-to-3)
    (define-key map (kbd "4") #'persp-harpoon-jump-to-4)
    (define-key map (kbd "5") #'persp-harpoon-jump-to-5)
    (define-key map (kbd "6") #'persp-harpoon-jump-to-6)
    (define-key map (kbd "7") #'persp-harpoon-jump-to-7)
    (define-key map (kbd "8") #'persp-harpoon-jump-to-8)
    (define-key map (kbd "9") #'persp-harpoon-jump-to-9)
    (define-key map (kbd "h") #'persp-harpoon-switch-to)
    map)
  "Sub keymap for `persp-harpoon-mode'.
Will be triggered by `persp-harpoon-keymap-prefix-key' when
`persp-harpoon-mode' is active.")

(defvar persp-harpoon-mode-map
  (let ((map (make-sparse-keymap)))
    (when persp-harpoon-keymap-prefix-key
      (define-key map (kbd persp-harpoon-keymap-prefix-key) persp-harpoon-keymap))
    map)
  "Keymap for `persp-harpoon-mode'.")

(defvar persp-harpoon-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'persp-harpoon--show-mark-delete)
    (define-key map "s" #'persp-harpoon--show-set-index)
    (define-key map "a" #'persp-harpoon--show-add)
    (define-key map "k" #'previous-line)
    (define-key map "j" #'next-line)
    (define-key map (kbd "C-c C-c") #'persp-harpoon--show-end-process)
    map)
  "Keymap for `persp-harpoon-menu-mode'")

;;; Main functions of harpoon
;;;###autoload
(defun persp-harpoon-save ()
  "Save the current state of the harpoons."
  (let ((hashtable (persp-harpoon-load)))
    (puthash (persp-harpoon--current-persp-name)
             (map-into persp-harpoon--buffers 'hash-table)
             hashtable)
    (with-temp-file persp-harpoon--cache-file
      (json-insert hashtable))))

(defun persp-harpoon--current-persp-name ()
  "Wrapper for `persp-harpoon-current-persp-name-function'.
Errors if not set."
  (unless persp-harpoon-current-persp-name-function
    (user-error "`persp-harpoon-current-persp-name-function' is not set"))
  (funcall persp-harpoon-current-persp-name-function))

(defun persp-harpoon--current-persp-buffers-list ()
  "Wrapper for `persp-harpoon-current-persp-buffers-list-function'.
Errors if not set."
  (unless persp-harpoon-current-persp-buffers-list-function
    (user-error "`persp-harpoon-current-persp-buffers-list-function' is not set"))
  (funcall persp-harpoon-current-persp-buffers-list-function))

;;;###autoload
(defun persp-harpoon-load (&optional persp-name)
  "Get the stored persp-harpoon state as a hashtable.
If PERSP-NAME is provided, returns an alist of (file-name . index),
otherwise returns the entire hash table of harpoons."
  (unless (file-exists-p persp-harpoon--cache-file)
    (with-temp-buffer
      (set-visited-file-name persp-harpoon--cache-file t)
      (let (message-log-max)
        (save-buffer))))
  (let ((hashtable (or (ignore-error json-error
                         (with-temp-buffer
                           (insert-file-contents-literally persp-harpoon--cache-file)
                           (json-parse-string (buffer-string))))
                       (make-hash-table))))
    (if persp-name
        (when-let ((-persp-hashtable (gethash persp-name hashtable)))
          (map-into -persp-hashtable 'alist))
      hashtable)))

(defun persp-harpoon--add-file-to-top (buffer-full-name)
  "Add BUFFER-FULL-NAME to the top of the harpoon buffer list."
  (unless persp-harpoon--buffers
    (setq persp-harpoon--buffers '()))
  (unless (assoc buffer-full-name persp-harpoon--buffers)
    (push (cons buffer-full-name (--first
                                  (not (rassoc it persp-harpoon--buffers))
                                  (number-sequence 1 9)))
          persp-harpoon--buffers))
  (setq persp-harpoon--buffers-list (delete buffer-full-name persp-harpoon--buffers-list))
  (push buffer-full-name persp-harpoon--buffers-list))

;;;###autoload
(defun persp-harpoon-add-buffer ()
  "Add current buffer-file to the harpoon list for the current perspective.
Reports an error if not a file buffer."
  (interactive)
  (if-let (b (buffer-file-name))
      (prog1 (persp-harpoon--add-file-to-top (file-truename b))
        (persp-harpoon-save))
    (user-error "Buffer not a file")))

;;;###autoload
(defun persp-harpoon-remove-buffer (&optional buffer)
  "Remove BUFFER (or current if nil) from the harpoon buffer list.
Removes from the list and re-saves state."
  (interactive)
  (if-let (b (or buffer (buffer-file-name)))
      (let ((buffer-full-name (file-truename b)))
        (setq persp-harpoon--buffers (assoc-delete-all buffer-full-name persp-harpoon--buffers))
        (setq persp-harpoon--buffers-list (delete buffer-full-name persp-harpoon--buffers-list))
        (persp-harpoon-save))
    (user-error "Buffer not a file")))

;;;###autoload
(defun persp-harpoon-clear-buffers ()
  "Clear all buffers from the harpoon list for current perspective.

Prompts for confirmation."
  (interactive)
  (when (y-or-n-p (format "Clear harpoon (%s)?" (persp-harpoon--current-persp-name)))
    (setq persp-harpoon--buffers '())
    (setq persp-harpoon--buffers-list nil)
    (persp-harpoon-save)))

;;;###autoload
(defun persp-harpoon-on-buffer-switch (&rest _)
  "Update harpoon buffer list order after a buffer switch.
Moves the newly visited file buffer to the top of the harpoon order if
present.  Meant to be used with `buffer-list-update-hook'."
  (when-let* ((b (buffer-file-name))
              (buffer-full-name (file-truename b)))
    (when (member buffer-full-name persp-harpoon--buffers-list)
      (persp-harpoon--add-file-to-top (buffer-file-name)))))

(defun persp-harpoon-jump-to-index (index)
  "Jump to the buffer with harpoon INDEX in the current perspective."
  (if-let ((-buffer (rassoc index persp-harpoon--buffers)))
      (switch-to-buffer (find-file-noselect (car -buffer)))
    (user-error "No buffer for index %s" index)))

;;;###autoload
(defun persp-harpoon-jump-to-1 ()
  "Jump to the buffer with harpoon index 1 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 1))

;;;###autoload
(defun persp-harpoon-jump-to-2 ()
  "Jump to the buffer with harpoon index 2 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 2))

;;;###autoload
(defun persp-harpoon-jump-to-3 ()
  "Jump to the buffer with harpoon index 3 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 3))

;;;###autoload
(defun persp-harpoon-jump-to-4 ()
  "Jump to the buffer with harpoon index 4 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 4))

;;;###autoload
(defun persp-harpoon-jump-to-5 ()
  "Jump to the buffer with harpoon index 5 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 5))

;;;###autoload
(defun persp-harpoon-jump-to-6 ()
  "Jump to the buffer with harpoon index 6 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 6))

;;;###autoload
(defun persp-harpoon-jump-to-7 ()
  "Jump to the buffer with harpoon index 7 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 7))

;;;###autoload
(defun persp-harpoon-jump-to-8 ()
  "Jump to the buffer with harpoon index 8 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 8))

;;;###autoload
(defun persp-harpoon-jump-to-9 ()
  "Jump to the buffer with harpoon index 9 in the current perspective."
  (interactive)
  (persp-harpoon-jump-to-index 9))

(defun persp-harpoon-on-switch (&rest _)
  "Refresh harpoon buffer lists when switching perspectives."
  (setq persp-harpoon--buffers (or (persp-harpoon-load (persp-harpoon--current-persp-name))
                                  '()))
  (setq persp-harpoon--buffers-list (mapcar (lambda (el) (car el)) persp-harpoon--buffers)))

;;;###autoload
(defun persp-harpoon-switch-to ()
  "Prompt and switch to buffer in harpoon list of current perspective.
Presents a completion menu with annotated indices and buffer names."
  (interactive)
  (let* ((annotated-buffers (persp-harpoon--get-buffers-for-completion t))
         (buffer-annotation-function
          (completion-metadata-get '((category . buffer)) 'annotation-function))
         (buffer-affixation-function
          (completion-metadata-get '((category . buffer)) 'affixation-function)))
    (switch-to-buffer
     (cdr
      (assoc
       (completing-read
        (format "Switch to harpoon (%s):" (persp-harpoon--current-persp-name))
        (lambda (string pred action)
          (cond
           ((eq action 'metadata)
            (cons
             'metadata
             `(;; (category . buffer)
               (annotation-function . ,(lambda (str)
                                         (funcall buffer-annotation-function
                                                  (substring str 2))))
               (affixation-function . ,(lambda (collection)
                                         (cl-loop for original-el in collection
                                                  for affixed in (funcall buffer-affixation-function
                                                                          (--map
                                                                           (cdr
                                                                            (assoc it annotated-buffers))
                                                                           collection))
                                                  collect
                                                  (let ((buffer (substring original-el 2))
                                                        (index (substring original-el 0 1))
                                                        (prefix (cadr affixed))
                                                        (suffix (caddr affixed)))
                                                    (list
                                                     buffer
                                                     (if prefix
                                                         (format "%s %s" index prefix)
                                                       prefix)
                                                     suffix)))))
               (display-sort-function . ,#'identity))))
           (t
            (complete-with-action
             action
             annotated-buffers
             string
             pred))))
        nil 'require-match)
       annotated-buffers)))))

(defun persp-harpoon--get-buffers-for-completion (&optional annotate-index)
  "Return buffer names list suitable for `completing-read'.
If ANNOTATE-INDEX is non-nil, each entry is (string . buffer_name)
where string contains the harpoon index."
  (mapcar (lambda (b)
            (let ((-b (buffer-name (find-file-noselect b))))
              (if annotate-index
                  (cons (format "%s %s"
                                (if-let ((idx (assoc b persp-harpoon--buffers)))
                                    (cdr idx) " ")
                                -b)
                        -b)
                -b)))
          persp-harpoon--buffers-list))

;;;###autoload
(defun persp-harpoon-switch-other ()
  "Switch to the next buffer in harpoon list, or first if not present."
  (interactive)
  (let ((current-buffer-name (file-truename (buffer-file-name))))
    (if (and current-buffer-name
             (member (file-truename current-buffer-name) persp-harpoon--buffers-list))
        (if (> 2 (length persp-harpoon--buffers-list))
            (user-error "Only %s buffer(s) in harpoon" (length persp-harpoon--buffers-list))
          (persp-harpoon--add-file-to-top (cadr persp-harpoon--buffers-list))
          (switch-to-buffer (find-file-noselect (car persp-harpoon--buffers-list))))
      (switch-to-buffer (find-file-noselect (car persp-harpoon--buffers-list))))))

;;;###autoload
(defun persp-harpoon-kill-non-harpoon-buffers ()
  "Kill all current perspective buffers not present in the harpoon list."
  (interactive)
  (when (y-or-n-p "Kill buffers not in harpoon? ")
    (let ((killed 0))
      (dolist (b (persp-harpoon--current-persp-buffers-list))
        (let ((-buffer-file-name (buffer-file-name (get-buffer b))))
          (when (or (null -buffer-file-name)
                    (not (member (file-truename -buffer-file-name) persp-harpoon--buffers-list)))
            (cl-incf killed)
            (kill-buffer b))))
      (message "Killed %s buffer(s)." killed))))

;;; Persp-harpoon menu and related functions
;;;###autoload
(define-derived-mode persp-harpoon-menu-mode special-mode "persp-harpoon"
  "Major mode for managing and editing the harpoon buffer list.
This will be the currect perspective.  Allows reordering, renumbering,
adding or deleting entries interactively."
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local persp-harpoon-show--current-hashtable (make-hash-table))
  (setq header-line-format
        (substitute-command-keys
         (concat
          "Quit: \\[quit-window], Apply changes \\[persp-harpoon--show-end-process], "
          "Delete entry: \\[persp-harpoon--show-mark-delete], Set new index for entry: \\[persp-harpoon--show-set-index], "
          "Add new entry: \\[persp-harpoon--show-add], "
          "Previous line: \\[previous-line], Next line: \\[next-line]")))
  (buffer-disable-undo))

;;;###autoload
(defun persp-harpoon-show-list ()
  "Show special buffer listing harpoon buffers for current perspective.
Enables interactive manipulation of the order and assignments."
  (interactive)
  (let ((buf (get-buffer-create "*persp-harpoon-list*")))
    (with-current-buffer buf
      (persp-harpoon-menu-mode)
      (setq persp-harpoon-show--current-hashtable (map-into persp-harpoon--buffers 'hash-table))
      (persp-harpoon-show--redisplay-lines))
    (pop-to-buffer buf)))

(defun persp-harpoon--show-mark-delete ()
  "Mark current entry for deletion.

This is meant to be sued in the harpoon buffer list."
  (interactive)
  (when (derived-mode-p 'persp-harpoon-menu-mode)
    (beginning-of-line)
    (puthash (get-text-property (point) 'fname) "d" persp-harpoon-show--current-hashtable)
    (persp-harpoon-show--redisplay-lines)))

(defun persp-harpoon--show-set-index ()
  "Set a new index for the current buffer entry.

This is meant to be sued in the harpoon buffer list.  Prompt for a
number, possibly marking other buffers as unassigned if there's a
clash."
  (interactive)
  (when (derived-mode-p 'persp-harpoon-menu-mode)
    (beginning-of-line)
    (let ((new-index (string-to-number (char-to-string (read-char "Enter new index[0-9]:")))))
      (maphash (lambda (fname order)
                 (when (and (numberp order) (= order new-index))
                   (puthash fname "?" persp-harpoon-show--current-hashtable)))
               persp-harpoon-show--current-hashtable)
      (puthash (get-text-property (point) 'fname) new-index persp-harpoon-show--current-hashtable)
      (persp-harpoon-show--redisplay-lines))))

(defun persp-harpoon--show-add ()
  "Prompt to add a file buffer to harpoon list.

This is meant to be sued in the harpoon buffer list.  Automatically
assigns the lowest available index."
  (interactive)
  (when (derived-mode-p 'persp-harpoon-menu-mode)
    (beginning-of-line)
    (let ((new-entry (completing-read "Buffer to add: "
                                      (delq nil
                                       (mapcar (lambda (buf)
                                                 (buffer-file-name (get-buffer buf)))
                                               (persp-harpoon--current-persp-buffers-list)))))
          (new-order (-min (-difference (number-sequence 1 9)
                                        (map-keys persp-harpoon-show--current-hashtable)))))
      (puthash new-entry new-order persp-harpoon-show--current-hashtable)
      (persp-harpoon-show--redisplay-lines))))

(defun persp-harpoon--show-end-process ()
  "Apply the current edits to the harpoon buffer & save.

This is meant to be sued in the harpoon buffer list.  Removes entries
marked for deletion or with no assigned index."
  (interactive)
  (when (derived-mode-p 'persp-harpoon-menu-mode)
    (beginning-of-line)
    (let* ((new-buffer-list (--filter (not (equal "d" (cdr it))) (map-into persp-harpoon-show--current-hashtable 'alist)))
           (all-valid (--all-p (numberp (cdr it)) new-buffer-list)))
      (when (or all-valid (y-or-n-p "There are unassigned entried, remove them?"))
        (setq persp-harpoon--buffers (--filter (numberp (cdr it)) new-buffer-list))
        (setq persp-harpoon--buffers-list (-map #'car persp-harpoon--buffers))
        (persp-harpoon-save))
      (quit-window))))

(defun persp-harpoon-show--redisplay-lines ()
  "Re-generate the contents of the harpoon management buffer."
  (when (derived-mode-p 'persp-harpoon-menu-mode)
    (let ((inhibit-read-only t)
          (name-col-length 0)
          lines)
      (maphash (lambda (k _) (let ((len (length k)))
                               (when (< name-col-length len)
                                 (setq name-col-length len))))
               persp-harpoon-show--current-hashtable)
      (erase-buffer)
      (maphash
       (lambda (fname order)
         (push (concat
                (propertize (format "%s  " order) 'face 'bold 'order order 'fname fname 'keymap persp-harpoon-menu-mode-map)
                (propertize
                 (format (format "%%-%ds" name-col-length) fname)
                 'order order 'fname fname 'keymap persp-harpoon-menu-mode-map))
               lines))
       persp-harpoon-show--current-hashtable)
      (insert (string-join lines "\n"))
      (beginning-of-line))))

;;; persp-harpoon-menu-mode and related helper functions
(define-minor-mode persp-harpoon-mode
  "Toggle persp-harpoon mode.
When active will track the buffers and update what
`persp-harpoon-switch-other' should do."
  :global t
  :keymap persp-harpoon-mode-map
  (if persp-harpoon-mode
      (add-hook 'buffer-list-update-hook #'persp-harpoon-on-buffer-switch)
    (remove-hook 'buffer-list-update-hook #'persp-harpoon-on-buffer-switch)))

;;;###autoload
(defun persp-harpoon-configure (current-persp-name-function current-persp-buffers-list-function)
  "Configure persp-harpoon.

CURRENT-PERSP-NAME-FUNCTION should be a function returning the current
perspective name.  (see also `persp-harpoon-current-persp-name-function')
CURRENT-PERSP-BUFFERS-LIST-FUNCTION should be a function returning the
list of buffers in the current perspective.  (see also
`persp-harpoon-current-persp-buffers-list-function')"
  (when current-persp-name-function
    (setf persp-harpoon-current-persp-name-function current-persp-name-function))
  (when current-persp-buffers-list-function
    (setf persp-harpoon-current-persp-buffers-list-function current-persp-buffers-list-function)))

(declare-function persp-current-name nil)
(declare-function persp-current-buffers nil)
(declare-function persp-make-variable-persp-local nil)
(declare-function projectile-project-name nil)
(declare-function projectile-project-buffers nil)

;;;###autoload
(defun persp-harpoon-configure-for-perspective (&optional unset)
  "Configure `persp-harpoon' to work with `perspective' mode.

If UNSET is non-nil, then remove hooks and reset persp-harpoon vars."
  (if unset
      (progn
        (remove-hook 'persp-switch-hook #'persp-harpoon-on-switch)
        (remove-hook 'persp-mode-hook #'persp-harpoon-on-switch)
        (setf persp-harpoon-current-persp-name-function nil
              persp-harpoon-current-persp-buffers-list-function nil))
    (unless (featurep 'perspective)
      (require 'perspective))
    (add-hook 'persp-switch-hook #'persp-harpoon-on-switch)
    (add-hook 'persp-mode-hook #'persp-harpoon-on-switch)
    (persp-make-variable-persp-local 'persp-harpoon--buffers)
    (persp-harpoon-configure #'persp-current-name (lambda () (persp-buffers (persp-curr))))))

;;;###autoload
(defun persp-harpoon-configure-for-projectile (&optional unset)
  "Configure `persp-harpoon' to work with `projectile' mode.

If UNSET is non-nil, then remove hooks and reset persp-harpoon vars."
  (if unset
      (progn
        (remove-hook 'projectile-after-switch-project-hook #'persp-harpoon-on-switch)
        (remove-hook 'persp-mode-hook #'persp-harpoon-on-switch)
        (setf persp-harpoon-current-persp-name-function nil
              persp-harpoon-current-persp-buffers-list-function nil))
    (unless (featurep 'projectile)
      (require 'projectile))
    (add-hook 'projectile-after-switch-project-hook #'persp-harpoon-on-switch)
    (add-hook 'persp-mode-hook #'persp-harpoon-on-switch)
    (persp-harpoon-configure #'projectile-project-name #'projectile-project-buffers)))

(provide 'persp-harpoon)

;;; persp-harpoon.el ends here
