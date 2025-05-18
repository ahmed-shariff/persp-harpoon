;;; persp-harpoon.el --- another harpoon -*- lexical-binding: t -*-

;; Copyright (C) 2025 Shariff AM Faleel

;; Author: Shariff AM Faleel
;; Package-Requires: ((emacs "28") (org-roam "2.2.0") (s "1.12.0") (magit-section "3.3.0") (transient "0.4") (org-super-agenda "1.2") (dash "2.0"))
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

;; This is a harpoon implementation that is inspired by harpoon for vim (by ThePrimeagen).

;;; Code:

(defvar persp-harpoon-cache-file "~/.emacs.d/.cache/perspective-harpoon.json")
(defvar persp-harpoon-buffers nil)
(defvar persp-harpoon-buffers-list nil)
(defvar persp-harpoon-show-buffer-list-fn #'persp-get-buffer-names)

(persp-make-variable-persp-local 'persp-harpoon-buffers)

(defun persp-harpoon-save (&optional persp-name buffer-table)
  "Save the current state of the harpoons.
HASHTABLEs keys are names of perspectives. values are lists of file-names."
  (let ((hashtable (persp-harpoon-load)))
    (puthash (or persp-name
                 (persp-current-name))
             (or buffer-table
                 (map-into persp-harpoon-buffers 'hash-table))
             hashtable)
    ;; (with-temp-buffer
    ;;   (json-insert hashtable)
    ;;   (set-visited-file-name persp-harpoon-cache-file t)
    ;;   (let (message-log-max)
    ;;     (save-buffer)))))
    (with-temp-file persp-harpoon-cache-file
      (json-insert hashtable))))

(defun persp-harpoon-load (&optional persp-name)
  "Get the stored persp-harpoon state as a hashtable."
  (unless (file-exists-p persp-harpoon-cache-file)
    (with-temp-buffer
      (set-visited-file-name persp-harpoon-cache-file t)
      (let (message-log-max)
        (save-buffer))))
  (let ((hashtable (or (ignore-error 'json-error
                         (with-temp-buffer
                           (insert-file-literally persp-harpoon-cache-file)
                           (json-parse-string (buffer-string))))
                       (make-hash-table))))
    (if persp-name
        (when-let ((-persp-hashtable (gethash persp-name hashtable)))
          (map-into -persp-hashtable 'alist))
      hashtable)))

(defun persp-harpoon--add-file-to-top (buffer-full-name)
  (unless persp-harpoon-buffers
    (setq persp-harpoon-buffers '()))
  (unless (assoc buffer-full-name persp-harpoon-buffers)
    (push (cons buffer-full-name (--first
                                  (not (rassoc it persp-harpoon-buffers))
                                  (number-sequence 1 9)))
          persp-harpoon-buffers))
  (setq persp-harpoon-buffers-list (delete buffer-full-name persp-harpoon-buffers-list))
  (push buffer-full-name persp-harpoon-buffers-list))

(defun persp-harpoon-add-buffer ()
  (interactive)
  (if-let (b (buffer-file-name))
      (let ((buffer-full-name (file-truename b)))
        (persp-harpoon--add-file-to-top (buffer-file-name))
        (persp-harpoon-save))
    (user-error "buffer not a file")))

(defun persp-harpoon-remove-buffer (&optional buffer)
  (interactive)
  (if-let (b (or buffer (buffer-file-name)))
      (let ((buffer-full-name (file-truename b)))
        (setq persp-harpoon-buffers (assoc-delete-all buffer-full-name persp-harpoon-buffers))
        (setq persp-harpoon-buffers-list (delete buffer-full-name persp-harpoon-buffers-list))
        (persp-harpoon-save))
    (user-error "buffer not a file")))

(defun persp-harpoon-clear-buffers ()
  (interactive)
  (when (y-or-n-p (format "Clear harpoon (%s)?" (persp-current-name)))
    (setq persp-harpoon-buffers '())
    (setq persp-harpoon-buffers-list nil)
    (persp-harpoon-save)))

(defun persp-harpoon-on-buffer-switch (&rest _)
  (when-let* ((b (buffer-file-name))
              (buffer-full-name (file-truename b))
              (_ (member buffer-full-name persp-harpoon-buffers-list)))
    (persp-harpoon--add-file-to-top (buffer-file-name))))

(defun persp-harpoon-jump-to-index (index)
  "Jump to the buffer with index INDEX."
  (if-let ((-buffer (rassoc index persp-harpoon-buffers)))
      (switch-to-buffer (find-file-noselect (car -buffer)))
    (user-error "No buffer for index %s" index)))

(dolist (index (number-sequence 1 9))
  (fset (intern (format "persp-harpoon-jump-to-%s" index))
        `(lambda ()
           ""
           (interactive)
           (persp-harpoon-jump-to-index ,index))))

(defun persp-harpoon-on-switch (&rest _)
  (setq persp-harpoon-buffers (or (persp-harpoon-load (persp-current-name))
                                  '()))
  (setq persp-harpoon-buffers-list (mapcar (lambda (el) (car el)) persp-harpoon-buffers)))

(defun persp-harpoon-switch-to ()
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
        (format "Switch to harpoon (%s):" (persp-current-name))
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
  (let ((buffers (mapcar (lambda (b)
                           (let ((-b (buffer-name (find-file-noselect b))))
                             (if annotate-index
                                 (cons (format "%s %s"
                                               (if-let ((idx (assoc b persp-harpoon-buffers)))
                                                   (cdr idx) " ")
                                               -b)
                                       -b)
                               -b)))
                         persp-harpoon-buffers-list))
        (-buffer-name (file-truename (buffer-name))))
    ;; (if (and (> (length buffers) 2) (member -buffer-name buffers))
    ;;     (append (delete -buffer-name buffers) (list -buffer-name))
    ;;   buffers)))
    buffers))

(defun persp-harpoon-switch-other ()
  (interactive)
  (let ((current-buffer-name (file-truename (buffer-file-name))))
    (if (and current-buffer-name
             (member (file-truename current-buffer-name) persp-harpoon-buffers-list))
        (if (> 2 (length persp-harpoon-buffers-list))
            (user-error (format "Only %s buffer(s) in harpoon" (length persp-harpoon-buffers-list)))
          (persp-harpoon--add-file-to-top (cadr persp-harpoon-buffers-list))
          (switch-to-buffer (find-file-noselect (car persp-harpoon-buffers-list))))
      (switch-to-buffer (find-file-noselect (car persp-harpoon-buffers-list))))))

(defun persp-harpoon-kill-non-harpoon-buffers ()
  (interactive)
  (when (y-or-n-p "Kill buffers not in harpoon? ")
    (let ((killed 0))
      (dolist (b (persp-current-buffers))
        (let ((-buffer-file-name (buffer-file-name b)))
          (when (or (null -buffer-file-name)
                    (not (member (file-truename -buffer-file-name) persp-harpoon-buffers-list)))
            (cl-incf killed)
            (kill-buffer b))))
      (message (format "Killed %s buffer(s)." killed)))))

(add-hook 'buffer-list-update-hook #'persp-harpoon-on-buffer-switch)
(add-hook 'persp-switch-hook #'persp-harpoon-on-switch)
(add-hook 'persp-mode-hook #'persp-harpoon-on-switch)

(defvar-local persp-harpoon-show--current-hashtable nil)

(defvar persp-harpoon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'persp-harpoon-show-mark-delete)
    (define-key map "s" #'persp-harpoon-show-set-index)
    (define-key map "a" #'persp-harpoon-show-add)
    (define-key map "k" #'previous-line)
    (define-key map "j" #'next-line)
    (define-key map (kbd "C-c C-c") #'persp-harpoon-show-end-process)
    map))

(define-derived-mode persp-harpoon-mode special-mode "persp-harpoon"
  "Mode used to modify the order/renumber of the files."
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local persp-harpoon-show--current-hashtable (make-hash-table))
  (setq header-line-format
        (substitute-command-keys
         (concat
          "Quit (burry-buffer): \\[quit-window] When done \\[persp-harpoon-show-end-process];  "
          "Delete entry: \\[persp-harpoon-show-mark-delete]; Set new index: \\[persp-harpoon-show-set-index]; Add new entry: \\[persp-harpoon-show-add];  "
          "Previous line: \\[previous-line]; Next line: \\[next-line];")))
  (buffer-disable-undo))

(defun persp-harpoon-show-list ()
  (interactive)
  (let ((buf (get-buffer-create "*persp-harpoon-list*")))
    (with-current-buffer buf
      (persp-harpoon-mode)
      (setq persp-harpoon-show--current-hashtable (map-into persp-harpoon-buffers 'hash-table))
      (persp-harpoon-show--redisplay-lines))
    (pop-to-buffer buf)))

(defun persp-harpoon-show-mark-delete ()
  (interactive)
  (when (derived-mode-p 'persp-harpoon-mode)
    (beginning-of-line)
    (puthash (get-text-property (point) 'fname) "d" persp-harpoon-show--current-hashtable)
    (persp-harpoon-show--redisplay-lines)))

(defun persp-harpoon-show-set-index ()
  (interactive)
  (when (derived-mode-p 'persp-harpoon-mode)
    (beginning-of-line)
    (let ((new-index (string-to-number (char-to-string (read-char "Enter new index[0-9]:"))))
          existing-entry)
      (maphash (lambda (fname order)
                 (when (and (numberp order) (= order new-index))
                   (puthash fname "?" persp-harpoon-show--current-hashtable)))
               persp-harpoon-show--current-hashtable)
      (puthash (get-text-property (point) 'fname) new-index persp-harpoon-show--current-hashtable)
      (persp-harpoon-show--redisplay-lines))))

(defun persp-harpoon-show-add ()
  (interactive)
  (when (derived-mode-p 'persp-harpoon-mode)
    (beginning-of-line)
    (let ((new-entry (completing-read "Buffer to add: "
                                      (-non-nil
                                       (--map (buffer-file-name (get-buffer it))
                                              (funcall persp-harpoon-show-buffer-list-fn)))))
          (new-order (-min (-difference (number-sequence 1 9) (hash-table-values persp-harpoon-show--current-hashtable)))))
      (puthash new-entry new-order persp-harpoon-show--current-hashtable)
      (persp-harpoon-show--redisplay-lines))))

(defun persp-harpoon-show-end-process ()
  (interactive)
  (when (derived-mode-p 'persp-harpoon-mode)
    (beginning-of-line)
    (let* ((new-buffer-list (--filter (not (equal "d" (cdr it))) (map-into persp-harpoon-show--current-hashtable 'alist)))
           (all-valid (--all-p (numberp (cdr it)) new-buffer-list)))
      (when (or all-valid (y-or-n-p "There are unassigned entried, remove them?"))
        (setq persp-harpoon-buffers (--filter (numberp (cdr it)) new-buffer-list))
        (setq persp-harpoon-buffers-list (-map #'car persp-harpoon-buffers))
        (persp-harpoon-save))
      (quit-window))))

(defun persp-harpoon-show--redisplay-lines ()
  (when (derived-mode-p 'persp-harpoon-mode)
    (let ((inhibit-read-only t)
          (name-col-length 0)
          lines)
      (maphash (lambda (k v) (let ((len (length k)))
                               (when (< name-col-length len)
                                 (setq name-col-length len))))
               persp-harpoon-show--current-hashtable)
      (erase-buffer)
      (maphash
       (lambda (fname order)
         (push (concat
                (propertize (format "%s  " order) 'face 'bold 'order order 'fname fname 'keymap persp-harpoon-mode-map)
                (propertize
                 (format (format "%%%ds" name-col-length) fname)
                 'order order 'fname fname 'keymap persp-harpoon-mode-map))
               lines))
       persp-harpoon-show--current-hashtable)
      (insert (string-join lines "\n"))
      (beginning-of-line))))

(provide 'persp-harpoon)

;;; persp-harpoon.el ends here
