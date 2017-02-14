;;; dired-stack.el --- stack function for dired

;; Copyright (C) 2015  Koji Hachiya

;; Author: Koji.H <koji.hachiya@gmail.com>
;; Keywords: emacs dired

;;; Commentary:
;;; Code:

(require 'cl)
(require 'dired)

(defvar dired-stack-files (list))
(defvar dired-stack-buffer-name "*dired-stack*")
(push '("*dired-stack*" display-buffer-in-side-window (side . bottom) (height . 0.3) (slot . 1))
      display-buffer-alist)

(advice-add 'switch-to-buffer :after 'dired-stack-view)
(advice-add 'other-window :after 'dired-stack-view)

;; Clear Stack Files
(defun dired-stack-clear ()
  "Clear files in stack for dired."
  (interactive)
  (setq dired-stack-files (list))
  (dired-stack-buffer-update)
  )

;; Dired Stack Clear and Close window
(defun dired-stack-clear-and-close-window ()
  "Clear stack for dired and close buffer for it."
  (interactive)
  (dired-stack-clear)
  (dired-stack-buffer-close-window)
  )

;; Update stack buffer
(defun dired-stack-buffer-update ()
  "Update buffer of stack for dired."
  (let ((curbuf (current-buffer))
	(stackbuf (get-buffer-create dired-stack-buffer-name)))
    (set-buffer stackbuf)
    (setq buffer-read-only nil)
    (let ((start (point-min))
	  (end   (point-max)))
      (delete-region start end))
    (mapc #'(lambda (f)
	      (progn
		(insert f)
		(newline)
		)
	      )
	  dired-stack-files)
    (setq buffer-read-only t)
    (set-buffer curbuf)
    stackbuf
    )
  )

;; Close window for stack buffer
(defun dired-stack-buffer-close-window ()
  "Close window for buffer of stack for dired."
  (interactive)
  (let ((window (get-buffer-window dired-stack-buffer-name)))
    (if window
	(delete-window window)
      )
    )
  )

;; Push marked files to stack
(defun dired-stack-push-files ()
  "Push marked files to stack."
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
	 (first-file (car marked-files))
	 (cur-file (dired-get-filename)))
    (dolist (file marked-files)
      (pushnew file dired-stack-files :test 'equal)
      )
    (when (and (= (safe-length marked-files) 1)
	       (string= first-file cur-file))
      (next-line))
    (dired-stack-view)
    )
  )

(defun dired-stack-file-move-to-top (file)
  "Move FILE to top of stacks."
    (delete file dired-stack-files)
    (pushnew file dired-stack-files)
  )

;; move file to top of stack
(when (fboundp 'helm)
  (defun dired-stack-do-file-move-to-top ()
    "Move selected file to top of stacks."
    (interactive)
    (when dired-stack-files
      (dired-stack-buffer-close-window)
      (helm :sources '((name . "Move file to top of stack")
		       (candidates . dired-stack-files)
		       (action . dired-stack-file-move-to-top)))
      (dired-stack-view)
      )
    )
  )

;; Clear a file from stack
(defun dired-stack-clear-file ()
    "Pop file from stack."
    (interactive)
    (pop dired-stack-files)
    (dired-stack-view)
    )

;; Show stacked status
(defun dired-stack-view (&rest args)
  "View stacked files."
  (interactive)
  (if (and dired-stack-files (eq major-mode 'dired-mode))
    (display-buffer-in-side-window (dired-stack-buffer-update) display-buffer-alist)
    (dired-stack-buffer-close-window)
  )
)

;; Show stacked status (Toggle)
(defun dired-stack-view-toggle ()
  "View stacked files."
  (interactive)
  (let ((stackwindow (get-buffer-window dired-stack-buffer-name)))
    (if stackwindow
	(dired-stack-buffer-close-window)
      (dired-stack-view)
      )
    )
  )

;; prompt override file
(defun dired-stack-prompt-override-file (fs)
  "Prompt override or not FS 0 to FS 1."
  (let ((sfile (car fs))
	(dfile (cadr fs))
	)
    (cond ((not (file-exists-p sfile))
	   'nil)
	  ((and (file-exists-p dfile) (not (file-directory-p dfile)))
	   (format "Override %s to %s?" sfile dfile)
	   )
	  ((and (file-directory-p sfile) (eq sfile dfile))
	   'nil)
	  (t
	   't)
	  )
    )
  )

;; do copy
(defun dired-stack-copy-file-or-directory (fs)
  "Copy FS 0 to FS 1."
  (let ((sfile (car fs))
	(dfile (cadr fs)))
    (message (format "Copy %s to %s" sfile dfile))
    (if (file-directory-p sfile)
	(condition-case nil
	    (copy-directory sfile dfile)
	  )
      (copy-file sfile dfile t)
      )
    (setq dired-stack-files (delq sfile dired-stack-files))
    )
  )

;; do rename
(defun dired-stack-rename-file-or-directory (fs)
  "Copy FS 0 to FS 1."
  (let ((sfile (car fs))
	(dfile (cadr fs)))
    (message (format "Rename %s to %s" sfile dfile))
    (rename-file sfile dfile t)
    (setq dired-stack-files (delq sfile dired-stack-files))
    )
  )

;; do operate
(defun dired-stack-do-and-update (f fileset)
  "Do operate F and FILESET and update func."
  (map-y-or-n-p 'dired-stack-prompt-override-file f fileset)
  (dired-stack-buffer-update)
  (if (null dired-stack-files)
      (dired-stack-buffer-close-window)
    )
  (revert-buffer)
  )


(defun dired-stack-pop-all-do-operate (f)
  "Do operate F to stacked files."
  (let ((curdir (dired-current-directory))
	(fileset (list)))
    (dolist (file dired-stack-files)
      (push (list file (concat curdir (file-name-nondirectory file))) fileset)
      )
    (dired-stack-do-and-update f fileset)
    )
  )

(defun dired-stack-pop-do-operate (f)
  "Do operate F to top of stacked file."
  (let ((curdir (dired-current-directory))
	(file (car dired-stack-files))
	)
    (dired-stack-do-and-update f (list (list file (concat curdir (file-name-nondirectory file)))))
    )
  )

;; Dired Stack Copy command
(defun dired-stack-pop-all-do-copy ()
  "Copy stacked files."
  (interactive)
  (if dired-stack-files
      (when (y-or-n-p "Do you copy all files in stack to this direcotry ? ")
	(dired-stack-pop-all-do-operate 'dired-stack-copy-file-or-directory)
	)
    (dired-do-copy)
    )
  )
(defun dired-stack-pop-do-copy ()
  "Copy stacked a file."
  (interactive)
  (if dired-stack-files
      (dired-stack-pop-do-operate 'dired-stack-copy-file-or-directory)
    (dired-do-copy)
      )
  )


;; Dired Stack Rename command
(defun dired-stack-pop-all-do-rename ()
  "Rename stacked files."
  (interactive)
  (if dired-stack-files
      (when (y-or-n-p "Do you rename all files in stack to this direcotry ? ")
	(dired-stack-pop-all-do-operate 'dired-stack-rename-file-or-directory)
	)
    (dired-do-rename)
    )
  )

(defun dired-stack-pop-do-rename ()
  "Rename stacked a file."
  (interactive)
  (if dired-stack-files
      (dired-stack-pop-do-operate 'dired-stack-rename-file-or-directory)
    (dired-do-rename)
    )
  )


(defun ediff-compare-file-or-dir (a b)
  "Compare file or dir."
  (if (file-directory-p a)
      (ediff-directories a b "")
    (ediff a b)
      )
  )

;; Diff marked files
(defun dired-diff-marked-files ()
  "Compare marked files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (let ((afile (car files))
	  (bfile (cadr files)))
      (if (and afile bfile)
	  (ediff-compare-file-or-dir afile bfile)
	(message "Please mark above two files")
	)
      )
    )
  )

;;; Diff
(defun dired-stack-pop-do-diff ()
  "Execute diff to file in stack."
  (interactive)
  (let ((filea (pop dired-stack-files))
	(fileb (pop dired-stack-files)))
    (cond ((and filea fileb)
	   (ediff-compare-file-or-dir filea fileb)
	   )
	  (filea
	   (ediff-compare-file-or-dir filea (dired-get-filename))
	   )
	  (t
	   (dired-diff-marked-files)
	   )
	  )
    )
  (if (null dired-stack-files)
    (dired-stack-buffer-close-window)
    )
  )

;;; open
(defun dired-stack-pop-all-do-open()
  "Open files in stack."
  (interactive)
  (if dired-stack-files
      (when (y-or-n-p "Do you open all files in stack? ")
	(mapc 'find-file dired-stack-files)
	(dired-stack-clear-and-close-window)
	)
    )
  )

(provide 'dired-stack)

;;; dired-stack.el ends here
