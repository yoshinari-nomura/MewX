;;; mew-extra.el --- Add some functionarities to Mew

;; Description: Add some functionarities to Mew
;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2016-08-24
;; Version: 0.1.0
;; Keywords: Mew mail
;; URL:
;; Package-Requires:

;;;
;;; Commentary:
;;;

;;; Code:

;; eliminate compiler warnings

(defvar mew-draft-folder)
(defvar mew-regex-sumsyn-long)
(defvar mew-regex-sumsyn-short)
(defvar mew-case)
(defvar mew-cs-text-for-read)
(defvar mew-header-reasonable-size)
(defvar mew-decode-syntax)
(defvar mew-message-mode-map)

(declare-function helm-comp-read "helm")
(declare-function helm-migemo-mode "helm")
(declare-function mew-cache-message "mew")
(declare-function mew-case-folder "mew")
(declare-function mew-case:folder-case "mew")
(declare-function mew-case:folder-folder "mew")
(declare-function mew-decode-syntax-copy "mew")
(declare-function mew-expand-folder "mew")
(declare-function mew-expand-msg "mew")
(declare-function mew-header-get-value "mew")
(declare-function mew-header-replace-value "mew")
(declare-function mew-imap-folder-alist "mew")
(declare-function mew-insert-message "mew")
(declare-function mew-join "mew")
(declare-function mew-local-folder-alist "mew")
(declare-function mew-message-mode "mew")
(declare-function mew-mime-message/rfc822 "mew")
(declare-function mew-msg-get-filename "mew")
(declare-function mew-refile-decode-subject "mew")
(declare-function mew-refile-guess "mew")
(declare-function mew-summary-display-preamble "mew")
(declare-function mew-summary-goto-message "mew")
(declare-function mew-summary-reply-with-citation "mew")
(declare-function mew-summary-set-count-line "mew")
(declare-function mew-summary-visit-folder "mew")
(declare-function mew-sumsyn-folder-name "mew")
(declare-function mew-sumsyn-match "mew")
(declare-function mew-sumsyn-message-number "mew")
(declare-function mew-virtual-mode "mew")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Folder, message-path, header manipulation

(defmacro mewx-with-current-message-buffer (&rest body)
  "Eval BODY after switch from summary to message."
  `(save-excursion
     (mew-summary-goto-message)
     (mew-sumsyn-match mew-regex-sumsyn-short)
     (mew-summary-set-message-buffer
      (mew-sumsyn-folder-name)
      (mew-sumsyn-message-number))
     ,@body))

;;; XXX (mew-summary-case-proto) → ("default" "%") みたいなのがあるので，それを使ったほうがよさそう
(defun mewx-current-info (property)
  "Get PROPERTY of pointed messsage."
  (when (mew-sumsyn-match mew-regex-sumsyn-long)
    (let* ((folder (mew-sumsyn-folder-name))
           (number (mew-sumsyn-message-number))
           (path (mew-msg-get-filename (mew-expand-msg folder number)))
           (directory (mew-expand-folder folder))
           (case (or (mew-case:folder-case folder) mew-case))
           (proto (substring (mew-case:folder-folder folder) 0 1)))
      (cdr (assoc
            property
            `((folder    . ,folder)
              (number    . ,number)
              (path      . ,path)
              (directory . ,directory)
              (case . ,case)
              (proto . ,proto)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set refile destination on opening draft

(defun mewx-refile-guess-current-message ()
  (interactive)
  (let (mew-inherit-refile-case
        mew-inherit-refile-proto
        fld msg guess)
    ;; Get current folder name and message number
    (save-excursion
      (mew-summary-goto-message)
      (mew-sumsyn-match mew-regex-sumsyn-short)
      (setq fld (mew-sumsyn-folder-name))
      (setq msg (mew-sumsyn-message-number)))
    ;; Set hints for mew-refile-guess function
    (setq mew-inherit-refile-case
          (or (mew-case:folder-case fld) mew-case)
          mew-inherit-refile-proto
          (substring (mew-case:folder-folder fld) 0 1))
    ;; Setup tmp buffer to perform guess function
    (with-temp-buffer
      (mew-insert-message
       fld msg mew-cs-text-for-read mew-header-reasonable-size)
      (mew-refile-decode-subject)
      (setq guess (mew-refile-guess nil t)))
    ;; return candidates as folder list
    (car guess)))

(defun mewx-draft-append-string-list-to-header (string-list header)
  "Append STRING-LIST to HEADER as comma-separated values.
Example: (mewx-draft-append-string-list-to-header '(\"%backup\" \"%reusable\") \"Fcc:\")"
  (let* ((current-list (split-string
                        (or (mew-header-get-value header) "")
                        "," t "[\t ]")))
    (dolist (value string-list)
      (unless (member value current-list)
        (setq current-list (append current-list (list value)))))
    (mew-header-replace-value header (mew-join ", " current-list))))

(defun mewx-summary-reply-with-citation-and-fcc-folders (&optional replysender)
  "Ask Fcc: folders and reply.  See also ``mew-summary-reply-with-citation'' about REPLYSENDER."
  (interactive "P")
  (let* ((case (mewx-current-info 'case))
         (proto (mewx-current-info 'proto))
         (folders (mewx-mew-input-refile-folders
                   (mewx-refile-guess-current-message) nil case proto)))
    (mew-summary-reply-with-citation replysender)
    (mewx-draft-append-string-list-to-header folders "Fcc:")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helm with Mew
;;;
;;
;; This is replaced by Simple folder selection using completing-read
;;
;; (defun mewx-helm-mew-input-folders (default-folder-list singlep &optional case proto must-match)
;;   "Ask folder list using helm."
;;   (let ()
;;     (helm-migemo-mode +1)
;;     (if case
;;         (setq default-folder-list
;;               (mapcar (lambda (folder)
;;                         (mew-case-folder case folder))
;;                       default-folder-list)))
;;     (helm-comp-read "Folder: "
;;                     (append default-folder-list
;;                             (mapcar (lambda (f) (mew-case-folder case (car f))) (mew-imap-folder-alist case))
;;                             (mapcar (lambda (f) (mew-case-folder case (car f))) (mew-local-folder-alist)))
;;                     :must-match must-match
;;                     :marked-candidates t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple folder selection using completing-read
;;;

;; Mew has three main functions to select folder(s)
;; mew-minibuf.el::
;; (defun mew-input-refile-folders (folder-list singlep case proto)...)
;; (defun mew-input-folders (case:folder)
;; (defun mew-input-folder (case folder)
;;

;; helper function

(defun mewx-full-case-folder (case folder)
  "Make folder name from CASE and FOLDER in the form of CASE:FOLDER.
if CASE is nil or \"default\", CASE part will be omited.
Otherwise, function adds CASE part even if the FOLDER is local.
(mewx-full-notation-case-folder \"default\" \"+inbox/\") → +inbox
(mewx-full-notation-case-folder \"foo\" \"+inbox/\") → foo:+inbox
(mewx-full-notation-case-folder \"default\" \"%inbox/\") → %inbox
(mewx-full-notation-case-folder \"foo\" \"%inbox/\") → foo:%inbox
"
  (let* ((case (or case "default"))
         (full (directory-file-name (mew-case-folder case folder))))
    (if (and (mew-folder-localp folder)
             (not (string= case "default")))
        (concat case ":" full)
      full)))

(defun mewx-gather-folder-list (case proto &optional list-all prepend-list)
  "Return list of all mail folders in CASE and PROTO.
If optional LIST-ALL is non-nil, list folders in all cases and protos.
If optional PREPEND-LIST is non-nil, it will be prepended to the result.
"
  (let* ((first-case-proto (cons (or case "default")
                                 (or proto (mew-proto case))))
         (all-case-proto (mapcar (lambda (ent)
                                   (let ((case (mew-config-case-name ent)))
                                     (cons case (mew-proto case))))
                                 mew-config-alist))
         (cases-protos (if list-all
                           (cons first-case-proto
                                 (assoc-delete-all (car first-case-proto) all-case-proto))
                         (list first-case-proto))))
    (apply 'append
           prepend-list
           (mapcar (lambda (case-proto)
                     (let ((case (car case-proto))
                           (proto (cdr case-proto)))
                       (append
                        ;; List CASE:[$%+]inbox...
                        (mapcar (lambda (folder)
                                  (mewx-full-case-folder case (car folder)))
                                (mew-proto-folder-alist proto case))
                        ;; If list-all and CASE was [%$], add CASE:+inbox...
                        (if (and list-all (mew-folder-remotep proto))
                            (mapcar (lambda (folder)
                                      (mewx-full-case-folder case (car folder)))
                                    (mew-local-folder-alist))))))
                   cases-protos))))

(defun mewx-mew-input-refile-folders (folder-list singlep case proto)
  "Substitution of mew-input-refile-folders using simple
`completing-read-multiple`."
  (let* ((case-fold-search t)
         (default (if singlep (car folder-list)))
         (initial (if singlep (mew-folder-prefix default)
                    (mew-join "," folder-list)))
         (candidate-folders (mewx-gather-folder-list case proto nil folder-list))
         (prompt (concat
                  (if singlep "Folder name" "Folder names")
                  (if case (format " <%s:>" case))
                  (if default (format " (%s)" default))
                  ": "))
         (completing-read-func (if singlep #'completing-read-multiple
                                 #'completing-read-multiple))
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action action candidate-folders string pred))))
         (folder
          (funcall completing-read-func
                   prompt completion-table nil nil
                   initial 'mew-input-folder-hist default)))
    (if (and singlep
             (or (string= (car folder) "")
                 (string= (car folder) initial)))
        (setq folder (list default)))
    (mew-input-refile-folder-check
     folder (if (mew-folder-imapp proto) 'imap 'local))))

;; Substitution of mew-input-folders using simple `completing-read-multiple`
;; Original interface:
;;   mew-input-folders (case:folder)
;;
;; Used in:
;;   mew-summary-selection-by-search
;;   mew-edit-make-refile
;;
;; mew-edit-make-refile は，メッセージ送信時に使われている?
;; mew-summary-reedit "E" ではなく，mew-summary-edit "M-e" で draft を開いたときのみ
;; 送信時 mew-edit-make が走るっぽい．
;;   mew-summary-edit の場合，mode-name が "Draft" ではなく "Edit" になる
;;   その場合だけ，
;;     mew-draft-send-message, mew-draft-make-message, 経由で
;;     mew-edit-make → mew-edit-make-refile と呼ばれて， mew-input-folders に到達
;;
;; つまり，IMAP の %draft 中のメッセージを開いて，C-cC-c で送信するの
;; ではなく，フォルダに push する操作のときにフォルダを選択する必要が
;; あって，そのときに使っているっぽい．
;;
(defun mewx-mew-input-folders (case:folder)
  "Substitution of mew-input-folders using simple `completing-read-multiple`."
  (message "mewx-mew-input-folders(%s)" case:folder)
  (let* ((init-case (mew-case:folder-case case:folder))
         (init-folder (mew-case:folder-folder case:folder))
         (folder (mewx-mew-input-folder init-case init-folder)))
    (list folder)))

;; Substitution of mew-input-folder using simple `completing-read`
;; Original interface:
;;   mew-input-folder (case folder)
;;
(defun mewx-mew-input-folder (case folder)
  "Substitution of mew-input-folder using simple
`completing-read`.
CASE:FOLDER is used as default. CASE:% or CASE:+ will be used
as initial input. % or + is taken from FOLDER."
  (let* ((case-fold-search t)
         (default (mew-case-folder case folder))
         (proto (mew-folder-prefix folder))
         (initial (mew-case-folder case proto))
         (candidate-folders (mewx-gather-folder-list case proto t (list default)))
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action action candidate-folders string pred))))
         (folder (completing-read
                  (format "Folder name (%s): " default)
                  completion-table nil nil initial nil default)))
    (when (or (string= folder "") (string= folder initial))
      (setq folder default))
    (if (member (mew-case:folder-folder folder)
                (list mew-inbox-folder
                      mew-pop-inbox-folder
                      mew-imap-inbox-folder
                      mew-nntp-newsgroup))
        folder
      (car (mew-input-folder-check (list folder))))))

(with-eval-after-load 'mew
  (fset 'mew-input-refile-folders-orig (symbol-function 'mew-input-refile-folders))
  (fset 'mew-input-folder-orig (symbol-function 'mew-input-folder))
  (fset 'mew-input-folders-orig (symbol-function 'mew-input-folders))

  (defvar mewx-input-system-type "mew")

  (defun mewx-change-input-system (&optional name)
    "Change Mew minibuffer input system.
If NAME is
  String: Change system type to NAME.
  non-nil: Toggle system type.
  nil: Ask name interactively."
    (interactive "P")
    (let* ((orig mewx-input-system-type)
           (next (if (string= orig "mew") "plain" "mew")))
      (setq name
            (cond ((stringp name) name)
                  (name next)
                  (t (completing-read
                      (format "Mew input system (current:%s): " orig)
                      '("mew" "plain")
                      nil t nil nil next)))))
    (cond ((string= name "plain")
           (fset 'mew-input-folder (function mewx-mew-input-folder))
           (fset 'mew-input-folders (function mewx-mew-input-folders))
           (fset 'mew-input-refile-folders (function mewx-mew-input-refile-folders)))
          (t
           (fset 'mew-input-folder (function mew-input-folder-orig))
           (fset 'mew-input-folders (function mew-input-folders-orig))
           (fset 'mew-input-refile-folders (function mew-input-refile-folders-orig))))
    (message "Set current Mew input system to %s." name)
    (setq mewx-input-system-type name))

  (mewx-change-input-system "plain")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch draft rolling

(defun mew-draft-get-number (&optional buffer)
  (let ((buffer-name (buffer-name (or buffer (current-buffer)))))
    (if (string-match
         (concat "^" (regexp-quote mew-draft-folder) "/\\([0-9]+\\)") buffer-name)
        (string-to-number (substring buffer-name (match-beginning 1))))))

(defun mew-draft-revolve-list (item item-list)
  (let (next (top (car item-list)))
    (while (car item-list)
      (if (equal item (car item-list))
          (setq next (car (cdr item-list))))
      (setq item-list (cdr item-list)))
    (or next top)))

(defun mew-draft-numbers-in-buffer-list ()
  (sort
   (delq nil (mapcar (lambda (buf)
                       (mew-draft-get-number buf))
                     (buffer-list)))
   '<))

(defun mew-draft-next-draft ()
  (interactive)
  (let ((next-number
         (mew-draft-revolve-list (mew-draft-get-number)
                                 (mew-draft-numbers-in-buffer-list))))
    (if next-number
        (progn
          (message (format "%s/%d.eml" mew-draft-folder next-number))
          (switch-to-buffer (format "%s/%d.eml" mew-draft-folder next-number)))
      (message "No Draft buffer")
      (mew-summary-visit-folder mew-draft-folder))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add keyword and insert

(defvar mewx-default-keyword "%Reusable")

(defun mewx-draft-ask-fcc ()
  (interactive)
  (let ((folders (mewx-mew-input-folders "%backup")))
    (if folders
        (mewx-draft-append-string-list-to-header folders "Fcc:"))))

(defun mewx-draft-ask-reusable ()
  (interactive)
  (if (y-or-n-p "Is this mail reusable? ")
      (mewx-draft-append-string-list-to-header '("%reusable") "Fcc:")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; summary mode generation

;; (mewx-show-eml-file "~/Mail/all" "15af5a09d4140878.eml")

(defun mewx-show-eml-file (fld msg)
  (interactive)
  (let ((cache)
        (buf (get-buffer-create "TEST"))
        (mew-summary-reply-position nil))
    (pop-to-buffer buf)
    (setq inhibit-read-only t)
    ;; (mew-message-citation-frame-id (mew-frame-id))
    (mew-summary-display-preamble)
    (setq cache (mew-cache-message fld msg t))
    (mew-decode-syntax-copy cache)
    (mew-mime-message/rfc822 cache mew-decode-syntax)
    ;; (mew-cache-flush)
    (mew-message-mode)
    ))

(define-key mew-message-mode-map "." 'mewx-recenter)

(defun mewx-recenter ()
  (interactive)
  (let ((vispos (if (get-text-property (point-min) 'mew-visible)
                    (point-min)
                  (or (next-single-property-change (point-min) 'mew-visible)
                      (point-max)))))
    (set-window-start (selected-window) vispos)))


;; (mewx-show-eml-file "~/Mail/all" "15af5a09d4140878.eml")
;; (mew-summary-set-message-buffer "+inbox" "1")
;; (mew-summary-msg-or-part "OK")

(defun mewx-make-virtual-current-buffer ()
  (setq inhibit-read-only t
        buffer-read-only nil
        selective-display t
        selective-display-ellipses nil
        indent-tabs-mode nil)
  (widen))

;; (defun mhc-mew-generate-test ()
;;   (interactive)
;;   (let ((date (mhc-date-new 2017 4 1)))
;;     (mhc-mew-generate-summary-buffer date)
;;     (mhc-mew-summary-mode-setup date)))

;; (mewx-glima-generate-summary-buffer)
(defun mewx-glima-generate-summary-buffer ()
  (interactive)
  (let* ((current-folder (buffer-name))
         (folder (mew-input-folder nil current-folder))
         (vfolder (replace-regexp-in-string "^%" "+" folder))
         (range ""))
    (switch-to-buffer
     (set-buffer (get-buffer-create folder)))
    (setq inhibit-read-only t
          buffer-read-only nil
          selective-display t
          selective-display-ellipses nil
          indent-tabs-mode nil)
    (widen)
    (if (string= folder current-folder)
        (setq range "next")
      (delete-region (point-min) (point-max)))
    (message "Fetching %s current:%s..." folder current-folder)
    (sit-for 0.1)
    (goto-char (point-max))
    (save-excursion
      (message (format "glima scan %s %s --format=mew" vfolder range))
      (insert (shell-command-to-string (format "glima scan %s %s --format=mew" vfolder range))))
    ;; (insert (shell-command-to-string (format "glima scan %s %s --format=mew" vfolder "")))
    (message "Fetching %s...done" folder)
    (mew-virtual-mode)
    (mew-summary-set-count-line)
    ;; (mew-summary-mode)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq inhibit-read-only nil)
    (setq truncate-lines t)))

(provide 'mew-extra)

;;; mew-extra.el ends here
