;;; notmuch-x.el --- Notmuch extensions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/notmuch-x
;; Version: 0.2
;; Package-Requires: ((emacs "27.2"))

;; This file is NOT part of GNU Emacs.

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

;; This package provides a set of custom extensions for notmuch-emacs.


;;; Code:

(require 'notmuch)
(require 'browse-url)


;;;; Custom variables

(defgroup notmuch-x nil
  "Group for `notmuch-x' customizations."
  :group 'notmuch)

(defcustom notmuch-x-auto-update-interval 15
  "Number of minutes between each mail retrieval by `notmuch-x-update'."
  :tag "Notmuch retrieval interval"
  :group 'notmuch-x
  :type 'integer)

(defcustom notmuch-x-update-buffer "*notmuch-update*"
  "Name of the buffer for the `notmuch-x-update' output."
  :tag "Notmuch retrieval output buffer"
  :group 'notmuch-x
  :type 'string)

(defcustom notmuch-x-update-notify t
  "Notify in echo area when `notmuch-x-update' starts and finishes."
  :tag "Notmuch retrieval output buffer"
  :group 'notmuch-x
  :type 'boolean)

(defcustom notmuch-x-search-query-new-mail
  "date:2d..now and tag:unread and not tag:trash"
  "Search query for new mail for `notmuch-x-indicator-mode'."
  :tag "Search query for new mail"
  :group 'notmuch-x
  :type 'string)

(defcustom notmuch-x-mail-indicator-idle-interval 15
  "Number of idle seconds to update the `notmuch-x-modeline-indicator'."
  :tag "Update interval for the mode line indicator"
  :group 'notmuch-x
  :type 'integer)

(defcustom notmuch-x-mail-indicator-auto-hide t
  "Hide the `notmuch-x-modeline-indicator' when there are no new messages."
  :tag "Update interval for the mode line indicator"
  :group 'notmuch-x
  :type 'boolean)

(defcustom notmuch-x-view-part-temp-dir
  "/tmp/"
  "Temporary directory to save MIME parts."
  :tag "Temporary directory to save MIME parts"
  :group 'notmuch-x
  :type 'string)

(defcustom notmuch-x-auto-update t
  "When non-nil, `notmuch-x-run-notmuch' will start the auto update timer."
  :tag "Auto update"
  :group 'notmuch-x
  :type 'boolean)


;;;; Update Database

(defvar notmuch-x-update-notify-if-interactive nil
  "Notify update when `notmuch-x-update-dwim' is called interactively.")

(defun notmuch-x--update-notify (str)
  "Notify update status STR."
  (when (or notmuch-x-update-notify-if-interactive
            notmuch-x-update-notify)
    (message str)))

(defun notmuch-x-update ()
  "Retrieve mail and update notmuch database."
  (interactive)
  (notmuch-x--update-notify "[notmuch] Retrieving mail...")
  (make-process :name     "notmuch-update"
                :buffer   notmuch-x-update-buffer
                :command  '("notmuch" "new")
                :sentinel 'notmuch-x-update-sentinel)
  (with-current-buffer notmuch-x-update-buffer
    (special-mode)))

(defun notmuch-x-update-sentinel (process event)
  "Sentinel to run after notmuch update."
  (with-current-buffer notmuch-x-update-buffer
    (setq-local buffer-read-only nil)
    (goto-char (point-max)))
  (if (string= event "finished\n")
      (progn
        (notmuch-x--update-notify "[notmuch] Retrieving mail...done")
        (with-temp-buffer
          (insert (format "\nLast database update: %s\n\n"
                          (format-time-string "%F %T")))
          (append-to-buffer notmuch-x-update-buffer
                            (point-min) (point-max)))
        (when notmuch-x-indicator-mode (notmuch-x-indicator-update)))
    (message "[notmuch] Something went wrong.")
    (switch-to-buffer notmuch-x-update-buffer))
  (with-current-buffer notmuch-x-update-buffer
    (setq-local buffer-read-only t))
  (setq notmuch-x-update-notify-if-interactive nil))

(defvar notmuch-x--auto-update-timer nil
  "The auto update timer for `notmuch-x-update'.")

(defun notmuch-x-auto-update-start-timer ()
  "Start auto update timer for `notmuch-x-update'."
  (interactive)
  (if (not notmuch-x--auto-update-timer)
      (setq notmuch-x--auto-update-timer
            (run-with-timer 1 (* 60 notmuch-x-auto-update-interval)
                            #'notmuch-x-update))
    (message "[notmuch] Auto update timer is already running.")))

(defun notmuch-x-auto-update-stop-timer ()
  "Stop auto update timer for `notmuch-x-update'."
  (interactive)
  (if (not notmuch-x--auto-update-timer)
      (message "[notmuch] There is no auto update timer running.")
    (cancel-function-timers #'notmuch-x-update)
    (setq notmuch-x--auto-update-timer nil)
    (message "[notmuch] Auto update timer canceled.")))

;;;###autoload
(define-minor-mode notmuch-x-auto-update-mode
  "Toggle the auto update timer."
  :init-value nil
  :global t
  :group 'notmuch-x
  (if notmuch-x-auto-update-mode
      (notmuch-x-auto-update-start-timer)
    (notmuch-x-auto-update-stop-timer)))

;;;###autoload
(defun notmuch-x-update-dwim (&optional arg)
  "Retrieve mail and update notmuch database. With ARG, start timer."
  (interactive "P")
  (when arg (notmuch-x-auto-update-start-timer))
  (setq notmuch-x-update-notify-if-interactive
        (called-interactively-p 'interactive))
  (notmuch-x-update))

;;;###autoload
(defun notmuch-x-run-notmuch ()
  "Run `notmuch' and activate `notmuch-x-indicator-mode'.
When `notmuch-x-auto-update' is non-nil, also start auto update timer."
  (interactive)
  (setq notmuch-x-update-notify-if-interactive
        (called-interactively-p 'interactive))
  (notmuch-x-auto-update-mode notmuch-x-auto-update)
  (notmuch-x-indicator-mode +1)
  (notmuch))


;;;; Mail Indicator

(defun notmuch-x-search-new-mail ()
  "Search query for new mail."
  (interactive)
  (delete-other-windows)
  (notmuch-search notmuch-x-search-query-new-mail))

(defun notmuch-x-new-mail-counter (&optional query)
  "Return the mail count for QUERY or `notmuch-x-search-query-new-mail'."
  (string-to-number (notmuch-saved-search-count
                     (or query notmuch-x-search-query-new-mail))))

(defun notmuch-x-new-mail-p (&optional query)
  "Return non-nil when there is new mail for optional QUERY.
Default query is defined by `notmuch-x-search-query-new-mail'."
  (> (notmuch-x-new-mail-counter query) 0))

(defvar notmuch-x--modeline-indicator-string nil)

(defvar-local notmuch-x-modeline-indicator
    '(:eval notmuch-x--modeline-indicator-string))

(put 'notmuch-x-modeline-indicator 'risky-local-variable t)

(defun notmuch-x-indicator-update ()
  "Update notmuch mode line indicator."
  (setq notmuch-x--modeline-indicator-string
        (when (or (notmuch-x-new-mail-p)
                  (not notmuch-x-mail-indicator-auto-hide))
          (propertize
           "@M"
           'display (when (display-graphic-p)
                      (find-image
                       '((:type xpm :file "letter.xpm" :ascent center)
                         (:type pbm :file "letter.pbm" :ascent center))))
           'mouse-face 'mode-line-highlight
           'keymap '(mode-line keymap (mouse-1 . notmuch-x-search-new-mail))
           'help-echo (if (notmuch-x-new-mail-p)
                          (let ((num (notmuch-x-new-mail-counter)))
                            (format "%s message%s" num (if (> num 1) "s" "")))
                        "No new mail")))))

;;;###autoload
(define-minor-mode notmuch-x-indicator-mode
  "Toggle the `notmuch-x-modeline-indicator'."
  :init-value nil
  :global t
  :group 'notmuch-x
  (if notmuch-x-indicator-mode
      (progn
        (run-with-idle-timer notmuch-x-mail-indicator-idle-interval
                             t #'notmuch-x-indicator-update)
        (add-to-list 'global-mode-string notmuch-x-modeline-indicator))
    (cancel-function-timers #'notmuch-x-indicator-update)
    (setq global-mode-string
          (delete notmuch-x-modeline-indicator global-mode-string))))


;;;; Tagging

(defun notmuch-x-tag-dwim (tags)
  "Set TAGS to message(s)."
  (interactive)
  (let ((notmuch-archive-tags tags))
    (if (eq major-mode 'notmuch-search-mode)
        (notmuch-search-archive-thread)
      (notmuch-show-archive-message)
      (if (not (notmuch-show-next-open-message t))
          (notmuch-search-refresh-view)))))

(defun notmuch-x-tag-thread-dwim (tags &optional show-next)
  "Set TAGS to thread. When SHOW-NEXT is non-nil, go to next thread."
  (interactive)
  (let ((notmuch-archive-tags tags))
    (if (eq major-mode 'notmuch-search-mode)
        (notmuch-search-archive-thread)
      (notmuch-show-archive-thread)
      (if show-next
          (notmuch-show-next-thread show-next)
        (notmuch-show-next-thread)
        (notmuch-search-refresh-view)))))

(defun notmuch-x-tag-toggle-dwim (tag)
  "Toggle TAG."
  (interactive)
  (let ((current-tags (if (eq major-mode 'notmuch-search-mode)
                          (notmuch-search-get-tags)
                        (notmuch-show-get-tags))))
    (notmuch-x-tag-dwim (if (member tag current-tags)
                             (list (concat "-" tag))
                           (list (concat "+" tag))))))


;;;; Search mode

(defun notmuch-x-edit-current-search ()
  "Edit current notmuch search query."
  (interactive)
  (let ((query (read-from-minibuffer "Notmuch search: "
                                     notmuch-search-query-string
                                     nil nil 'notmuch-search-history)))
    (add-to-list 'notmuch-search-history query)
    (notmuch-search query)))

(defun notmuch-x-kill-all-search-buffers ()
  "Kill all notmuch search buffers."
  (interactive)
  (kill-matching-buffers "\\*notmuch-\\(saved-\\)?search" nil t))


;;;; Show mode

(defun notmuch-x-toggle-message-or-browse-url ()
  "Toggle message or browse url at point."
  (interactive)
  (if (thing-at-point-url-at-point)
      (browse-url (thing-at-point-url-at-point))
    (notmuch-show-toggle-message)))

(defun notmuch-x-toggle-thread-visibility ()
  "Toggle expand/collapse all messages in thread."
  (interactive)
  (let ((position (notmuch-show-message-top))
        (visibility (plist-get
                     (notmuch-show-get-message-properties)
                     :message-visible)))
    (goto-char (point-min))
    (while (progn
             (let ((props (notmuch-show-get-message-properties)))
               (notmuch-show-message-visible props
                                             (not visibility)))
             (notmuch-show-goto-message-next)))
    (goto-char position)))

(defvar notmuch-x--button-url-regexp (concat
                                      "\\[ .*\\]$\\|"
                                      browse-url-button-regexp)
  "Regexp for searching buttons and link on notmuch-show.")

(defun notmuch-x-next-button-or-link (&optional backwards)
  "Goto to the next button or link. When BACKWARDS is non-nil, go backwards."
  (interactive)
  (let ((button-pos) (link-pos) (match))
    (setq match (if backwards
                    (text-property-search-backward 'shr-url nil nil t)
                  (text-property-search-forward 'shr-url nil nil t)))
    (if match
        (setq link-pos (prop-match-beginning match)))
    (goto-char (1+ (point))) ; NOTE: prevents getting stuck in word beginning
    (if backwards
        (search-backward-regexp notmuch-x--button-url-regexp nil t nil)
      (search-forward-regexp notmuch-x--button-url-regexp nil t nil))
    (setq button-pos (match-beginning 0))

    ;;  NOTE: not the ideal, but a good enough behavior
    (if link-pos
        (goto-char link-pos)
      (goto-char button-pos))))

(defun notmuch-x-previous-button-or-link ()
  "Goto to the previous button or link."
  (interactive)
  (notmuch-x-next-button-or-link t))

(defun notmuch-x-view-part-in-browser ()
  "Save MIME part at point in temp dir and open with browser."
  (interactive)
  (let ((handle (notmuch-show-current-part-handle))
        (file (concat notmuch-x-view-part-temp-dir
                      (format "notmuch-mime-part.")
                      (format "%s." (notmuch-show-get-timestamp))
                      (replace-regexp-in-string "\\([^[:print:]]\\|/\\)" ""
                                                (notmuch-show-get-subject)))))
    (unwind-protect
        (with-temp-buffer
          (mm-save-part-to-file handle file))
      (browse-url-xdg-open file)
      (kill-buffer (mm-handle-buffer handle)))))


(provide 'notmuch-x)

;;; notmuch-x.el ends here
