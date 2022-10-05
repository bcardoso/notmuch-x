;;; notmuch-x.el --- Notmuch extensions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Bruno Cardoso

;; Author: Bruno Cardoso <cardoso.bc@gmail.com>
;; URL: https://github.com/bcardoso/notmuch-x
;; Version: 0.1
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


;;;; Custom variables

(defgroup notmuch-x nil
  "Group for `notmuch-x' customizations."
  :group 'notmuch)

(defcustom notmuch-x--notmuch-update-interval 15
  "Number of minutes between each mail retrieval by `notmuch-x-update'."
  :tag "Notmuch retrieval interval"
  :group 'notmuch-x
  :type 'integer)

(defcustom notmuch-x--notmuch-update-buffer
  "*notmuch-update*"
  "Name of the buffer for the `notmuch-x-update' output."
  :tag "Notmuch retrieval output buffer"
  :group 'notmuch-x
  :type 'string)

(defcustom notmuch-x--search-query-new-mail
  "date:2d..now and tag:unread and not tag:trash"
  "Search query for new mail run by `notmuch-x-mode-line-indicator'."
  :tag "Search query for new mail"
  :group 'notmuch-x
  :type 'string)

(defcustom notmuch-x--indicator-timer-update-interval 30
  "Number of seconds to update the `notmuch-x-mode-line-indicator'."
  :tag "Update interval for the mode line indicator"
  :group 'notmuch-x
  :type 'integer)

(defcustom notmuch-x--view-part-temp-dir
  "/tmp/"
  "Temporary directory to save MIME parts."
  :tag "Temporary directory to save MIME parts"
  :group 'notmuch-x
  :type 'string)

(defcustom notmuch-x--auto-update t
  "When non-nil, `notmuch-x-run-notmuch' will also start the `notmuch-x-update-timer'."
  :tag "Temporary directory to save MIME parts"
  :group 'notmuch-x
  :type 'string)


;;;; Update Database

;;;###autoload
(defun notmuch-x-run-notmuch ()
  "Run `notmuch' and `notmuch-x-toggle-mode-line-indicator'. When user
option `notmuch-x--auto-update' is non-nil, also run `notmuch-x-update-timer'."
  (interactive)
  (if (not notmuch-x--update-timer)
      (progn (if notmuch-x--auto-update (notmuch-x-update-timer))
             (notmuch-x-update-dwim)
             (notmuch-x-toggle-mode-line-indicator t)))
  (notmuch))

;;;###autoload
(defun notmuch-x-update-dwim ()
  "Retrieve mail and update notmuch database. With ARG, start timer."
  (interactive)
  (if current-prefix-arg
      (progn
        (if (not notmuch-x--update-timer)
            (notmuch-x-update-timer)
          (message "notmuch-x-update-timer is already running.")))
    (notmuch-x-update)))

(defvar notmuch-x--update-timer nil
  "The notmuch update timer.")

(defun notmuch-x-update-timer ()
  "Periodic mail retrieval according to `notmuch-x--notmuch-update-interval'"
  (interactive)
  (setq notmuch-x--update-timer
        (run-at-time t (* 60 notmuch-x--notmuch-update-interval)
                     #'notmuch-x-update)))

(defun notmuch-x-update-timer-stop ()
  "Cancel `notmuch-x-update-timer'"
  (interactive)
  (cancel-function-timers #'notmuch-x-update)
  (setq notmuch-x--update-timer nil))

(defun notmuch-x-update ()
  "Retrieve mail and update notmuch database."
  (interactive)
  (message "[notmuch] Retrieving mail...")
  (make-process :name     "notmuch-update"
                :buffer   notmuch-x--notmuch-update-buffer
                :command  '("notmuch" "new")
                :sentinel 'notmuch-x-update-sentinel)
  (with-current-buffer notmuch-x--notmuch-update-buffer
    (special-mode)))

(defun notmuch-x-update-sentinel (process event)
  "Sentinel to run after notmuch update."
  (with-current-buffer notmuch-x--notmuch-update-buffer
    (setq-local buffer-read-only nil)
    (goto-char (point-max)))
  (if (string= event "finished\n")
      (progn
        (message "[notmuch] Retrieving mail...done")
        (with-temp-buffer
          (insert (format "\nLast database update: %s\n\n"
                          (format-time-string "%F %T")))
          (append-to-buffer notmuch-x--notmuch-update-buffer
                            (point-min) (point-max))))
    (progn
      (message "[notmuch] Something went wrong.")
      (switch-to-buffer notmuch-x--notmuch-update-buffer)))
  (with-current-buffer notmuch-x--notmuch-update-buffer
    (setq-local buffer-read-only t)))


;;;; Mail Indicator

(defun notmuch-x-search-new-mail ()
  "Search query for new mail."
  (interactive)
  (delete-other-windows)
  (notmuch-search notmuch-x--search-query-new-mail nil))

(defun notmuch-x--mode-string (count)
  "Add properties to mode line indicator."
  (when (not (string= count "0"))
    (propertize "@M"
                'display (when (display-graphic-p) display-time-mail-icon)
                'mouse-face 'mode-line-highlight
                'keymap '(mode-line keymap
                                    (mouse-1 . notmuch-x-search-new-mail))
                'help-echo "New mail")))

(defvar notmuch-x--indicator-timer nil
  "The mode line indicator timer.")

(defvar notmuch-x-mode-line-indicator nil
  "Mode line indicator element.")

(defun notmuch-x-mode-line-indicator (&optional disable)
  "Toggle the mode line indicator element."
  (let* ((count (notmuch-saved-search-count notmuch-x--search-query-new-mail))
         (indicator (notmuch-x--mode-string count)))
    (cond ((and (not disable) (>= (string-to-number count) 1))
           (if (not notmuch-x--indicator-timer)
               (setq notmuch-x--indicator-timer
                     (run-at-time t notmuch-x--indicator-timer-update-interval
                                  #'notmuch-x-mode-line-indicator)))
           (add-to-list 'global-mode-string indicator)
           (setq notmuch-x-mode-line-indicator indicator))
          (t
           (cancel-function-timers #'notmuch-x-mode-line-indicator)
           (setq global-mode-string
                 (delete notmuch-x-mode-line-indicator global-mode-string))
           (setq notmuch-x--indicator-timer nil)
           (setq notmuch-x-mode-line-indicator nil)))))

;;;###autoload
(defun notmuch-x-toggle-mode-line-indicator (&optional force-on)
  "Toggle mode line indicator for new mail."
  (interactive)
  (if (and (not force-on) notmuch-x-mode-line-indicator)
      (notmuch-x-mode-line-indicator 'disable)
    (notmuch-x-mode-line-indicator)))


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
  "Set TAGS to thread."
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
  "Edit current notmuch search query"
  (interactive)
  (let ((query (read-from-minibuffer "Notmuch search: "
                                     notmuch-search-query-string)))
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
  "Regexp for searching buttons and link on notmuch-show")

(defun notmuch-x-next-button-or-link (&optional backwards)
  "Goto to the next button or link."
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
        (file (concat notmuch-x--view-part-temp-dir
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
