;;; simple-menu.el --- Simple Menu

;; Copyright (C) 2013  -

;; Author: - <gronpy@gronpy.gronpy>
;; Keywords: tools

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

;; ---

;;; Code:



;;=========================================================================
;; Variables
;;=========================================================================

(defvar smenu-menu)
(setq smenu-menu (list
                  'google-this-line
                  'wikipedia-this-line
                  'browse-url
                  'debug-message
                  ))
(defvar smenu-trigger-keys (list
                            "1"
                            "2"
                            "3"
                            "4"
                            "5"
                            "6"
                            "7"
                            "8"
                            "9"
                            "a"
                            "b"
                            "c"
                            "d"
                            "e"
                            "f"
                            "g"
                            "h"
                            "i"
                            "j"
                            "k"
                            "l"
                            "m"
                            "n"
                            "o"
                            "p"
                            "q"
                            "r"
                            "s"
                            "t"
                            "u"
                            "v"
                            "w"
                            "x"
                            "y"
                            "z"
                            ))
(defvar smenu-exit-trigger-key "q")
(defvar smenu-assoc (list))
(defvar smenu-counter 0)
(defvar smenu-buffer "*Simple Menu*")
(defvar smenu-previous-buffer)
(defvar smenu-header "--------------------------------Simple Menu---------\
-----------------------")
(defvar smenu-current-command)


;;=========================================================================
;; Functions
;;=========================================================================

(defun smenu-kill-buffer ()
  (interactive)
  (kill-buffer smenu-buffer))

(defun smenu-build-menu ()
  (setq smenu-assoc (append smenu-assoc (list (list
                                               smenu-exit-trigger-key
                                               'smenu-kill-buffer))))
  (dotimes (i (length smenu-menu))
    (let ((trigger-key (nth i smenu-trigger-keys)))
      (unless (equal trigger-key smenu-exit-trigger-key)
        ;; Other key bindings
        (setq smenu-assoc
              (append smenu-assoc
                      (list (list trigger-key
                                  (nth i smenu-menu)))))))))

(defun smenu-insert-menu-entry (menu-element)
  (insert (format "[ %s ]    %s\n" (nth 0 menu-element)
                  (symbol-name (nth 1 menu-element)))))

(defun smenu-make-function (menu-command)
  (lexical-let ((menu-command menu-command))
    (with-current-buffer smenu-previous-buffer
      (funcall (nth 1 menu-command)))))

(defun smenu-configure-local-keys ()
  (let ((got-valid-input nil))
    (while (not got-valid-input)
      (let* ((input-char (read-char-exclusive "Choose a menu option."))
             (entry (find-if (lambda (menu-entry)
                               (equal input-char
                                      (string-to-char (nth 0 menu-entry))))
                             smenu-assoc)))
        (when entry
          (setq got-valid-input t)
          (with-current-buffer smenu-previous-buffer
            (call-interactively (nth 1 entry))
            (smenu-kill-buffer)))))))

(defun smenu-show-menu ()
  (interactive)
  (save-window-excursion  ; Jump back to original buffer
    (setq smenu-previous-buffer (current-buffer))
    (pop-to-buffer smenu-buffer)  ; Show menu in other window, if possible
    (with-selected-window (get-buffer-window smenu-buffer)
      ;; Make buffer writable
      (toggle-read-only -1)
      (erase-buffer)
      (insert (format "%s\n\n" smenu-header))
      (dolist (list-element smenu-assoc)
        (smenu-insert-menu-entry list-element))
      ;; Make buffer read-only
      (toggle-read-only 1)
      (smenu-configure-local-keys))))


;; Init
(setq smenu-assoc (list))


;; REMOVE
;;=========================================================================
;; Debug
;;=========================================================================

(defun debug-insert (message)
  (insert (format "\n%s" message)))

(defun debug-message ()
  (interactive)
  (message (thing-at-point 'line)))

(smenu-build-menu)
(smenu-show-menu)





;;=========================================================================
;; Provide
;;=========================================================================

(provide 'simple-menu)
;;; simple-menu.el ends here
