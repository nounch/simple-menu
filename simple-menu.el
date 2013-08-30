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

(defvar smenu-menu (list
                    'google-this-line
                    'wikipedia-this-line
                    'browse-url
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
                            ;; "q"  ; Reserved as exit key
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
(defvar smenu-assoc (list))
(defvar smenu-counter 0)
(defvar smenu-buffer "*Simple Menu*")
(defvar smenu-header "--------------------------------Simple Menu---------\
-----------------------")


;;=========================================================================
;; Functions
;;=========================================================================

(defun smenu-build-menu ()
  (dotimes (i (length smenu-menu))
    (setq smenu-assoc
          (append smenu-assoc
                  (list (list (nth i smenu-trigger-keys)
                              (nth i smenu-menu)))))))
(defun smenu-insert-menu-entry (menu-element)
  (insert (format "[ %s ]    %s\n" (nth 0 menu-element)
                  (symbol-name (nth 1 menu-element)))))

(defun smenu-show-menu ()
  (switch-to-buffer smenu-buffer)
  (with-selected-window (get-buffer-window smenu-buffer)
    ;; Make buffer writable
    (toggle-read-only -1)
    (erase-buffer)
    (insert (format "%s\n\n" smenu-header))
    (dolist (list-element smenu-assoc)
      (smenu-insert-menu-entry list-element))
    ;; Make buffer read-only
    (toggle-read-only 1)))


;; Init
(setq smenu-assoc (list))


;; REMOVE
;;=========================================================================
;; Debug
;;=========================================================================

(defun debug-insert (message)
  (insert (format "\n%s" message)))


(smenu-build-menu)
(smenu-show-menu)




;;=========================================================================
;; Provide
;;=========================================================================

(provide 'simple-menu)
;;; simple-menu.el ends here
