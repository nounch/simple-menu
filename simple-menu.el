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

;; Provides a simple menu consisting of mappings between single key presses
;; and user-defined functions.
;;
;; The functions can be either interactive or non-interactive and will be
;; executed in the context of the invocation buffer whereas a visual list
;; of key-command bindings is displayed in the `*Simple Menu*' buffer which
;; appears in another window, if possible.
;;
;; The list and sort order of commands is customizable via the variable
;; `simple-menu-commands' like this:
;;   `M-x customize-variable RET simple-menu-commands'
;;
;; The list and sort order of commands is customizable via the variable
;; `simple-menu-trigger-keys' like this:
;;   `M-x customize-variable RET simple-menu-trigger-keys'

;;; Code:


;;=========================================================================
;; Customizable variables
;;=========================================================================

;; Note:
;; The strange list layout for `smenu-menu' and `smenu-trigger-keys'
;; intentional.

(defgroup smenu-custom-group nil
  "Simple Menu customization group")

(defcustom smenu-menu (list
		       'browse-url
		       'smenu-debug-message
		       'yank
		       'help
		       'info
		       )
  "List of available commands in the menu. The order corresponds to the
order in which the commands are displayed as menu options.

`smenu-kill-buffer' is available by default as the first command in the
menu. Its key binding can be set via `smenu-exit-trigger-key'."
  :link '(function-link :tag
                        "smenu kill buffer function (`smenu-kill-buffer')"
                        smenu-kill-buffer)
  :link '(custom-group-link
          :tag
          "smenu exit trigger key (smenu-exit-trigger-key)"
          smenu-exit-trigger-key)
  :group 'smenu-custom-group
  :type '(repeat function))

(defcustom smenu-trigger-keys (list
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
                               )
  "List of available keys."
  :group 'smenu-custom-group)

(defcustom smenu-exit-trigger-key "q"
  "Trigger key for `smenu-kill-buffer' which kills the menu buffer."
  :link '(function-link :tag "smenu kill buffer function"
                        smenu-kill-buffer)
  :group 'smenu-custom-group)
(defcustom smenu-buffer "*Simple Menu*"
  "Name of the menu buffer."
  :group 'smenu-custom-group)
(defcustom smenu-header "------------------------------- Simple Menu --------\
-----------------------"
  "Header for the menu buffer. This is displayed at the top of the menu
buffer."
  :group 'smenu-custom-group)


;;=========================================================================
;; Variables
;;=========================================================================

(defvar smenu-assoc (list)
  "Data structure representing menu entries

It should look like this:

  ((\"1\" 'some-function)
   (\"1\" 'some-other-function)
   (\"3\" 'yet-another-function)
   ;; ...
  )

The car of each list item is a key represented as a string and the cdr is a
function which is to be associated with the corresponding key.")
(defvar smenu-previous-buffer nil
  "Active buffer at the time `smenu-show-menu' or `simple-menu-show' is
called.")


;;=========================================================================
;; Functions
;;=========================================================================

(defun smenu-debug-message ()
  "Helper function to be used as a debugging menu option.

Writeds `thing-at-point' to the `*MESSAGE*' buffer upon invocation."
  (interactive)
  (message (thing-at-point 'line)))

(defun smenu-kill-buffer ()
  "Kills the Simple Menu buffer `smenu-buffer'."
  (interactive)
  (kill-buffer smenu-buffer))

(defun smenu-build-menu ()
  "Builds the data structure `smenu-assoc' which represents the
key-function bindings constituting menu entries."
  (setq smenu-assoc (list))  ; Clear previous entries
  ;; Append the exit trigger binding first
  (setq smenu-assoc (append smenu-assoc (list (list
                                               smenu-exit-trigger-key
                                               'smenu-kill-buffer))))
  ;; Append all bindings except the one for the exit trigger
  (dotimes (i (length smenu-menu))
    (let ((trigger-key (nth i smenu-trigger-keys)))
      (unless (equal trigger-key smenu-exit-trigger-key)
        (setq smenu-assoc
              (append smenu-assoc
                      (list (list trigger-key
                                  (nth i smenu-menu)))))))))

(defun smenu-insert-menu-entry (menu-element)
  "Inserts MENU-ELEMENT into the current buffer.

MENU-ELEMENT has to be a list of the form `(string symbol)'."
  (insert (format "[ %s ]    %s\n" (nth 0 menu-element)
                  (symbol-name (nth 1 menu-element)))))

(defun smenu-dispatch-keys ()
  "Dispatches key presses in the context of the current buffer.

Only single key events are handled; so multi-key input will not work."
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
  "Shows the Simple Menu buffer in another window, if possible, and
dispatches key events in the context of the invocation buffer."
  (interactive)
  (smenu-build-menu)
  (setq smenu-previous-buffer (current-buffer))
  (display-buffer (get-buffer-create smenu-buffer) t)
  (with-selected-window (get-buffer-window smenu-buffer)
    ;; Make buffer writable
    (toggle-read-only -1)
    (erase-buffer)
    (insert (format "%s\n\n" smenu-header))
    (dotimes (i (length smenu-assoc))
      (when (equal i 1)
	(insert "\n"))
      (smenu-insert-menu-entry (nth i smenu-assoc)))
    ;; Make buffer read-only
    (toggle-read-only 1))
  (smenu-dispatch-keys))


;;=========================================================================
;; External interface
;;=========================================================================

(defalias 'simple-menu 'smenu-show-menu
  "Show Simple Menu.

This is a facade function for `smenu-show-menu'.")

(defvaralias 'simple-menu-commands 'smenu-menu
  "List of user-defined Simple Menu commands.

This is a facade variable for `smenu-menu'.")

(defvaralias 'simple-menu-trigger-keys 'smenu-trigger-keys
  "List of Simple Menu trigger keys.

This is a facade variable for `smenu-trigger-keys'.")


;;=========================================================================
;; Provide
;;=========================================================================

(provide 'simple-menu)
;;; simple-menu.el ends here
