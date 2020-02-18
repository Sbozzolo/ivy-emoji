;;; ivy-emoji.el --- Insert emojis with ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gabriele Bozzola

;; Author: Gabriele Bozzola <sbozzolator@gmail.com>
;; URL: https://github.com/sbozzolo/ivy-emoji.git
;; Version: 0.1
;; Package-Requires: ((emacs "24") (ivy "0.13.0"))
;; Keywords: emoji ivy convenience
;; Prefix: ivy-emoji

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

;; ivy-emoji provides a convenient way to insert any emoji in any buffer using
;; ivy to select the emoji by its name.

;; A font that supports emoji is needed.  The best results are obtained with Noto
;; Color Emoji or Symbola.  It might be necessary to instruct Emacs to use such
;; font with a line like the following.
;;
;; (set-fontset-font t 'symbol
;;                      (font-spec :family "Noto Color Emoji") nil 'prepend)

;;; Code:

(require 'ivy)

;; The idea of generating the list from codepoint ranges is taken from the
;; no-emoji package
(defconst ivy-emoji-codepoint-ranges
  '((#x1f000 . #x1f9ff))
  "List of codepoint ranges (inclusive) corresponding to all the emojis.")

(defun ivy-emoji---clean-name (name)
  "Convert NAME to the string that should be shown.
E.g. convert spaces to -, surround with :."
  (concat ":" (replace-regexp-in-string " " "-" (downcase name)) ":"))

(defun ivy-emoji--create-list ()
  "Create list of emojis with the emoji as first character.
This is done by parsing the codepoint ranges.

This function is used to produce the constant `ivy-emoji-list'."
  (let (emoji-list)
    (dolist (range ivy-emoji-codepoint-ranges) ; Loop over different ranges
      (dotimes (i (- (cdr range) (car range))) ; Loop over the codepoints in the range
        (let ((codepoint (+ (car range) i)))
          (let* ((name (get-char-code-property codepoint 'name))
                 (emoji (with-temp-buffer (insert codepoint) (buffer-substring 1 2))))
            (if name                    ; If the emoji is not available
                                        ; name would be nil
                                        ; Those emoji should not be included
                (setq emoji-list (append emoji-list
                                        ; The way we want to format emoji is the
                                        ; following 🌵 :cactus:
                                        ; We will insert the emoji by taking the
                                        ; first character of this string
                     (list (concat emoji " " (ivy-emoji---clean-name name))))))))))
    emoji-list ; Return value
    ))

(defun ivy-emoji--insert-emoji (emoji)
  "Insert EMOJI by extracting the first character.

This function is supposed to be used with
`ivy-emoji--create-list'."
  (insert (substring emoji 0 1)))

;; Create list of emojis using the ranges in the codepoints
(defconst ivy-emoji-list (ivy-emoji--create-list)
  "Cached list of propertized emojis.

The format is:
...
🌵 :cactus:
🍝 :spaghetti:
...
The emoji character will be selected as substring and inserted by
`ivy-emoji--insert-emoji'.")

;;;###autoload
(defun ivy-emoji ()
  "Select an emoji and insert it."
  (interactive)
  (ivy-read "Emoji: "
            ivy-emoji-list
            :require-match t
            :action #'ivy-emoji--insert-emoji))

(provide 'ivy-emoji)

;;; ivy-emoji.el ends here
