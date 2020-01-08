;;; ivy-emoji.el --- Insert emojis with ivy

;; Copyright (C) 2020 Gabriele Bozzola

;; Author: Gabriele Bozzola <sbozzolator@gmail.com>
;; URL: https://github.com/sbozzolo/ivy-emoji.git
;; Version: 0.0.1
;; Package-Requires: ((company-emoji "2.5.2") (ivy "0.13.0"))
;; Keywords: emoji ivy
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

;; This package uses on company-emoji to generate the emoji list

(require 'ivy)
(require 'company-emoji-list)

(defun ivy-emoji--list-create ()
  "Create list of emojis with the emoji as first character."
  ;; The output of company-emoji-list-create is a list with elements
  ;; of the form #(":grinning:" 0 1 (:unicode "😀"))
  ;; So we extract the emoji and prepend it to the string
  (mapcar '(lambda (arg)
             (concat
              (get-text-property 0 :unicode arg) ;; Get the emoji
              " "                                ;; Add space
              (substring-no-properties arg))     ;; Print the name
             )
          (company-emoji-list-create)
          )
  )

(defun ivy-emoji--insert-emoji (emoji)
  "Insert emoji by extracting the first character. This function
   is supposed to be used with ivy-emoji--list-create"
  (insert (substring emoji 0 1))
  )


;; Create list of emojis using company-emoji
(defconst ivy-emojis (ivy-emoji--list-create)
  "Cached list of propertized emojis.")

;;;###autoload
(defun ivy-emoji ()
  "Select an emoji and insert it."
  (interactive)
  (ivy-read "Emoji: "
            ivy-emojis
            :require-match t
            :action #'ivy-emoji--insert-emoji
            )
  )

(provide 'ivy-emoji)
;;; ivy-emoji.el ends here
