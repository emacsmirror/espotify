;;; embark-spotify.el --- Embark actions for espotify  -*- lexical-binding: t; -*-

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Maintainer: Jose A Ortega Ruiz
;; Keywords: media
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://codeberg.org/jao/espotify
;; Package-Requires: ((emacs "26.1") (consult-spotify "0.1") (embark "0.10"))

;; Copyright (C) 2021  Jose A Ortega Ruiz

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Comentary:

;; This package provides embark actions to invoke when using
;; the commands in consult-spotify.

;; This file has been automatically generated from the literate program
;; https://codeberg.org/jao/espotify/src/branch/main/readme.org

;;; Code:

(require 'consult-spotify)
(require 'embark)

(defun espotify--show-info (candidate)
  "Show low-level info (an alist) about selection."
  (pop-to-buffer (get-buffer-create "*espotify info*"))
  (read-only-mode -1)
  (delete-region (point-min) (point-max))
  (insert (propertize candidate 'face 'bold))
  (newline)
  (when-let (item (espotify--item candidate))
    (insert (pp-to-string item)))
  (newline)
  (goto-char (point-min))
  (read-only-mode 1))

(defun espotify--play-album (candidate)
  "Play album associated with selected item."
  (when-let (item (espotify--item candidate))
    (if-let (album (if (string= "album" (alist-get 'type item ""))
                       item
                     (alist-get 'album item)))
        (espotify-play-uri (alist-get 'uri album))
      (error "No album for %s" (alist-get 'name item)))))

(defun espotify--yank-url (candidate)
  "Add to kill ring the Spotify URL of this entry"
  (when-let (item (espotify--item candidate))
    (if-let (url (alist-get 'spotify (alist-get 'external_urls item)))
        (kill-new url)
      (message "No spotify URL for this candidate"))))

(embark-define-keymap espotify-item-keymap
  "Actions for Spotify search results"
  ("y" espotify--yank-url)
  ("a" espotify--play-album)
  ("h" espotify--show-info))

(defun espotify--annotate-item (cand)
  (setq espotify--current-item (espotify--item cand))
  (cons 'espotify-search-item cand))

(add-to-list 'embark-keymap-alist
             '(espotify-search-item . espotify-item-keymap))

(provide 'embark-spotify)
;;; embark-spotify.el ends here
