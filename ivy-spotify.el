;;; ivy-spotify.el --- Search spotify with ivy/counsel  -*- lexical-binding: t; -*-

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Maintainer: Jose A Ortega Ruiz
;; Keywords: multimedia
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://codeberg.org/jao/espotify
;; Package-Requires: ((emacs "26.1") (spotify "0.1") (ivy "0.10"))

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

;;; Commentary:

;; This package provides a counsel interface to spotify's search API,
;; analogous to counsel-spotify.

;; This file has been automatically generated from the literate program
;; https://codeberg.org/jao/espotify/src/branch/main/readme.org

;;; Code:

(require 'espotify)
(require 'ivy)


(defun ivy-spotify--search-by (type)
  "Perform an asynchronous spotify search, for resources of the given TYPE."
  (let ((current-term ""))
    (lambda (term)
      (when-let (term (espotify-check-term current-term term))
        (espotify-search-all
         (lambda (its)
           (let ((cs (mapcar #'espotify-format-item its)))
             (ivy-update-candidates cs)))
         (setq current-term term)
         type))
      0)))


(defun ivy-spotify--play-album (candidate)
  "Play album associated with selected item."
  (interactive "s")
  (let ((item (espotify-candidate-metadata candidate)))
    (if-let (album (if (string= "album" (alist-get 'type item ""))
                       item
                     (alist-get 'album item)))
        (espotify-play-uri (alist-get 'uri album))
      (error "No album for %s" (alist-get 'name item)))))

(defun ivy-spotify-search-by (type)
  (ivy-read (format "Search %s: " type)
            (ivy-spotify--search-by type)
            :dynamic-collection t
            :action `(1 ("a" ivy-spotify--play-album "Play album")
                        ("p" espotify-play-candidate ,(format "Play %s" type)))))

;;;###autoload
(defun ivy-spotify-album ()
  (interactive)
  (ivy-spotify-search-by 'album))

;;;###autoload
(defun ivy-spotify-artist ()
  (interactive)
  (ivy-spotify-search-by 'artist))

;;;###autoload
(defun ivy-spotify-track ()
  (interactive)
  (ivy-spotify-search-by 'track))

;;;###autoload
(defun ivy-spotify-playlist ()
  (interactive)
  (ivy-spotify-search-by 'playlist))


(provide 'ivy-spotify)
;;; ivy-spotify.el ends here
