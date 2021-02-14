;;; consult-spotify.el --- Spotify queries using consult  -*- lexical-binding: t; -*-

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Maintainer: Jose A Ortega Ruiz
;; Keywords: multimedia
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://codeberg.org/jao/espotify
;; Package-Requires: ((emacs "26.1") (consult "0.5") (marginalia "0.3") (espotify "0.1"))

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

;; This package provides functions to interactively query
;; Spotify using consult.  Its main entry points are the
;; commands `consult-spotify-album', `consult-spotify-artist',
;; `consult-spotify-playlist' and `consult-spotify-track'

;; This file has been automatically generated from the literate program
;; https://codeberg.org/jao/espotify/src/branch/main/readme.org

;;; Code:

(require 'seq)
(require 'espotify)
(require 'consult)
(require 'marginalia)

(defvar consult-spotify-history nil)

(defun consult-spotify-by (type &optional filter)
  (consult--read (consult-spotify--search-generator type filter)
                 :prompt (format "Search %ss: " type)
                 :lookup 'consult-spotify--consult-lookup
                 :category 'spotify-search-item
                 :history 'consult-spotify-history
                 :initial consult-async-default-split
                 :require-match t))


(defun consult-spotify--search-generator (type filter)
  (thread-first (consult--async-sink)
    (consult--async-refresh-immediate)
    (consult--async-map #'consult-spotify--format-item)
    (consult-spotify--async-search type filter)
    (consult--async-throttle)
    (consult--async-split)))


(defun consult-spotify--async-search (next type filter)
  (let ((current ""))
    (lambda (action)
      (pcase action
        ((pred stringp)
         (when-let (term (consult-spotify-check-term current action))
           (setq current term)
           (espotify-search-all
            (lambda (x)
              (funcall next 'flush)
              (funcall next x))
            current
            type
            filter)))
        (_ (funcall next action))))))

(defvar consult-spotify-search-suffix "="
  "Suffix in the search string launching an actual Web query.")

(defvar consult-spotify-search-threshold 8
  "Threshold to automatically launch an actual Web query.")

(defun consult-spotify--d (a b)
  "Distance between strings A and B."
  (if (fboundp 'string-distance)
      (string-distance a b)
    (abs (- (length a) (length b)))))

(defun consult-spotify-check-term (prev new)
  "Compare search terms PREV and NEW return the one we should search, if any."
  (when (not (string-blank-p new))
    (cond ((string-suffix-p consult-spotify-search-suffix new)
           (substring new 0 (- (length new)
                               (length consult-spotify-search-suffix))))
          ((>= (consult-spotify--d prev new) consult-spotify-search-threshold)
           new))))

(defun consult-spotify--additional-info (x)
  (mapconcat 'identity
             (seq-filter 'identity
                         `(,(alist-get 'name (alist-get 'album x))
                           ,(alist-get 'name (car (alist-get 'artists x)))
                           ,(alist-get 'display_name (alist-get 'owner x))))
             ", "))

(defun consult-spotify--format-item (x)
  (propertize (format "%s%s"
                      (alist-get 'name x)
                      (if-let ((info (consult-spotify--additional-info x)))
                          (format " (%s)" info)
                        ""))
              'espotify-item x))

(defun consult-spotify--item (cand)
  (get-text-property 0 'espotify-item cand))

(defun consult-spotify--uri (cand)
  (alist-get 'uri (consult-spotify--item cand)))


(defun consult-spotify--consult-lookup (_input cands cand)
  (seq-find (lambda (x) (string= cand x)) cands))


(defun consult-spotify--maybe-play (cand)
  (when-let (uri (when cand (consult-spotify--uri cand)))
    (espotify-play-uri uri)))


;;;###autoload
(defun consult-spotify-album ()
  "Query spotify for an album using consult."
  (interactive)
  (consult-spotify--maybe-play (consult-spotify-by 'album)))


;;;###autoload
(defun consult-spotify-artist ()
  "Query spotify for an artist using consult."
  (interactive)
  (consult-spotify--maybe-play (consult-spotify-by 'artist)))

;;;###autoload
(defun consult-spotify-track ()
  "Query spotify for a track using consult."
  (interactive)
  (consult-spotify--maybe-play (consult-spotify-by 'track)))

;;;###autoload
(defun consult-spotify-playlist ()
  "Query spotify for a track using consult."
  (interactive)
  (consult-spotify--maybe-play (consult-spotify-by 'playlist)))


(defun consult-spotify--annotate (cand)
  "Compute marginalia fields for candidate CAND."
  (when-let (x (consult-spotify--item cand))
    (marginalia--fields
     ((alist-get 'type x "") :face 'marginalia-mode :width 10)
     ((if-let (d (alist-get 'duration_ms x))
          (let ((secs (/ d 1000)))
            (format "%02d:%02d" (/ secs 60) (mod secs 60)))
        ""))
     ((if-let (d (alist-get 'total_tracks x)) (format "%s tracks" d) "")
      :face 'marginalia-size :width 12)
     ((if-let (d (alist-get 'release_date (alist-get 'album x x)))
          (format "%s" d)
        "")
      :face 'marginalia-date :width 10))))

(add-to-list 'marginalia-annotators-heavy
             '(spotify-search-item . consult-spotify--annotate))


(provide 'consult-spotify)
;;; consult-spotify.el ends here
