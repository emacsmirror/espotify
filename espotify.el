;;; espotify.el --- Spotify access library  -*- lexical-binding: t; -*-

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Maintainer: Jose A Ortega Ruiz
;; Keywords: media
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://codeberg.org/jao/espotify
;; Package-Requires: ((emacs "26.1"))

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

;; This package provides generic utilities to access Spotify and use
;; its query APIs, as well as controlling local players via their
;; dbus interface.

;;; Code:

(defgroup espotify nil
  "Access to Spotify API and clients"
  :group 'multimedia)

(defcustom espotify-service-name "mopidy"
  "Name of the DBUS service used by the client we talk to.

The official Spotify client uses `spotify', but one can also use
alternative clients such as mopidy or spotifyd."
  :type 'string)

(defcustom espotify-use-system-bus-p t
  "Whether to access the spotify client using the system DBUS."
  :type 'boolean)


(defvar espotify-spotify-api-url "https://api.spotify.com/v1")

(defvar espotify-spotify-api-authentication-url
  "https://accounts.spotify.com/api/token")

(defvar espotify-client-id nil "Spotify application client ID.")

(defvar espotify-client-secret nil "Spotify application client secret.")

(defun espotify--basic-auth-credentials ()
  (let ((credential (concat espotify-client-id ":" espotify-client-secret)))
    (concat "Basic " (base64-encode-string credential t))))

(defvar url-http-end-of-headers)

(defun espotify--with-auth-token (callback)
  (let ((url-request-method "POST")
        (url-request-data "&grant_type=client_credentials")
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Authorization" . ,(espotify--basic-auth-credentials)))))
     (url-retrieve espotify-spotify-api-authentication-url
                   (lambda (_status)
                     (goto-char url-http-end-of-headers)
                     (funcall callback
                              (alist-get 'access_token (json-read)))))))

(defun espotify--make-search-url (term types &optional filter)
  (when (null types)
    (error "Must supply a non-empty list of types to search for"))
  (let ((term (url-encode-url term)))
    (format "%s/search?q=%s&type=%s&limit=50"
            espotify-spotify-api-url
            (if filter (format "%s:%s" filter term) term)
            (mapconcat #'symbol-name types ","))))

(defun espotify--with-query-results (token url callback)
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " token)))))
    (url-retrieve url
                  (lambda (_status)
                    (goto-char url-http-end-of-headers)
                    (funcall callback
                             (let ((json-array-type 'list))
                               (thread-first
                                   (buffer-substring (point) (point-max))
                                 (decode-coding-string 'utf-8)
                                 (json-read-from-string))))))))

(defun espotify-get (callback url)
  (espotify--with-auth-token
     (lambda (token)
       (espotify--with-query-results token url callback))))

(defun espotify-search (callback term types &optional filter)
  (espotify-get callback (espotify--make-search-url term types filter)))

(defun espotify--type-items (res type)
  (alist-get 'items (alist-get (intern (format "%ss" type)) res)))

(defun espotify-search* (callback term types &optional filter)
  (let* ((types (if (listp types) types (list types)))
         (cb (lambda (res)
               (let ((its (mapcar (lambda (tp)
                                    (espotify--type-items res tp))
                                  types)))
                 (apply callback its)))))
    (espotify-search cb term types filter)))

(defun espotify-search-all (callback term &optional types filter)
  (let ((types (or types '(album track artist playlist))))
    (espotify-search* (lambda (&rest items)
                        (funcall callback (apply 'append items)))
                      term
                      types
                      filter)))

(defun espotify-call-spotify-via-dbus (method &rest args)
  "Tell Spotify to execute METHOD with ARGS through DBUS."
  (apply #'dbus-call-method `(,(if espotify-use-system-bus-p :system :session)
                              ,(format "org.mpris.MediaPlayer2.%s"
                                       espotify-service-name)
                              "/org/mpris/MediaPlayer2"
                              "org.mpris.MediaPlayer2.Player"
                              ,method
                              ,@args)))

(defun espotify-play-uri (uri)
  (espotify-call-spotify-via-dbus "OpenUri" uri))

;;;###autoload
(defun espotify-play-pause ()
  (interactive)
  (espotify-call-spotify-via-dbus "PlayPause"))

;;;###autoload
(defun espotify-next ()
  (interactive)
  (espotify-call-spotify-via-dbus "Next"))

;;;###autoload
(defun espotify-previous ()
  (interactive)
  (espotify-call-spotify-via-dbus "Previous"))

(defvar espotify-search-suffix "="
  "Suffix in the search string launching an actual Web query.")

(defvar espotify-search-threshold 8
  "Threshold to automatically launch an actual Web query.")

(defun espotify-check-term (prev new)
  (when (not (string-blank-p new))
    (cond ((string-suffix-p espotify-search-suffix new)
           (substring new 0 (- (length new) (length espotify-search-suffix))))
          ((>= (string-distance prev new) espotify-search-threshold) new))))

(defun espotify--additional-info (x)
  (mapconcat 'identity
             (seq-filter 'identity
                         `(,(alist-get 'name (alist-get 'album x))
                           ,(alist-get 'name (car (alist-get 'artists x)))
                           ,(alist-get 'display_name (alist-get 'owner x))))
             ", "))

(defun espotify--format-item (x)
  (propertize (format "%s%s"
                      (alist-get 'name x)
                      (if-let ((info (espotify--additional-info x)))
                          (format " (%s)" info)
                        ""))
              'espotify-item x))

(defun espotify--item (cand)
  (get-text-property 0 'espotify-item cand))

(defun espotify--uri (cand)
  (alist-get 'uri (espotify--item cand)))


(defun espotify--maybe-play (cand)
  (when-let (uri (when cand (espotify--uri cand)))
    (espotify-play-uri uri)))


(provide 'espotify)
;;; espotify.el ends here
