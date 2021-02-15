;;; counsel-espotify.el - counsel and spotify -  -*- lexical-binding: t; -*-
(require 'espotify)
(require 'consult-spotify)
(require 'ivy)

(defun espotify-counsel--search-by (type filter)
  (let ((current-term ""))
    (lambda (term)
      (when-let (term (espotify-check-term current-term term))
        (espotify-search-all
         (lambda (its)
           (let ((cs (mapcar #'espotify-format-item its)))
             (ivy-update-candidates cs)))
         (setq current-term term)
         type
         filter))
      0)))

(defun espotify-counsel--play-album (candidate)
  "Play album associated with selected item."
  (interactive "s")
  (let ((item (espotify-candidate-metadata candidate)))
    (if-let (album (if (string= "album" (alist-get 'type item ""))
                       item
                     (alist-get 'album item)))
        (espotify-play-uri (alist-get 'uri album))
      (error "No album for %s" (alist-get 'name item)))))

(defun espotify-counsel-search-by (type filter)
  (ivy-read (format "Search %s: " type)
            (espotify-counsel--search-by type filter)
            :dynamic-collection t
            :action `(1 ("a" espotify-counsel--play-album "Play album")
                        ("p" espotify-play-candidate ,(format "Play %s" type)))))

(defun espotify-counsel-album (&optional filter)
  (interactive)
  (espotify-counsel-search-by 'album filter))

(defun espotify-counsel-artist (&optional filter)
  (interactive)
  (espotify-counsel-search-by 'artist filter))

(defun espotify-counsel-track (&optional filter)
  (interactive)
  (espotify-counsel-search-by 'track filter))

(defun espotify-counsel-playlist (&optional filter)
  (interactive)
  (espotify-counsel-search-by 'playlist filter))

(provide 'counsel-espotify)
