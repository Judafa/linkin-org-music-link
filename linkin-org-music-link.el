;;; linkin-org.el --- an emacs workflow with fast, reliable links -*- lexical-binding: t -*-

;; Copyright 2025 Julien Dallot

;; Author: Julien Dallot <judafa@protonmail.com>
;; Maintainer: Julien Dallot <judafa@protonmail.com>
;; URL: https://github.com/Judafa/linkin-org
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (pdf-tools "1.1.0"))

;; This file is not part of GNU Emacs

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License. 

;;; Commentary:
;; linkin-org proposes to access your data with reliable links to place your written notes at the center of your workflow.
;; The links work fast and are easy to create; most importantly, the links are reliable and can robustly support a whole link-based workflow.

(require 'ol)
(require 'dired)
(require 'pdf-tools)


(defun org-mpd-open (string-link)
  "STRING-LINK is a string containing the paths to the song (an mp3 file or so, or a .cue file with a trailing /track<number>) as a Lisp list, each song is a string element of the list then ::,then, a timestamp in format readable by mpd, for instance 1:23:45."
  (let* (
	     (link-parts (split-string string-link "::"))
	     (songs (read (car link-parts)))
         (metadata (when (cadr link-parts)
                     (read (cadr link-parts))))
	     ;; unescape the link
	     ;; (link (unescape-special-characters link))
	     ;; use the read function that parses a string as code
	     ;; (songs (read (car link-parts)))
	     (timestamp
          (if (and (plistp metadata) (= 2 (length link-parts)))
              ;; if the metadata are in the plist format
              (plist-get metadata :timestamp)
            ;; else if the data is just separated by ::
            (cadr link-parts))))
    ;; (message (concat "song:" (prin1-to-string songs)))
    ;; (simple-mpc-call-mpc nil (cons "add" songs))
    (apply #'call-process "mpc" nil nil nil (cons "add" songs))))

;; code that takes a mpd entry list (with file, title, etc) and returns the title
(defun linkin-org-get-mpd-track-title (lst)
  "Return the element after 'Title if present in LST, else the element after 'file."
  (let ((title-pos (cl-position 'Title lst))
        (file-pos (cl-position 'file lst)))
    (cond
     ;; if there is a 'Title elem in the list and if there is a value (a next elem after 'Title)
     ((and title-pos (nth (1+ title-pos) lst))
      (nth (1+ title-pos) lst)) ;; Return the element after 'Title
     ;; else, if there is a 'file elem in the list and if there is a value (a next elem after 'file)
     ((and file-pos (nth (1+ file-pos) lst))
      (let*
	  (
	   (file-path (nth (1+ file-pos) lst))
	   ;; Get the file name from the file path
	   (file-name (file-name-nondirectory file-path))
	   ;; get the file name without the extension
	   (file-name (file-name-sans-extension file-name))
	   ;; shorten the file name if it is too long
	   (max-length 50)
	   (file-name (if (> (length file-name) max-length)
			  ;; If file-name is longer than 15 characters, truncate it
			  (concat (substring file-name 0 max-length) "[___]")
			file-name)))
	  file-name))
     ;; Return nil if neither found
     (t nil))))


;; returns a link in string format towards the mingus entry at point
(defun linkin-org-lien-mpd-mingus ()
  "Return a link in string format towards the mingus entry at point."
  (let* (
	 (list-songs
	  (mapcar
	   (lambda (index)
	     ;; the song normally is the second element
	     (nth 1 (car (mpd-get-playlist-entry mpd-inter-conn index nil t))))
	   mingus-marked-list))
	 ;; remove any nil element
	 (list-songs (remove nil list-songs))
	 ;; reverse the list
	 (list-songs (reverse list-songs)))
    ;; if there are marked songs
    (if list-songs
	(let
	    (
	     ;; get the file name of the first song
	     (title (if list-songs
			(linkin-org-get-mpd-track-title (car
						 (mpd-get-playlist-entry
						  mpd-inter-conn
						  (car (last mingus-marked-list))
						  nil
						  t))))))
	 (format
	  ;; "[[mpd:%s::00:00:00][[music] %s _ 00:00]]"
	  "[[mpd:%s::00:00:00][[music] %s]]"
	  (linkin-org-transform-square-brackets (prin1-to-string list-songs))
	  title))
      ;; else
      (let (
	    (track-path (nth 1 (mingus-get-details)))
	    (title (linkin-org-get-mpd-track-title (mingus-get-details))))
	;; (format "[[mpd:(\"%s\")::00:00:00][[music] %s _ 00:00]]" track-path title)
	(format "[[mpd:(\"%s\")::(:timestamp \"00:00:00\")][[music] %s]]" (linkin-org-transform-square-brackets track-path) title)))))


(defun linkin-org-link-mpd-simple-mpc ()
  "Return a link in string form to the current 'simple-mpc' entry."
  (let*
      (
       (track-path
	(simple-mpc-query-get-%file%-for-result
	 (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	
       ;; remove the folder part and the extension
       (title
	    (file-name-nondirectory (file-name-sans-extension track-path))))
    (format
     "[[mpd:(\"%s\")::00:00:00][[music] %s]]"
     track-path
     title)))


;;;; add the link type
(let ((inhibit-message t)) ;; dont print messages while loading the package
  (org-add-link-type "mpd" 'org-mpd-open nil))


(provide 'linkin-org-music-link)
