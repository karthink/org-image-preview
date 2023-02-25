;;; org-image-preview.el --- Preview many link types in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, multimedia

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

;; 

;;; Code:

(require 'org)
(require 'org-persist)

(defvar org-image-preview-file-name-extensions
  (purecopy '("mp4" "mkv" "mov" "avi" "flv")))

(defun org-image-preview--video-file-name-regexp ()
  "Return a regular expression matching image-file filenames."
  (and org-image-preview-file-name-extensions
	      (concat "\\."
		      (regexp-opt
                       (append (mapcar #'upcase
                                       org-image-preview-file-name-extensions)
			       org-image-preview-file-name-extensions)
		       t)
		      "\\'")))

(defun org-image-preview--in-region (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
	(let* ((case-fold-search t)
	       (image-extension-re (image-file-name-regexp))
	       (video-extension-re (org-image-preview--video-file-name-regexp))
               (link-abbrevs (mapcar #'car
				     (append org-link-abbrev-alist-local
					     org-link-abbrev-alist)))
	       ;; Check absolute, relative file names and explicit
	       ;; "file:" links.  Also check link abbreviations since
	       ;; some might expand to "file" links.
	       (file-types-re
		(format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
			(if (not link-abbrevs) ""
			  (concat "\\|" (regexp-opt link-abbrevs))))))
	  (while (re-search-forward file-types-re end t)
	    (let* ((link (org-element-lineage
			  (save-match-data (org-element-context))
			  '(link) t))
                   (linktype (org-element-property :type link))
		   (inner-start (match-beginning 1))
		   (path
		    (cond
		     ;; No link at point; no inline image.
		     ((not link) nil)
		     ;; File link without a description.  Also handle
		     ;; INCLUDE-LINKED here since it should have
		     ;; precedence over the next case.  I.e., if link
		     ;; contains filenames in both the path and the
		     ;; description, prioritize the path only when
		     ;; INCLUDE-LINKED is non-nil.
		     ((or (not (org-element-property :contents-begin link))
			  include-linked)
		      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
			   (org-element-property :path link)))
		     ;; Link with a description.  Check if description
		     ;; is a filename.  Even if Org doesn't have syntax
		     ;; for those -- clickable image -- constructs, fake
		     ;; them, as in `org-export-insert-image-links'.
		     ((not inner-start) nil)
		     (t
		      (org-with-point-at inner-start
			(and (looking-at
			      (if (char-equal ?< (char-after inner-start))
				  org-link-angle-re
				org-link-plain-re))
			     ;; File name must fill the whole
			     ;; description.
			     (= (org-element-property :contents-end link)
				(match-end 0))
			     (match-string 2)))))))
	      (when path
                (let ((file 
                       (cond
                        ((string-match-p image-extension-re path)
                         (if (equal "attachment" linktype)
			     (progn
                               (require 'org-attach)
			       (ignore-errors (org-attach-expand path)))
                           (expand-file-name path)))
                        ((and org-image-preview--video-previewer
                              (string-match-p video-extension-re path))
                         (org-image-preview--cache-path
                          (if (equal "attachment" linktype)
			     (progn
                               (require 'org-attach)
			       (ignore-errors (org-attach-expand path)))
                           (expand-file-name path)))))))
                  (when (and file (file-exists-p file))
		    (org-image-preview--update-overlay file link)))))))))))

(defvar org-image-preview-use-persist nil
  "Persist images using org persist.")

(defun org-image-preview--cache-path (path)
  "TODO"
  (when (file-exists-p path)
    (let* ((mtime (file-attribute-modification-time
                  (file-attributes path)))
           (hash (sha1 (concat path (prin1-to-string mtime)))))
      (if org-image-preview-use-persist
          (or (cadr (org-persist-read org-image-preview--cache-name
                                      (list :key hash)
                                      nil nil
                                      :read-related t))
              (let ((tempfile (org-image-preview--make-video-preview path)))
                (prog1
                    (cadr (org-persist-register `(,org-image-preview--cache-name
                                                  (file ,tempfile))
                                                (list :key hash)
                                                :expiry 7
                                                :write-immediately t))
                  (and tempfile (delete-file tempfile)))))
        (let ((image-file (expand-file-name (concat "org-image-preview-" hash ".png")
                                            temporary-file-directory)))
          (if (file-exists-p image-file)
              image-file
            (when-let ((new-preview (org-image-preview--make-video-preview path)))
              (rename-file new-preview image-file 'overwrite)
              image-file)))))))

(defun org-image-preview--make-video-preview (path)
  (let ((output-file (make-temp-file "org-image-preview-" nil ".png")))
    (when (= 0 
             (call-process
              org-image-preview--video-previewer
              nil (get-buffer-create "*Org Image Preview Output*")
              nil
              "-i" path "-s0" "-m" "-f" "-o" output-file))
      output-file)))

(defconst org-image-preview--cache-name "Org link cached imaged data"
  "Name used for cached image preview data in the org-persist cache.")

(defvar org-image-preview--video-previewer
  (executable-find "ffmpegthumbnailer"))

;; Other options
;; ffmpeg -i input.mp4 -vf  "thumbnail,scale=640:360" -frames:v 1 thumb.png
;; ffmpeg -ss 3 -i input.mp4 -vf "select=gt(scene\,0.4)" -frames:v 5 -vsync vfr -vf fps=fps=1/600 out%02d.jpg
;; ffmpeg -ss 00:00:01.0 -i input.mp4 -frames:v 1 thumb.png

(defun org-image-preview--update-overlay (file link)
  (let ((width (org-display-inline-image--width link))
	(old (get-char-property-and-overlay
	      (org-element-property :begin link)
	      'org-image-overlay)))
    (if (and (car-safe old) refresh)
        (image-flush (overlay-get (cdr old) 'display))
      (let ((image (org--create-inline-image file width)))
	(when image
	  (let ((ov (make-overlay
		     (org-element-property :begin link)
		     (progn
		       (goto-char
			(org-element-property :end link))
		       (skip-chars-backward " \t")
		       (point)))))
            ;; FIXME: See bug#59902.  We cannot rely
            ;; on Emacs to update image if the file
            ;; has changed.
            (image-flush image)
	    (overlay-put ov 'display image)
	    (overlay-put ov 'face 'default)
	    (overlay-put ov 'org-image-overlay t)
	    (overlay-put
	     ov 'modification-hooks
	     (list 'org-display-inline-remove-overlay))
	    (when (boundp 'image-map)
	      (overlay-put ov 'keymap image-map))
	    (push ov org-inline-image-overlays)))))))

;;;###autoload
(defun org-image-preview (&optional arg)
  "Toggle preview of the org link at point."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ((equal arg '(64))
    (if (org--inline-image-overlays (point-min) (point-max))
        (org-remove-inline-images (point-min) (point-max)))
    (message "Cleared image previews from buffer."))
   ((equal arg '(16))
    (org-image-preview--in-region nil 'refresh (point-min) (point-max))
    (message "Created image previews in buffer."))
   ((equal arg '(4))
    (org-remove-inline-images
     (if (use-region-p)
         (region-beginning)
       (if (org-before-first-heading-p) (point-min)
         (save-excursion
           (org-with-limited-levels (org-back-to-heading t) (point)))))
     (if (use-region-p)
         (region-end)
       (org-with-limited-levels (org-entry-end-position)))))
   ((use-region-p)
    (org-image-preview--in-region nil nil (region-beginning) (region-end)))
   ((let* ((datum (org-element-context)))
      (and (eq (org-element-type datum) 'link)
           (let ((beg (org-element-property :begin datum))
                 (end (org-element-property :end datum)))
             (if (org--inline-image-overlays beg end)
                 (org-remove-inline-images beg end)
               (org-image-preview--in-region nil 'refresh beg end))
             t)))
    (message "Toggled image at point."))
   (t
    (let ((beg (if (org-before-first-heading-p) (point-min)
                 (save-excursion
                   (org-with-limited-levels (org-back-to-heading t) (point)))))
          (end (org-with-limited-levels (org-entry-end-position))))
      (message "Created image previews in section.")
      (org-image-preview--in-region nil 'refresh beg end)))))

(provide 'org-image-preview)
;;; org-image-preview.el ends here
