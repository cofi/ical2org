;;; ics2org.el -- convert icalendar to org

;; Copyright (C) 2010, 2011 Michael Markert
;; Author: Michael Markert <markert.michael@googlemail.com>
;; Created: 2010/12/29
;; Version: 0.3.1
;; Keywords: org, calendar

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:
;;
;; Installation:
;;
;;   (require 'ical2org)
;;

;;; Code:

(require 'icalendar)
(require 'org)
(eval-when-compile
  (require 'cl))

(defconst ical2org/version "0.3.1")

(defgroup ical2org nil
  "Convert iCalendar files to orgmode files."
  :link '(url-link :tag "Homepage" "http://github.com/cofi/ical2org")
  :group 'calendar
  :prefix "ical2org/")

(defcustom ical2org/event-format
"* {SUMMARY} at {LOCATION}           :{CATEGORY}:
  {TIME}
  {ORGANIZER}
  {URL}
  {DESCRIPTION}"
  "String used to format an event.
Syntax is {FIELD} valid values for FIELD are: SUMMARY, LOCATION, TIME, URL,
DESCRIPTION, ORGANIZER, CATEGORY.  Namely the slots of the `ical2org/event'
struct (capitalized)."
  :type '(string))

(defcustom ical2org/category-separator ":"
  "String used to separate multiple categories."
  :type '(string))

(defcustom ical2org/completing-read #'ido-completing-read
  "Function used for completing read.
Has to be compatible to `completing-read'."
  :type '(function))

(defun ical2org/convert-file (fname outfile &optional nosave)
  "Convert ical events from file `FNAME' to `OUTFILE' and save when `NOSAVE' is non-nil."
  (interactive "fFile to convert: \nFSave as: \nP")
  (let ((events
         (with-temp-buffer
           (insert-file-contents (expand-file-name fname))
           (ical2org/import-buffer (current-buffer)))))
    (save-current-buffer
      (find-file outfile)
      (goto-char (point-max))
      (newline)
      (dolist (e events)
        (insert (ical2org/format e))
        (newline))
      (unless nosave
        (save-buffer)))))

(defun ical2org/import-to-agenda (fname &optional nosave)
  "Import ical events from file `FNAME' to agenda file (will be prompted).
Saves when `NOSAVE' is non-nil."
  (interactive "fFile to import: \nP")
  (let ((agenda-file (funcall ical2org/completing-read
                              "Agenda file: "
                              (org-agenda-files)))
        (events
         (with-temp-buffer
           (insert-file-contents (expand-file-name fname))
           (ical2org/import-buffer (current-buffer)))))
    (save-current-buffer
      (find-file agenda-file)
      (goto-char (point-max))
      (newline)
      (dolist (e events)
        (insert (ical2org/format e))
        (newline))
      (unless nosave
        (save-buffer)))))

(defun ical2org/buffer-to-buffer (in out)
  "Convert ical events from buffer `IN' to buffer `OUT'."
  (interactive "bIn: \nBOut: ")
  (save-current-buffer
    (let ((events (ical2org/import-buffer in)))
      (set-buffer (generate-new-buffer out))
      (dolist (e events)
        (insert (ical2org/format e))
        (newline))
      (set-window-buffer nil out)
      (org-mode))))

;; private

;; output formatting
(defun ical2org/format (event)
  "Replace formatstrings with slots of `EVENT'."
  (replace-regexp-in-string "{.*?}"
                            (lambda (z)
                              (cdr (assoc z
                                          `(("{SUMMARY}"     . ,(ical2org/event-summary event))
                                            ("{LOCATION}"    . ,(ical2org/event-location event))
                                            ("{TIME}"        . ,(ical2org/event-org-timestr event))
                                            ("{URL}"         . ,(ical2org/event-url event))
                                            ("{DESCRIPTION}" . ,(ical2org/event-description event))
                                            ("{ORGANIZER}"   . ,(ical2org/event-organizer event))
                                            ("{CATEGORY}"    . ,(mapconcat 'identity
                                                                           (ical2org/event-category event)
                                                                           ical2org/category-separator)))
                                          )))
                            ical2org/event-format
                            t t))

(defun ical2org/org-recurrent (event start-decoded start-time end-time)
  "Wrap `icalendar--convert-recurring-to-diary' diary in an org timestamp."
  (format "<%s>"
          (icalendar--convert-recurring-to-diary event start-decoded
                                                 start-time end-time)))

(defun ical2org/org-timestamp (start end)
  "Format `START' and `END' as `org-time-stamp'."
  (let ((start-time (nth 2 start))
        (end-time (nth 2 end))
        (start (car start))
        (end (car end)))
    (if end
        (format "%s--%s" (ical2org/org-time-fmt start start-time)
                (ical2org/org-time-fmt end end-time))
      (if start
          (ical2org/org-time-fmt start start-time)))))

(defun ical2org/org-time-fmt (time &optional with-hm)
  "Format `TIME' as `org-time-stamp', if `WITH-HM' is non-nil included hh:mm.
`TIME' is an decoded time as returned from `decode-time'."
  (let ((fmt (if with-hm
                 (cdr org-time-stamp-formats)
               (car org-time-stamp-formats)))
        (encoded-time (apply 'encode-time time)))
    (format-time-string fmt encoded-time)))

;; entry processing

(defstruct ical2org/event
  (summary "")
  (location "")
  (org-timestr "")
  (url "")
  (description "")
  (organizer "")
  (category '()))

(defun ics2org/datetime (property event zone-map)
  "Return datetime values for `PROPERTY' of `EVENT' with `ZONE-MAP'.
Return a triple of (decoded isodate time).
Where `decoded' is a decoded datetime,
      `isodate' a date as yy mm dd string,
      `time' a time as hh:mm string."
  (let* ((dt (icalendar--get-event-property event property))
         (zone (icalendar--find-time-zone
                (icalendar--get-event-property-attributes event property) zone-map))
         (decoded (icalendar--decode-isodatetime dt nil zone-map)))
    (list decoded
          (icalendar--datetime-to-iso-date decoded)
          (ignore-errors
            (icalendar--datetime-to-colontime decoded)))))

(defun ical2org/get-property (event property &optional default clean)
  "Return `PROPERTY' of `EVENT' or `DEFAULT'."
  (let ((prop (or (icalendar--get-event-property event property)
                 default)))
    (if clean
        (icalendar--convert-string-for-import prop)
      prop)))

(defun ical2org/get-org-timestr (event zone-map)
  "Return org-timestring for `EVENT' with `ZONE-MAP'."
  (let* ((start (ics2org/datetime 'DTSTART event zone-map))
         (start-day (nth 1 start))
         (start-time (nth 2 start))
         (end (ics2org/datetime 'DTEND event zone-map))
         (end-day (or (nth 1 end) start-day))
         (end-time (or (nth 2 end) start-time))
         (rrule (icalendar--get-event-property event 'RRULE))
         (rdate (icalendar--get-event-property event 'RDATE))
         (duration (icalendar--get-event-property event 'DURATION)))
    (when duration
      (let ((new-end (icalendar--add-decoded-times
                      (car start)
                      (icalendar--decode-isoduration duration))))
        (setq end-day (icalendar--datetime-to-iso-date new-end))
        (setq end-time (icalendar--datetime-to-colontime new-end))
        (setq end (list new-end end-day end-time))))

    (cond
     (rrule (ical2org/org-recurrent event (car start) start-time end-time))
     (t (ical2org/org-timestamp start end)))))

(defun ical2org/extract-event (ical-event zone-map)
  "Extracts `ical2org/event' from `ICAL-EVENT' using the timezone map `ZONE-MAP'."
  (let ((summary (ical2org/get-property ical-event 'SUMMARY "" t))
        (location (ical2org/get-property ical-event 'LOCATION "" t))
        (org-timestr (ical2org/get-org-timestr ical-event zone-map))
        (url (ical2org/get-property ical-event 'URL ""))
        (description (ical2org/get-property ical-event 'DESCRIPTION "" t))
        (organizer (ical2org/get-property ical-event 'ORGANIZER "" t))
        (category (split-string (ical2org/get-property ical-event 'CATEGORIES "" t)
                                "," t)))
    (make-ical2org/event :summary summary
                         :location location
                         :org-timestr org-timestr
                         :url url
                         :description description
                         :organizer organizer
                         :category category)))

(defun ical2org/import-elements (ical-elements)
  "Collects events from `ICAL-ELEMENTS' into a list of `ical2org/event's."
  (let ((events (icalendar--all-events ical-elements))
        (zone-map (icalendar--convert-all-timezones ical-elements)))
    (loop for event in events
          collect (ical2org/extract-event event zone-map))))

(defun ical2org/import-buffer (buffer)
  "Return all events in icalendar `BUFFER' as `ical2org/event's."
  (save-current-buffer
    (set-buffer (icalendar--get-unfolded-buffer buffer))
    (goto-char (point-min))
    (if (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (progn
          (beginning-of-line)
          (ical2org/import-elements (icalendar--read-element nil nil)))
      (message "Buffer does not contain icalendar contents!"))))

(provide 'ical2org)

;;; ical2org.el ends here
