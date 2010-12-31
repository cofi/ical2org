;;; ics2org.el -- convert icalendar to org

;; Copyright (C) 2010 by Michael Markert 
;; Author: Michael Markert <markert.michael@googlemail.com>
;; Created: 2010/12/29
;; Time-stamp: <2010-12-30 08:23:32 cofi>
;; Version: 0.1

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
(eval-when-compile
  (require 'cl))

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
          (newline)))))

;; private

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
                                            ("{CATEGORY}"    . ,(ical2org/event-category event)))
                                          )))
                            ical2org/event-format
                            t t))

(defstruct ical2org/event
  (summary "")
  (location "")
  (org-timestr "")
  (url "")
  (description "")
  (organizer "")
  (category ""))

(defun ics2org/decode-dt (property event zone-map)
  "Return decoded daytime `PROPERTY' of `EVENT' with `ZONE-MAP' as pair `(isoday . time)'."
  (let* ((dt (icalendar--get-event-property event property))
         (zone (icalendar--find-time-zone
                (icalendar--get-event-property-attributes event property) zone-map))
         (decoded (icalendar--decode-isodatetime dt nil zone-map))
         (time-missing (not (and decoded
                             (string= (cadr (icalendar--get-event-property-attributes
                                             event property))
                                      "DATE")))))
    (when decoded
      (cons
       (icalendar--datetime-to-iso-date decoded)
       (if time-missing
           nil
         (icalendar--datetime-to-colontime decoded))))))

(defun ical2org/get-org-timestr (event zone-map)
  "Return org-timestring for `EVENT' with `ZONE-MAP'."
  (let* ((date+time (ics2org/decode-dt 'DTSTART event zone-map))
         (start-day (car date+time))
         (start-time (cdr date+time))
         (date+time (ics2org/decode-dt 'DTEND event zone-map))
         (end-day (if date+time
                      (car date+time)
                    start-day))
         (end-time (if date+time
                      (cdr date+time)
                    start-time))
         ;; TODO: Cover recurrences
         (rrule (icalendar--get-event-property event 'RRULE))
         (rdate (icalendar--get-event-property event 'RDATE))
         (duration (icalendar--get-event-property event 'DURATION)))

    (if (string= start-day end-day)
        (if (string= start-time end-time)
            (format "<%s %s>" start-day start-time)
          (format "<%s %s-%s>" start-day start-time end-time))
      (format "<%s %s>--<%s %s>" start-day start-time end-day end-time))))

(defun ical2org/org-timestamp (start end)
  "Format `START' and `END' as org-time-stamp."
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
  "Format `TIME' as org-time-stamp, if `WITH-HM' is non-nil included hh:mm.
`TIME' is an decoded time as returned from `decode-time'."
  (let ((fmt (if with-hm
                 (cdr org-time-stamp-formats)
               (car org-time-stamp-formats)))
        (encoded-time (apply 'encode-time time)))
    (format-time-string fmt encoded-time)))

(defun ical2org/extract-event (ical-event zone-map)
  "Extracts `ical2org/event' from `ICAL-EVENT' using the timezone map `ZONE-MAP'."
  (let ((summary (ical2org/get-property ical-event 'SUMMARY "" t))
        (location (ical2org/get-property ical-event 'LOCATION "" t))
        (org-timestr (ical2org/get-org-timestr ical-event zone-map))
        (url (ical2org/get-property ical-event 'URL ""))
        (description (ical2org/get-property ical-event 'DESCRIPTION "" t))
        (organizer (ical2org/get-property ical-event 'ORGANIZER "" t))
        (category (replace-regexp-in-string "," ":"
                     (ical2org/get-property ical-event 'CATEGORIES "" t))))
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

(defun ical2org/get-property (event property &optional default clean)
  "Return `PROPERTY' of `EVENT' or `DEFAULT'."
  (let ((prop (or (icalendar--get-event-property event property)
                 default)))
    (if clean
        (icalendar--convert-string-for-import prop)
      prop)))

(provide 'ical2org)

;;; ical2org.el ends here
