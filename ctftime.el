;;; ctftime.el --- CTFtime dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Giovanni Francesco Iannaccone
;;
;; Author: Giovanni Francesco Iannaccone <iannacconegiovanni444@gmail.com>
;; Mainteiner: Giovanni Francesco Iannaccone <iannacconegiovanni444@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ctf tools games
;; URL: https://github.com/giovanni-iannaccone/ctftime.el

;;; Commentary:

;; CTFtime dashboard directly in our favourite editor.
;;
;; Browse upcoming CTF competitions, filter them by format,
;; keywords or online availability, export in ORG mode and
;; open event details directly from Emacs.
;;
;; Keybindings:
;;
;; RET   Open CTFtime event
;; d     Show event details
;; g     Refresh
;; /     Search CTFs
;; o     Toggle online-only filter
;; t     Filter by format
;; f     Change number of days
;; c     Clear filters
;; SPC   Select/deselect event
;; S-SPC Select all visible events
;; u	 Clear event selection
;; x     Export selected events to a new Org buffer
;; q     Quit
;;

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)
(require 'tabulated-list)
(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)

(defgroup ctftime nil
  "CTFtime integration."
  :group 'tools)

(defcustom ctftime-days 30
  "Number of days into the future to retrieve."
  :type 'integer
  :group 'ctftime)

(defcustom ctftime-limit 100
  "Maximum number of events to retrieve."
  :type 'integer
  :group 'ctftime)

(defcustom ctftime-cache-duration 3600
  "Number of seconds to keep the API cache."
  :type 'integer
  :group 'ctftime)

(defface ctftime-today-face
  '((t (:foreground "#ff3333" :weight bold)))
  "Face used for events happening today."
  :group 'ctftime)

(defface ctftime-tomorrow-face
  '((t (:foreground "#ffd633" :weight bold)))
  "Face used for events happening tomorrow."
  :group 'ctftime)

(defface ctftime-week-face
  '((t (:foreground "#33cc66" :weight bold)))
  "Face used for events happening within the next week."
  :group 'ctftime)

(defface ctftime-later-face
  '((t (:inherit default)))
  "Face used for later events."
  :group 'ctftime)

(defconst ctftime-api-url
  "https://ctftime.org/api/v1/events/"
  "CTFtime events API endpoint.")

(defvar ctftime--cache nil
  "Cached CTFtime events.")

(defvar ctftime--cache-time nil
  "Time when the CTFtime cache was updated.")

(defvar ctftime--image-files nil
  "Temporary files containing downloaded CTF logos.")

(defvar ctftime--request-in-progress nil
  "Non-nil while a CTFtime API request is in progress.")

(defvar ctftime--request-buffers nil
  "Buffers waiting for the current CTFtime API request.")

(defvar-local ctftime--events nil
  "Events currently displayed in the current buffer.")

(defvar-local ctftime--filter nil
  "Current free-text filter.")

(defvar-local ctftime--format-filter nil
  "Current event format filter.")

(defvar-local ctftime--online-only nil
  "Whether to show only online events.")

(defvar-local ctftime--details-url nil
  "URL of the event displayed in the details buffer.")

(defvar-local ctftime--selected-events nil
  "IDs of CTFtime events selected in the current buffer.")

(defun ctftime--number (value)
  "Convert VALUE to a number.

Return 0 when VALUE cannot be converted."
  (cond
   ((numberp value) value)
   ((stringp value) (string-to-number value))
   (t 0)))

(defun ctftime--time (value)
  "Convert CTFtime VALUE into an Emacs time value.

VALUE may be a Unix timestamp or a date string."
  (cond
   ((numberp value)
    (seconds-to-time value))

   ((stringp value)
    (if (string-match-p
         "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'"
         value)
        (seconds-to-time (string-to-number value))
      (condition-case nil
          (date-to-time value)
        (error nil))))

   (t nil)))

(defun ctftime--timestamp (value)
  "Return VALUE as a floating-point Unix timestamp.

Return 0 when VALUE is invalid."
  (if-let ((time (ctftime--time value)))
      (float-time time)
    0))

(defun ctftime--string (value &optional fallback)
  "Return VALUE as a non-empty string.

Use FALLBACK when VALUE is nil or an empty string."
  (if (and (stringp value)
           (not (string-empty-p value)))
      value
    (or fallback "-")))

(defun ctftime--format-date (value)
  "Format VALUE as a local date/time."
  (if-let ((time (ctftime--time value)))
      (format-time-string "%a %d %b %H:%M" time)
    "?"))

(defun ctftime--duration (event)
  "Return a human-readable duration for EVENT."
  (let ((duration (alist-get 'duration event)))
    (if (listp duration)
        (let ((days (ctftime--number
                     (alist-get 'days duration)))
              (hours (ctftime--number
                      (alist-get 'hours duration))))
          (cond
           ((and (> days 0) (> hours 0))
            (format "%dd %dh" days hours))
           ((> days 0)
            (format "%dd" days))
           ((> hours 0)
            (format "%dh" hours))
           (t
            "<1h")))

      (let ((start (ctftime--time
                    (alist-get 'start event)))
            (finish (ctftime--time
                     (alist-get 'finish event))))
        (if (and start finish)
            (let* ((seconds (float-time
                             (time-subtract finish start)))
                   (hours (floor (/ seconds 3600)))
                   (days (floor (/ hours 24))))
              (cond
               ((> days 0)
                (format "%dd %dh"
                        days
                        (% hours 24)))
               ((> hours 0)
                (format "%dh" hours))
               (t
                "<1h")))
          "?")))))

(defun ctftime--api-url ()
  "Build the CTFtime API URL."
  (let* ((start (current-time))
         (finish (time-add
                  start
                  (days-to-time ctftime-days))))
    (format "%s?limit=%d&start=%d&finish=%d"
            ctftime-api-url
            ctftime-limit
            (truncate (float-time start))
            (truncate (float-time finish)))))

(defun ctftime--parse-response ()
  "Parse the current CTFtime HTTP response buffer.
Signal an error when the response is invalid."
  (let ((status (and (boundp 'url-http-response-status)
                     url-http-response-status)))
    (unless (and status
                 (>= status 200)
                 (< status 300))
      (error "CTFtime HTTP error: %s"
             (or status "unknown"))))

  (goto-char (point-min))

  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (error "Invalid HTTP response from CTFtime"))

  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read)))


(defun ctftime--request-callback (status)
  "Handle the asynchronous CTFtime API response.
STATUS is the status plist passed by `url-retrieve'."
  (let ((events nil)
        (error-message nil))

    (unwind-protect
        (condition-case err
            (progn
              (when-let ((error-data (plist-get status :error)))
                (signal (car error-data)
                        (cdr error-data)))

              (setq events
                    (ctftime--parse-response)))

          (error
           (setq error-message
                 (error-message-string err))))
      (setq ctftime--request-in-progress nil)
      (when (buffer-live-p (current-buffer))
        (kill-buffer (current-buffer))))

    (if error-message
        (progn 
          (message "CTFtime: request failed: %s"
                   error-message)

          (dolist (buffer ctftime--request-buffers)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (when (derived-mode-p 'ctftime-mode)
                  (ctftime--render t)))))

          (setq ctftime--request-buffers nil))
      (setq ctftime--cache events
            ctftime--cache-time (current-time))

      (let ((buffers ctftime--request-buffers))
        (setq ctftime--request-buffers nil)

        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (derived-mode-p 'ctftime-mode)
                (ctftime--render t))))))

      (message "CTFtime: downloaded %d events"
               (length events)))))

(defun ctftime--request-async ()
  "Retrieve CTFtime events asynchronously.
Return non-nil when a request was started."
  (unless ctftime--request-in-progress
    (setq ctftime--request-in-progress t)

    (condition-case err
        (let ((url-request-extra-headers
               '(("User-Agent" . "Emacs-CTFtime/1.0"))))
          (url-retrieve
           (ctftime--api-url)
           #'ctftime--request-callback
           nil
           t)
          t)

      (error
       (setq ctftime--request-in-progress nil)
       (message "CTFtime: unable to start request: %s"
                (error-message-string err))
       nil))))

(defun ctftime--cache-valid-p ()
  "Return non-nil when the CTFtime cache is still valid."
  (and ctftime--cache
       ctftime--cache-time
       (< (float-time
           (time-subtract (current-time)
                          ctftime--cache-time))
          ctftime-cache-duration)))

(defun ctftime--events ()
  "Return the currently cached CTFtime events.
Return nil when no cache is available yet."
  ctftime--cache)

(defun ctftime--title (event)
  "Return the title of EVENT."
  (ctftime--string
   (alist-get 'title event)
   "Unknown"))

(defun ctftime--location (event)
  "Return the location of EVENT."
  (ctftime--string
   (alist-get 'location event)
   "Online"))

(defun ctftime--format (event)
  "Return the format of EVENT."
  (ctftime--string
   (alist-get 'format event)
   "-"))

(defun ctftime--weight (event)
  "Return the weight of EVENT."
  (let ((weight (alist-get 'weight event)))
    (if weight
        (format "%s" weight)
      "-")))

(defun ctftime--url (event)
  "Return the URL of EVENT."
  (or (alist-get 'ctftime_url event)
      (alist-get 'url event)))

(defun ctftime--online-p (event)
  "Return non-nil when EVENT is an online event."
  (let ((location
         (downcase
          (or (alist-get 'location event)
              ""))))
    (or (string-empty-p location)
        (string-match-p "on[- ]?line" location))))

(defun ctftime--day-start (time)
  "Return the beginning of the local day containing TIME."
  (pcase-let ((`(,_second ,_minute ,_hour ,day ,month ,year)
               (decode-time time)))
    (encode-time 0 0 0 day month year)))

(defun ctftime--days-from-today (time)
  "Return calendar days between TIME and today."
  (let* ((today (ctftime--day-start (current-time)))
         (date (ctftime--day-start time))
         (seconds (float-time
                   (time-subtract date today))))
    (floor (/ seconds 86400))))

(defun ctftime--face-for-event (event)
  "Return the appropriate face for EVENT."
  (if-let ((start (ctftime--time
                   (alist-get 'start event))))
      (let ((days (ctftime--days-from-today start)))
        (cond
         ((= days 0)
          'ctftime-today-face)

         ((= days 1)
          'ctftime-tomorrow-face)

         ((and (> days 1)
               (<= days 7))
          'ctftime-week-face)

         (t
          'ctftime-later-face)))

    'ctftime-later-face))

(defun ctftime--match-term-p (term text)
  "Return non-nil when TERM matches TEXT."
  (let* ((negated (string-prefix-p "!" term))
         (term (if negated
                   (substring term 1)
                 term))
         (regexp (and (>= (length term) 2)
                      (string-prefix-p "/" term)
                      (string-suffix-p "/" term)))
         (pattern (if regexp
                      (substring term 1 -1)
                    (regexp-quote term)))
         match)
    (condition-case nil
        (setq match (string-match-p pattern text))
      (invalid-regexp
       (setq match nil)))
    (if negated
        (not match)
      match)))

(defun ctftime--text-match-p (event)
  "Return non-nil when EVENT matches the free-text filter."
  (if (string-empty-p (or ctftime--filter ""))
      t
    (let ((text
           (downcase
            (format "%s %s %s %s"
                    (ctftime--title event)
                    (ctftime--location event)
                    (ctftime--format event)
                    (or (alist-get 'description event)
                        ""))))
          (query (downcase (string-trim ctftime--filter))))

      (if (string-match-p " & " query)
          (cl-every
           (lambda (term)
             (ctftime--match-term-p term text))
           (split-string query " & " t))

        (cl-some
         (lambda (term)
           (ctftime--match-term-p term text))
         (split-string query "[ \t]+" t))))))

(defun ctftime--format-match-p (event)
  "Return non-nil when EVENT matches the format filter."
  (or (string-empty-p (or ctftime--format-filter ""))
      (string-equal
       (downcase (ctftime--format event))
       (downcase ctftime--format-filter))))

(defun ctftime--online-match-p (event)
  "Return non-nil when EVENT matches the online-only filter."
  (or (not ctftime--online-only)
      (ctftime--online-p event)))

(defun ctftime--event-matches-p (event)
  "Return non-nil when EVENT passes all active filters."
  (and (ctftime--text-match-p event)
       (ctftime--format-match-p event)
       (ctftime--online-match-p event)))

(defun ctftime--filtered-events ()
  "Return events after applying all active filters."
  (sort
   (cl-remove-if-not
    #'ctftime--event-matches-p
    (copy-sequence (ctftime--events)))
   (lambda (a b)
     (< (ctftime--timestamp (alist-get 'start a))
        (ctftime--timestamp (alist-get 'start b))))))

(defvar ctftime--filter-buffer nil)

(defun ctftime-filter ()
  "Set the free-text filter with live updates."
  (interactive)

  (let ((ctftime--filter-buffer (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'after-change-functions
                    #'ctftime--live-filter-update
                    nil
                    t))
      (setq ctftime--filter
            (read-string "Search CTFs: "
                         ctftime--filter)))))

(defun ctftime--live-filter-update (beg end len)
  "Update CTFtime results when the minibuffer contents change."
  (ignore beg end len)
  (let ((filter (minibuffer-contents-no-properties))
        (buffer ctftime--filter-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq ctftime--filter filter)
        (ctftime--render))
      (when-let ((window (get-buffer-window buffer)))
        (force-window-update window)))))

(defun ctftime-filter-format ()
  "Set the event format filter."
  (interactive)

  (unless ctftime--cache
    (user-error "CTFtime events are still loading"))
  (let* ((formats
          (delete-dups
           (mapcar #'ctftime--format
                   ctftime--cache)))
         (choice
          (completing-read
           "Format: "
           formats
           nil
           t
           nil
           nil
           ctftime--format-filter)))
    (setq ctftime--format-filter
          (unless (string-empty-p choice)
            choice))

    (ctftime--render)))

(defun ctftime-toggle-online ()
  "Toggle the online-only filter."
  (interactive)

  (setq ctftime--online-only
        (not ctftime--online-only))

  (ctftime--render))

(defun ctftime-clear-filter ()
  "Clear all active filters."
  (interactive)

  (setq ctftime--filter nil
        ctftime--format-filter nil
        ctftime--online-only nil)

  (ctftime--render))

(defun ctftime--filter-status ()
  "Return a human-readable description of active filters."
  (let (parts)

    (when (not (string-empty-p (or ctftime--filter "")))
      (push (format "search:%s" ctftime--filter)
            parts))

    (when (not (string-empty-p
                (or ctftime--format-filter "")))
      (push (format "format:%s" ctftime--format-filter)
            parts))

    (when ctftime--online-only
      (push "online" parts))

    (if parts
        (format "[%s]"
                (mapconcat #'identity
                           (nreverse parts)
                           " "))
      "[all]")))

(defun ctftime--event-selected-p (event)
  "Return non-nil when EVENT is selected."
  (member (alist-get 'id event)
          ctftime--selected-events))

(defun ctftime--selection-marker (event)
  "Return the selection marker for EVENT."
  (if (ctftime--event-selected-p event)
      "✓ "
    "  "))

(defun ctftime-toggle-selection ()
  "Select or deselect the event at point."
  (interactive)

  (let ((event (ctftime--event-at-point)))
    (unless event
      (user-error "No event at point"))

    (let ((id (alist-get 'id event)))
      (if (member id ctftime--selected-events)
          (setq ctftime--selected-events
                (delete id ctftime--selected-events))
        (push id ctftime--selected-events)))
    (ctftime--render)))

(defun ctftime-select-all ()
  "Select all currently visible CTFtime events."
  (interactive)
  
  (setq ctftime--selected-events
        (mapcar (lambda (event)
                  (alist-get 'id event))
                (ctftime--filtered-events)))
  (ctftime--render)

  (message "CTFtime: selected %d events"
           (length ctftime--selected-events)))

(defun ctftime--entry (event)
  "Create a tabulated-list entry for EVENT."
  (let ((face (ctftime--face-for-event event)))
    (list
     (alist-get 'id event)
     (vector
      (propertize
       (concat
        (ctftime--selection-marker event)
        (ctftime--title event))
       'face face)
      
      (propertize
       (ctftime--format-date
        (alist-get 'start event))
       'face face)

      (propertize
       (ctftime--duration event)
       'face face)

      (propertize
       (ctftime--format event)
       'face face)

      (propertize
       (ctftime--location event)
       'face face)

      (propertize
       (ctftime--weight event)
       'face face)))))

(defun ctftime--event-at-point ()
  "Return the event represented by the current table row."
  (when-let ((id (tabulated-list-get-id)))

    (cl-find-if
     (lambda (event)
       (equal id (alist-get 'id event)))
     ctftime--events)))

(defun ctftime--org-timestamp (value)
  "Convert CTFtime VALUE into an Org timestamp."
  (when-let ((time (ctftime--time value)))
    (format-time-string
     "<%Y-%m-%d %a %H:%M>"
     time)))

(defun ctftime--org-entry (event)
  "Return an Org-mode entry representing EVENT."

  (let* ((title
          (ctftime--title event))

         (id
          (alist-get 'id event))

         (start
          (ctftime--org-timestamp
           (alist-get 'start event)))

         (finish
          (ctftime--org-timestamp
           (alist-get 'finish event)))

         (format
          (ctftime--format event))

         (location
          (ctftime--location event))

         (weight
          (ctftime--weight event))

         (ctftime-url
          (alist-get 'ctftime_url event))

         (official-url
          (alist-get 'url event))

         (description
          (alist-get 'description event)))

    (concat
     "* TODO " title "\n"
     (when (and start finish)
       (format "  %s--%s\n"
               start
               finish))     
     ":PROPERTIES:\n"

     (format
      ":CTFTIME_ID: %s\n"
      id)

     (when format
       (format
        ":FORMAT: %s\n"
        format))

     (when location
       (format
        ":LOCATION: %s\n"
        location))

     (when weight
       (format
        ":WEIGHT: %s\n"
        weight))

     (when ctftime-url
       (format
        ":CTFTIME_URL: %s\n"
        ctftime-url))

     (when official-url
       (format
        ":URL: %s\n"
        official-url))
     ":END:\n"

     (when (and (stringp description)
                (not (string-empty-p description)))
       (concat
        "\n"
        description
        "\n"))
     "\n")))

(defun ctftime--selected-events ()
  "Return the currently selected events."
  (cl-remove-if-not
   #'ctftime--event-selected-p
   ctftime--events))

(defun ctftime-deselect-all ()
  "Deselect all CTFtime events."
  (interactive)

  (setq ctftime--selected-events nil)
  (ctftime--render)
  (message "CTFtime: all events deselected"))

(defun ctftime-export-org ()
  "Export selected CTFtime events to a new Org buffer."
  (interactive)

  (let ((events (ctftime--selected-events)))

    (unless events
      (user-error
       "No events selected; press SPC to select events"))

    (require 'org)
    (let ((buffer (generate-new-buffer
                   (format "*CTFtime Org (%d events)*"
                           (length events)))))

      (with-current-buffer buffer
        (org-mode)
        (insert
         "#+TITLE: CTFtime Events\n"
         "#+STARTUP: overview\n\n")

        (dolist (event events)
          (insert
           (ctftime--org-entry event)))

        (goto-char (point-min))
        (when (re-search-forward
               "^\\* "
               nil
               t)
          (beginning-of-line)))
      (switch-to-buffer buffer)

      (message
       "CTFtime: exported %d event%s to Org"
       (length events)
       (if (= (length events) 1)
           ""
         "s")))))

(defun ctftime-open-event ()
  "Open the CTFtime page of the event at point."
  (interactive)

  (let ((event (ctftime--event-at-point)))
    (unless event
      (user-error "No event at point"))
    (let ((url (ctftime--url event)))
      (unless url
        (user-error "No URL available for this event"))
      (browse-url url))))

(defvar ctftime-details-mode-map
  (let ((map (make-sparse-keymap)))

    (set-keymap-parent map special-mode-map)
    
    (define-key map (kbd "RET")
                #'ctftime-details-open)

    (define-key map (kbd "q")
                #'quit-window)
    map)
  "Keymap for `ctftime-details-mode'.")

(define-derived-mode ctftime-details-mode
  special-mode
  "CTF Details"
  "Major mode for displaying CTFtime event details.")

(defun ctftime-details-open ()
  "Open the current event in a browser."
  (interactive)
  
  (if ctftime--details-url
      (browse-url ctftime--details-url)
    (user-error "No event URL available")))

(defun ctftime--insert-detail (label value)
  "Insert a detail with LABEL and VALUE."
  (insert
   (propertize
    (format "%-12s" label)
    'face 'font-lock-keyword-face)
   (format "%s\n" (or value "-"))))

(defun ctftime--insert-logo-async (url buffer)
  "Insert the CTF logo from URL asynchronously into BUFFER."
  (when (and (stringp url)
             (not (string-empty-p url)))

    (let ((marker (copy-marker (point)))
          (placeholder "[Loading logo...]\n\n"))
      (insert placeholder)

      (condition-case nil
          (url-retrieve
           url
           (lambda (status)
             (let ((image-data nil))
               (unless (plist-get status :error)
                 (goto-char (point-min))

                 (when (re-search-forward "\r?\n\r?\n" nil t)
                   (setq image-data
                         (buffer-substring-no-properties
                          (point)
                          (point-max)))))

               (kill-buffer (current-buffer))
               (when (and image-data
                          (buffer-live-p buffer)
                          (marker-position marker))

                 (with-current-buffer buffer
                   (let ((inhibit-read-only t))
                     (goto-char (marker-position marker))

                     (delete-region
                      (point)
                      (+ (point)
                         (length placeholder)))

                     (condition-case nil
                         (let ((image
                                (create-image
                                 image-data
                                 nil
                                 t
                                 :max-width 500
                                 :max-height 300)))
                           (when image
                             (insert-image image)
                             (insert "\n\n")))
                       (error nil))
                     (set-marker marker nil))))))
           nil
           t)

        (error
         (delete-region
          (marker-position marker)
          (+ (marker-position marker)
             (length placeholder)))
         (set-marker marker nil))))))

(defun ctftime-show-details ()
  "Show details for the event at point."
  (interactive)

  (let ((event (ctftime--event-at-point)))
    (unless event
      (user-error "No event at point"))

    (let ((buffer
           (get-buffer-create "*CTFtime Details*")))

      (with-current-buffer buffer
        (ctftime-details-mode)
        (let ((inhibit-read-only t)
              (description
               (alist-get
                'description
                event)))
          (erase-buffer)
          
          (when-let ((logo (alist-get 'logo event)))
            (ctftime--insert-logo-async logo buffer))
          
          (setq ctftime--details-url
                (ctftime--url event))

          (insert
           (propertize
            (ctftime--title event)
            'face '(:weight bold :height 1.4))
           "\n\n")

          (ctftime--insert-detail
           "Start:"
           (ctftime--format-date
            (alist-get 'start event)))

          (ctftime--insert-detail
           "Finish:"
           (ctftime--format-date
            (alist-get 'finish event)))

          (ctftime--insert-detail
           "Duration:"
           (ctftime--duration event))

          (ctftime--insert-detail
           "Format:"
           (ctftime--format event))

          (ctftime--insert-detail
           "Location:"
           (ctftime--location event))

          (ctftime--insert-detail
           "Weight:"
           (ctftime--weight event))

          (insert "\n")

          (ctftime--insert-detail
           "CTFtime:"
           (alist-get 'ctftime_url
                      event))

          (ctftime--insert-detail
           "Official:"
           (alist-get 'url
                      event))

          (when (and (stringp description)
                     (not (string-empty-p
                           description)))

            (insert
             "\n"
             (propertize
              "Description\n"
              'face
              'font-lock-keyword-face)
             "\n"
             description
             "\n"))
          (goto-char
           (point-min))))
      
      (pop-to-buffer buffer))))

(defun ctftime-refresh ()
  "Clear the cache and asynchronously refresh the current CTFtime buffer."
  (interactive)

  (setq ctftime--cache nil
        ctftime--cache-time nil)
  (unless (memq (current-buffer)
                ctftime--request-buffers)
    (push (current-buffer)
          ctftime--request-buffers))

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     (propertize
      "Refreshing CTFtime events..."
      'face 'font-lock-comment-face)
     "\n"))

  (ctftime--request-async))

(defun ctftime-set-days ()
  "Change the number of days to retrieve."
  (interactive)

  (setq ctftime-days
        (read-number
         "Days ahead: "
         ctftime-days))

  (ctftime-refresh))

(defun ctftime--render (&optional silent)
  "Render the CTFtime table.
Use cached data immediately and refresh stale data asynchronously."
  (let ((current-id (tabulated-list-get-id))
        (cache-available (and ctftime--cache
                              (not (null ctftime--cache))))
        (cache-valid (ctftime--cache-valid-p)))
    (unless cache-available
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (propertize
          "Loading CTFtime events..."
          'face 'font-lock-comment-face)
         "\n"))
      (unless (memq (current-buffer)
                    ctftime--request-buffers)
        (push (current-buffer)
              ctftime--request-buffers))

      (ctftime--request-async))
    (when cache-available
      (let ((events
             (ctftime--filtered-events)))
        (setq ctftime--events events)

        (setq ctftime--selected-events
              (cl-remove-if-not
               (lambda (id)
                 (cl-find-if
                  (lambda (event)
                    (equal id
                           (alist-get 'id event)))
                  ctftime--events))
               ctftime--selected-events))

        (let ((inhibit-read-only t))
          (erase-buffer)

          (setq tabulated-list-entries
                (mapcar #'ctftime--entry
                        events))

          (tabulated-list-print t)
          (when current-id
            (goto-char (point-min))

            (catch 'found
              (while (not (eobp))
                (when (equal
                       current-id
                       (tabulated-list-get-id))
                  (throw 'found t))
                (forward-line 1))))))

      (unless cache-valid
        (ctftime--request-async)))

    (unless silent
      (cond
       ((not cache-available)
        (message "CTFtime: loading events..."))

       ((not cache-valid)
        (message
         "CTFtime: showing cached data; refreshing..."))

       (t
        (message
         "CTFtime: %d events %s | %d days | %d selected"
         (length ctftime--events)
         (ctftime--filter-status)
         ctftime-days
         (length ctftime--selected-events)))))))

(define-derived-mode ctftime-mode
  tabulated-list-mode
  "CTFtime"
  "Major mode for browsing CTFtime events."

  (setq
   tabulated-list-format
   [("Event" 34 t)
    ("Start" 18 t)
    ("Duration" 10 t)
    ("Format" 18 t)
    ("Location" 24 t)
    ("Weight" 8 t)]
   tabulated-list-padding 2)

  (define-key  ctftime-mode-map (kbd "RET")
               #'ctftime-open-event)

  (define-key ctftime-mode-map (kbd "d")
              #'ctftime-show-details)

  (define-key ctftime-mode-map (kbd "g")
              #'ctftime-refresh)

  (define-key ctftime-mode-map (kbd "/")
              #'ctftime-filter)

  (define-key ctftime-mode-map (kbd "o")
              #'ctftime-toggle-online)

  (define-key ctftime-mode-map (kbd "t")
              #'ctftime-filter-format)

  (define-key ctftime-mode-map (kbd "f")
              #'ctftime-set-days)

  (define-key ctftime-mode-map (kbd "c")
              #'ctftime-clear-filter)

  (define-key ctftime-mode-map (kbd "SPC")
              #'ctftime-toggle-selection)

  (define-key ctftime-mode-map (kbd "S-SPC")
              #'ctftime-select-all)

  (define-key ctftime-mode-map (kbd "u")
              #'ctftime-deselect-all)
  
  (define-key ctftime-mode-map (kbd "x")
              #'ctftime-export-org)

  (define-key ctftime-mode-map (kbd "q")
              #'quit-window)

  (tabulated-list-init-header))

(defun ctftime ()
  "Open the CTFtime dashboard."
  (interactive)

  (let ((buffer
         (get-buffer-create "*CTFtime*")))
    (with-current-buffer buffer
      (ctftime-mode)
      (ctftime--render))

    (pop-to-buffer buffer)))

(provide 'ctftime)

;;; ctftime.el ends here
