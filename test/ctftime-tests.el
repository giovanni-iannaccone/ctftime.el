;;; ctftime-tests.el --- Tests for ctftime.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'url-http)
(require 'tabulated-list)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'ctftime)

(defconst ctftime-test--event-online
  '((id . 1)
    (title . "PwnFest")
    (description . "A pwn and web CTF")
    (location . "Online")
    (format . "Jeopardy")
    (weight . 10)
    (start . "2030-01-10T10:00:00+00:00")
    (finish . "2030-01-10T14:00:00+00:00")
    (ctftime_url . "https://ctftime.org/event/1/")
    (url . "https://example.com/pwnfest")))

(defconst ctftime-test--event-onsite
  '((id . 2)
    (title . "CryptoCamp")
    (description . "Cryptography challenge")
    (location . "Milan, Italy")
    (format . "Jeopardy")
    (weight . 20)
    (start . "2030-01-11T10:00:00+00:00")
    (finish . "2030-01-11T18:00:00+00:00")
    (ctftime_url . "https://ctftime.org/event/2/")
    (url . "https://example.com/cryptocamp")))

(defconst ctftime-test--event-wargame
  '((id . 3)
    (title . "Web Wargame")
    (description . "Web exploitation")
    (location . "")
    (format . "Attack-Defense")
    (weight . 5)
    (start . "2030-01-12T10:00:00+00:00")
    (finish . "2030-01-13T10:00:00+00:00")
    (ctftime_url . "https://ctftime.org/event/3/")))

(defmacro ctftime-test--with-state (&rest body)
  "Execute BODY with CTFtime global state isolated."
  (declare (indent 0) (debug t))
  `(let ((ctftime--cache nil)
         (ctftime--cache-time nil)
         (ctftime--request-in-progress nil)
         (ctftime--request-buffers nil))
     ,@body))

(ert-deftest ctftime-test-number ()
  (should (= 42 (ctftime--number 42)))
  (should (= 42 (ctftime--number "42")))
  (should (= 3.14 (ctftime--number "3.14")))
  (should (= 0 (ctftime--number nil)))
  (should (= 0 (ctftime--number '(foo bar)))))

(ert-deftest ctftime-test-time ()
  (let ((numeric (ctftime--time 1234567890))
        (string (ctftime--time "1234567890"))
        (date (ctftime--time "2030-01-01T12:00:00+00:00")))
    (should numeric)
    (should string)
    (should date)
    (should (= (float-time numeric)
               (float-time string))))
  (should-not (ctftime--time "not-a-date"))
  (should-not (ctftime--time nil)))

(ert-deftest ctftime-test-timestamp ()
  (should (> (ctftime--timestamp "2030-01-01T00:00:00+00:00") 0))
  (should (= 0 (ctftime--timestamp nil)))
  (should (= 0 (ctftime--timestamp "invalid"))))

(ert-deftest ctftime-test-string ()
  (should (equal "hello" (ctftime--string "hello")))
  (should (equal "fallback" (ctftime--string "" "fallback")))
  (should (equal "fallback" (ctftime--string nil "fallback")))
  (should (equal "-" (ctftime--string nil)))
  (should (equal "-" (ctftime--string 42))))

(ert-deftest ctftime-test-event-accessors ()
  (let ((event ctftime-test--event-online))
    (should (equal "PwnFest" (ctftime--title event)))
    (should (equal "Online" (ctftime--location event)))
    (should (equal "Jeopardy" (ctftime--format event)))
    (should (equal "10" (ctftime--weight event)))
    (should (equal "https://ctftime.org/event/1/"
                   (ctftime--url event)))
    (should (ctftime--online-p event))))

(ert-deftest ctftime-test-event-accessor-fallbacks ()
  (let ((event '((id . 4))))
    (should (equal "Unknown" (ctftime--title event)))
    (should (equal "Online" (ctftime--location event)))
    (should (equal "-" (ctftime--format event)))
    (should (equal "-" (ctftime--weight event)))
    (should-not (ctftime--url event))
    (should (ctftime--online-p event))))

(ert-deftest ctftime-test-url-fallback ()
  (let ((event '((url . "https://example.com"))))
    (should (equal "https://example.com"
                   (ctftime--url event)))))

(ert-deftest ctftime-test-duration-from-api ()
  (should (equal "2d 3h"
                 (ctftime--duration
                  '((duration
                     (days . 2)
                     (hours . 3))))))
  (should (equal "2d"
                 (ctftime--duration
                  '((duration (days . 2) (hours . 0))))))
  (should (equal "3h"
                 (ctftime--duration
                  '((duration (days . 0) (hours . 3))))))
  (should (equal "<1h"
                 (ctftime--duration
                  '((duration (days . 0) (hours . 0)))))))

(ert-deftest ctftime-test-duration-from-start-finish ()
  (should (equal "1d 2h"
                 (ctftime--duration
                  `((duration . "invalid")
                    (start . 0)
                    (finish . ,(* 26 3600)))))))

(ert-deftest ctftime-test-format-date ()
  (let ((value "2030-01-01T12:34:00+00:00"))
    (should
     (equal
      (ctftime--format-date value)
      (format-time-string
       "%a %d %b %H:%M"
       (date-to-time value)))))
  (should (equal "?" (ctftime--format-date nil))))

(ert-deftest ctftime-test-org-timestamp ()
  (let ((value "2030-01-01T12:34:00+00:00"))
    (should
     (equal
      (ctftime--org-timestamp value)
      (format-time-string
       "<%Y-%m-%d %a %H:%M>"
       (date-to-time value)))))
  (should-not (ctftime--org-timestamp nil)))

(ert-deftest ctftime-test-online-p ()
  (dolist (location '("Online"
                      "online"
                      "ON-LINE"
                      "on line"
                      "Online / Europe"
                      ""))
    (should
     (ctftime--online-p `((location . ,location)))))

  (dolist (location '("Milan, Italy"
                      "Paris"
                      "Tokyo"))
    (should-not
     (ctftime--online-p `((location . ,location))))))


(ert-deftest ctftime-test-match-term-literal ()
  (should (ctftime--match-term-p "pwn" "PWN web crypto"))
  (should-not (ctftime--match-term-p "pwn" "web crypto"))

  (should (ctftime--match-term-p "." "foo . bar"))
  (should-not (ctftime--match-term-p "." "foo bar")))

(ert-deftest ctftime-test-match-term-regexp ()
  (should (ctftime--match-term-p "/pwn.*/"
                                 "web pwn challenge"))
  (should (ctftime--match-term-p "/crypto[0-9]+/"
                                 "crypto123"))
  (should-not (ctftime--match-term-p "/crypto[0-9]+/"
                                     "cryptoabc")))

(ert-deftest ctftime-test-match-term-negation ()
  (should (ctftime--match-term-p "!crypto"
                                 "pwn web"))
  (should-not (ctftime--match-term-p "!crypto"
                                     "pwn crypto web"))
  (should (ctftime--match-term-p "!/crypto.*/"
                                 "pwn web"))
  (should-not (ctftime--match-term-p "!/crypto.*/"
                                     "pwn crypto")))

(ert-deftest ctftime-test-match-term-invalid-regexp ()
  (should-not
   (ctftime--match-term-p "/[/"
                          "anything")))

(ert-deftest ctftime-test-text-match-empty ()
  (let ((ctftime--filter ""))
    (should (ctftime--text-match-p ctftime-test--event-online))))

(ert-deftest ctftime-test-text-match-or ()
  (let ((ctftime--filter "pwn crypto"))
    (should (ctftime--text-match-p ctftime-test--event-online)))

  (let ((ctftime--filter "crypto"))
    (should-not (ctftime--text-match-p ctftime-test--event-online))))

(ert-deftest ctftime-test-text-match-and ()
  (let ((ctftime--filter "pwn & web"))
    (should (ctftime--text-match-p ctftime-test--event-online)))

  (let ((ctftime--filter "pwn & crypto"))
    (should-not (ctftime--text-match-p ctftime-test--event-online))))

(ert-deftest ctftime-test-text-match-negation ()
  (let ((ctftime--filter "pwn & !crypto"))
    (should (ctftime--text-match-p ctftime-test--event-online)))

  (let ((ctftime--filter "pwn & !web"))
    (should-not (ctftime--text-match-p ctftime-test--event-online))))

(ert-deftest ctftime-test-text-match-case-insensitive ()
  (let ((ctftime--filter "PWN"))
    (should (ctftime--text-match-p ctftime-test--event-online))))

(ert-deftest ctftime-test-text-match-all-searchable-fields ()
  (let ((ctftime--filter "milan"))
    (should (ctftime--text-match-p ctftime-test--event-onsite)))

  (let ((ctftime--filter "jeopardy"))
    (should (ctftime--text-match-p ctftime-test--event-online)))

  (let ((ctftime--filter "pwn"))
    (should (ctftime--text-match-p ctftime-test--event-online))))

(ert-deftest ctftime-test-format-match ()
  (let ((ctftime--format-filter ""))
    (should (ctftime--format-match-p
             ctftime-test--event-online)))

  (let ((ctftime--format-filter "jeopardy"))
    (should (ctftime--format-match-p
             ctftime-test--event-online)))

  (let ((ctftime--format-filter "Attack-Defense"))
    (should (ctftime--format-match-p
             ctftime-test--event-wargame)))

  (let ((ctftime--format-filter "King of the Hill"))
    (should-not (ctftime--format-match-p
                 ctftime-test--event-online))))

(ert-deftest ctftime-test-online-filter ()
  (let ((ctftime--online-only nil))
    (should (ctftime--online-match-p
             ctftime-test--event-online))
    (should (ctftime--online-match-p
             ctftime-test--event-onsite)))

  (let ((ctftime--online-only t))
    (should (ctftime--online-match-p
             ctftime-test--event-online))
    (should-not (ctftime--online-match-p
                 ctftime-test--event-onsite))))

(ert-deftest ctftime-test-event-matches-all-filters ()
  (let ((ctftime--filter "pwn")
        (ctftime--format-filter "Jeopardy")
        (ctftime--online-only t))
    (should
     (ctftime--event-matches-p
      ctftime-test--event-online))

    (should-not
     (ctftime--event-matches-p
      ctftime-test--event-onsite))))

(ert-deftest ctftime-test-filtered-events ()
  (let ((ctftime--cache
         (list ctftime-test--event-wargame
               ctftime-test--event-online
               ctftime-test--event-onsite))
        (ctftime--filter "")
        (ctftime--format-filter "")
        (ctftime--online-only nil))
    (let ((events (ctftime--filtered-events)))
      (should (equal '(1 2 3)
                     (mapcar (lambda (event)
                               (alist-get 'id event))
                             events))))))

(ert-deftest ctftime-test-filtered-events-with-filters ()
  (let ((ctftime--cache
         (list ctftime-test--event-wargame
               ctftime-test--event-online
               ctftime-test--event-onsite))
        (ctftime--filter "pwn")
        (ctftime--format-filter "Jeopardy")
        (ctftime--online-only t))
    (should (equal '(1)
                   (mapcar (lambda (event)
                             (alist-get 'id event))
                           (ctftime--filtered-events))))))

(ert-deftest ctftime-test-filter-status ()
  (let ((ctftime--filter nil)
        (ctftime--format-filter nil)
        (ctftime--online-only nil))
    (should (equal "[all]"
                   (ctftime--filter-status))))

  (let ((ctftime--filter "pwn")
        (ctftime--format-filter "Jeopardy")
        (ctftime--online-only t))
    (should
     (equal "[search:pwn format:Jeopardy online]"
            (ctftime--filter-status)))))

(ert-deftest ctftime-test-cache-invalid-when-empty ()
  (let ((ctftime--cache nil)
        (ctftime--cache-time nil))
    (should-not (ctftime--cache-valid-p))))

(ert-deftest ctftime-test-cache-valid ()
  (let ((ctftime--cache '(foo))
        (ctftime--cache-time (current-time))
        (ctftime-cache-duration 3600))
    (should (ctftime--cache-valid-p))))

(ert-deftest ctftime-test-cache-expired ()
  (let ((ctftime--cache '(foo))
        (ctftime--cache-time
         (time-subtract (current-time)
                        (seconds-to-time 7200)))
        (ctftime-cache-duration 3600))
    (should-not (ctftime--cache-valid-p))))

(ert-deftest ctftime-test-events-returns-cache ()
  (let ((ctftime--cache '(foo bar)))
    (should (equal '(foo bar)
                   (ctftime--events)))))

(ert-deftest ctftime-test-api-url ()
  (let ((ctftime-days 30)
        (ctftime-limit 50))
    (let ((url (ctftime--api-url)))
      (should (string-prefix-p
               "https://ctftime.org/api/v1/events/"
               url))
      (should (string-match-p
               "limit=50"
               url))
      (should (string-match-p
               "start=[0-9]+"
               url))
      (should (string-match-p
               "finish=[0-9]+"
               url)))))

(ert-deftest ctftime-test-parse-response-success ()
  (with-temp-buffer
    (insert
     "HTTP/1.1 200 OK\r\n"
     "Content-Type: application/json\r\n"
     "\r\n"
     "[{\"id\":1,\"title\":\"PwnFest\"}]")
    (setq-local url-http-response-status 200)
    (let ((result (ctftime--parse-response)))
      (should (listp result))
      (should (= 1 (alist-get 'id (car result))))
      (should (equal "PwnFest"
                     (alist-get 'title (car result)))))))

(ert-deftest ctftime-test-parse-response-http-error ()
  (with-temp-buffer
    (insert
     "HTTP/1.1 500 Internal Server Error\r\n"
     "\r\n"
     "{}")
    (setq-local url-http-response-status 500)
    (should-error
     (ctftime--parse-response)
     :type 'error)))

(ert-deftest ctftime-test-parse-response-missing-status ()
  (with-temp-buffer
    (insert
     "HTTP/1.1 200 OK\r\n"
     "\r\n"
     "[]")
    (setq-local url-http-response-status nil)
    (should-error
     (ctftime--parse-response)
     :type 'error)))

(ert-deftest ctftime-test-parse-response-invalid-http ()
  (with-temp-buffer
    (insert "[{\"id\":1}]")
    (setq-local url-http-response-status 200)
    (should-error
     (ctftime--parse-response)
     :type 'error)))

(ert-deftest ctftime-test-request-async-does-not-start-twice ()
  (ctftime-test--with-state
    (let (calls)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest args)
                   (push args calls))))
        (should (ctftime--request-async))
        (should-not (ctftime--request-async))
        (should (= 1 (length calls)))
        (should ctftime--request-in-progress)))))

(ert-deftest ctftime-test-request-async-resets-state-on-error ()
  (ctftime-test--with-state
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _args)
                 (error "network unavailable"))))
      (should-not (ctftime--request-async))
      (should-not ctftime--request-in-progress))))

(ert-deftest ctftime-test-selection-marker ()
  (let ((ctftime--selected-events '(1)))
    (should (equal "✓ "
                   (ctftime--selection-marker
                    ctftime-test--event-online)))
    (should (equal "  "
                   (ctftime--selection-marker
                    ctftime-test--event-onsite)))))

(ert-deftest ctftime-test-event-selected-p ()
  (let ((ctftime--selected-events '(1 3)))
    (should (ctftime--event-selected-p
             ctftime-test--event-online))
    (should-not (ctftime--event-selected-p
                 ctftime-test--event-onsite))))

(ert-deftest ctftime-test-selected-events ()
  (let ((ctftime--events
         (list ctftime-test--event-online
               ctftime-test--event-onsite
               ctftime-test--event-wargame))
        (ctftime--selected-events '(1 3)))
    (should
     (equal '(1 3)
            (mapcar (lambda (event)
                      (alist-get 'id event))
                    (ctftime--selected-events))))))

(ert-deftest ctftime-test-org-entry ()
  (let ((entry
         (ctftime--org-entry ctftime-test--event-online)))
    (should (string-prefix-p
             "* TODO PwnFest\n"
             entry))
    (should (string-match-p
             ":CTFTIME_ID: 1"
             entry))
    (should (string-match-p
             ":FORMAT: Jeopardy"
             entry))
    (should (string-match-p
             ":LOCATION: Online"
             entry))
    (should (string-match-p
             ":WEIGHT: 10"
             entry))
    (should (string-match-p
             ":CTFTIME_URL: https://ctftime.org/event/1/"
             entry))
    (should (string-match-p
             ":URL: https://example.com/pwnfest"
             entry))
    (should (string-match-p
             "A pwn and web CTF"
             entry))))

(ert-deftest ctftime-test-org-entry-without-description ()
  (let ((event '((id . 42)
                 (title . "No Description")
                 (format . "Jeopardy")
                 (location . "Online"))))
    (let ((entry (ctftime--org-entry event)))
      (should (string-match-p
               "\\* TODO No Description"
               entry))
      (should-not
       (string-match-p
        "\nDescription\n"
        entry)))))

(ert-deftest ctftime-test-entry ()
  (let ((ctftime--selected-events nil)
        (entry (ctftime--entry ctftime-test--event-online)))
    (should (= 1 (car entry)))
    (should (vectorp (cadr entry)))
    (should (= 6 (length (cadr entry))))
    (should (equal "  PwnFest"
                   (substring-no-properties
                    (aref (cadr entry) 0))))
    (should (equal "Jeopardy"
                   (substring-no-properties
                    (aref (cadr entry) 3))))))

(ert-deftest ctftime-test-event-at-point ()
  (with-temp-buffer
    (ctftime-mode)
    (setq-local ctftime--events
                (list ctftime-test--event-online
                      ctftime-test--event-onsite))
    (setq tabulated-list-entries
          (mapcar #'ctftime--entry ctftime--events))
    (tabulated-list-print t)

    (goto-char (point-min))
    (while (and (not (eobp))
                (not (tabulated-list-get-id)))
      (forward-line 1))

    (should (= 1
               (alist-get 'id
                          (ctftime--event-at-point))))))

(ert-deftest ctftime-test-toggle-online ()
  (with-temp-buffer
    (ctftime-mode)
    (let ((called 0))
      (cl-letf (((symbol-function 'ctftime--render)
                 (lambda (&rest _args)
                   (setq called (1+ called)))))
        (setq ctftime--online-only nil)
        (ctftime-toggle-online)
        (should ctftime--online-only)
        (ctftime-toggle-online)
        (should-not ctftime--online-only)
        (should (= 2 called))))))

(ert-deftest ctftime-test-clear-filter ()
  (with-temp-buffer
    (ctftime-mode)
    (let ((ctftime--filter "pwn")
          (ctftime--format-filter "Jeopardy")
          (ctftime--online-only t)
          (called 0))
      (cl-letf (((symbol-function 'ctftime--render)
                 (lambda (&rest _args)
                   (setq called (1+ called)))))
        (ctftime-clear-filter)
        (should-not ctftime--filter)
        (should-not ctftime--format-filter)
        (should-not ctftime--online-only)
        (should (= 1 called))))))

(ert-deftest ctftime-test-deselect-all ()
  (with-temp-buffer
    (ctftime-mode)
    (let ((ctftime--selected-events '(1 2))
          (called 0))
      (cl-letf (((symbol-function 'ctftime--render)
                 (lambda (&rest _args)
                   (setq called (1+ called)))))
        (ctftime-deselect-all)
        (should-not ctftime--selected-events)
        (should (= 1 called))))))

(ert-deftest ctftime-test-select-all ()
  (with-temp-buffer
    (ctftime-mode)
    (setq ctftime--cache
          (list ctftime-test--event-wargame
                ctftime-test--event-online
                ctftime-test--event-onsite))
    (let (called)
      (cl-letf (((symbol-function 'ctftime--render)
                 (lambda (&rest _args)
                   (setq called t))))
        (ctftime-select-all)
        (should called)
        (should (equal '(1 2 3)
                       (sort (copy-sequence ctftime--selected-events)
                             #'<)))))))

(ert-deftest ctftime-test-open-event ()
  (with-temp-buffer
    (ctftime-mode)
    (setq-local ctftime--events
                (list ctftime-test--event-online))
    (setq tabulated-list-entries
          (list (ctftime--entry ctftime-test--event-online)))
    (tabulated-list-print t)

    (goto-char (point-min))
    (while (and (not (eobp))
                (not (tabulated-list-get-id)))
      (forward-line 1))

    (let (opened)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url)
                   (setq opened url))))
        (ctftime-open-event)
        (should (equal
                 "https://ctftime.org/event/1/"
                 opened))))))

(ert-deftest ctftime-test-open-event-without-url ()
  (with-temp-buffer
    (ctftime-mode)
    (setq-local ctftime--events
                (list '((id . 1)
                        (title . "No URL"))))
    (setq tabulated-list-entries
          (list (ctftime--entry (car ctftime--events))))
    (tabulated-list-print t)

    (goto-char (point-min))
    (while (and (not (eobp))
                (not (tabulated-list-get-id)))
      (forward-line 1))

    (should-error
     (ctftime-open-event)
     :type 'user-error)))

(ert-deftest ctftime-test-insert-detail ()
  (with-temp-buffer
    (ctftime--insert-detail "Format:" "Jeopardy")
    (should (string-match-p
             "Format:.*Jeopardy"
             (buffer-string)))))

(ert-deftest ctftime-test-details-open ()
  (with-temp-buffer
    (ctftime-details-mode)
    (let (opened)
      (setq-local ctftime--details-url
                  "https://ctftime.org/event/1/")
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url)
                   (setq opened url))))
        (ctftime-details-open)
        (should (equal
                 "https://ctftime.org/event/1/"
                 opened))))))

(ert-deftest ctftime-test-details-open-without-url ()
  (with-temp-buffer
    (ctftime-details-mode)
    (setq-local ctftime--details-url nil)
    (should-error
     (ctftime-details-open)
     :type 'user-error)))

(ert-deftest ctftime-test-render-uses-cache ()
  (with-temp-buffer
    (ctftime-mode)
    (setq ctftime--cache
          (list ctftime-test--event-online
                ctftime-test--event-onsite))
    (setq ctftime--cache-time (current-time))
    (setq ctftime-cache-duration 3600)

    (cl-letf (((symbol-function 'ctftime--request-async)
               (lambda () nil)))
      (ctftime--render t))

    (should (= 2 (length ctftime--events)))
    (should (= 2 (length tabulated-list-entries)))))

(ert-deftest ctftime-test-render-refreshes-stale-cache ()
  (with-temp-buffer
    (ctftime-mode)
    (setq ctftime--cache
          (list ctftime-test--event-online))
    (setq ctftime--cache-time
          (time-subtract (current-time)
                         (seconds-to-time 7200)))
    (setq ctftime-cache-duration 3600)

    (let ((called 0))
      (cl-letf (((symbol-function 'ctftime--request-async)
                 (lambda ()
                   (setq called (1+ called)))))
        (ctftime--render t)
        (should (= 1 called))))))

(ert-deftest ctftime-test-render-loading-without-cache ()
  (with-temp-buffer
    (ctftime-mode)
    (setq ctftime--cache nil)
    (let ((called 0))
      (cl-letf (((symbol-function 'ctftime--request-async)
                 (lambda ()
                   (setq called (1+ called)))))
        (ctftime--render t)
        (should (= 1 called))
        (should
         (string-match-p
          "Loading CTFtime events"
          (buffer-string)))))))

(ert-deftest ctftime-test-mode-keybindings ()
  (should (eq (lookup-key ctftime-mode-map (kbd "RET"))
              #'ctftime-open-event))
  (should (eq (lookup-key ctftime-mode-map (kbd "d"))
              #'ctftime-show-details))
  (should (eq (lookup-key ctftime-mode-map (kbd "g"))
              #'ctftime-refresh))
  (should (eq (lookup-key ctftime-mode-map (kbd "/"))
              #'ctftime-filter))
  (should (eq (lookup-key ctftime-mode-map (kbd "o"))
              #'ctftime-toggle-online))
  (should (eq (lookup-key ctftime-mode-map (kbd "t"))
              #'ctftime-filter-format))
  (should (eq (lookup-key ctftime-mode-map (kbd "f"))
              #'ctftime-set-days))
  (should (eq (lookup-key ctftime-mode-map (kbd "c"))
              #'ctftime-clear-filter))
  (should (eq (lookup-key ctftime-mode-map (kbd "SPC"))
              #'ctftime-toggle-selection))
  (should (eq (lookup-key ctftime-mode-map (kbd "S-SPC"))
              #'ctftime-select-all))
  (should (eq (lookup-key ctftime-mode-map (kbd "u"))
              #'ctftime-deselect-all))
  (should (eq (lookup-key ctftime-mode-map (kbd "x"))
              #'ctftime-export-org))
  (should (eq (lookup-key ctftime-mode-map (kbd "q"))
              #'quit-window)))

(ert-deftest ctftime-test-details-mode ()
  (with-temp-buffer
    (ctftime-details-mode)
    (should (derived-mode-p 'ctftime-details-mode))
    (should (eq (lookup-key ctftime-details-mode-map (kbd "RET"))
                #'ctftime-details-open))
    (should (eq (lookup-key ctftime-details-mode-map (kbd "q"))
                #'quit-window))))

(ert-deftest ctftime-test-refresh ()
  (with-temp-buffer
    (ctftime-mode)
    (setq ctftime--cache '(old))
    (setq ctftime--cache-time (current-time))

    (let (requested)
      (cl-letf (((symbol-function 'ctftime--request-async)
                 (lambda ()
                   (setq requested t))))
        (ctftime-refresh)

        (should requested)
        (should-not ctftime--cache)
        (should-not ctftime--cache-time)
        (should
         (string-match-p
          "Refreshing CTFtime events"
          (buffer-string)))))))

(ert-deftest ctftime-test-filter-format-requires-cache ()
  (with-temp-buffer
    (ctftime-mode)
    (setq ctftime--cache nil)
    (should-error
     (ctftime-filter-format)
     :type 'user-error)))

(ert-deftest ctftime-test-filter-format ()
  (with-temp-buffer
    (ctftime-mode)
    (setq ctftime--cache
          (list ctftime-test--event-online
                ctftime-test--event-onsite))
    (let (rendered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   "Jeopardy"))
                ((symbol-function 'ctftime--render)
                 (lambda (&rest _args)
                   (setq rendered t))))
        (ctftime-filter-format)
        (should (equal "Jeopardy"
                       ctftime--format-filter))
        (should rendered)))))

(ert-deftest ctftime-test-set-days ()
  (with-temp-buffer
    (ctftime-mode)
    (let (refreshed)
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _args)
                   42))
                ((symbol-function 'ctftime-refresh)
                 (lambda ()
                   (setq refreshed t))))
        (let ((ctftime-days 30))
          (ctftime-set-days)
          (should (= 42 ctftime-days))
          (should refreshed))))))

(ert-deftest ctftime-test-request-callback-success ()
  (let ((response-buffer
         (generate-new-buffer " *ctftime-test-response*"))
        (display-buffer
         (generate-new-buffer " *ctftime-test-display*"))
        (ctftime--request-in-progress t)
        rendered)

    (unwind-protect
        (progn
          (with-current-buffer display-buffer
            (ctftime-mode))

          (setq ctftime--request-buffers
                (list display-buffer))

          (with-current-buffer response-buffer
            (insert
             "HTTP/1.1 200 OK\r\n"
             "\r\n"
             "[{\"id\":1,\"title\":\"PwnFest\"}]")
            (setq-local url-http-response-status 200))

          (cl-letf (((symbol-function 'ctftime--render)
                     (lambda (&rest _args)
                       (setq rendered t))))
            (with-current-buffer response-buffer
              (ctftime--request-callback nil)))

          (should rendered)
          (should-not ctftime--request-in-progress)
          (should (= 1 (length ctftime--cache)))
          (should (= 1
                     (alist-get 'id
                                (car ctftime--cache)))))
      (when (buffer-live-p response-buffer)
        (kill-buffer response-buffer))
      (when (buffer-live-p display-buffer)
        (kill-buffer display-buffer)))))

(ert-deftest ctftime-test-face-for-event-without-start ()
  (should
   (eq 'ctftime-later-face
       (ctftime--face-for-event '((title . "No start"))))))

(ert-deftest ctftime-test-face-for-event-today ()
  (let ((timestamp 1000000000))
    (cl-letf (((symbol-function 'current-time)
               (lambda ()
                 (seconds-to-time timestamp))))
      (should
       (eq 'ctftime-today-face
           (ctftime--face-for-event
            `((start . ,timestamp))))))))

(ert-deftest ctftime-test-main-command ()
  (let ((buffer-name "*CTFtime*"))
    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))

    (cl-letf (((symbol-function 'ctftime--render)
               (lambda (&rest _args) nil))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer)
                 buffer)))
      (ctftime)

      (should (get-buffer buffer-name))
      (with-current-buffer buffer-name
        (should (derived-mode-p 'ctftime-mode))))

    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))))

(provide 'ctftime-test)

;;; ctftime-tests.el ends here
