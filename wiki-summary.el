;;; wiki-summary.el --- Easily view Wikipedia summaries  -*- lexical-binding: t; -*-

;; Copyright (c) 2015-2020 Danny Gratzer <jozefg@cmu.edu>

;; Author: Danny Gratzer
;; URL: https://github.com/jozefg/wiki-summary.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24"))
;; Version: 0.1

;;; Commentary:

;; It's often the case when reading some document in Emacs (be it
;; code text or prose) that I come across a word or phrase that I
;; don't know. In order to simplify my feedback loop when wiki-summary
;; lets me look up something in a couple seconds.
;;
;; To use this package, simply call M-x wiki-summary (or bind it to a key).
;; This will prompt you for an article title to search. For convience,
;; this will default to the word under the point. When you hit enter
;; this will query Wikipedia and if an article is found, bring up the
;; title in a separate window. Spaces will be properly escaped so
;; something like "Haskell (programming language)" will bring up the
;; intended page.
;;
;; I'm not sure exactly what else people would want out of this package.
;; Feature request issues are welcome.

;;; Code:

(require 'url)
(require 'json)
(require 'thingatpt)

(eval-when-compile
  ; This stops the compiler from complaining.
  (defvar url-http-end-of-headers))


(defgroup wiki-summary ()
  "Easily view Wikipedia summaries."
  :prefix 'wiki-summary-
  :group 'convenience)

(defcustom wiki-summary-language-string "en"
  "Language string of requested Wikipedia, e.g. \"en\", \"fr\", \"de\".

For available list of Wikipedias see <https://en.wikipedia.org/wiki/List_of_Wikipedias>"
  :type 'string
  :safe 'stringp)

(defvar wiki-summary--pre-url-format-string
  "https://%s.wikipedia.org/w/api.php?continue=&action=query&titles=")

(defvar wiki-summary--post-url-format-string
  "&prop=extracts&exintro=&explaintext=&format=json&redirects")

;; Define a proper keymap for wiki-summary buffers
(defvar wiki-summary-mode-map
  (let ((map (make-sparse-keymap)))
    ;; 'q' kills the buffer instead of burying it
    (define-key map (kbd "q") (lambda () (interactive) (quit-window t)))
    ;; Copy content to kill ring
    (define-key map (kbd "y") 'wiki-summary-copy-to-kill-ring)
    ;; Insert content into original buffer and quit
    (define-key map (kbd "i") 'wiki-summary-insert-to-original-buffer)
    map)
  "Keymap for wiki-summary-mode.")

;; Define a minor mode for wiki-summary buffers
(define-minor-mode wiki-summary-mode
  "Minor mode for wiki-summary buffers.
\\{wiki-summary-mode-map}"
  :lighter " WikiSum"
  :keymap wiki-summary-mode-map)

;; We'll use other-buffer instead of tracking the origin explicitly

;; Function to copy content to kill ring
(defun wiki-summary-copy-to-kill-ring ()
  "Copy wiki summary content to kill ring.
If region is active, copy that region; otherwise, copy the whole buffer."
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end))
                   (buffer-string))))
    (kill-new content)
    (message "Content copied to kill ring")))

;; Function to insert content into original buffer and quit
(defun wiki-summary-insert-to-original-buffer ()
  "Insert wiki summary content into the original buffer and quit.
If region is active, insert that region; otherwise, insert the whole buffer."
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end))
                   (buffer-string)))
        (target-buffer (other-buffer (current-buffer) t)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (insert content))
      (message "Content inserted into buffer %s" (buffer-name target-buffer)))
    (quit-window t))) ; Kill the buffer, don't just bury it

(defun wiki-summary-make-api-query (s)
  "Given a wiki page title, generate the url for the API call
   to get the page info"
  (let ((pre (format wiki-summary--pre-url-format-string wiki-summary-language-string))
        (post wiki-summary--post-url-format-string)
        (term (url-hexify-string (replace-regexp-in-string " " "_" s))))
    (concat pre term post)))

(defun wiki-summary-extract-summary (resp)
  "Given the JSON reponse from the webpage, grab the summary as a string"
  (let* ((query (plist-get resp 'query))
         (pages (plist-get query 'pages))
         (info (cadr pages)))
    (plist-get info 'extract)))

(defun wiki-summary-format-summary-in-buffer (summary)
  "Given a summary, stick it in the *wiki-summary* buffer and display the buffer"
  (let ((buf (generate-new-buffer "*wiki-summary*")))
    (with-current-buffer buf
      (insert summary) ; Using insert instead of princ for better buffer handling
      (fill-paragraph)
      (goto-char (point-min))
      (text-mode)
      (view-mode)
      (wiki-summary-mode))
    (pop-to-buffer buf)))

(defun wiki-summary-format-summary-into-buffer (summary buffer)
  "Given a summary, stick it in the *wiki-summary* buffer and display the buffer"
  (let ((this-buffer (get-buffer buffer)))
    (with-current-buffer (get-buffer this-buffer)
      (barf-if-buffer-read-only)
      (insert summary)
      (fill-paragraph))
    (display-buffer (get-buffer this-buffer))))

;;;###autoload
(defun wiki-summary (s)
  "Return the wikipedia page's summary for a term"
  (interactive
   (list
    (read-string (concat
                  "Wikipedia Article"
                  (if (thing-at-point 'word)
                      (concat " (" (thing-at-point 'word) ")")
                    "")
                  ": ")
                 nil
                 nil
                 (thing-at-point 'word))))
  (save-excursion
    (url-retrieve (wiki-summary-make-api-query s)
       (lambda (events)
         (message "") ; Clear the annoying minibuffer display
         (goto-char url-http-end-of-headers)
         (let ((json-object-type 'plist)
               (json-key-type 'symbol)
               (json-array-type 'vector))
           (let* ((result (json-read))
                  (summary (wiki-summary-extract-summary result)))
             (if (not summary)
                 (message "No article found")
               (wiki-summary-format-summary-in-buffer summary))))))))

;;;###autoload
(defun wiki-summary-insert (s)
  "Return the wikipedia page's summary for a term"
  (interactive
   (list
    (read-string (concat
                  "Wikipedia Article"
                  (if (thing-at-point 'word)
                      (concat " (" (thing-at-point 'word) ")")
                    "")
                  ": ")
                 nil
                 nil
                 (thing-at-point 'word))))
  (save-excursion
    (url-retrieve
     (wiki-summary-make-api-query s)
     (lambda (events buf)
       (message "") ; Clear the annoying minibuffer display
       (goto-char url-http-end-of-headers)
       (let ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
         (let* ((result (json-read))
                (summary (wiki-summary-extract-summary result)))
           (if (not summary)
               (message "No article found")
             (wiki-summary-format-summary-into-buffer summary buf)))))
     (list (buffer-name (current-buffer))))))

(provide 'wiki-summary)
;;; wiki-summary.el ends here
