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
;; Special Keys in wiki-summary-mode:
;; q - Quit and kill the buffer
;; y - Copy content to kill ring (region or whole buffer)
;; i - Insert content into original buffer and quit
;; e - Expand to show the full article
;; w - Open article in EWW browser
;; b - Open article in external browser

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

(defvar wiki-summary--full-post-url-format-string
  "&prop=extracts&explaintext=&format=json&redirects"
  "URL parameters for fetching the full article content.")

;; Track article information for expansion and browsing
(defvar-local wiki-summary-article-title nil
  "The title of the Wikipedia article being displayed.")

(defvar-local wiki-summary-article-language nil
  "The language of the Wikipedia article being displayed.")

(defvar-local wiki-summary-is-full-article nil
  "Whether the current buffer contains the full article or just a summary.")

(defvar-local wiki-summary-origin-buffer nil
  "The buffer from which the wiki summary was requested.")

;; Imenu Integration
(defvar wiki-summary-imenu-generic-expression
  '(("Sections" "^\\(==+ \\(.+?\\) ==+\\)$" 2))
  "Imenu expression for `wiki-summary-mode'.
This will match Wikipedia-style section headings like '== History ==' or
'=== Early years ===' and create imenu entries for them.")

(defun wiki-summary-imenu-create-index ()
  "Create an imenu index for the current wiki-summary buffer.
This function scans the buffer for Wikipedia-style section headings."
  (let ((index nil)
        (case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward "^==+ \\(.+?\\) ==+$" nil t)
      (let* ((heading (match-string-no-properties 1))
             (level (- (match-end 0) (match-beginning 0) 
                       (length heading) 2)) ; Calculate heading level based on = chars
             (indent (make-string (* 2 (- (/ level 2) 1)) ?\s))
             (entry (cons (concat indent heading) (match-beginning 0))))
        (push entry index)))
    (nreverse index)))

(defun wiki-summary-setup-imenu ()
  "Setup imenu for the current wiki-summary buffer."
  (when (fboundp 'imenu-add-to-menubar)
    (setq-local imenu-create-index-function #'wiki-summary-imenu-create-index)
    (imenu-add-to-menubar "Sections")))

;; Define the keymap
(defvar wiki-summary-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Basic navigation is provided by special-mode
    (define-key map (kbd "q") 'wiki-summary-quit)
    (define-key map (kbd "y") 'wiki-summary-copy-to-kill-ring)
    (define-key map (kbd "i") 'wiki-summary-insert-to-original-buffer)
    (define-key map (kbd "e") 'wiki-summary-get-full-article)
    (define-key map (kbd "w") 'wiki-summary-open-in-eww)
    (define-key map (kbd "b") 'wiki-summary-open-in-browser)
    map)
  "Keymap for `wiki-summary-mode'.")

(define-derived-mode wiki-summary-mode special-mode "Wiki-Summary"
  "Major mode for viewing Wikipedia article summaries.

\\{wiki-summary-mode-map}"
  :group 'wiki-summary
  (setq buffer-read-only t)
  (buffer-disable-undo)
  ;; Set up imenu if it's available
  (wiki-summary-setup-imenu))

;; Custom functions for keybindings

(defun wiki-summary-quit ()
  "Quit and kill the wiki-summary buffer."
  (interactive)
  (kill-buffer))

(defun wiki-summary-copy-to-kill-ring ()
  "Copy wiki summary content to kill ring.
If region is active, copy that region; otherwise, copy the whole buffer."
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end))
                   (buffer-string))))
    (kill-new content)
    (message "Content copied to kill ring")))

(defun wiki-summary-insert-to-original-buffer ()
  "Insert wiki summary content into the original buffer and quit.
If region is active, insert that region; otherwise, insert the whole buffer."
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end))
                   (buffer-string)))
        (target-buffer (other-buffer (current-buffer) t)))
    (if (buffer-live-p target-buffer)
        (progn
          (with-current-buffer target-buffer
            (insert content))
          (message "Content inserted into buffer %s" (buffer-name target-buffer))
          (if (string-match-p "\\*wiki-summary:" (buffer-name))
	      (kill-buffer))
	  )
      (message "No suitable target buffer found."))))

(defun wiki-summary-make-api-query (s &optional full)
  "Given a wiki page title S, generate the url for the API call.
When FULL is non-nil, fetch the full article instead of just the summary."
  (let ((pre (format wiki-summary--pre-url-format-string wiki-summary-language-string))
        (post (if full wiki-summary--full-post-url-format-string
                wiki-summary--post-url-format-string))
        (term (url-hexify-string (replace-regexp-in-string " " "_" s))))
    (concat pre term post)))

(defun wiki-summary-make-wiki-url (title &optional language)
  "Generate a standard Wikipedia URL for TITLE in LANGUAGE."
  (let ((lang (or language wiki-summary-language-string "en"))
        (title-encoded (url-hexify-string (replace-regexp-in-string " " "_" title))))
    (format "https://%s.wikipedia.org/wiki/%s" lang title-encoded)))

(defun wiki-summary-extract-summary (resp)
  "Given the JSON reponse from the webpage, grab the summary as a string"
  (let* ((query (plist-get resp 'query))
         (pages (plist-get query 'pages))
         (info (cadr pages)))
    (plist-get info 'extract)))

(defun wiki-summary-format-summary-in-buffer (summary title)
  "Given a SUMMARY and TITLE, display it in a wiki-summary buffer."
  (let ((buf (generate-new-buffer (format "*wiki-summary: %s*" title)))
        (origin-buffer (current-buffer)))
    (with-current-buffer buf
      (wiki-summary-mode)
      (let ((inhibit-read-only t))
        (insert summary)
        (fill-paragraph)
        (goto-char (point-min)))
      ;; Store article information for later use
      (setq-local wiki-summary-article-title title)
      (setq-local wiki-summary-article-language wiki-summary-language-string)
      (setq-local wiki-summary-is-full-article nil)
      (setq-local wiki-summary-origin-buffer origin-buffer)
      ;; Setup imenu if available
      (wiki-summary-setup-imenu))
    (pop-to-buffer buf)
    ;; Return the buffer in case it's needed
    buf))

(defun wiki-summary-format-summary-into-buffer (summary buffer)
  "Given a SUMMARY, stick it in the BUFFER and display the buffer"
  (let ((this-buffer (get-buffer buffer)))
    (with-current-buffer (get-buffer this-buffer)
      (barf-if-buffer-read-only)
      (insert summary)
      (fill-paragraph))
    (display-buffer (get-buffer this-buffer))))

;;;###autoload
(defun wiki-summary (s)
  "Return the wikipedia page's summary for a term S.
If region is active, use the selected text as the search term."
  (interactive
   (list
    (read-string (concat
                  "Wikipedia Article"
                  (cond
                   ((use-region-p)
                    (concat " (" (buffer-substring-no-properties
                                 (region-beginning) (region-end)) ")"))
                   ((thing-at-point 'word)
                    (concat " (" (thing-at-point 'word) ")"))
                   (t ""))
                  ": ")
                 nil
                 nil
                 (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'word)))))
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
               (wiki-summary-format-summary-in-buffer summary s))))))))

;;;###autoload
(defun wiki-summary-insert (s)
  "Insert the wikipedia page's summary for a term S.
If region is active, use the selected text as the search term."
  (interactive
   (list
    (read-string (concat
                  "Wikipedia Article"
                  (cond
                   ((use-region-p)
                    (concat " (" (buffer-substring-no-properties
                                 (region-beginning) (region-end)) ")"))
                   ((thing-at-point 'word)
                    (concat " (" (thing-at-point 'word) ")"))
                   (t ""))
                  ": ")
                 nil
                 nil
                 (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'word)))))
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

;; Functions for article expansion and browser integration

(defun wiki-summary-get-full-article ()
  "Fetch the full content of the current Wikipedia article."
  (interactive)
  (when wiki-summary-article-title
    (if wiki-summary-is-full-article
        (message "Already viewing the full article.")
      (let ((title wiki-summary-article-title)
            (language wiki-summary-article-language))
        (message "Fetching full article for %s..." title)
        (url-retrieve (wiki-summary-make-api-query title t)
                      (lambda (events)
                        (message "")
                        (goto-char url-http-end-of-headers)
                        (let ((json-object-type 'plist)
                              (json-key-type 'symbol)
                              (json-array-type 'vector))
                          (let* ((result (json-read))
                                 (full-content (wiki-summary-extract-summary result)))
                            (if (not full-content)
                                (message "Could not retrieve full article.")
                              (with-current-buffer (get-buffer (format "*wiki-summary: %s*" title))
                                (let ((inhibit-read-only t)
                                      (pos (point)))
                                  (erase-buffer)
                                  (insert full-content)
                                  (fill-paragraph)
                                  (goto-char (min pos (point-max)))
                                  (setq-local wiki-summary-is-full-article t)
                                  (rename-buffer (format "*wiki-full: %s*" title) t)
                                  ;; Re-setup imenu for the full article
                                  (wiki-summary-setup-imenu)
                                  (message "Expanded to full article for %s." title))))))))))
    (pulse-momentary-highlight-one-line (point))))

(defun wiki-summary-open-in-eww ()
  "Open the current Wikipedia article in EWW browser."
  (interactive)
  (if (and wiki-summary-article-title wiki-summary-article-language)
      (progn
        (require 'eww)
        (let ((url (wiki-summary-make-wiki-url wiki-summary-article-title
                                              wiki-summary-article-language)))
          (message "Opening %s in EWW..." wiki-summary-article-title)
          (eww-browse-url url)
          (message "Opened %s in EWW." wiki-summary-article-title)))
    (message "No article information available.")))

(defun wiki-summary-open-in-browser ()
  "Open the current Wikipedia article in external browser."
  (interactive)
  (if (and wiki-summary-article-title wiki-summary-article-language)
      (let ((url (wiki-summary-make-wiki-url wiki-summary-article-title
                                            wiki-summary-article-language)))
        (message "Opening %s in browser..." wiki-summary-article-title)
        (browse-url url)
        (message "Opened %s in browser." wiki-summary-article-title))
    (message "No article information available.")))

(provide 'wiki-summary)
;;; wiki-summary.el ends here
