;;; v-spotify.el --- spotify client inside emacs.

;;; Commentary:
;; use Spotify API right inside Emacs.
;;; Code:

(require 'url)
(require 'json)
(require 'ivy)

(defvar v/sample-href "spotify:track:7bEDDsy2LFC0KSqhZp5nPE")

(defvar v/spotify-api-url "https://api.spotify.com/v1")
(defvar v/spotify-api-authentication-url "https://accounts.spotify.com/api/token")
(defvar v/spotify-client-id "8084cacc042344ef801c0fd2495b9d14")
(defvar v/spotify-client-secret "faeb0576b5b9415f864ac86a168fce10")

(defun v/play-spotify-url (trackId)
  "Run command to play spotify music with TRACKID."
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
        "Spotify"
        trackId)))

(defun v/generate-spotify-search-url (search-term)
  "Return a search query from SEARCH-TERM."
  (format "%s/search?q=%s&type=track" v/spotify-api-url search-term))

(defun v/generate-spotify-auth-token ()
  "Get Spotify auth token."
  (let ((url-request-method "POST")
      (url-request-data "&grant_type=client_credentials")
      (url-request-extra-headers
       `(("Content-Type" . "application/x-www-form-urlencoded")
         ("Authorization" . ,(concat "Basic " (base64-encode-string (concat v/spotify-client-id ":" v/spotify-client-secret) t))))))
  (with-current-buffer
      (url-retrieve-synchronously v/spotify-api-authentication-url)
    (goto-char url-http-end-of-headers)
    (let* ((response-body (json-read))
           (token-type (alist-get 'token_type response-body))
           (token (alist-get 'access_token response-body)))
      (cons token-type token)))))

(defun v/request-spotify-search-auth (url token)
  "Request Spotify using URL and TOKEN passed from v/generate-request-args."
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " token)))))
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char url-http-end-of-headers)
    (json-read))))


(defun v/request-spotify-search (url)
  "Generate request call with URL."
  (let* ((token (v/generate-spotify-auth-token))
       (access-token (cdr token))
       (token-type (car token)))
    (v/request-spotify-search-auth url access-token)))

(defun v/search-track (search-term)
  "Search Spotify with SEARCH-TERM."
  (let ((url (v/generate-spotify-search-url search-term)))
    (v/request-spotify-search url)))

(defun v/helper-alist-get (symbols alist)
  "Look up value for the chain of SYMBOLS in ALIST."
  (if symbols
      (v/helper-alist-get (cdr symbols)
                          (assoc (car symbols) alist))
    (cdr alist)))

(defun v/play-spotify-track (track)
  "Get uri from TRACK."
  (v/play-spotify-url (alist-get 'uri (get-text-property 0 'property track))))

(defun v/format-track-output (track)
  "Given a TRACK, return a formatted string."
  (let ((track-name (v/helper-alist-get '(name) track))
        (artist-names (mapcar
                       (lambda (artist) (v/helper-alist-get '(name) artist))
                       (v/helper-alist-get '(artists) track))))
    (format "%s - %s"
            track-name
            (mapconcat 'identity artist-names "/"))))
(defun v/display-formatted-track (search-term)
  "Format the SEARCH-TERM output."
  (if (< (length search-term) 3)
      '("More input required" . nil)
    (mapcar (lambda (track) (propertize (v/format-track-output track) 'property track))
            (v/helper-alist-get '(tracks items) (v/search-track search-term)))))
(defun v/ivy-spotify-search ()
  "Ivy frontend."
  (interactive)
  (ivy-read "Search track: " #'v/display-formatted-track
            :dynamic-collection t
            :action 'v/play-spotify-track))

(v/ivy-spotify-search)

(provide 'spotify)
;;; spotify.el ends here
