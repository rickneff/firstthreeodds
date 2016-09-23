
;;; dmleat.el --- functions for DMLEAT

;; Copyright (C) 2016 Rick Neff

;; Author: Rick Neff
;; Keywords: discrete mathematics, language, environment, tools
;; Homepage: http://firstthreeodds.org

;; This file is NOT part of GNU Emacs.

(defun get-started (username password)
  "Download a private key from the fto server and install it in
  ~/.emacs.d/private/id_rsa. Get set for further server interaction."
  (setq username (downcase username))
  (let* ((private-dir "~/.emacs.d/private")
         (fto-server "firstthreeodds.org")
         (creds-file (format "%s/creds.org" private-dir))
         (id-file (format "%s/id_rsa.%s" private-dir username))
         (ssh-config-dir "~/.ssh")
         (ssh-config-file (format "%s/config" ssh-config-dir))
         (ssh-host-configuration
          (format "Host %s\n      Hostname=%s\n      IdentityFile=%s\n      StrictHostKeyChecking=no\n\n"
                  username fto-server id-file))
          (make-backup-files nil))
    (with-temp-buffer
      (insert (format "%s %s\n" username password))
      (write-file creds-file))
    (if (executable-find "curl")
        (let ((result
               (shell-command-to-string
                (format "cd %s; curl -k -s -F fileUploaded=@creds.org https://%s/run/app?get-private-key"
                        private-dir fto-server))))
          (with-temp-buffer
            (insert result)
            (write-file id-file))
          (delete-file creds-file)
          (set-file-modes id-file #o600)
          (mkdir ssh-config-dir t)
          (with-temp-buffer
            (when (file-readable-p ssh-config-file)
              (insert-file-contents ssh-config-file))
            (goto-char (point-min))
            (unless (re-search-forward (concat "^Host " username "$") nil t)
              (goto-char (point-max))
              (insert ssh-host-configuration))
            (write-file ssh-config-file)))
      (error "No curl executable found")
      )))

(defun get-ready-and-get-set ()
  (switch-to-buffer "*scratch*")
  (insert (concat "\nYour username is your TLA (case insensitive).\n"
                  "\nYour password is your I-number (9 digits only, no hyphens).\n\n"))
  (let* ((username (read-no-blanks-input "Enter your username: "))
         (password (read-passwd "Enter your password: " t)))
    (get-started username password)))

(defun get-tla-if-there ()
  (let* ((private-dir "~/.emacs.d/private")
         (id-files-in-private-dir (directory-files private-dir nil "id_rsa.[a-z][a-z][a-z]")))
    (mapcar (lambda (x) (substring x 7)) id-files-in-private-dir)))

(defun clone-repository (tla)
  (if (executable-find "git")
      (shell-command-to-string (format "git clone git@%s:%s.git ~/%s" tla tla tla))
    (error "No git executable found")))

(defun ensure-readiness (filename)
  (save-buffer)
  (when (null (setq tla (get-tla-if-there)))
    (get-ready-and-get-set)
    (switch-to-buffer filename))
  (setq tla (get-tla-if-there))
  (if (listp tla) (setq tla (car tla)))
  (unless (file-readable-p (format "~/%s/.git/config" tla))
    (clone-repository tla)))

(defvar git-update-shell-command-template
  "git pull && git add %s && git commit -m Committed. && git push")

(defun send-receive-changes (filename)
  (ensure-readiness filename)
  (if (executable-find "git")
      (shell-command-to-string
       (format git-update-shell-command-template (file-name-nondirectory filename)))
    (error "No git executable found")))

(defun src ()
  (interactive)
  (let ((bfn (buffer-file-name)))
    (when bfn
      (send-receive-changes bfn))))

(defun convert-to-letter-grade (weighted-percentage)
  (let* ((number (ceiling weighted-percentage))
         (tensDigit (/ number 10))
         (onesDigit (mod number 10))
         (index (min (max (- tensDigit 5) 0) 4))
         (letter (substring "FDCBA" index (+ index 1)))
         (sign (if (<= onesDigit 2) "-" (if (>= onesDigit 7) "+")))
        )
    (concat letter (if (and (< number 95) (>= number 60)) sign))
  )
)

(defun compute-final-grade (grades)
  (let ((weighted-percentage (apply '+ (mapcar 'fourth (cddr grades)))))
    (list weighted-percentage (convert-to-letter-grade weighted-percentage))))

(provide 'dmleat)
