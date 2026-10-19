;;; scrcpy.el --- Control scrcpy from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: R.W. van 't Veer
;; Keywords: tools processes android
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.3.0") (adb "0.1.0"))
;; URL: https://github.com/cat-emacs/scrcpy

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;;; Commentary:

;; Provides a Transient interface for starting and managing scrcpy
;; processes.  Common scrcpy options are exposed as infix arguments,
;; while `scrcpy-extra-arguments' supports less common options.

;;; Code:

(require 'comint)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'adb)

(defgroup scrcpy nil
  "Control scrcpy from Emacs."
  :group 'tools)

(defun scrcpy--log (format-string &rest args)
  "Log scrcpy message FORMAT-STRING with ARGS."
  (apply #'message (concat "scrcpy: " format-string) args))

(defcustom scrcpy-program "scrcpy"
  "Executable used to run scrcpy.
This may be either an executable name found in variable `exec-path' or an
absolute path."
  :type 'string
  :group 'scrcpy)

(defcustom scrcpy-extra-arguments nil
  "Additional arguments appended to every scrcpy invocation.
Each list item must be one complete command-line argument."
  :type '(repeat string)
  :group 'scrcpy)

(defcustom scrcpy-display-buffer-on-start nil
  "Whether to display the scrcpy log buffer when starting scrcpy."
  :type 'boolean
  :group 'scrcpy)

(defvar scrcpy--processes nil
  "Scrcpy processes started by Emacs.")

(define-derived-mode scrcpy-output-mode comint-mode "scrcpy"
  "Major mode for scrcpy process output."
  (setq-local comint-process-echoes nil))

(defun scrcpy--program ()
  "Return the executable path for `scrcpy-program'."
  (or (and (file-name-absolute-p scrcpy-program)
           (file-executable-p scrcpy-program)
           scrcpy-program)
      (executable-find scrcpy-program)
      (user-error "Cannot find scrcpy executable: %s"
                  scrcpy-program)))

(defun scrcpy--device-candidates ()
  "Return completion candidates for connected Android devices."
  (mapcar (lambda (device)
            (cons (adb-device-label device)
                  (plist-get device :serial)))
          (adb-devices t)))

(defun scrcpy--read-device (prompt initial-input _history)
  "Read a device serial using PROMPT and INITIAL-INPUT."
  (let* ((candidates (scrcpy--device-candidates))
         (display-values (mapcar #'car candidates))
         (initial
          (or (car (rassoc initial-input candidates))
              initial-input))
         (choice
          (completing-read prompt display-values nil nil initial)))
    (or (cdr (assoc choice candidates)) choice)))

(defun scrcpy--read-record-file (prompt initial-input _history)
  "Read a recording path using PROMPT and INITIAL-INPUT."
  (expand-file-name
   (read-file-name prompt nil initial-input nil initial-input)))

(defun scrcpy--argument-value (prefix args)
  "Return the value of the first argument beginning with PREFIX in ARGS."
  (when-let* ((argument
               (seq-find (lambda (arg) (string-prefix-p prefix arg)) args)))
    (string-remove-prefix prefix argument)))

(defun scrcpy--target-name (args)
  "Return a short target name inferred from scrcpy ARGS."
  (cond
   ((scrcpy--argument-value "--serial=" args))
   ((member "--select-usb" args) "usb")
   ((member "--select-tcpip" args) "tcpip")
   ((scrcpy--argument-value "--tcpip=" args)
    (concat "tcpip-"
            (scrcpy--argument-value "--tcpip=" args)))
   (t "default")))

(defun scrcpy--buffer-name (args)
  "Return a process buffer name for scrcpy ARGS."
  (format "*scrcpy:%s*"
          (replace-regexp-in-string
           "[^[:alnum:]._-]" "-"
           (scrcpy--target-name args))))

(defun scrcpy--command (args)
  "Return the complete scrcpy command for transient ARGS."
  (append (list (scrcpy--program))
          scrcpy-extra-arguments
          args))

(defun scrcpy--connection-arguments (args)
  "Return only device connection arguments from ARGS."
  (seq-filter
   (lambda (arg)
     (or (member arg '("--select-usb" "--select-tcpip"))
         (string-prefix-p "--serial=" arg)
         (string-prefix-p "--tcpip=" arg)))
   args))

(defun scrcpy--format-command (command)
  "Return shell-quoted display text for COMMAND."
  (mapconcat #'shell-quote-argument command " "))

(defun scrcpy--live-processes ()
  "Return live scrcpy processes and discard stale entries."
  (setq scrcpy--processes
        (seq-filter #'process-live-p scrcpy--processes)))

(defun scrcpy--read-process (&optional prompt)
  "Read a live scrcpy process using optional PROMPT."
  (let ((processes (scrcpy--live-processes)))
    (cond
     ((null processes)
      (user-error "No scrcpy process is running"))
     ((= (length processes) 1)
      (car processes))
     (t
      (let* ((candidates
              (mapcar
               (lambda (process)
                 (cons
                  (format "%s [%s]"
                          (process-get process 'scrcpy-target)
                          (process-name process))
                  process))
               processes))
             (choice
              (completing-read (or prompt "scrcpy process: ")
                               (mapcar #'car candidates) nil t)))
        (cdr (assoc choice candidates)))))))

(defun scrcpy--sentinel (process event)
  "Handle scrcpy PROCESS state change described by EVENT."
  (when (memq (process-status process) '(exit signal))
    (let ((expected-stop
           (process-get process 'scrcpy-expected-stop)))
      (setq scrcpy--processes
            (delq process scrcpy--processes))
      (when (buffer-live-p (process-buffer process))
        (with-current-buffer (process-buffer process)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "\nProcess %s" (string-trim event))))))
      (if (or expected-stop (zerop (process-exit-status process)))
        (scrcpy--log "scrcpy stopped for %s"
                      (process-get process 'scrcpy-target))
        (when (buffer-live-p (process-buffer process))
          (pop-to-buffer (process-buffer process)))
        (scrcpy--log "scrcpy failed for %s (exit %d)"
                      (process-get process 'scrcpy-target)
                      (process-exit-status process))))))

(defun scrcpy--start (args)
  "Start scrcpy asynchronously with ARGS."
  (let* ((command (scrcpy--command args))
         (target (scrcpy--target-name args))
         (buffer (get-buffer-create (scrcpy--buffer-name args)))
         (process-environment (copy-sequence process-environment)))
    (setenv "ADB" (adb-executable))
    (when-let* ((existing (get-buffer-process buffer))
                ((process-live-p existing)))
      (user-error "Scrcpy is already running for %s" target))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "$ " (scrcpy--format-command command) "\n\n"))
      (scrcpy-output-mode))
    (let ((process
           (make-process
            :name (format "scrcpy-%s" target)
            :buffer buffer
            :command command
            :connection-type 'pipe
            :noquery t
            :sentinel #'scrcpy--sentinel)))
      (process-put process 'scrcpy-arguments args)
      (process-put process 'scrcpy-target target)
      (push process scrcpy--processes)
      (when scrcpy-display-buffer-on-start
        (display-buffer buffer))
      (scrcpy--log "started scrcpy for %s" target)
      process)))

;;;###autoload
(defun scrcpy-start (&optional args)
  "Start scrcpy using transient ARGS.
Interactively, use the current values from `scrcpy'."
  (interactive (list (transient-args 'scrcpy)))
  (scrcpy--start (or args nil)))

(defun scrcpy-stop (&optional process)
  "Stop a running scrcpy PROCESS."
  (interactive)
  (let ((process (or process (scrcpy--read-process))))
    (process-put process 'scrcpy-expected-stop t)
    (condition-case nil
        (interrupt-process process)
      (error (delete-process process)))
    (scrcpy--log "stopping scrcpy for %s"
                  (process-get process 'scrcpy-target))))

(defun scrcpy-restart (&optional process)
  "Restart a running scrcpy PROCESS with its original arguments."
  (interactive)
  (let* ((process (or process (scrcpy--read-process)))
         (args (process-get process 'scrcpy-arguments)))
    (process-put process 'scrcpy-expected-stop t)
    (delete-process process)
    (scrcpy--start args)))

(defun scrcpy-show-output (&optional process)
  "Display the output buffer for scrcpy PROCESS."
  (interactive)
  (pop-to-buffer
   (process-buffer (or process (scrcpy--read-process)))))

(defun scrcpy-copy-command ()
  "Copy the scrcpy command represented by the current transient values."
  (interactive)
  (let ((command
         (scrcpy--format-command
          (scrcpy--command (transient-args 'scrcpy)))))
    (kill-new command)
    (message "Copied: %s" command)))

(defun scrcpy--run-info (title &rest args)
  "Run scrcpy with ARGS and display the output in buffer TITLE."
  (let ((buffer (get-buffer-create title))
        (process-environment (copy-sequence process-environment))
        status)
    (setenv "ADB" (adb-executable))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq status
              (apply #'call-process
                     (scrcpy--program) nil t nil args)))
      (special-mode))
    (pop-to-buffer buffer)
    (unless (zerop status)
      (user-error "Scrcpy exited with status %s" status))))

(defun scrcpy-version ()
  "Display the installed scrcpy version."
  (interactive)
  (scrcpy--run-info "*scrcpy-version*" "--version"))

(defun scrcpy-list-displays ()
  "List displays reported by the selected Android device."
  (interactive)
  (apply #'scrcpy--run-info
         "*scrcpy-displays*"
         (append
          (scrcpy--connection-arguments
           (transient-args 'scrcpy))
          '("--list-displays"))))

(defun scrcpy-list-encoders ()
  "List encoders reported by the selected Android device."
  (interactive)
  (apply #'scrcpy--run-info
         "*scrcpy-encoders*"
         (append
          (scrcpy--connection-arguments
           (transient-args 'scrcpy))
          '("--list-encoders"))))

(defun scrcpy-list-cameras ()
  "List cameras reported by the selected Android device."
  (interactive)
  (apply #'scrcpy--run-info
         "*scrcpy-cameras*"
         (append
          (scrcpy--connection-arguments
           (transient-args 'scrcpy))
          '("--list-cameras"))))

;;;###autoload (autoload 'scrcpy "scrcpy" nil t)
(transient-define-prefix scrcpy ()
  "Configure, start, and manage scrcpy."
  [["Connection"
    ("-s" "Device serial" "--serial="
     :reader scrcpy--read-device)
    ("-u" "Select USB device" "--select-usb")
    ("-e" "Select TCP/IP device" "--select-tcpip")
    ("-t" "Connect over TCP/IP" "--tcpip=")
    ("-i" "Display id" "--display-id=")]
   ["Video"
    ("-m" "Maximum size" "--max-size=")
    ("-f" "Maximum FPS" "--max-fps=")
    ("-b" "Video bit rate" "--video-bit-rate=")
    ("-v" "Video codec" "--video-codec="
     :choices ("h264" "h265" "av1" "raw"))
    ("-c" "Crop" "--crop=")
    ("-o" "Capture orientation" "--capture-orientation="
     :choices ("0" "90" "180" "270"
               "flip0" "flip90" "flip180" "flip270"))]
   ["Window"
    ("-F" "Fullscreen" "--fullscreen")
    ("-a" "Always on top" "--always-on-top")
    ("-B" "Borderless" "--window-borderless")
    ("-d" "Disable screensaver" "--disable-screensaver")
    ("-W" "No window" "--no-window")
    ("-l" "Window title" "--window-title=")]]
  [["Control and input"
    ("-C" "No control" "--no-control")
    ("-S" "Turn screen off" "--turn-screen-off")
    ("-k" "Stay awake" "--stay-awake")
    ("-K" "Keep active" "--keep-active")
    ("-p" "Power off on close" "--power-off-on-close")
    ("-h" "Show touches" "--show-touches")
    ("-q" "Keyboard mode" "--keyboard="
     :choices ("disabled" "sdk" "uhid" "aoa"))
    ("-M" "Mouse mode" "--mouse="
     :choices ("disabled" "sdk" "uhid" "aoa"))]
   ["Audio"
    ("-A" "Disable audio" "--no-audio")
    ("-j" "Audio source" "--audio-source="
     :choices ("output" "playback" "mic" "mic-unprocessed"
               "mic-camcorder" "mic-voice-recognition"
               "mic-voice-communication" "voice-call"
               "voice-call-uplink" "voice-call-downlink"
               "voice-performance"))
    ("-J" "Audio codec" "--audio-codec="
     :choices ("opus" "aac" "flac" "raw"))
    ("-g" "Audio bit rate" "--audio-bit-rate=")
    ("-y" "Duplicate audio" "--audio-dup")]
   ["Recording"
    ("-r" "Record to file" "--record="
     :reader scrcpy--read-record-file)
    ("-R" "Record format" "--record-format="
     :choices ("mp4" "mkv" "m4a" "mka" "opus" "aac" "flac" "wav"))
    ("-N" "No playback" "--no-playback")
    ("-V" "No video playback" "--no-video-playback")
    ("-O" "No audio playback" "--no-audio-playback")
    ("-L" "Time limit" "--time-limit=")]]
  [["Actions"
    ("RET" "Start" scrcpy-start)
    ("x" "Stop" scrcpy-stop)
    ("X" "Restart" scrcpy-restart)
    ("z" "Show output" scrcpy-show-output)
    ("w" "Copy command" scrcpy-copy-command)]
   ["Inspect"
    ("?" "Version" scrcpy-version)
    ("D" "List displays" scrcpy-list-displays)
    ("E" "List encoders" scrcpy-list-encoders)
    ("I" "List cameras" scrcpy-list-cameras)]])

(provide 'scrcpy)

;;; scrcpy.el ends here
