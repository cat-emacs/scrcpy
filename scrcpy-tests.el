;;; scrcpy-tests.el --- Tests for scrcpy integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for scrcpy helpers.

;;; Code:

(require 'ert)
(require 'scrcpy)

(ert-deftest scrcpy-device-candidates ()
  "Device candidates contain readable labels and serial values."
  (cl-letf (((symbol-function 'adb-devices)
             (lambda (&optional _available-only)
               '((:serial "emulator-5554" :state device :model "Pixel_9")
                 (:serial "192.0.2.10:5555" :state device :model "Tablet")))))
    (should
     (equal
      (scrcpy--device-candidates)
      '(("Pixel_9 (emulator-5554)" . "emulator-5554")
        ("Tablet (192.0.2.10:5555)" . "192.0.2.10:5555"))))))

(ert-deftest scrcpy-target-name ()
  "Target names are inferred from connection arguments."
  (should
   (equal (scrcpy--target-name
           '("--serial=emulator-5554" "--fullscreen"))
          "emulator-5554"))
  (should
   (equal (scrcpy--target-name '("--select-usb")) "usb"))
  (should
   (equal (scrcpy--target-name '("--select-tcpip")) "tcpip"))
  (should
   (equal (scrcpy--target-name '("--tcpip=192.0.2.10:5555"))
          "tcpip-192.0.2.10:5555"))
  (should
   (equal (scrcpy--target-name '("--fullscreen")) "default")))

(ert-deftest scrcpy-command-appends-configured-arguments ()
  "The command contains configured arguments before transient arguments."
  (let ((scrcpy-program "/usr/local/bin/scrcpy")
        (scrcpy-extra-arguments '("--verbosity=debug")))
    (cl-letf (((symbol-function 'file-executable-p) (lambda (_file) t)))
      (should
       (equal
        (scrcpy--command
         '("--serial=emulator-5554" "--fullscreen"))
        '("/usr/local/bin/scrcpy"
          "--verbosity=debug"
          "--serial=emulator-5554"
          "--fullscreen"))))))

(ert-deftest scrcpy-connection-arguments ()
  "Inspect commands keep device selection but ignore launch options."
  (should
   (equal
    (scrcpy--connection-arguments
     '("--serial=emulator-5554" "--fullscreen" "--max-fps=60"))
    '("--serial=emulator-5554")))
  (should
   (equal
    (scrcpy--connection-arguments
     '("--select-tcpip" "--no-audio"))
    '("--select-tcpip"))))

(ert-deftest scrcpy-buffer-name-is-safe ()
  "TCP/IP target names are converted to safe buffer names."
  (should
   (equal
    (scrcpy--buffer-name '("--tcpip=192.0.2.10:5555"))
    "*scrcpy:tcpip-192.0.2.10-5555*")))

(ert-deftest scrcpy-start-shares-adb-without-changing-environment ()
  "Scrcpy inherits the resolved adb executable, not a global environment edit."
  (let ((process-environment (copy-sequence process-environment))
        (scrcpy--processes nil)
        child-adb process buffer)
    (setenv "ADB" "original")
    (cl-letf (((symbol-function 'adb-executable)
               (lambda () "/test/sdk/adb"))
              ((symbol-function 'scrcpy--command)
               (lambda (_) (list shell-file-name "-c" "exit 0")))
              ((symbol-function 'make-process)
               (let ((original (symbol-function 'make-process)))
                 (lambda (&rest arguments)
                   (setq child-adb (getenv "ADB"))
                   (apply original arguments)))))
      (unwind-protect
          (progn
            (setq process (scrcpy--start '("--serial=test-env"))
                  buffer (process-buffer process))
            (while (process-live-p process) (accept-process-output process 0.05))
            (should (equal child-adb "/test/sdk/adb"))
            (should (equal (getenv "ADB") "original")))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest scrcpy-listed-options-keep-values-and-descriptions ()
  (should
   (equal
    (scrcpy--parse-listed-options
     "List of displays:\n    --display-id=0    (1080x2400)\n    --display-id=2    (1920x1080)\n"
     "display-id")
    '(("0" . "(1080x2400)") ("2" . "(1920x1080)"))))
  (should
   (equal
    (scrcpy--parse-listed-options
     "    --camera-id=0    (back, 4032x3024)\n    --camera-id=1    (front, 1920x1080)\n"
     "camera-id")
    '(("0" . "(back, 4032x3024)") ("1" . "(front, 1920x1080)")))))

(ert-deftest scrcpy-encoder-parser-keeps-codec-and-marker ()
  (should
   (equal
    (scrcpy--parse-encoders
     "    --video-codec=h264 --video-encoder=c2.android.avc.encoder          (hw)\n    --audio-codec=opus --audio-encoder=c2.android.opus.encoder\n"
     "video")
    '(("c2.android.avc.encoder" . "(h264, hw)"))))
  (should
   (equal
    (scrcpy--parse-encoders
     "    --audio-codec=opus --audio-encoder=c2.android.opus.encoder\n"
     "audio")
    '(("c2.android.opus.encoder" . "(opus)")))))

(ert-deftest scrcpy-tcpip-reader-offers-wireless-devices ()
  (let (choices)
    (cl-letf (((symbol-function 'adb-devices)
               (lambda (&optional _)
                 '((:serial "emulator-5554" :state device :model "Pixel")
                   (:serial "192.0.2.10:5555" :state device :model "Tablet")
                   (:serial "192.0.2.11:5555" :state offline))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 "192.0.2.10:5555 (Tablet, device)")))
      (should (equal (scrcpy--read-tcpip "TCP/IP: " nil nil)
                     "192.0.2.10:5555"))
      (should (equal choices
                     '("192.0.2.10:5555 (Tablet, device)"
                       "192.0.2.11:5555 (192.0.2.11:5555, offline)"))))))

(ert-deftest scrcpy-display-reader-uses-current-connection ()
  (let (queried)
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_command)
                 '("--serial=SER1" "--fullscreen")))
              ((symbol-function 'scrcpy--query)
               (lambda (arguments)
                 (setq queried arguments)
                 "    --display-id=0    (1080x2400)\n    --display-id=3    (640x480)\n"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "3 (640x480)" collection))
                 "3 (640x480)")))
      (let ((scrcpy--list-cache nil))
        (should (equal (scrcpy--read-display "Display: " nil nil) "3"))
        (should (equal queried '("--serial=SER1" "--list-displays")))))))

(ert-deftest scrcpy-encoder-reader-returns-encoder-name ()
  (cl-letf (((symbol-function 'transient-args) (lambda (_) nil))
            ((symbol-function 'scrcpy--query)
             (lambda (_arguments)
               "    --video-codec=h264 --video-encoder=OMX.google.h264.encoder (sw)\n"))
            ((symbol-function 'completing-read)
             (lambda (_prompt _collection &rest _)
               "OMX.google.h264.encoder (h264, sw)")))
    (let ((scrcpy--list-cache nil))
      (should (equal (scrcpy--read-video-encoder "Encoder: " nil nil)
                     "OMX.google.h264.encoder")))))

(ert-deftest scrcpy-listed-reader-falls-back-when-query-fails ()
  (let (completed)
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) (user-error "No transient")))
              ((symbol-function 'scrcpy--query)
               (lambda (_) (user-error "device offline")))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (setq completed t) "bad"))
              ((symbol-function 'read-string)
               (lambda (_prompt initial &rest _) (or initial "9"))))
      (should (equal (scrcpy--read-camera "Camera: " nil nil) "9"))
      (should-not completed))))

(ert-deftest scrcpy-transient-separates-arguments-from-actions ()
  "Use Magit-style parameter keys without changing action bindings."
  (let (keys)
    (cl-labels
        ((properties (key)
           (should-not (member key keys))
           (push key keys)
           (let ((suffix (transient-get-suffix 'scrcpy key)))
             ;; Before Transient's versioned layout, properties occupied slot 2.
             (if (keywordp (cadr suffix)) (cdr suffix) (nth 2 suffix)))))
      (dolist (entry '(("-s" . "--serial=")
                       ("-u" . "--select-usb")
                       ("-e" . "--select-tcpip")
                       ("-t" . "--tcpip=")
                       ("-i" . "--display-id=")
                       ("-P" . "--camera-id=")
                       ("-Z" . "--camera-facing=")
                       ("-H" . "--video-source=")
                       ("-m" . "--max-size=")
                       ("-f" . "--max-fps=")
                       ("-b" . "--video-bit-rate=")
                       ("-v" . "--video-codec=")
                       ("-n" . "--video-encoder=")
                       ("-c" . "--crop=")
                       ("-o" . "--capture-orientation=")
                       ("-F" . "--fullscreen")
                       ("-a" . "--always-on-top")
                       ("-B" . "--window-borderless")
                       ("-d" . "--disable-screensaver")
                       ("-W" . "--no-window")
                       ("-l" . "--window-title=")
                       ("-C" . "--no-control")
                       ("-S" . "--turn-screen-off")
                       ("-k" . "--stay-awake")
                       ("-K" . "--keep-active")
                       ("-p" . "--power-off-on-close")
                       ("-h" . "--show-touches")
                       ("-q" . "--keyboard=")
                       ("-M" . "--mouse=")
                       ("-A" . "--no-audio")
                       ("-j" . "--audio-source=")
                       ("-J" . "--audio-codec=")
                       ("-U" . "--audio-encoder=")
                       ("-g" . "--audio-bit-rate=")
                       ("-y" . "--audio-dup")
                       ("-r" . "--record=")
                       ("-R" . "--record-format=")
                       ("-N" . "--no-playback")
                       ("-V" . "--no-video-playback")
                       ("-O" . "--no-audio-playback")
                       ("-L" . "--time-limit=")))
        (let ((props (properties (car entry))))
          (should (equal (plist-get props :key) (car entry)))
          (should (equal (plist-get props :argument) (cdr entry)))))
      (dolist (entry '(("RET" . scrcpy-start)
                       ("x" . scrcpy-stop)
                       ("X" . scrcpy-restart)
                       ("z" . scrcpy-show-output)
                       ("w" . scrcpy-copy-command)
                       ("?" . scrcpy-version)
                       ("D" . scrcpy-list-displays)
                       ("E" . scrcpy-list-encoders)
                       ("I" . scrcpy-list-cameras)))
        (let ((props (properties (car entry))))
          (should (eq (plist-get props :command) (cdr entry)))
          (should-not (plist-get props :argument)))))))

(provide 'scrcpy-tests)

;;; scrcpy-tests.el ends here
