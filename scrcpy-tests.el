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
                       ("-m" . "--max-size=")
                       ("-f" . "--max-fps=")
                       ("-b" . "--video-bit-rate=")
                       ("-v" . "--video-codec=")
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
