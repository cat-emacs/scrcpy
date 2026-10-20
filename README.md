# scrcpy

Standalone scrcpy Transient interface for Emacs 28.1+. Requires Transient 0.3.0,
adb 0.1.0, and installed scrcpy/adb executables. No Android project or
android-mode is required.

## Installation

```elisp
(use-package adb
  :vc (:url "https://github.com/cat-emacs/adb"))
(use-package scrcpy
  :vc (:url "https://github.com/cat-emacs/scrcpy")
  :commands (scrcpy scrcpy-start))
```

Run `M-x scrcpy` to select a device and configure video, window, input,
audio, and recording options. RET starts scrcpy. The menu also stops/restarts
sessions, shows output, copies commands, and inspects displays/encoders/cameras.
Multiple devices can have independent sessions.

Like Magit, parameter keys begin with `-`: for example, `-s` selects the
device serial, `-F` toggles fullscreen, and `-r` sets the recording path.
Action keys remain unprefixed (`RET` start, `x` stop, `X` restart). These
menu keys do not change the scrcpy command-line arguments.

Set `scrcpy-program` for an explicit scrcpy executable and
`scrcpy-extra-arguments` for options absent from the menu. Device, TCP/IP
address, display, camera, and encoder prompts offer live candidates. TCP/IP
addresses come from adb; displays, cameras, and encoders come from scrcpy's
list commands for the selected connection. A typed value is still accepted when
a query has no candidates. Scrcpy subprocesses receive the same adb executable
through their ADB environment variable without changing Emacs's environment.
This package was previously named `android-scrcpy`. Replace its package
declaration with `scrcpy` and rename configured `android-scrcpy-*` symbols to
`scrcpy-*`. Update the shared dependency from `android-adb` to `adb` as well.
Restart Emacs after installing the renamed packages; do not load both old and
new implementations.

## Development

Run `make install-deps`, then `make lint build test`. For a sibling local
checkout, pass `ADB_DIR=/path/to/adb`.
Licensed under GPL-3.0-or-later. Extracted from cat-emacs/android-mode.
