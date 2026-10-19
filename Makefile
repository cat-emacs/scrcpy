EMACS ?= emacs
BATCH = $(EMACS) -Q --batch
TEST_DEPS_DIR ?= .test-deps
PACKAGE_USER_DIR ?= $(abspath $(TEST_DEPS_DIR)/elpa)
ADB_DIR ?= $(abspath $(TEST_DEPS_DIR)/adb)
LOAD_PATH = -L . -L "$(ADB_DIR)"
ARCHIVES = --eval "(setq package-user-dir \"$(PACKAGE_USER_DIR)\")" \
           --eval "(require 'package)" \
           --eval "(package-initialize)"

.PHONY: install-deps lint build test clean
install-deps:
	mkdir -p "$(TEST_DEPS_DIR)"
	test -d "$(ADB_DIR)/.git" || git clone --depth 1 https://github.com/cat-emacs/adb "$(ADB_DIR)"
	$(BATCH) $(ARCHIVES) \
	  --eval "(require 'package-vc)" \
	  --eval "(unless (package-installed-p 'adb) (package-vc-install-from-checkout \"$(ADB_DIR)\" \"adb\"))"

lint:
	$(BATCH) $(ARCHIVES) $(LOAD_PATH) --eval '(checkdoc-file "scrcpy.el")' \
	  --eval '(with-temp-buffer (insert-file-contents "scrcpy.el") (emacs-lisp-mode) (check-parens))'

build:
	$(BATCH) $(ARCHIVES) $(LOAD_PATH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile scrcpy.el

test:
	$(BATCH) $(ARCHIVES) $(LOAD_PATH) -l scrcpy.el -l scrcpy-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
