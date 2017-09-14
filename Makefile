EMACS?=emacs

default:
	echo "A little more love :3"

love: --love usage

--love:
	@echo "I love you too :3"

usage:
	@echo "Usage:\n\
   --clean: wipe old configuration\n\
   --install: install configuration files\n\
   --all: wipe old configuration, then install plugin files, then install configuration files\n\
   --test-all: run tests\n"

all:
	clean
	install

clean:
	rm -rfv ~/.emacs.d

install:
	mkdir -p ~/.emacs.d
	mkdir -p ~/.emacs.d/plugin
	mkdir -p ~/.emacs.d/extension
	mkdir -p ~/.emacs.d/lib
	mkdir -p ~/.emacs.d/.save

	#curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
	cp -Rv src/* ~/.emacs.d

test: test-all

test-all: test-core test-task

test-core:
	$(EMACS) -batch -l ert -L src/core -l test/core/core--execution.el -f ert-run-tests-batch-and-exit

test-task: test-task-zero test-task-core

test-task-zero:
	#$(EMACS) -batch -l ert -l src/task/zero/util/cond/funcs.el         -l test/task/zero/util/cond/funcs.el         -f ert-run-tests-batch-and-exit
	#$(EMACS) -batch -l ert -l src/task/zero/util/keymap/funcs.el       -l test/task/zero/util/keymap/funcs.el       -f ert-run-tests-batch-and-exit

test-task-core:
	$(EMACS) -batch -l ert -l src/task/core/interface/emojify/funcs.el -l test/task/core/interface/emojify/funcs.el -f ert-run-tests-batch-and-exit