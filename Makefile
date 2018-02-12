EMACS?=emacs

default:
	echo "A little more love :3"

love: --love usage

--love:
	@echo "I love you too :3"

usage:
	@echo "Usage:"
	@echo "    clean: wipe old configuration"
	@echo "    install: install configuration files"
	@echo "    all: wipe old configuration, then install plugin files, then install configuration files"
	@echo "    test: run tests"

all:
	clean
	install

clean:
	rm -rfv ~/.emacs.d

install:
	mkdir -p ~/.emacs.d
	mkdir -p ~/.emacs.d/plugin
	mkdir -p ~/.emacs.d/.save
	mkdir -p ~/.local-data/.virtualenvs

	#curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
	@cp -Rv src/.emacs.d   ~
	@cp -Rvn src/.serika.config ~

test: test-all

test-all: test-core test-task

test-core:
	cask exec buttercup -L src/core test/core

test-task: test-task-zero test-task-two

test-task-two: test-task-two-org

test-task-two-org:
	@#cask exec buttercup --eval "(load-file \"src/task/two/organization/org/funcs.el\")" test/task/two/organization/org/

test-task-zero: test-task-zero-util-keymap test-task-zero-util-func

test-task-zero-util-keymap:
	cask exec buttercup --eval "(load-file \"src/task/zero/util/keymap/funcs.el\")" test/task/zero/util/keymap/

test-task-zero-util-func:
	cask exec buttercup --eval "(load-file \"src/task/zero/util/func/funcs.el\")" test/task/zero/util/func/

test-task-core:
	@#$(EMACS) -batch -l ert -l src/task/core/interface/emojify/funcs.el -l test/task/core/interface/emojify/funcs.el -f ert-run-tests-batch-and-exit
