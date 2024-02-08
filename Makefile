
top: reg

dev: k
	stack run kissat/test/cnf/ph5.cnf

reg: tests.log
	git diff tests.log

tests.log: k src/*.hs Makefile
	stack run > tests.log

tests: k
	stack run

k: kissat
	ln -s kissat/test k

kissat:
	git clone git@github.com:arminbiere/kissat.git
