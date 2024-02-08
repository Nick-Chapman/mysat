
top: reg

q: k
	stack run cnf/queens.cnf

dev: k
	stack run kissat/test/cnf/sqrt4489.cnf

reg: tests.log
	git diff tests.log

tests.log: k src/*.hs Makefile
	stack run reg > tests.log

tests: k
	stack run tests

k: kissat
	ln -s kissat/test k

kissat:
	git clone git@github.com:arminbiere/kissat.git
