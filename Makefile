
top: reg


p: k #Sat
	stack run k/cnf/prime169.cnf

h: k #UnSat
	stack run k/cnf/ph6.cnf

q: k #Sat
	stack run cnf/queens.cnf

x: k #UnSat
	stack run cnf/running_example.cnf


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
