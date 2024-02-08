
tests: k
	stack run

k: kissat
	ln -s kissat/test k

kissat:
	git clone git@github.com:arminbiere/kissat.git
