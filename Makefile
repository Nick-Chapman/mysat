
top: bedlam

reg: tests.log
	git diff tests.log

exe = .stack-work/dist/x86_64-linux/ghc-9.2.7/build/main.exe/main.exe

bedlam: ek bedlam.cnf
	kissat bedlam.cnf | grep ^v | $(exe) bedlam pp

bedlam.cnf: ek
	$(exe) bedlam gen $@

tests.log: ek src/*.hs
	$(exe) reg > tests.log

p: ek #Sat
	$(exe) k/cnf/prime169.cnf

h: ek #UnSat
	$(exe) k/cnf/ph6.cnf

q: ek #Sat
	$(exe) cnf/queens.cnf

x: ek #UnSat
	$(exe) cnf/running_example.cnf

tests: ek
	$(exe) tests

ek: e k

e: $(exe)

$(exe): src/*.hs
	stack build; touch $(exe)

k: kissat
	rm -rf k; ln -s kissat/test k

kissat:
	git clone git@github.com:arminbiere/kissat.git
