
top: bedlam

reg: tests.log
	git diff tests.log

#exe = .stack-work/dist/x86_64-linux/ghc-9.2.7/build/main.exe/main.exe
exe = .stack-work/dist/x86_64-linux-tinfo6/ghc-9.2.7/build/main.exe/main.exe

bedlam: $(exe) bedlam.cnf
	kissat bedlam.cnf | grep ^v | $(exe) bedlam pp

bedlam.cnf: $(exe)
	$(exe) bedlam gen $@

wooden.cnf: $(exe)
	$(exe) wooden gen $@

wooden: $(exe) wooden.cnf
	kissat wooden.cnf | grep ^v | $(exe) wooden pp

tests.log: $(exe) src/*.hs
	$(exe) reg > tests.log

p: $(exe) #Sat
	$(exe) k/cnf/prime169.cnf

h: $(exe) #UnSat
	$(exe) k/cnf/ph6.cnf

q: $(exe) #Sat
	$(exe) cnf/queens.cnf

x: $(exe) #UnSat
	$(exe) cnf/running_example.cnf

tests: $(exe)
	$(exe) tests

$(exe): src/*.hs
	stack build; touch $(exe)
