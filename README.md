# mysat

## Dependency (for solving Bedlam)
- [kissat](https://github.com/arminbiere/kissat)

## Explore Sat Solving.

Goal: implement _conflict driven clause learning_ (CDCL)

### Resources
- [Silent Revolution](https://cacm.acm.org/magazines/2023/6/273222-the-silent-revolution-of-sat/fulltext)
- [N QueensPuzzle encoder](https://github.com/bglezseoane/sat-nqueens/tree/master?tab=readme-ov-file)
- [Kissat](https://github.com/arminbiere/kissat)

### Status
- `132 tests ran; 20 timeout; 112 pass.` (timeout = 1/10s)
- `132 tests ran; 12 timeout; 120 pass.` (timeout = 1s)

### Focus:

Examples which timeout after 1/10s, but complete in 1s
```
k/cnf/ph6             : PASS (UnSat; Counts {decisions = 6496, forced = 138790, conflicts = 6497}) (42/133)
k/cnf/prime529        : PASS (Sat; Counts {decisions = 57, forced = 7526, conflicts = 55}) (791/2320)
k/cnf/prime841        : PASS (Sat; Counts {decisions = 54, forced = 7118, conflicts = 52}) (791/2320)
k/cnf/sqrt1042441     : PASS (Sat; Counts {decisions = 2, forced = 1067, conflicts = 2}) (659/1877)
k/cnf/prime961        : PASS (Sat; Counts {decisions = 64, forced = 8014, conflicts = 63}) (791/2320)
k/cnf/prime169        : PASS (Sat; Counts {decisions = 31, forced = 2897, conflicts = 29}) (461/1342)
k/cnf/prime289        : PASS (Sat; Counts {decisions = 31, forced = 3462, conflicts = 28}) (615/1798)
k/cnf/prime361        : PASS (Sat; Counts {decisions = 35, forced = 3670, conflicts = 32}) (615/1798)
```

## Use sat solver to solve Bedlam cube
```
make bedlam
```

## Attempt to use sat solver to solve [Dilemma cube](https://www.dilemma-games.com/index2.php?id=10&catId=6&productId=898&lang=ENG)
```
make wooden
```

Unlikely to complete. It is not smart:
No constraints prevent the 25 identical pieces from being tried in all permutations!
