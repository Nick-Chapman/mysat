# mysat

Explore Sat Solving.

Goal: implement _conflict driven clause learning_ (CDCL)

### Resources
- [Silent Revolution](https://cacm.acm.org/magazines/2023/6/273222-the-silent-revolution-of-sat/fulltext)
- [N QueensPuzzle encoder](https://github.com/bglezseoane/sat-nqueens/tree/master?tab=readme-ov-file)
- [Kissat](https://github.com/arminbiere/kissat)


### Focus

Of 47 timeouts:
3 will run to completion if we increase timeout duration from 1/10 to 3/10sec
```
k/cnf/add4            : TIMEOUT (UnSat)
k/cnf/ph5             : TIMEOUT (UnSat)
k/cover/cover0018     : TIMEOUT (Sat)
```
So lets address these first.
