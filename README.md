# String diagrams for functions - termination proof

This repository hosts the code used in proving termination and local confluence for a string diagrammatic presentation of functions between finite sets.


## Termination
The termination proof can be found in the `termination.py` file, which is a Z3 Python script, implementing the termination argument of the string diagrammatic representation of finite functions using polynomial interpretation.

To run, please install the Z3 solver and run 
```
python termination.py
```
in terminal

## Local confluence

The local confluence proofs were generated using the code inside the `strings/` folder (the produced html is in `docs/` and can be viewed [here](https://goodlyrottenapple.github.io/string-diagrams-functions/confluence.html)). The code is a rudimentary Haskell library, which implements the rewrite system and an SVG pretty printing. 

To run, first compile the code using [stack](https://docs.haskellstack.org/en/stable/README/)
```
stack build
```
and then run
```
stack exec strings-exe
```
