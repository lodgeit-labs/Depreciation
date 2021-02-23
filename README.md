# Depreciation

* todo: extract authorships / extract full history from original repo.

* process_request_depreciation_new.pl is a part of the Robust server code. It mainly translates from our excel sheet rdf input into terms used in depreciation_computation.pl

* depreciation_computation.pl and event_calculus.pl is where the logic is.

* there is a small dependency on `/prolog/utils/` from `https://github.com/lodgeit-labs/FOL_solvers`


### running depreciation computations outside of Robust source tree:
```
swipl -s ../../master2/sources/public_lib/lodgeit_solvers/prolog/utils/utils.pl depreciation_computation.pl
% Started server at http://localhost:1234/
Welcome to SWI-Prolog (threaded, 64 bits, version 8.3.17-18-gb5933a38b-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).



```



