# Depreciation

* todo: extract authorships / extract full history from original repo.

* process_request_depreciation_new.pl is a part of the Robust server code. It mainly translates from our excel sheet rdf input into terms used in depreciation_computation.pl

* depreciation_computation.pl and event_calculus.pl is where the logic is.

* there is a small dependency on `/prolog/utils/` from `https://github.com/lodgeit-labs/FOL_solvers`, which in turn depends on `https://github.com/koo5/fnotation`

### running depreciation computations outside of Robust source tree:
```
swipl -s ../../master2/sources/public_lib/lodgeit_solvers/prolog/utils/utils.pl depreciation_tests.pl
% PL-Unit: depreciation ....
...
```
