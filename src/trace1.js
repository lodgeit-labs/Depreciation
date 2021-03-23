import {f} from "./trace_import.js";
f( {
  "@id":"node1",
  "args": ["arg1" ],
  "parent":"<dummy>",
  "str":"t0",
  "type":"control:tracer_invocation"
});
f( {
  "@id":"node2",
  "args": ["arg2" ],
  "parent":"arg1",
  "str":"t0",
  "type":"proof:loaded"
});
f( {
  "@id":"node3",
  "args": ["arg3", "arg4" ],
  "parent":"arg2",
  "str":"_25220=prime_cost,_25226=car123,_25232=5,depreciation_rate(_25226,_25220,_25242,_25244,_25232,_25248),assertion(_25248==20)",
  "type":"proof:conjunction"
});
f( {
  "@id":"node4",
  "args": [],
  "parent":"arg3",
  "str":"_25220=prime_cost",
  "type":"proof#builtin"
});
f( {
  "@id":"node5",
  "args": ["arg5", "arg6" ],
  "parent":"arg4",
  "str":"_25226=car123,_25232=5,depreciation_rate(_25226,prime_cost,_25242,_25244,_25232,_25248),assertion(_25248==20)",
  "type":"proof:conjunction"
});
f( {
  "@id":"node6",
  "args": [],
  "parent":"arg5",
  "str":"_25226=car123",
  "type":"proof#builtin"
});
f( {
  "@id":"node7",
  "args": ["arg7", "arg8" ],
  "parent":"arg6",
  "str":"_25232=5,depreciation_rate(car123,prime_cost,_25242,_25244,_25232,_25248),assertion(_25248==20)",
  "type":"proof:conjunction"
});
f( {
  "@id":"node8",
  "args": [],
  "parent":"arg7",
  "str":"_25232=5",
  "type":"proof#builtin"
});
f( {
  "@id":"node9",
  "args": ["arg9", "arg10" ],
  "parent":"arg8",
  "str":"depreciation_rate(car123,prime_cost,_25242,_25244,5,_25248),assertion(_25248==20)",
  "type":"proof:conjunction"
});
f( {
  "@id":"node10",
  "args": ["arg11" ],
  "parent":"arg9",
  "str":"depreciation_rate(car123,prime_cost,_25242,_25244,5,_25248)",
  "type":"proof:loaded"
});
f( {
  "@id":"node11",
  "args": ["arg12", "arg13" ],
  "parent":"arg11",
  "str":"not(pool(car123)),_25248 is 100*1/5",
  "type":"proof:conjunction"
});
f( {
  "@id":"node12",
  "args": ["arg14" ],
  "parent":"arg12",
  "str":"not(pool(car123))",
  "type":"proof:not"
});
f( {
  "@id":"node13",
  "args": [],
  "parent":"arg13",
  "str":"_25248 is 100*1/5",
  "type":"proof#builtin"
});
f( {
  "@id":"node14",
  "args": ["arg15" ],
  "parent":"arg10",
  "str":"assertion(20==20)",
  "type":"proof:loaded"
});
f( {
  "@id":"node15",
  "args": ["arg16", "arg17" ],
  "parent":"arg15",
  "str":"\\+ \\+catch(20==20,_31618,assertion_failed(_31618,20==20)),(!)",
  "type":"proof:conjunction"
});
f( {
  "@id":"node16",
  "args": ["arg18" ],
  "parent":"arg16",
  "str":"\\+ \\+catch(20==20,_31618,assertion_failed(_31618,20==20))",
  "type":"proof:not"
});
f( {
  "@id":"node17",
  "args": ["arg19" ],
  "parent":"arg18",
  "str":"\\+catch(20==20,_31618,assertion_failed(_31618,20==20))",
  "type":"proof:not"
});
f( {
  "@id":"node18",
  "args": ["arg20", "arg21", "arg22" ],
  "parent":"arg19",
  "str":"catch(20==20,_31618,assertion_failed(_31618,20==20))",
  "type":"proof:catch"
});
f( {
  "@id":"node19",
  "args": [],
  "parent":"arg20",
  "str":"20==20",
  "type":"proof#builtin"
});
f( {
  "@id":"node20",
  "args": [],
  "parent":"arg17",
  "str":"!",
  "type":"proof#builtin"
});
