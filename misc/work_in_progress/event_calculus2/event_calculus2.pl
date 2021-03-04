# I'm sending you the event_calculus2.pl, it's the 2nd version that uses constraint programming properly (applied to another domain - invoices), one can get all the life of some asset in a list output using this, but one would need to parse this list from the start again for depreciation calculations, so it wouldn't bring any benefit.

:- dynamic happens/2, invoice/5.
:- use_module(library(clpfd)).


% Define constraint in days, max 10000 days
time(T):- T #=< 1000, T #>= -1.

% Event calculus general definitions:
initiated(F,T):- time(T), happens(E,T), initiates(E,F,T).
%initiated(F,-1):- initially(F).

%triggered(F,T):- time(T), happens(E,T), triggers(E,F,T).

terminated(F,T):- happens(E,T), terminates(E,F,T), time(T).

initiatedBefore(F,T1,T):- time(T), time(T1), initiated(F,T1), T1#<T.
terminatedBetween(F,T1,T2):-  time(T), time(T1), time(T2), terminated(F,T), T#>=T1, T#<T2.
terminatedAfter(F,T1,T):- time(T), time(T1), terminated(F,T), T#>=T1.

holdsAt(F,T):- time(T), time(T1), initiatedBefore(F,T1,T), \+ terminatedBetween(F,T1,T).
holdsExactlyBetween(F,T1,T2):- time(T1), time(T2), initiated(F,T1), terminated(F,T2).

% for pretty printing of lists
dom_integers(D, Is) :- phrase(dom_integers_(D), Is).

dom_integers_(I)      --> { integer(I) }, [I].
dom_integers_(L..U)   --> { numlist(L, U, Is) }, Is.
dom_integers_(D1\/D2) --> dom_integers_(D1), dom_integers_(D2).
%%

holdsIntervals(F,T1,T2):- T = T1, holdsAt(F,T), fd_dom(T1, D), dom_integers(D, Is), min_list(Is,T1), max_list(Is,T2).
holdsIntervals(F,T1,T2):- holdsExactlyBetween(F,T1,T2).
%holdsAt(F,T):- triggered(F,T).

% Event calculus definitions for the specific problem:
% planning invoice, considering how much and for what
% preparing invoice, complete the record in the accounting system
% check invoice, carried out by an authorized person
% send invoice, trigger

% Invoice has 7 statuses or fluents:
% planning, planned, preparing, prepared, checking, checked and sent
fluent(planning(Invoice_id)).
fluent(planned(Invoice_id)).
fluent(preparing(Invoice_id)).
fluent(prepared(Invoice_id)).
fluent(checking(Invoice_id)).
fluent(checked(Invoice_id)).
fluent(sent(Invoice_id)).


% Invoice has these fields:
% invoice(Invoice_id,How_much,For_what,Other_info,Person_checking)

% Definition of what event initiates and terminates what and conditions for successful events
initiates(begin_planning(Invoice_id), planning(Invoice_id), T):- invoice(Invoice_id,_,_,_,_).
terminates(finish_planning(Invoice_id,How_much,For_what), planning(Invoice_id),T):- invoice(Invoice_id,_,_,_,_).
initiates(finish_planning(Invoice_id,How_much,For_what), planned(Invoice_id),T):- invoice(Invoice_id,_,_,_,_).

initiates(begin_preparing(Invoice_id), preparing(Invoice_id), T):- holdsAt(planned(Invoice_id),T).
terminates(finish_preparing(Invoice_id, Other_info), preparing(Invoice_id),T):- invoice(Invoice_id,_,_,Other_info,_).
initiates(finish_preparing(Invoice_id, Other_info), prepared(Invoice_id), T):- invoice(Invoice_id,_,_,Other_info,_).

initiates(begin_checking(Invoice_id,Person), checking(Invoice_id), T):- holdsAt(prepared(Invoice_id),T), authorized(Person).
terminates(finish_checking(Invoice_id,Person), checking(Invoice_id),T):- authorized(Person), invoice(Invoice_id,_,_,_,Person).
initiates(finish_checking(Invoice_id,Person), checked(Invoice_id), T):- authorized(Person), invoice(Invoice_id,_,_,_,Person).

initiates(send(Invoice_id),sent(Invoice_id),T):- holdsAt(checked(Invoice_id),T).

% Definition of the starting default
%initially(not(planning(_))).
% Event calculus application
% register a new invoice
invoice(123,_,_,_,_).
% Previous list of authorized persons
authorized(bob).
% happens(Event,Time).
happens(begin_planning(123),
5).
happens(finish_planning(123,1000,service_A),
10).
happens(begin_preparing(123),
12).
happens(finish_preparing(123,other_important_info),
14).
happens(begin_checking(123,bob),
15).
happens(finish_checking(123,bob),
17).
happens(send(123),
18).

% some queries to check what holds at what time
%holdsAt(sent(123),19).
%holdsAt(prepared(123),100).
%holdsAt(checked(123),100).
%holdsAt(planned(123),100).
%holdsAt(planning(123),100).
%holdsAt(planning(123),7).
%holdsAt(planning(123),11).
%holdsAt(preparing(123),13).
%holdsAt(preparing(123),10).
%holdsAt(preparing(123),15).
%holdsIntervals(checked(123),T1,T2).
%holdsIntervals(planning(123),T1,T2).
%findall((F,T1,T2),holdsIntervals(F,T1,T2),Results).



%List of possible events, unused
/*
event(begin_planning(Invoice_id)).
event(finish_planning(Invoice_id)).
event(begin_preparing(Invoice_id)).
event(finish_preparing(Invoice_id)).
event(begin_checking(Invoice_id)).
event(finish_checking(Invoice_id)).
event(send(Invoice_id)).
*/
