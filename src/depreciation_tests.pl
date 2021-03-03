:- use_module(event_calculus,[
	depreciation_value/6,
	depreciation_rate/6,
	depreciationAsset/12,
	days_from_begin_accounting/2,
	assert_asset/4,
	assert_event/2,
	asset/4
]).

:- begin_tests(depreciation_rate).


test(depreciation_rate_prime_cost, all(x=[x])) :-
    Method = prime_cost,
    Asset_id = car123,
    Effective_life_years = 5,
    depreciation_rate(Asset_id, Method,_,_,Effective_life_years, Rate),
    assertion(Rate == 20).


test(depreciation_rate_diminishing_value) :-
    Method = diminishing_value,
    Asset_id = car123,
    Effective_life_years = 5,
    Start_date = date(2017,1,1),
    depreciation_rate(Asset_id, Method,_,Start_date,Effective_life_years, Rate),
    assertion(Rate == 40).

test(depreciation_rate_general_pool, all(x=[x])) :-
    Asset_id = general_pool,
    Effective_life_years = 5,
    Start_date = date(2017,1,1),
    Year_from_start = 1,
    depreciation_rate(Asset_id, _,Year_from_start,Start_date,Effective_life_years, Rate),
    assertion(Rate == 15).

test(depreciation_rate_general_pool_fail, fail) :-
    % It should fail because general_pool can only use diminishing value method
    Asset_id = general_pool,
    Method = prime_cost,
    Effective_life_years = 5,
    Start_date = date(2017,1,1),
    Year_from_start = 1,
    depreciation_rate(Asset_id, Method,Year_from_start,Start_date,Effective_life_years, Rate),
    assertion(Rate == 15).


:- end_tests(depreciation_rate).



:- begin_tests(depreciation_value).


test(depreciation_value_prime_cost0, all(x=[x])) :-
	% Depreciation_value is Asset_cost * (Days_held / 365) * Depreciation_rate
	depreciation_value(prime_cost, 1000, 800, 200, 0.2, Depreciation_value),
	assertion(floats_close_enough(Depreciation_value, 1.0958904109589042)).


test(depreciation_value_prime_cost1) :-
    Method = prime_cost,
    Asset_cost = 1000,
    Asset_base_value = 800,
    Days_held = 200,
    Depreciation_rate = 20,
    Correct_depreciation_value is Asset_cost * (Days_held / 365) * Depreciation_rate / 100,
    depreciation_value(Method, Asset_cost, Asset_base_value, Days_held,
        Depreciation_rate, Depreciation_value),
    assertion(Depreciation_value == Correct_depreciation_value).

test(depreciation_value_dimishing_value):-
    Method = diminishing_value,
    Asset_cost = 1000,
    Asset_base_value = 800,
    Days_held = 200,
    Depreciation_rate = 20,
    Correct_depreciation_value is Asset_base_value * (Days_held / 365) * Depreciation_rate / 100,
    depreciation_value(Method, Asset_cost, Asset_base_value, Days_held, Depreciation_rate,
        Depreciation_value),
    assertion(Depreciation_value == Correct_depreciation_value).

test(depreciation_value_prime_cost_fail,fail) :-
    Method = prime_cost,
    Asset_cost = 1000,
    Asset_base_value = 800,
    Days_held = 367,
    Depreciation_rate = 20,
    Correct_depreciation_value is Asset_cost * (Days_held / 365) * Depreciation_rate / 100,
    depreciation_value(Method, Asset_cost, Asset_base_value, Days_held, Depreciation_rate,
        Depreciation_value),
    assertion(Depreciation_value == Correct_depreciation_value).

test(depreciation_value_dimishing_value_fail,fail):-
    Method = diminishing_value,
    Asset_cost = 1000,
    Asset_base_value = 800,
    Days_held = 400,
    Depreciation_rate = 20,
    Correct_depreciation_value is Asset_base_value * (Days_held / 365) * Depreciation_rate / 100,
    depreciation_value(Method, Asset_cost, Asset_base_value, Days_held, Depreciation_rate,
        Depreciation_value),
    assertion(Depreciation_value == Correct_depreciation_value).


:- end_tests(depreciation_value).


