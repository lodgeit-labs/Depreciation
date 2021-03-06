:- use_module(event_calculus,[
	depreciation_value/6,
	depreciation_rate/6,
	depreciationAsset/12,
	days_from_begin_accounting/2,
	assert_asset/4,
	assert_event/2,
	asset/4
]).

:- use_module(depreciation_computation,[
    depreciation_between_start_date_and_other_date/11,
    depreciation_pool_from_start/4,
    depreciation_pool_between_two_dates/5,
    written_down_value/5,
    depreciation_between_two_dates/5,
    profit_and_loss/6
]).

cars0 :-

	days_from_begin_accounting(date(2017,05,01), D1),
	days_from_begin_accounting(date(2015,03,16), D2),
	assert_asset(car123,1000,D1,5),
	assert_asset(car456,2000,D2,8),

	/* note that these event dates are slightly different from the excel template: */

	% Transfer car123 to general pool in date(2017,7,1)
	days_from_begin_accounting(date(2017,7,1),D3),
	% Days = 10043
	assert_event(transfer_asset_to_pool(car123,general_pool),D3),

	% Transfer car456 to general pool in date(2015,7,1)
	days_from_begin_accounting(date(2015,7,1),D4),
	% Days = 9312
	assert_event(transfer_asset_to_pool(car456,general_pool),D4),

	% Remove car123 from general pool in date(2021,6,1) by disposal
	days_from_begin_accounting(date(2021,6,1),D5),
	% Days = 11474
	assert_event(remove_asset_from_pool(car123,general_pool),D5),

	% Remove car456 from general pool in date(2020,7,31) by disposal
	days_from_begin_accounting(date(2020,7,31),D6),
	% Days = 11169
	assert_event(remove_asset_from_pool(car456,general_pool),D6),

	true.


% this one is brought out for tracer testing
depreciationAsset_all_life_2 :-
    Asset_id = car456,
    T1 = 9300,
    T2 = 9500,
    Begin_value = 1000,
    Method = diminishing_value,
    Year_of_depreciation = 1,
    depreciationAsset(Asset_id,T1,T2,Begin_value,End_value,Method,Year_of_depreciation,Life,
        false,_,0,Final_depreciation_value),
    Expected = [[1000,not_in_pool(car456),9300,9313,916.9858087821355,6.678082191780821,18.75],
        [993.3219178082192,in_pool(car456,general_pool),9313,9500,916.9858087821355,76.33610902608369,15]],
    round_term(Life, Life_r),
    round_term(Expected, Expected_r),
    format(user_error,'~ngot:~n~q',[Life_r]),
    format(user_error,'~nexpected:~n~q',[Expected_r]),
    assertion(terms_with_floats_close_enough(Life,Expected)),
    assertion(floats_close_enough(Final_depreciation_value,83.01419121786451)),
    Correct_end_value is Begin_value - Final_depreciation_value,
    assertion(floats_close_enough(Correct_end_value,End_value)).



:- begin_tests(depreciation_computation2, [setup(cars0), cleanup(ec_cleanup)]).


test(depreciationAsset_all_life_1, [
	all(x=[x])
	,blocked('result needs review')
]):-
    Asset_id = car123,
    T1 = 10000,
    T2 = 10213,
    Begin_value = 1000,
    Method = diminishing_value,
    Year_of_depreciation = 1,
    depreciationAsset(Asset_id,T1,T2,Begin_value,End_value,Method,Year_of_depreciation,
        Life,false,_,0,Final_depreciation_value),
    Expected = [[1000,not_in_pool(car123),10000,10044,885.6776881215987,48.21917808219178,40],
        [951.7808219178082,in_pool(car123,general_pool),10044,10213,885.6776881215987,66.10313379620942,15]],
    round_term(Life, Life_r),
    round_term(Expected, Expected_r),
    format(user_error,'~ngot:~n~q',[Life_r]),
    format(user_error,'~nexpected:~n~q',[Expected_r]),
    assertion(terms_with_floats_close_enough(Life, Expected)),
    assertion(floats_close_enough(Final_depreciation_value,114.3223118784012)),
    Correct_end_value is Begin_value - Final_depreciation_value,
    assertion(floats_close_enough(Correct_end_value, End_value)).


test(depreciationAsset_all_life_2, [
	all(x=[x])
]) :- depreciationAsset_all_life_2.



test(depreciationAsset_all_life_3, [
	all(x=[x])
]):-
    Asset_id = car123,
    T1 = 9982,
    T2 = 10050,
    Begin_value = 1000,
    Method = diminishing_value,
    Year_of_depreciation = 1,
    depreciationAsset(Asset_id,T1,T2,Begin_value,End_value,Method,Year_of_depreciation,
        Life,false,_,0,Final_depreciation_value),
    Expected = [[1000,not_in_pool(car123),9982,10044,929.7565772189904,67.94520547945206,40],
        [932.054794520548,in_pool(car123,general_pool),10044,10050,929.7565772189904,2.298217301557515,15]],
    round_term(Life, Life_r),
    round_term(Expected, Expected_r),
    format(user_error,'~ngot:~n~q',[Life_r]),
    format(user_error,'~nexpected:~n~q',[Expected_r]),
	assertion(terms_with_floats_close_enough(Life,Expected)),
    assertion(floats_close_enough(Final_depreciation_value,70.24342278100957)),
    Correct_end_value is Begin_value - Final_depreciation_value,
    assertion(floats_close_enough(End_value,Correct_end_value)).


test(depreciationAsset_only_in_pool_1, [
	all(x=[x])
]):-
    Asset_id = car123,
    T1 = 10000,
    T2 = 10213,
    Begin_value = 1000,
    Method = diminishing_value,
    Year_of_depreciation = 1,
    depreciationAsset(Asset_id,T1,T2,Begin_value,_,Method,Year_of_depreciation,Life,true,
        general_pool,0,Final_depreciation_value),
    Expected = [[1000,not_in_pool(car123),10000,10044,885.6776881215987,48.21917808219178,40],
        [951.7808219178082,in_pool(car123,general_pool),10044,10213,885.6776881215987,66.10313379620942,15]],
    round_term(Life, Life_r),
    round_term(Expected, Expected_r),
    format(user_error,'~ngot:~n~q',[Life_r]),
    format(user_error,'~nexpected:~n~q',[Expected_r]),
    assertion(terms_with_floats_close_enough(Life,Expected)),
    assertion(floats_close_enough(Final_depreciation_value,66.10313379620942)).


test(depreciationAsset_only_in_pool_2,[
	all(x=[x])
]):-
    Asset_id = car456,
    T1 = 9300,
    T2 = 9500,
    Begin_value = 1000,
    Method = diminishing_value,
    Year_of_depreciation = 1,
    depreciationAsset(Asset_id,T1,T2,Begin_value,_,Method,Year_of_depreciation,Life,true,
        general_pool,0,Final_depreciation_value),
    Expected = [[1000,not_in_pool(car456),9300,9313,916.9858087821355,6.678082191780821,18.75],
        [993.3219178082192,in_pool(car456,general_pool),9313,9500,916.9858087821355,76.33610902608369,15]],
    round_term(Life, Life_r),
    round_term(Expected, Expected_r),
    format(user_error,'~ngot:~n~q',[Life_r]),
    format(user_error,'~nexpected:~n~q',[Expected_r]),
    assertion(terms_with_floats_close_enough(Life,Expected)),
    assertion(floats_close_enough(Final_depreciation_value,76.33610902608369)).


test(written_down_value_asset_1):-
    Asset_id = car123,
    asset(Asset_id,Cost,Start_date,_),
    Written_down_date = date(2019,2,2),
    Method = diminishing_value,
    depreciation_between_start_date_and_other_date(Cost,diminishing_value,Start_date,
        Written_down_date,car123,_,1,false,_,0,Total_depreciation_value),
    written_down_value(Asset_id, Written_down_date, Method, _, Written_down_value),
    Remaining_value is Cost - Total_depreciation_value,
    assertion(floats_close_enough(Written_down_value, Remaining_value)).

test(written_down_value_asset_2):-
    Asset_id = car456,
    Written_down_date = date(2019,2,2),
    Method = diminishing_value,
    written_down_value(Asset_id, Written_down_date, Method, _, Written_down_value),
    assertion(floats_close_enough(Written_down_value, 532.9252925877597)).

test(written_down_value_asset_3):-
    Asset_id = car123,
    Written_down_date = date(2017,6,1),
    Method = diminishing_value,
    written_down_value(Asset_id, Written_down_date, Method, _, Written_down_value),
    assertion(floats_close_enough(Written_down_value, 966.027397260274)).

test(written_down_value_asset_4):-
    Asset_id = car456,
    Written_down_date = date(2017,6,1),
    Method = diminishing_value,
    written_down_value(Asset_id, Written_down_date, Method, _, Written_down_value),
    assertion(floats_close_enough(Written_down_value, 958.2641496788701)).

test(written_down_value_asset_5, fail):-
    %It should fail because at this date car456 wasnt purchased yet
    Asset_id = car456,
    Written_down_date = date(2014,6,1),
    Method = diminishing_value,
    written_down_value(Asset_id, Written_down_date, Method, _, Written_down_value),
    assertion(floats_close_enough(Written_down_value, 977.3206045515285)).


/*
test(depreciation_pool_from_start_1):-
	days_from_begin_accounting
    depreciation_pool_from_start(general_pool,date(2019,2,2),diminishing_value,Total_depreciation),
    % From above, the pool should add the depreciation values of each asset for the same period while in pool
    Correct_total_depreciation is 1858.9925927632983,
    assertion(floats_close_enough(Total_depreciation, Correct_total_depreciation)).

test(depreciation_pool_from_start_2):-
    depreciation_pool_from_start(low_value_pool,date(2019,2,2),diminishing_value,Total_depreciation),
    % Not any asset was placed in the low value pool so it should be zero
    Correct_total_depreciation is 0,
    assertion(floats_close_enough(Total_depreciation, Correct_total_depreciation)).

test(depreciation_pool_from_start_3):-
    depreciation_pool_from_start(general_pool,date(2013,2,2),diminishing_value,Total_depreciation),
    % Not any asset was in the pool before this date so it should be zero
    Correct_total_depreciation is 0,
    assertion(floats_close_enough(Total_depreciation, Correct_total_depreciation)).

test(depreciation_pool_from_start_5):-
    depreciation_pool_from_start(general_pool,date(2020,2,2),diminishing_value,Total_depreciation),
    % From above, the pool should add the depreciation values of each asset for the same period while in pool
    Correct_total_depreciation is 2201.2948149343088,
    assertion(floats_close_enough(Total_depreciation, Correct_total_depreciation)).

test(depreciation_pool_between_two_dates_1):-
    depreciation_pool_between_two_dates(general_pool,date(2019,2,2),date(2020,2,2),diminishing_value,Total_depreciation),
    Correct_total_depreciation is 342.3022221710105,
    assertion(floats_close_enough(Total_depreciation, Correct_total_depreciation)).
*/

test(depreciation_between_two_dates_1):-
    Asset_id = car456,
    From_date = date(2017,6,1),
    To_date = date(2019,2,2),
    Method = diminishing_value,
    depreciation_between_two_dates(Asset_id, From_date, To_date, Method, Depreciation_value),
    Correct_depreciation_value is 425.3388570911104,
    assertion(floats_close_enough(Correct_depreciation_value, Depreciation_value)).

test(profit_and_loss_1):-
    Asset_id = car123,
    asset(Asset_id,Asset_cost,Start_date,_),
    Termination_date = date(2019,7,7),
    Termination_value = 500,
    profit_and_loss(Asset_id, Termination_value, Termination_date, _, _,Profit_and_loss),
    depreciation_between_start_date_and_other_date(Asset_cost,diminishing_value,Start_date,
        Termination_date,Asset_id,_,1,false,_,0,Total_depreciation_value),
    Correct_profit_and_loss is Termination_value - (Asset_cost - Total_depreciation_value),
    assertion(floats_close_enough(Correct_profit_and_loss, Profit_and_loss)).

test(profit_and_loss_2, fail):-
    % It should fail since the asset was placed in a general pool that forces a diminishing value method
    Method = prime_cost,
    Asset_id = car123,
    asset(Asset_id,Asset_cost,Start_date,_),
    Termination_date = date(2019,7,7),
    Termination_value = 500,
    profit_and_loss(Asset_id, Termination_value, Termination_date, Method, _,Profit_and_loss),
    depreciation_between_start_date_and_other_date(Asset_cost,Method,Start_date,
        Termination_date,Asset_id,_,1,false,_,0,Total_depreciation_value),
    Correct_profit_and_loss is Termination_value - (Asset_cost - Total_depreciation_value),
    assertion(floats_close_enough(Correct_profit_and_loss, Profit_and_loss)).


:- end_tests(depreciation_computation2).











:- begin_tests(depreciation_between, [setup(cars0), cleanup(ec_cleanup)]).

test(depreciation_between_start_date_and_other_date_while_in_pool_1, [
	all(x=[x])
]):-
    depreciation_between_start_date_and_other_date(1000,diminishing_value,date(2017,6,1),
        date(2019,10,2),car123,_,1,true,general_pool,0,Total_depreciation_value),
    assertion(floats_close_enough(Total_depreciation_value, 546.7114669107006)).

test(depreciation_between_start_date_and_other_date_while_in_pool_2, [
	all(x=[x])
]):-
    depreciation_between_start_date_and_other_date(2000,diminishing_value,date(2015,3,16),
        date(2019,10,2),car456,_,1,true,general_pool,0,Total_depreciation_value),
    assertion(floats_close_enough(Total_depreciation_value, 1556.4080604522424)).


test(depreciation_between_start_date_and_other_date_while_in_pool_3):-
    Asset_id = car123,
    asset(Asset_id,Cost,Start_date,_),
    depreciation_between_start_date_and_other_date(Cost,diminishing_value,Start_date,
        date(2019,2,2),Asset_id,_,1,true,general_pool,0,Total_depreciation_value),
    assertion(floats_close_enough(Total_depreciation_value, 423.32831447468874)).

test(depreciation_between_start_date_and_other_date_while_in_pool_4):-
    Asset_id = car456,
    asset(Asset_id,Cost,Start_date,_),
    depreciation_between_start_date_and_other_date(Cost,diminishing_value,Start_date,
        date(2019,2,2),Asset_id,_,1,true,general_pool,0,Total_depreciation_value),
    assertion(floats_close_enough(Total_depreciation_value, 1435.6642782886095)).

:- end_tests(depreciation_between).




