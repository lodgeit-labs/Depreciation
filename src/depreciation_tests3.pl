:- use_module(event_calculus,[
	depreciation_value/6,
	depreciation_rate/6,
	depreciationAsset/12,
	days_from_begin_accounting/2,
	assert_asset/4,
	assert_event/2,
	asset/4,
	ec_cleanup/0
]).

:- use_module(depreciation_computation,[
    depreciation_between_start_date_and_other_date/11,
    depreciation_pool_from_start/4,
    depreciation_pool_between_two_dates/5,
    written_down_value/5,
    depreciation_between_two_dates/5,
    profit_and_loss/6
]).

car123_2 :-
	days_from_begin_accounting(date(2017,05,01), D1),
	assert_asset(car123,1000,D1,5),
/*delete this whole?*/
	days_from_begin_accounting(date(2017,6,1),D3),
	assert_event(transfer_asset_to_pool(car123,general_pool),D3),

	days_from_begin_accounting(date(2020,7,31),D5),
	assert_event(remove_asset_from_pool(car123,general_pool),D5),
	true.


:- begin_tests(depreciation_computation3, [setup(car123_2), cleanup(ec_cleanup)]).


test(depreciation_between_start_date_and_other_date_all_life,[
	all(x=[x]),
	blocked('can only add asset to pool at beginning of income year')
]):-
    /*
    asset: car123, start depreciation date: 2017-5-1, cost: 1000, effective life years defined: 5
    Method: diminishing value
    Transfered to general pool in: 2017-6-1
    Removed from general pool in: 2020-7-31
    1st income year from 2017-5-1 to 2017-6-30
    2017-5-1 to 2017-6-1 depreciated as an individual asset: 32 days
    1000 * 32/365 * 2/5 = 35,068493151
    2017-6-2 to 2017-7-1 depreciated in the general pool: 29 days
    (1000-35,068493151)*29/365*0.15 = 11,499868643
    2nd income year from 2017-7-1 to 2018-7-1:365 days, rate is 0.3 from second year on
    (1000-35,068493151-11,499868643)* 365/365 * 0.3 = 286,029491462
    3rd income year from 2018-7-1 to 2019-7-1:365 days
    (1000-35,068493151-11,499868643-286,029491462)* 365/365 * 0.3 = 200,220644023
    4th income year from 2019-7-1 to 2019-10-2: 93 days(shortened due to the To_date defined)
    (1000-35,068493151-11,499868643-286,029491462-200,220644023) * 93/365 * 0.3 = 35,710586098
    Total depreciation value is 35,068493151 + 11,499868643+ 286,029491462 + 200,220644023 +
    35,710586098 =568,529083377
    */
    depreciation_between_start_date_and_other_date(1000,diminishing_value,date(2017,5,1),
        date(2019,10,2),car123,_,1,false,_,0,Total_depreciation_value),
    assertion(floats_close_enough(Total_depreciation_value, 577.6746187406319)).





:- end_tests(depreciation_computation3).

