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

