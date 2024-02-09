# Copula-Based Trading of Cointegrated Cryptocurrency Pairs

## Utilisation 

final_code.py : load, process data and compute spreads for each week of the two selected coins.
save_selected_spreads function save in a csv file these spread realisations.
call_backtest function apply the strategy on the mispricing indeces for all pairs and weeks.

copule.r : take spreads in input and compute mispricing index based on best fitted copulas and marginals
it produce trading_signal.csv that contains mispricing indeces

backtest.py : is called from final_code, apply the strategy and calculate returns

performance_metrics.py : compute other performance metrics on returns and price historical data 
