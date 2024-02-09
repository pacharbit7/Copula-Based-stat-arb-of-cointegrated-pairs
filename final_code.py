import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
from scipy.stats import linregress, skew, kurtosis
from heapq import nlargest
from statsmodels.tsa.stattools import adfuller
import statsmodels.api as sm
from backtest import run
from performance_metrics import PerformanceMetrics

coins = ["ADA", "ATOM", "BAT", "BCH", "BNB", "BTC", "DASH", "EOS", "ETC", "ETH", "IOTA", "LINK", "LTC", "ONT", "TRX",
         "XLM", "XMR", "XRP", "XTZ", "ZEC"]


def load_data(coin):
    data = pd.read_csv(filepath_or_buffer=f"Data/KLINE_INTERVAL_1HOUR_FUT_{coin}USDT.csv", sep=",", encoding="UTF-8",
                       index_col=False)
    data = data.drop("Unnamed: 0", axis=1)
    data['Open time'] = pd.to_datetime(data['Open time'], unit="ms")
    data['Close time'] = pd.to_datetime(data['Close time'], unit="ms")
    return data


def load_all_data(coins):
    dict_data = dict()
    for coin in coins:
        dict_data[coin] = load_data(coin=coin)
    return dict_data


def get_stats(dict_data):
    dict_stats = dict()
    for key, value in dict_data.items():
        dict_stats[key] = dict()
        dict_stats[key]["mean"] = round(np.mean(dict_data[key]["Close"]), 2)
        dict_stats[key]["return"] = round((dict_data[key]["Close"].iloc[-1] / dict_data[key]["Open"].iloc[0] - 1)*100, 2)
        dict_stats[key]["volatility"] = round(np.diff(np.log(dict_data[key]["Close"])).std()*np.sqrt(17780)*100, 2)
        dict_stats[key]["skew"] = skew(dict_data[key]["Close"] - dict_data[key]["Open"])
        dict_stats[key]["kurt"] = kurtosis(dict_data[key]["Close"] - dict_data[key]["Open"])
    return dict_stats


def cointegration_test(coin, dict_data, begin, end):
    y1 = dict_data["BTC"]["Close"][begin:end]
    y2 = dict_data[coin]["Close"][begin:end]
    model = sm.OLS(y1, y2).fit()
    beta = model.params[0]
    spread = y1 - beta * y2
    adf_result = adfuller(spread, maxlag=6, autolag=None, regression='n')
    return round(adf_result[1], 3), np.array(spread), beta


def cointegration_test_KSS(coin, dict_data, begin, end):
    y1 = dict_data["BTC"]["Close"][begin:end]
    y2 = dict_data[coin]["Close"][begin:end]
    model = sm.OLS(y1, y2).fit()
    beta = model.params[0]
    spread = model.resid
    spread_lagged = spread.iloc[:-1]
    spread_lagged_cubed = spread_lagged ** 3
    ds = spread.diff().iloc[1:]
    model = sm.OLS(np.array(ds), np.array(spread_lagged_cubed)).fit()
    t_stat = model.tvalues[0]
    return round(t_stat, 3), np.array(spread), beta


def kendall_tau(coin, dict_data, begin, end):
    log_return_btc = np.log(dict_data["BTC"]["Close"][begin:end] / dict_data["BTC"]["Close"][begin:end].shift(1))
    log_return_coin = np.log(dict_data[coin]["Close"][begin:end] / dict_data[coin]["Close"][begin:end].shift(1))
    # Supprimer les NaN qui résultent de la première différence
    log_return_btc = log_return_btc.dropna()
    log_return_coin = log_return_coin.dropna()
    # Appliquer le test de Kendall sur les séries de rendements
    res = stats.kendalltau(log_return_btc, log_return_coin)
    return res


def select_coins(coins, dict_data, begin, end):
    ranked_coins = dict()
    spreads = dict()
    beta = dict()
    for i in range(len(coins)):
        if "BTC" != coins[i]:
            pval1, spread1, beta1 = cointegration_test(coins[i], dict_data, begin, end)
            if pval1 < 0.1:
                ranked_coins[f"{coins[i]}"] = kendall_tau(coins[i], dict_data, begin, end)
                spreads[f"{coins[i]}"] = spread1
                beta[f"{coins[i]}"] = beta1
    selected_coins = nlargest(2, ranked_coins, key=ranked_coins.get)
    selected_spreads = [spreads[coin] for coin in selected_coins]
    selected_beta = [beta[coin] for coin in selected_coins]
    if len(selected_coins) != 2:
        selected_coins, selected_spreads, selected_beta = [], [], []
    return selected_coins, selected_spreads, selected_beta


def select_coins_all_period(coins, dict_data):
    n = 168  # nb of hours in a week
    lst_selected_coins, lst_selected_spreads, lst_selected_beta = list(), list(), list()
    for i in range(104):
        selected_coins, selected_spreads, selected_beta = select_coins(coins=coins, dict_data=dict_data, begin=n * i, end=(3 + i) * n)
        lst_selected_coins.append(selected_coins)
        lst_selected_spreads.append(selected_spreads)
        lst_selected_beta.append(selected_beta)
    return lst_selected_coins, lst_selected_spreads, lst_selected_beta


def save_selected_spreads(coins, path):
    # save the selected spreads for the whole period (training and trading)
    n = 168  # nb of hours in a week
    df = pd.DataFrame()
    dict_data = load_all_data(coins=coins)
    coins, spreads, betas = select_coins_all_period(coins, dict_data)
    for i in range(104):
        if coins[i]:
            y1 = dict_data["BTC"]["Close"][n * i:(4 + i) * n]
            y2 = dict_data[coins[i][0]]["Close"][n * i:(4 + i) * n]
            y3 = dict_data[coins[i][1]]["Close"][n * i:(4 + i) * n]
            spread1 = y1 - betas[i][0] * y2
            spread2 = y1 - betas[i][0] * y3
            df[f"week_{i + 1}_coin_1"] = np.array(spread1)
            df[f"week_{i + 1}_coin_2"] = np.array(spread2)
    df.to_csv(path_or_buf=path, sep=";", encoding='utf-8')


def load_trading_signals(path_filename):
    mi = pd.read_csv(path_filename, sep=";", encoding='utf-8')
    mi = mi.drop("Unnamed: 0", axis=1)
    mi = mi.astype(str)
    mi = mi.applymap(lambda x: str(x.replace(',', '.')))
    return mi


def call_backtest(coins, signals_file, alpha1, alpha2=0.1):
    dict_data = load_all_data(coins=coins)
    selected_coins, _, selected_betas = select_coins_all_period(coins=coins, dict_data=dict_data)
    mi = load_trading_signals(path_filename=signals_file)
    lst_return, total_return, returns, nb_trades = run(dict_data=dict_data, coins=selected_coins, betas=selected_betas, mi=mi, alpha1=alpha1, alpha2=alpha2)
    print(f"total return : {round(total_return / 2000, 2)}%")
    print("Nombre de transactions : ", nb_trades)
    amount_fee = nb_trades * 100000 * 0.0004
    print("Fees amount : ", amount_fee)
    print("Transaction Costs over Gross P&L : ", round(amount_fee * 100 / total_return, 1), "%")
    return returns


def plot_returns(returns):
    dict_data = load_all_data(coins=coins)
    plt.plot(dict_data["BTC"]["Open time"][505:], [item for item in returns for _ in range(168)])
    plt.show()


# print(get_stats(dict_data=load_all_data(coins=coins)))
# save_selected_spreads(coins=coins, path="./spreads_selected_ADF.csv")
# plot_returns(returns=call_backtest(coins=coins, signals_file="trading_signal_ADF.csv", alpha1=0.1))
# returns = call_backtest(coins=coins, signals_file="trading_signal_ADF.csv", alpha1=0.1)
# df = pd.DataFrame({'Value': returns}) + 200000
# PerformanceMetrics.stat_dashboard(df)
