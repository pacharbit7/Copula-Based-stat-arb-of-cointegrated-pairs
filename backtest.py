import numpy as np
import pandas as pd
from itertools import accumulate


class Strategy:
    def __init__(self, dict_data, coins, betas, mi, i, alpha1, alpha2):
        self.dict_data = dict_data
        self.betas = betas[i]  # list of 2 elements list of betas for the current week
        self.coins = coins[i]  # list of 2 elements list of the traded coins of the current week
        self.h12 = mi.loc[:, f"week_{i+1}_h12"]  # mispricing index coin 1
        self.h21 = mi.loc[:, f"week_{i+1}_h21"]  # mispricing index coin 2
        self.alpha1 = alpha1  # entry threshold
        self.alpha2 = alpha2  # closing threshold
        self.portfolio = pd.DataFrame(0, index=np.arange(168), columns=coins[i])  # Asset portfolio in quantity of asset for each traded coin of the current week
        self.portfolio_value = pd.DataFrame(0, index=np.arange(168), columns=["value"])  # overall portfolio value in USDT (profit)
        self.portfolio_value.at[0] = 200000  # initial amount of cash
        self.open_signal = None
        self.i = i  # index of the current week (between 0 and 103)
        self.nb_trades = 0  # cumulative number of transactions

    def f_open_signal(self):
        if float(self.h12.iloc[self.j]) < self.alpha1 and float(self.h21.iloc[self.j]) > (1-self.alpha1):
            self.open_signal = "Long2_short1"
        elif float(self.h21.iloc[self.j]) < self.alpha1 and float(self.h12.iloc[self.j]) > (1-self.alpha1):
            self.open_signal = "Long1_short2"
        else:
            self.open_signal = None
        
    def close_signal(self):
        if abs(float(self.h12.iloc[self.j]) - 0.5) < self.alpha2 and abs(float(self.h21.iloc[self.j]) - 0.5) < self.alpha2:
            return "close_both_positions"
        
    def go_Long1_short2(self):
        self.portfolio.at[self.j, self.coins[0]] = self.betas[0]
        self.portfolio.at[self.j, self.coins[1]] = -self.betas[1]
        self.nb_trades += 2

    def go_Long2_short1(self):
        self.portfolio.at[self.j, self.coins[0]] = -self.betas[0]
        self.portfolio.at[self.j, self.coins[1]] = self.betas[1]
        self.nb_trades += 2

    def maj_portfolio_value(self):
        # Update both portfolios assets quantities and profit
        previous_portfolio_value = self.portfolio_value.loc[self.j - 1]
        total_return = 0
        for coin in self.coins:
            r = self.calculate_returns(coin)
            total_return += r/2
            self.portfolio.at[self.j, coin] = self.portfolio.at[self.j - 1, coin]
        self.portfolio_value.at[self.j] = (1 + total_return) * previous_portfolio_value

    def close(self):
        # Close positions
        self.maj_portfolio_value()
        self.portfolio.at[self.j, self.coins[0]] = 0
        self.portfolio.at[self.j, self.coins[1]] = 0
        self.nb_trades += 2

    def calculate_returns(self, coin):
        # Compute current hourly returns for a coin
        previous_value = self.portfolio.loc[self.j - 1, coin] * self.dict_data[coin]['Close'].iloc[504 + self.i*168 + self.j - 1]
        current_value = self.portfolio.loc[self.j - 1, coin] * self.dict_data[coin]['Close'].iloc[504 + self.i*168 + self.j]
        r = (current_value - previous_value)/abs(previous_value)
        return r

    def apply_strategy(self):
        # If there is no current open position, open it if there is a trading signal
        if self.open_signal is None:
            self.f_open_signal()
            if self.open_signal == "Long1_short2":
                self.go_Long1_short2()
                if self.j != 0:
                    self.portfolio_value.at[self.j, 'value'] = self.portfolio_value.at[self.j - 1, 'value']
            elif self.open_signal == "Long2_short1":
                self.go_Long2_short1()
                if self.j != 0:
                    self.portfolio_value.at[self.j, 'value'] = self.portfolio_value.at[self.j - 1, 'value']
            else:
                if self.j != 0:
                    self.portfolio_value.at[self.j, 'value'] = self.portfolio_value.at[self.j - 1, 'value']

        # If there is an open position, check if the current signal tell us to close trades,
        # otherwise update portfolio value
        else:
            signal = self.close_signal()
            if signal == "close_both_positions":
                self.close()
                self.open_signal = None
            else:
                self.maj_portfolio_value()

    def evaluate_strategy(self):
        # evaluate the strategy every week in applying the strategy for its 168 hours
        for j in range(168):
            self.j = j
            self.apply_strategy()


def run(dict_data, coins, betas, mi, alpha1, alpha2):
    lst_return, returns = list(), list()
    nb_trades, total_return = 0, 0
    for i in range(104):
        if coins[i]:
            strategy = Strategy(dict_data, coins, betas, mi, i, alpha1, alpha2)
            strategy.evaluate_strategy()
            return_week = strategy.portfolio_value.values.tolist()
            last_return = float(return_week[-1][0] - 200000)
            nb_trades += strategy.nb_trades
        else:
            return_week = [[200000] for k in range(168)]
            last_return = 0
        lst_return.append(return_week)
        returns.append(last_return)
        total_return += last_return
    return lst_return, total_return, list(accumulate(returns)), nb_trades
