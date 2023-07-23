import json
from typing import List, Dict
import finplot as fplt
import pyqtgraph as pg
from math import nan
import pandas as pd
from PyQt6.QtWidgets import QComboBox, QCheckBox, QWidget, QGridLayout, QLabel
from PyQt6.QtWidgets import QApplication, QMainWindow, QVBoxLayout
import sys

app = QApplication(sys.argv)

# here is where you get price data
def process_dataPrice(file: str, units: int) -> List[Dict[str, float]]:
    with open(file, 'r') as f:
        data = json.load(f)
        
    candles = []
    for i in range(0, len(data), units):
        chunk = data[i:i+units]
        prices = [item['price'] for item in chunk]
        
        if len(prices) == units:
            candle = {
                'open': prices[0],
                'low': min(prices),
                'high': max(prices),
                'close': prices[-1]
            }
            candles.append(candle)
            
    return candles

# here is where you get volume data
def process_dataVolume(file: str, units: int) -> List[Dict[str, float]]:
    with open(file, 'r') as f:
        data = json.load(f)
        
    volumes = []
    for i in range(0, len(data), units):
        chunk = data[i:i+units]
        total_volume = sum(item['volume'] for item in chunk)
        
        if len(chunk) == units:
            volume_data = {
                'volume': total_volume
            }
            volumes.append(volume_data)
            
    return volumes

def create_plot():
    fplt.create_plot(init_zoom_periods=100)

def change_timeframe():
    time_frame = ctrl_panel.timeframe.currentText()

    if time_frame == '1 minute':
        units_per_candle = one_minute_unit
    elif time_frame == '5 minutes':
        units_per_candle = five_minute_unit
    else:
        units_per_candle = fifteen_minute_unit

    process_and_plot_data(units_per_candle)



def process_and_plot_data(units_per_candle):
    # Process and plot price data
    price_data_file = '/Users/janzimula/workspace/marketsim/output/bookInfo.json'
    candles_price = process_dataPrice(price_data_file, units_per_candle)

    # Process and plot volume data
    volume_data_file = '/Users/janzimula/workspace/marketsim/output/bookInfo.json' # Modify this if your volume data is in a different file
    candles_volume = process_dataVolume(volume_data_file, units_per_candle)

    # create dataframes
    df_price = pd.DataFrame(candles_price)
    df_volume = pd.DataFrame(candles_volume)

    # plot price
    fplt.candlestick_ochl(df_price[['open', 'close', 'high', 'low']])

    # plot volume
    fplt.volume_ocv(df_volume['volume'])

def create_ctrl_panel():
    win = QMainWindow()
    layout = QVBoxLayout()

    plot_widget = pg.plot()
    layout.addWidget(plot_widget)

    panel = QWidget()
    panel_layout = QGridLayout(panel)
    panel.timeframe = QComboBox(panel)
    [panel.timeframe.addItem(i) for i in ['1 minute', '5 minutes', '15 minutes']]
    panel_layout.addWidget(QLabel("Timeframe: "), 0, 0)
    panel_layout.addWidget(panel.timeframe, 0, 1)

    panel.timeframe.currentTextChanged.connect(change_timeframe)

    layout.addWidget(panel)
    
    central_widget = QWidget()
    central_widget.setLayout(layout)
    win.setCentralWidget(central_widget)

    return panel, win

# constants
one_minute_unit = 30
five_minute_unit = 150
fifteen_minute_unit = 450

ctrl_panel, win = create_ctrl_panel()

# initialize plot with 1 minute data
process_and_plot_data(one_minute_unit)

# apply dark theme
fplt.apply_theme('black')

# run the app
win.show()
fplt.show()
