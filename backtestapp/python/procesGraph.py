import json
from typing import List, Dict
from datetime import date
import finplot as fplt
import pandas as pd
import pyqtgraph as pg

def process_dataPrice(file: str, units: int) -> List[Dict[str, float]]:
    with open(file, 'r') as f:
        data = json.load(f)
        
    candles = []
    for i in range(0, len(data), units):
        chunk = data[i:i+units]
        prices = [item['price_book'] for item in chunk]
        
        if len(prices) == units:
            candle = {
                'open': prices[0],
                'low': min(prices),
                'high': max(prices),
                'close': prices[-1]
            }
            candles.append(candle)
            
    return candles

def write_dataPrice(file: str, data: List[Dict[str, float]]):
    with open(file, 'w') as f:
        json.dump(data, f, indent=4)


def process_dataVolume(file: str, units: int) -> List[Dict[str, float]]:
    with open(file, 'r') as f:
        data = json.load(f)
        
    volumes = []
    for i in range(0, len(data), units):
        chunk = data[i:i+units]
        total_volume = sum(item['volume_amount_book'] for item in chunk)
        
        if len(chunk) == units:
            volume_data = {
                'open': chunk[0]['volume_amount_book'],
                'close': chunk[-1]['volume_amount_book'],
                'total_volume': total_volume
            }
            volumes.append(volume_data)
            
    return volumes

def write_dataVolume(file: str, data: List[Dict[str, float]]):
    with open(file, 'w') as f:
        json.dump(data, f, indent=4)


# add the dark_mode_toggle function
def dark_mode_toggle(dark):
    # first set the colors we'll be using
    if dark:
        fplt.foreground = '#777'
        fplt.background = '#090c0e'
        fplt.candle_bull_color = fplt.candle_bull_body_color = '#0b0'
        fplt.candle_bear_color = '#a23'
        volume_transparency = '6'
    else:
        fplt.foreground = '#444'
        fplt.background = fplt.candle_bull_body_color = '#fff'
        fplt.candle_bull_color = '#380'
        fplt.candle_bear_color = '#c50'
        volume_transparency = 'c'
    fplt.volume_bull_color = fplt.volume_bull_body_color = fplt.candle_bull_color + volume_transparency
    fplt.volume_bear_color = fplt.candle_bear_color + volume_transparency
    fplt.cross_hair_color = fplt.foreground+'8'
    fplt.draw_line_color = '#888'
    fplt.draw_done_color = '#555'

    pg.setConfigOptions(foreground=fplt.foreground, background=fplt.background)

    # window background
    for win in fplt.windows:
        win.setBackground(fplt.background)

    # axis, crosshair, candlesticks, volumes
    axs = [ax for win in fplt.windows for ax in win.axs]
    vbs = set([ax.vb for ax in axs])
    axs += fplt.overlay_axs
    axis_pen = fplt._makepen(color=fplt.foreground)
    for ax in axs:
        ax.axes['right']['item'].setPen(axis_pen)
        ax.axes['right']['item'].setTextPen(axis_pen)
        ax.axes['bottom']['item'].setPen(axis_pen)
        ax.axes['bottom']['item'].setTextPen(axis_pen)
        if ax.crosshair is not None:
            ax.crosshair.vline.pen.setColor(pg.mkColor(fplt.foreground))
            ax.crosshair.hline.pen.setColor(pg.mkColor(fplt.foreground))
            ax.crosshair.xtext.setColor(fplt.foreground)
            ax.crosshair.ytext.setColor(fplt.foreground)
        for item in ax.items:
            if isinstance(item, fplt.FinPlotItem):
                isvolume = ax in fplt.overlay_axs
                if not isvolume:
                    item.colors.update(
                        dict(bull_shadow      = fplt.candle_bull_color,
                             bull_frame       = fplt.candle_bull_color,
                             bull_body        = fplt.candle_bull_body_color,
                             bear_shadow      = fplt.candle_bear_color,
                             bear_frame       = fplt.candle_bear_color,
                             bear_body        = fplt.candle_bear_color))
                else:
                    item.colors.update(
                        dict(bull_frame       = fplt.volume_bull_color,
                             bull_body        = fplt.volume_bull_body_color,
                             bear_frame       = fplt.volume_bear_color,
                             bear_body        = fplt.volume_bear_color))
                item.repaint()

def main():
    units_per_candle = 1  # change this to the desired units per candle

    # Process and write price data
    price_data_file = '/Users/janzimula/workspace/marketsim/output/bookInfo.json'
    candles_price = process_dataPrice(price_data_file, units_per_candle)
    output_price_file = '/Users/janzimula/workspace/marketsim/backtestapp/python/data/assetPriceData.json'
    write_dataPrice(output_price_file, candles_price)

    # Process and write volume data
    volume_data_file = '/Users/janzimula/workspace/marketsim/output/bookInfo.json' # Modify this if your volume data is in a different file
    candles_volume = process_dataVolume(volume_data_file, units_per_candle)
    output_volume_file = '/Users/janzimula/workspace/marketsim/backtestapp/python/assetVolumeData.json'
    write_dataVolume(output_volume_file, candles_volume)

    output_price_file = '/Users/janzimula/workspace/marketsim/backtestapp/python/data/assetPriceData.json'
    df_price = pd.read_json(output_price_file)

    output_volume_file = '/Users/janzimula/workspace/marketsim/backtestapp/python/assetVolumeData.json'
    df_volume = pd.read_json(output_volume_file)

    # Adding open and close to volume data to match the example
    df_volume['open'] = df_price['open']
    df_volume['close'] = df_price['close']

    # Plotting
    now = date.today().strftime('%Y-%m-%d')

    # Set a mock timestamp index (since we don't have real timestamps in your data)
    df_price.index = pd.date_range(start='2023-07-25', periods=len(df_price))
    df_volume.index = pd.date_range(start='2023-07-25', periods=len(df_volume))

    # plot price
    ax,ax2 = fplt.create_plot('Kaspers BTC -%s'%now.split('-')[0], rows=2)
    fplt.candlestick_ochl(df_price[['open', 'close', 'high', 'low']], ax=ax)

    # plot volume
    fplt.volume_ocv(df_volume[['open','close','total_volume']], ax=ax2)
    fplt.plot(df_volume.total_volume.ewm(span=24).mean(), ax=ax2, color=1)
    
    # monthly separator lines
    months = df_price.index.strftime('%m') 
    last_month = ''
    for x,(month,price) in enumerate(zip(months, df_price.close)):
        if month != last_month:
            fplt.add_line((x-0.5, price*0.5), (x-0.5, price*2), color='#bbb', style='--')
        last_month = month
    
    fplt.candle_bull_color = (204, 204, 204, 100)
    fplt.candle_bear_color = (204, 204, 204, 100)
    dark_mode_toggle(True)
    fplt.show()

if __name__ == "__main__":
    main()