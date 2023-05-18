import matplotlib.pyplot as plt
import mplcursors
import numpy as np

def read_prices_from_file(file_path):
    prices = []
    with open(file_path, 'r') as file:
        for line in file:
            try:
                price = float(line.strip())
                prices.append(price)
            except ValueError:
                pass
    return prices

def plot_prices(prices):
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.plot(prices)
    ax.set_title('Price History')
    ax.set_xlabel('Run Number')
    ax.set_ylabel('Price')
    ax.grid()

    # Add crosshair
    mplcursors.cursor(hover=True)

    # Customize plot appearance
    plt.tight_layout()

    # Percentage change measurement tool
    def on_click(event):
        if event.button == 1:
            global start_point
            start_point = event.xdata, event.ydata

    def on_release(event):
        if event.button == 1 and start_point is not None:
            end_point = event.xdata, event.ydata
            change_percent = ((end_point[1] - start_point[1]) / start_point[1]) * 100
            ax.annotate(
                f"{change_percent:.2f}%",
                xy=end_point,
                xytext=(end_point[0], end_point[1] + np.sign(change_percent) * 2),
                textcoords='data',
                arrowprops=dict(facecolor='black', arrowstyle="->"),
                fontsize=12,
                bbox=dict(boxstyle="round,pad=0.3", edgecolor="black", facecolor="white", alpha=0.5),
            )
            fig.canvas.draw()

    fig.canvas.mpl_connect("button_press_event", on_click)
    fig.canvas.mpl_connect("button_release_event", on_release)

    plt.show()

if __name__ == "__main__":
    file_path = "/Users/janzimula/Desktop/OB/Orderbook/Orderbook/App/Doc/pricehistory.txt"
    prices = read_prices_from_file(file_path)
    plot_prices(prices)
