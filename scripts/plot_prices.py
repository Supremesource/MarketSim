import os
import webbrowser
from bokeh.plotting import figure, show
from bokeh.io import output_file
from bokeh.models import Span, FreehandDrawTool, ColumnDataSource
from bokeh.models.tools import HoverTool, BoxZoomTool, PanTool, ResetTool, SaveTool, FullscreenTool
from bokeh.embed import file_html
from bokeh.resources import CDN


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
    # output to static HTML file
    output_file("prices.html", mode="inline", root_dir=None)

    # create a new plot with a title and axis labels
    p = figure(title="Price History", x_axis_label='Run Number', y_axis_label='Price', sizing_mode="stretch_both")

    # add a line renderer with legend and line thickness
    p.line(list(range(len(prices))), prices, legend_label="Price", line_width=2)

    # Adding tools for interaction
    hover = HoverTool(tooltips=[('Run Number', '$index'), ('Price', '$y')])
    p.add_tools(hover, BoxZoomTool(), PanTool(), ResetTool(), SaveTool(), FullscreenTool())

    # Highlight maximum and minimum price in the graph
    max_price = max(prices)
    min_price = min(prices)
    max_span = Span(location=max_price, dimension='width', line_color='green', line_width=3)
    min_span = Span(location=min_price, dimension='width', line_color='red', line_width=3)
    p.add_layout(max_span)
    p.add_layout(min_span)

    # Freehand drawing on plot
    source = ColumnDataSource(data=dict(xs=[[]], ys=[[]]))
    draw_tool = FreehandDrawTool(renderers=[p.multi_line(xs='xs', ys='ys', source=source)], num_objects=3)
    p.add_tools(draw_tool)
    p.toolbar.active_drag = draw_tool

    # Generate HTML file
    html = file_html(p, CDN, "Price History")
    with open('prices.html', 'w') as f:
        f.write(html)

    # Open the generated HTML file in the browser
    webbrowser.open('file://' + os.path.realpath('prices.html'))


if __name__ == "__main__":
    file_path = "output/pricehistory.txt"
    prices = read_prices_from_file(file_path)
    plot_prices(prices)
