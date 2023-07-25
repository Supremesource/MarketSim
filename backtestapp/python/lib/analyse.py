import json
import numpy as np

# read the data
with open("/Users/janzimula/workspace/marketsim/backtestapp/python/data/sumDataUnits.json", "r") as f:
    data = json.load(f)

# prepare lists for the quantities we are interested in
error_x = []  # MAE of first_split with respect to x
error_y = []  # MAE of second_split with respect to y
total_x = []  # total X values
total_y = []  # total Y values

for record in data:
    split = (record["overal_open_interest_pos"] + record["overal_volume_pos"]) / 2
    x_error = abs(record["total_x_pos_amount"] - split)
    y_error = abs(record["total_y_pos_amount"] - split)
    error_x.append(x_error)
    error_y.append(y_error)
    total_x.append(record["total_x_pos_amount"])
    total_y.append(record["total_y_pos_amount"])

# calculate average error
average_error_x = np.mean(error_x)
average_error_y = np.mean(error_y)

# calculate average total
average_total_x = np.mean(total_x)
average_total_y = np.mean(total_y)

# calculate percentage error
percentage_error_x = (average_error_x / average_total_x) * 100
percentage_error_y = (average_error_y / average_total_y) * 100

# print results
print(f"The average error of the split with respect to X is {average_error_x}, which is {percentage_error_x:.2f}% of the average total X.")
print(f"The average error of the split with respect to Y is {average_error_y}, which is {percentage_error_y:.2f}% of the average total Y.")
