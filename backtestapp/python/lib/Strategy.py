import json
from collections import defaultdict

def sum_units(unit_count):
    # define the keys you are interested in
    keys = ["total_x_pos_amount", "total_y_pos_amount", "total_z_pos_amount", 
            "total_f_pos_amount", "overal_open_interest_pos", "overal_volume_pos"]

    # load the json file
    with open("/Users/janzimula/workspace/marketsim/output/positionInfo.json", "r") as f:
        data = json.load(f)

    # initialize a defaultdict to accumulate the sums
    sums = defaultdict(int)
    
    # initialize a list to store all sums
    all_sums = []

    # sum over units for each key
    for i, record in enumerate(data, start=1):
        for key in keys:
            sums[key] += record.get(key, 0)
    
        # If we have summed 'unit_count' records, store the results and reset the sums
        if i % unit_count == 0:
            # Append the sums to all_sums and reset the sums for the next batch
            all_sums.append(sums)
            sums = defaultdict(int)

    # If there were less than 'unit_count' records in the last batch, store those sums too
    if i % unit_count != 0:
        all_sums.append(sums)

    # Write all sums to a single file
    with open("/Users/janzimula/workspace/marketsim/backtestapp/python/data/sumDataUnits.json", "w") as f:
        json.dump(all_sums, f, indent=4)  # add indent=4 for pretty print

# call the function with desired unit count
sum_units(50)

# read back the script at `/Users/janzimula/workspace/marketsim/backtestapp/python/data/sumDataUnits.json`
# do overal open interest + volume and split it to 1:1 call it first_split_open second_split_open
# compare to total_x_pos_amount and total_y_pos_amount and map the statistical difference from the split from the real x and y
# so to wrap it up, read the sumDataUnits.json, do the split, compare the split to the real x and y and map the difference, so at the end you can say something like 70% correlation between the split and the real x and y