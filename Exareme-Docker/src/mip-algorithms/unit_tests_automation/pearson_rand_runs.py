from __future__ import division, print_function

import numpy as np
import json
from scipy.stats import pearsonr

P_VALUE_CUTOFF = 0.001
P_VALUE_CUTOFF_STR = '< ' + str(P_VALUE_CUTOFF)


def main():
    adni = np.genfromtxt(
            '/Users/zazon/Dropbox (Personal)/code/madgik/exareme/Exareme-Docker/src/mip-algorithms/unit_tests/datasets/CSVs/adni.csv',
            delimiter=',', names=True, usecols=range(18, 81)
    )
    num_vars = len(adni.dtype.names)
    sp_results = []
    for _ in range(30):
        ii = np.random.randint(num_vars)
        while True:
            jj = np.random.randint(num_vars)
            if jj != ii:
                break
        x_name = adni.dtype.names[ii]
        y_name = adni.dtype.names[jj]
        x = adni[x_name]
        y = adni[y_name]
        mask = np.array([np.isnan(xi) or np.isnan(yi) for xi, yi in zip(x, y)])
        x = x[~mask]
        y = y[~mask]
        rr = list(pearsonr(x, y))
        if not np.isnan(rr[0]) and not np.isnan(rr[1]):
            if rr[1] < P_VALUE_CUTOFF:
                rr[1] = P_VALUE_CUTOFF_STR
            print(x_name + ' ~ ' + y_name, rr)
            input_data = [
                {
                    "name" : "x",
                    "value": x_name
                },
                {
                    "name" : "y",
                    "value": y_name
                },
                {
                    "name" : "dataset",
                    "value": "adni"
                },
                {
                    "name" : "filter",
                    "value": ""
                },
            ]
            output_data = {
                "pearson correl": rr[0],
                "p value": rr[1]
            }
            sp_results.append({
                "input": input_data,
                "output": output_data
            })
    sp_results = {"sp_results": sp_results}
    with open('test.json', 'w') as f:
        json.dump(sp_results, f)


if __name__ == '__main__':
    main()
