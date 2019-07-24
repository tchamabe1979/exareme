import numpy as np
import json
import csv
from sklearn.linear_model import LogisticRegression
from scipy.special import expit

NUM_RUNS = 100
P_VALUE_CUTOFF = 0.001
P_VALUE_CUTOFF_STR = '< ' + str(P_VALUE_CUTOFF)


def main():
    adni = np.genfromtxt(
            '/Users/zazon/Dropbox (Personal)/code/madgik/exareme/Exareme-Docker/src/mip-algorithms/unit_tests/datasets/CSVs/adni.csv',
            delimiter=',', names=True, usecols=range(18, 81)
    )
    dataset = np.array([adni[var_name] for var_name in adni.dtype.names]).T
    vmask = np.isnan(dataset).any(axis=0)
    dataset = dataset[:, ~vmask]
    hmask = np.isnan(dataset).any(axis=1)
    dataset = dataset[~hmask]
    num_cols = dataset.shape[1]
    all_names = np.array(adni.dtype.names)[~vmask]

    results = []
    count = 0
    y_columns = []
    while count < NUM_RUNS:
        num_covars = np.random.randint(3, 15)
        covar_idx = np.random.permutation(num_cols)[:num_covars]
        X = dataset[:, covar_idx]
        X = np.insert(X, 0, values=1, axis=1)

        if X.shape[0] != 0:
            covar_names = [all_names[i] for i in covar_idx]
            covar_names.insert(0, '(Intercept)')
            weights = np.random.randn(num_covars + 1)
            # Generate y using logit
            h = np.dot(X, weights)
            prob = expit(h)
            y = np.random.binomial(1, prob)
            if X.shape[0] * 0.8 > sum(y) > X.shape[0] * 0.2:
                y = np.array(['a' if yi < 0.5 else 'b' for yi in y])
                y_columns.append(y)
                clf = LogisticRegression(random_state=0, solver='newton-cg', multi_class = 'multinomial',
                                         penalty='none', fit_intercept=False).fit(X, y)
                print(clf.coef_)
                input_data = [
                    {
                        "name" : "x",
                        "value": ",".join(covar_names[1:])
                    },
                    {
                        "name" : "y",
                        "value": 'y' + str(count)
                    },
                    {
                        "name" : "dataset",
                        "value": "data_logisticRegression"
                    },
                    {
                        "name" : "filter",
                        "value": ""
                    }
                ]
                output_data = {
                    'Covariates': [
                        {
                            'Variable': covar_names[i],
                            'Coefficient': clf.coef_[0][i]
                        }
                        for i in range(len(covar_names))
                    ]
                }

                results.append({
                    "input" : input_data,
                    "output": output_data
                })
                count += 1
    y_columns = np.array(y_columns).T
    data = np.concatenate((dataset, y_columns), axis=1)
    print(data.shape)
    results = {"results": results}
    with open('logregr_runs.json', 'w') as f:
        json.dump(results, f)

    with open('logregr_dataset.csv', 'w') as f:
        writer = csv.writer(f)
        writer.writerow(list(all_names) + ['y' + str(i) for i in range(NUM_RUNS)])
        writer.writerows(data)

if __name__ == '__main__':
    main()
