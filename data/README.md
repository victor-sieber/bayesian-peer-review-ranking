# Data availability

The row-level analytical dataset used in this project is pseudonymized and is
not stored directly in this Git repository.

During project development, the dataset remains in approved local/UZH storage
and is excluded from Git.

Following final CRS review and authorization, the pseudonymized analytical
dataset archived separately on Zenodo

- [Zenodo record](https://doi.org/10.5281/zenodo.22808415)

The existing analysis scripts expect the dataset locally under the filename

```text
data/evaluation_anonymized.csv
```

The filename is retained for compatibility with the existing analysis workflow.
The data are more accurately described as pseudonymized because the
re-identification key is stored separately and is not part of this repository,
the analytical dataset release or the analysis workflow.

Instructions for reproducing the analysis after obtaining the dataset are
provided in

[`../REPRODUCIBILITY.md`](../REPRODUCIBILITY.md).