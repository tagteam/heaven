# heaven

`heaven` provides fast, memory-conscious tools for preparing large Danish
administrative and health registry datasets. It was developed for analysis in
the secure research environment at Statistics Denmark and uses `data.table`
throughout its core workflows.

The package includes tools for:

- importing selected variables and records from SAS datasets;
- splitting follow-up time into analysis intervals;
- identifying conditions from ICD, ATC, and other code lists;
- deriving prescription exposure periods;
- incidence-density and exposure-density matching;
- direct standardisation and common registry-data summaries; and
- inspecting Parquet datasets without collecting them in full.

## Installation

Until the package is released on CRAN, install the development version from
GitHub:

```r
# install.packages("remotes")
remotes::install_github("tagteam/heaven")
```

## Example

```r
library(heaven)
library(data.table)

set.seed(7)
population <- simPop(1000)
population[]
```

Most functions accept or return `data.table` objects. Because `data.table` can
modify objects by reference, use `data.table::copy()` before a call when the
original object must remain unchanged.

The SAS import helpers require access to a local or remote SAS installation.
The Parquet viewer uses the optional `arrow` and `dplyr` packages.

## Documentation and support

See `vignette("R_on_DST", package = "heaven")` for an introduction to working
with large datasets at Statistics Denmark. Report bugs or request features at
<https://github.com/tagteam/heaven/issues>.

## License

GPL (>= 3)
