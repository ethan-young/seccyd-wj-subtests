Data
================

*Last updated on Wednesday, April 26, 2023 at 11:38 AM*

This directory stores `Rdata` files containing processed data.

## Scripts Producing Analysis Data Sets

The table below contain references to the scripts that produced each:

| Script                                                                                                                   | Data Output             |
|:-------------------------------------------------------------------------------------------------------------------------|:------------------------|
| [1-compile-dvs.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/1-compile-dvs.R)                 | seccyd-dvs-wj.Rdata     |
| [1-compile-ivs-census.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/1-compile-ivs-census.R)   | seccyd-ivs-census.Rdata |
| [1-compile-ivs-demo.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/1-compile-ivs-demo.R)       | seccyd-ivs-demo.Rdata   |
| [1-compile-ivs-family.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/1-compile-ivs-family.R)   | seccyd-ivs-family.Rdata |
| [1-compile-ivs-phone.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/1-compile-ivs-phone.R)     | seccyd-ivs-phone.Rdata  |
| [1-compile-ivs-raw.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/1-compile-ivs-raw.R)         | ivs-data1.Rdata         |
| [2-aggregate-dvs.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/2-aggregate-dvs.R)             | analysis-dvs.Rdata      |
| [2-merge-aggregate-ivs.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/2-merge-aggregate-ivs.R) | analysis-ivs.Rdata      |
| [3-primary-analysis.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/3-primary-analysis.R)       | primary-results.Rdata   |

## Scripts Using Processed Data

The table below shows the scripts that use processed datasets and, for
each, which dataset is loaded.

| Script                                                                                                                   | Data Input                                                                                                                                     |
|:-------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------|
| [1-compile-ivs-raw.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/1-compile-ivs-raw.R)         | seccyd-ivs-phone.Rdata<br>seccyd-ivs-demo.Rdata                                                                                                |
| [2-aggregate-dvs.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/2-aggregate-dvs.R)             | seccyd-dvs-wj.Rdata                                                                                                                            |
| [2-merge-aggregate-ivs.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/2-merge-aggregate-ivs.R) | analysis-ivs.Rdata<br>ivs-data1.Rdata<br>seccyd-ivs-census.Rdata<br>seccyd-ivs-demo.Rdata<br>seccyd-ivs-family.Rdata<br>seccyd-ivs-phone.Rdata |
| [3-primary-analysis.R](https://github.com/ethan-young/seccyd-wj-subtests/tree/master/scripts/3-primary-analysis.R)       | analysis-dvs.Rdata<br>analysis-ivs.Rdata                                                                                                       |

## List of Raw SECCYDS Datasets

To faciliate full computational reproducibilty for those who have access
to raw SECCYD data, we created a table of raw dataset names from the
SECCYD repository. Anyone wishing to reproduce
data/results/figures/tables must save these raw datasets in a directory
outside their project root directory.

| Variable Type | Raw SECCYD Data Name |
|:--------------|:---------------------|
| DVs           | flv29x5a             |
| DVs           | flv6g1_a             |
| DVs           | flv7g1_a             |
| DVs           | flv13g3a             |
| DVs           | flv14g3a             |
| DVs           | f55k54_a             |
| DVs           | f55l54_a             |
| DVs           | flv38g5a             |
| Census IVs    | cens1990             |
| Census IVs    | resgrid              |
| Family IVs    | f02a                 |
| Family IVs    | f04a03               |
| Family IVs    | f04a09               |
| Family IVs    | f04a12               |
| Family IVs    | f06a                 |
| Family IVs    | f15a15               |
| Family IVs    | f15a24               |
| Family IVs    | f15a36               |
| Family IVs    | f18a21               |
| Family IVs    | f18a27               |
| Family IVs    | f18a30               |
| Family IVs    | f18a33               |
| Family IVs    | f40a40_f             |
| Family IVs    | f42a42_k             |
| Family IVs    | f46a46_k             |
| Family IVs    | f46a50_n             |
| Family IVs    | f54a54               |
| Family IVs    | f60a60               |
| Family IVs    | demo_k               |
| Family IVs    | demo1                |
| Family IVs    | demo15               |
| Family IVs    | demo24               |
| Family IVs    | demo36               |
| Family IVs    | demo54               |
| Family IVs    | demo6                |
| Family IVs    | demog1               |
| Family IVs    | demog2               |
| Family IVs    | demog3               |
| Family IVs    | demog4               |
| Family IVs    | demog5               |
| Family IVs    | demog6               |
| Family IVs    | demox4               |
| Family IVs    | demox5               |
| Family IVs    | f18a18               |
| Family IVs    | fam_k                |
| Family IVs    | fam1                 |
| Family IVs    | fam15                |
| Family IVs    | fam36                |
| Family IVs    | fam54                |
| Family IVs    | famg1                |
| Family IVs    | famg3                |
| Family IVs    | famg4                |
| Family IVs    | famg5                |
| Family IVs    | famg6                |
| Family IVs    | famx5                |
| Family IVs    | phon12               |
| Family IVs    | phon18               |
| Family IVs    | phon21               |
| Family IVs    | phon27               |
| Family IVs    | phon3                |
| Family IVs    | phon30               |
| Family IVs    | phon33               |
| Family IVs    | phon9                |
| Family IVs    | phone42              |
| Family IVs    | phone46              |
| Family IVs    | phone50              |
