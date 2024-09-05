# Cognate beginnings to bilingual lexical acquisition

| Link                                                             | Contents                                                       |
|------------------------------------------------------------------|----------------------------------------------------------------|
| [Website](https://gongcastro.github.io/cognate-beginnings)       | Instructions for reproducibility, data dictionaries, lab notes |
| [PsyArxiv](https://psyarxiv.com/dxsmz/)                          | Preprint and figures                                           |
| [GitHub](https://github.com/gongcastro/cognate-beginnings)       | Code, preprint and figures                                     |
| [OSF](https://osf.io/hy984/)                  | Code, preprint, and results (model outputs)                    |
| [Docker](https://hub.docker.com/r/gongcastro/cognate-beginnings) | Docker image with reproducible RStudio session                 |


# Repository structure and files 📂

This repository is organised as follows:

-   **data**: processed data in CSV format
    -   [items.csv](data/items.csv): information about words included in
        the analyses
    -   [participants.csv](data/participants.csv): information about
        participants
    -   [responses.csv](data/responses.csv): participant responses to
        the items. The model was fit on this dataset.
-   **data-raw**: raw data from the [Barcelona Vocabulary Questionnaire,
    BVQ](https://gongcastro.github.io/bvq). This is a RDS file
    containing a list of data frames with all the information necessary
    to generate the datasets in the data/ directory.
-   **docs**: source code to generate the documentation site of the
    project
    ([cognate-beginnings](https://gongcastro.github.com/cognate-beginnings)).
-   **manuscript**: Quarto document with the source code of the
    manuscript and appendix
-   **R**: R functions used in the targets to process and analyse the
    data.
    -   [processing.R](R/processing.R): code that preprocesses the raw data to generate `data/participants.csv`, `data/items.csv`, and `data/responses.csv`
    -   [models.R](R/models.R): to fit the Bayesian model and extract
        posterior draws
    -   [utils.R](R/utils.R): helper functions and wrappers used across
        in [processing.R](R/processing.R) and [models.R](R/models.R)
-   **renv**: internal settings to ensure reproducibility of the
    computing environment.
-   **results**: model outputs. You will need to run the code to
    generate the files that will be contained in this directory.
    -   fits: RDS files with the brmsfit of the Bayesian models
    -   posterior: CSV files with the posterior draws of the
        population-level and group-level coefficients
    -   predictions: CSV files with the posterior predictions
-   **src**: R functions to make programming tasks easier, not needed to
    reproduce the project.
-   **tests**: testthat scripts used to unit test the functions used
    across the project.
