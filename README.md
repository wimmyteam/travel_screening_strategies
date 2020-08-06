# travel_screening_strategies

This repository contains the R code used in [Clifford and Quilty et al. (2020)](https://www.medrxiv.org/content/10.1101/2020.07.24.20161281v2) - *Strategies to reduce the risk of SARS-CoV-2 re-introduction from international travellers*. **This article is not yet peer reviewed**.

This code can be downloaded as a ZIP file or cloned/forked as a github repository.

The main file to run the analysis is `travel_screening_strategies.R`, which calls `utils.R` and other necessary files and writes plotting output to `/figures/`.

The code license is available in [LICENSE](LICENSE).

## Submodules for other data sources

Data from [Gostic et al. (2020)](https://github.com/kgostic/traveller_screening) and [Kucirka et al. (2020)](https://github.com/HopkinsIDD/covidRTPCR) are used as the basis for some of the analysis contained here. We have included these as submodules; you may need to initialise them on your machine after cloning this repository.

```bash
git submodule update --init --recursive
```

This will ensure you have the data in the place that our script expects in order to fit models for probability of detection.

We replicate some of the data from [He et al. (2020)](https://www.nature.com/articles/s41591-020-0869-5) and [Wölfel et al. (2020)](https://www.nature.com/articles/s41586-020-2196-x). The code used to process the data for inclusion in our model is provided, respectively in `gostic.R`, `kucirka_fitting.R`, `kucirka_plot.R`, `he.R` and `wolfel.R`.

## Packages required to run this code

We call on a number of additional packages to run and visualise our models. These are managed in `packages.R` which uses the `{pacman}` package to install and load them.

## Model structure and results

The model simulates travellers from the EU and USA using estimates of travel volume for July based on CAA data and prevalence in relevant countries from [Russell et al. (2020)](https://cmmid.github.io/topics/covid19/global_cfr_estimates.html). The same travellers are screened according to the various scenarios and we track when they are released (e.g. prior to becoming infectious, during their infectious period, or after their infectious period has ended).

![Traveller trajectories for considered screening scenarios](figures/traveller_screening.svg)

Figure: Possible traveller trajectories for the considered screening scenarios. Screening (purple diamonds) occurs pre-flight and/or post-flight and may include managed quarantine periods (yellow boxes). Travellers found to be infected pre-flight are prevented from boarding (orange boxes pre-flight); travellers found to be infected during managed quarantine are diverted to mandatory quarantine (orange boxes post-flight). Travellers enter the community after the required number of negative tests (regardless of infection status) or after meeting the requirements of the mandatory quarantine.

We report the number travellers released while they still have the potential to infect, as well as the number of days for which these potentially infectious travellers remain infectious. These are presented as depending on country of origin, number of days spent in quarantine, stringency of quarantine, number of tests required to clear quarantine, and, optionally, whether they are ever-symptomatic or always asymptomatic.

All figures are, by default, created as PDF files. If you have [imagemagick](https://imagemagick.org/index.php) installed and configured (and the `{magick}` package) the `save_plot()` function can accept additional graphics formats and use imagemagick to convert them within the plotting code.

## Authors’ Contributions

Samuel Clifford, Billy J Quilty, Stefan Flasche and W John Edmunds conceived the study and wrote the report. Samuel Clifford, Billy J Quilty, Akira Endo, Stefan Flasche and W John Edmunds designed the model. Samuel Clifford and Billy J Quilty led the development and analysis of the screening model and produced the results and figures. Timothy W Russell led the development of and analysis of the prevalence model. Timothy W Russell, Yang Liu, Yung-Wai Desmond Chan, Carl AB Pearson, Akira Endo, Stefan Flasche, Rosalind M Eggo and W John Edmunds consulted on the analyses. The CMMID COVID-19 working group members contributed by interpreting the study findings, contributing to the report, and approving the work for publication. All authors approved the final version for publication.

A full list of the members of the CMMID COVID-19 working group and funding statement can be found in the [preprint](https://www.medrxiv.org/content/10.1101/2020.07.24.20161281v2).

## Getting help

We provide the code as-is, and assume you know your way around R if you're intending to use it. This code is not provided with any warranty. You can use this repository's [Issues](https://github.com/cmmid/travel_screening_strategies/issues) tab to raise any errors you receive, unexpected behaviour, or model feedback.