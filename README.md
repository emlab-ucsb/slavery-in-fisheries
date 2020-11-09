# slavery-in-fisheries
This repository contains the code and data for reproducing McDonald *et al.* 2020 (PNAS) *in press**: "Satellites can reveal global extent of forced labor in the world's fishing fleet".

The DOI for this code and data repository will be managed with Zenodo.

> **Title**: Satellites reveal global extent of forced labor in the world’s fishing fleet

> **Authors**: Gavin G. McDonald, Christopher Costello, Jennifer Bone, Reniel B. Cabral, Valerie Farabee, Timothy Hochberg, David Kroodsma, Tracey Mangin, Kyle C. Meng, Oliver Zahn

> **Abstract:** While forced labor in the world’s fishing fleet has been widely documented, its extent remains unknown. No methods previously existed for remotely identifying individual fishing vessels potentially engaged in these abuses on a global scale. By combining expertise from human rights practitioners and satellite vessel monitoring data, we show that vessels reported to use forced labor behave in systematically different ways from other vessels. We exploit this insight by using machine learning to identify high-risk vessels from among 16,000 industrial longliner, squid jigger, and trawler fishing vessels. Our model reveals that between 14 and 26% of vessels were high-risk, and also reveals patterns of where these vessels fished and which ports they visited. Between 57,000 and 100,000 individuals worked on these vessels, many of whom may have been forced labor victims. This information provides unprecedented opportunities for novel interventions to combat this humanitarian tragedy. More broadly, this research demonstrates a proof of concept for using remote sensing to detect forced labor abuses.

## Software

This analysis was performed in R. The script for fully reproducing the paper analysis can be found in `r/analysis.Rmd`.

## License

The software code, data, and figures contained within this repository are made available under the [Creative Commons CC BY-NC-ND 2.0](https://creativecommons.org/licenses/by-nc-nd/2.0/) license.

**Please note:** To ensure reproducibility and in order to manage package dependencies, we use the `renv` package linked to [R Studio Package Manager](https://packagemanager.rstudio.com/client/#/). When you first clone this repo onto your machine, run `renv::restore()` to ensure you have all correct package versions installed in the project. Please see the `renv` page for more information: https://rstudio.github.io/renv/articles/renv.html.

## Repository structure  

```
slavery-in-fisheries
  |__ raw_data
  |__ interim_data
  |__ output_figures
  |__ r
```

`r` contains the code necessary to run the analysis. `raw_data` contains raw data files necessary for reproducing the analysis. `interim_data`contains interim files created during the analysis that can be used to generate all figures and statistics in the paper. These interim files are saved due to the long run times required for the analysis. Each data folder contains a metadata document.  