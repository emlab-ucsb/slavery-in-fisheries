# slavery-in-fisheries
This repository contains the code and data for reproducing McDonald, *et al.* 2020 (PNAS) in review: "Satellites reveal global extent of modern slavery in the world’s fishing fleet".

The DOI for this code and data repository will be managed with Zenodo.

> **Title**: Satellites reveal global extent of modern slavery in the world’s fishing fleet

> **Authors**: Gavin G. McDonald, Christopher Costello, Jennifer Bone, Reniel B. Cabral, Valerie Farabee, Timothy Hochberg, David Kroodsma, Tracey Mangin, Kyle C. Meng, Oliver Zahn


> **Abstract:** While modern slavery in the world’s fishing fleet has been widely documented, its extent remains unknown and no methods previously existed for remotely identifying fishing vessels engaged in these abuses. By combining behavioral insights from economics, expertise from human rights practitioners, and satellite vessel monitoring data, we show that vessels known to use forced labor behave in systematically different ways from other vessels. We exploit this insight by using machine learning to identify high-risk vessels from among 16,000 industrial longliner, squid jigger, and trawler fishing vessels. Our model reveals that ~15% of vessels were high-risk, and also reveals patterns of where these vessels fished and which ports they visited. Nearly 60,000 individuals worked on these vessels, many of whom may have been forced labor victims. This information provides unprecedented opportunities for novel interventions to combat this humanitarian tragedy. More broadly, this research demonstrates the potential for remote sensing to detect human rights abuses.

## Repository structure  

```
slavery-in-fisheries
  |__ raw_data
  |__ interim_data
  |__ output_figures
  |__ r
```

## Software

This analysis was performed in R. The script for fully reproducing the paper analysis can be found in `r/analysis.Rmd`.

## License

The software code, data, and figures contained within this repository are made available under the [Creative Commons CC BY-NC-ND 2.0](https://creativecommons.org/licenses/by-nc-nd/2.0/) license.

**Please note:** To ensure reproducibility and in order to manage package dependencies, we use the `renv` package linked to [R Studio Package Manager](https://packagemanager.rstudio.com/client/#/). When you first clone this repo onto your machine, run `renv::restore()` to ensure you have all correct package versions installed in the project. Please see the `renv` page for more information: https://rstudio.github.io/renv/articles/renv.html.