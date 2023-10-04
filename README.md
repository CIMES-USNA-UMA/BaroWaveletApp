# BaroWaveletApp

## Description

Shiny app developed by Alvaro Chao-Ecija (2022-2023 Collaboration Scholarship at the Department of Physiology and the Autonomic Nervous System Unit at CIMES, University of Malaga), which started as part of my Final Degree Project, under the supervision of MD-PhD. Marc Stefan Dawid Milner. It has been designed to work
with the [BaroWavelet](https://github.com/CIMES-USNA-UMA/BaroWavelet) package.

## How to access the app

To use this shiny app, first you will need to install packages *shiny* and *BaroWavelet* (more info about BaroWavelet and how to download it [here](https://github.com/CIMES-USNA-UMA/BaroWavelet#readme)), including its dependencies. Then, you can access the app with 
the following code line in R:

```ruby
shiny::runGitHub("BaroWaveletApp", "CIMES-USNA-UMA", launch.browser = TRUE)
```

Alternatively, you can also access the app directly through the BaroWavelet package:

```ruby
BaroWavelet::RunBaroWaveletApp()
```

## Citation

To cite this project or *BaroWavelet*, use the following citation information:

```ruby
citation("BaroWavelet")
#>To cite BaroWavelet in publications use:
#>
#>  A. Chao-Ecija, M.S. Dawid-Milner, BaroWavelet: An R-based tool for dynamic baroreflex evaluation
#>  through wavelet analysis techniques, Comput Methods Programs Biomed. 242 (2023) 107758.
#>  https://doi.org/10.1016/j.cmpb.2023.107758.
#>
#>A BibTeX entry for LaTeX users is
#>
#>  @Article{CHAOECIJA2023107758,
#>    title = {BaroWavelet: An R-based tool for dynamic baroreflex evaluation through 
#>  wavelet analysis techniques},
#>    author = {A. Chao-Ecija and M.S. Dawid-Milner},
#>    journal = {Computer Methods and Programs in Biomedicine},
#>    year = {2023},
#>    volume = {242},
#>    pages = {107758},
#>    doi = {10.1016/j.cmpb.2023.107758},
#>    issn = {0169-2607},
#>    url = {https://www.sciencedirect.com/science/article/pii/S0169260723004248},
#>  }
```



## Issues and requests

Please access the following link to create an issue or request:

https://github.com/CIMES-USNA-UMA/BaroWaveletApp/issues

## Contact information

Email: alvarochaoecija.rprojects@gmail.com

ORCID: https://orcid.org/0000-0002-2691-6936
