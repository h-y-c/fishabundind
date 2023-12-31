

fishabundind: Fish Abundance Index
================
Hsiao-Yun Chang, Yong Chen

2024-01-05


# Introduction

Abundance indices are of paramount importance in the monitoring and assessment of fish population dynamics.
Fishery-independent surveys are often the preferred choice for deriving these indices due to their utilization of standardized or randomized designs, ensuring that sampling remains representative and unbiased across space and time ([Chang et al. 2023](https://www.frontiersin.org/articles/10.3389/fmars.2023.1237549/full)).

*fishabundind* offers a set of functions designed for the estimation and visualization of design-based annual fish abundance indices and their spatiotemporal distributions.
Additionally, it includes a function for determining the initial timing of the proposed season, particularly valuable for early life-stages.


# Installation

```{r getPackageDevel, eval = FALSE}

  devtools::install_github('h-y-c/fishabundind')

```

# Load the package into R session

```{r Load, message = FALSE}

  library(fishabundind)

```

## Demo:

# 1. Data

The data are assumed collected from a fishery-independent survey following a stratified random sampling design with i regions within the sampling area and s strata within a given region.
The survey was conducted over weeks for each year.
The datasets include the following variables:

*yr* Year of data collection.

*wk* Calender week of data collection.

*TowID* ID of each tow.

*regname* Names of regions where data were collected.

*stra* Strata code within the corresponding *regname* where data were collected.

*stravol* Volume of the corresponding *stra* where data were collected.

*regvol* Volume of the corresponding region where data were collected.

*volsamp* Volume of water sampled.

*nind* Number of individual sampled.

*spe* Species name(s).

```{r message = FALSE}

  library(dplyr)
  library(ggplot2)

  head(fishQ)

```

# 2. Initial time of the season

We will use two hypothetical datasets of two species to calculate the initial time of the proposed season.

Use `fishinitseasonEst` function to estimate the initial time of the proposed season.
The data collected for species Q were fish at early life-stage (e.g. eggs or larvae) which may have prominent seasonality.

The users can determine the definition of the initial time of the proposed season.
In this example, the first week of the proposed season was determined as the first week of the year in which the cumulative weekly density estimates surpass 5% of the total densities observed across all weeks of sampling.
And it is assumed that the proposed season will last for 7 weeks.

```{r message = FALSE}

 seasonQ<-fishinitseasonEst(dataset=fishQ,Year="yr",
                                Week="wk",
                                RegionName="regname",
                                Strata="stra",
                                StrataVolume="stravol",
                                RegionVolume="regvol",
                                VolumeSample="volsamp",
                                N_individual="nind",
                                first_thr=0.05,durationEst=7)

```

The function returns a dataframe including a variable of year and a variable of the initial week for each year.

```{r message = FALSE}

 seasonQ

```

To visualize the time series of the initial time of the proposed season:

```{r message = FALSE}

 plotinitseason(seasonQ)

```
![image](https://github.com/h-y-c/fishabundind/assets/90223612/6406b02d-595c-44ec-8de6-e08d3abcf3cc)

The estimates can be very useful for examining temporal changes in fish phenology.

# 3. Annual Abundance Index

Use `fishai` function to estimate the annual abundance indices.
Note that when `SeasonEst=TRUE`, inputs of `first_thr` and `durationEst` are necessary.
When `SeasonEst=TRUE`, only data that fall within the proposed weeks for each year will be used.
For example, if the estimated initial week in a given year is 20 and the `durationEst=7`, only data of weeks 20-27 in that year will be used.
When `SeasonEst=FALSE`, data of all weeks will be used.

```{r message = FALSE}

 aiQ<-fishai(dataset=fishQ,Year="yr",
            Week="wk",
            RegionName="regname",
            Strata="stra",
            StrataVolume="stravol",
            RegionVolume="regvol",
            VolumeSample="volsamp",
            N_individual="nind",
            SeasonEst=TRUE,first_thr=0.05,durationEst=7)

```

The `fishai` function returns a dataframe including a variable of year, a variable of annual abundance indices, and a variable of variance.

```{r message = FALSE}

 aiQ

```

To visualize the time series of the annual abundance indices.
The shade denotes the standard error.

```{r message = FALSE}

 plotfishai(aiQ)

```
![image](https://github.com/h-y-c/fishabundind/assets/90223612/6be146a9-3078-404f-86c0-8124afce4482)

Another example of using `fishai` using all data (`SeasonEst=FALSE`) for species X.

```{r message = FALSE}

 aiX<-fishai(dataset=fishX,Year="yr",
            Week="wk",
            RegionName="regname",
            Strata="stra",
            StrataVolume="stravol",
            RegionVolume="regvol",
            VolumeSample="volsamp",
            N_individual="nind",
            SeasonEst=FALSE)

```

Note that when `SeasonEst=FALSE`, `first_thr` and `durationEst` are not needed.

To visualize the time series of the annual abundance indices for species X.
Use `se=FALSE` to turn off the standard error (default is TRUE).

```{r message = FALSE}

 plotfishai(aiX,se=FALSE)

```

![image](https://github.com/h-y-c/fishabundind/assets/90223612/63bd7df0-33a7-4846-9446-5c7fdca8cc25)


# 4. Spatiotemporal Distribution

Use `fishstd` function to estimate the average weekly density over the survey area and years.
Note that when `SeasonEst=TRUE`, inputs of `first_thr` and `durationEst` are necessary.
When `SeasonEst=TRUE`, only data that fall within the proposed weeks for each year will be used.
For example, if the estimated initial week in a given year is 20 and the `durationEst=7`, only data of weeks 20-27 in that year will be used.
When `SeasonEst=FALSE`, data of all weeks will be used.

```{r message = FALSE}

 stdQ<-fishstd(dataset=fishQ,Year="yr",
              Week="wk",
              RegionName="regname",
              Strata="stra",
              StrataVolume="stravol",
              RegionVolume="regvol",
              VolumeSample="volsamp",
              N_individual="nind",
              SeasonEst=TRUE,first_thr=0.05,durationEst=7)

```

The `fishstd` returns a dataframe which includes averaged weekly density for each region and each year.

```{r message = FALSE}

 stdQ

```

To visualize the spatiotemporal distribution of average weekly density for species Q.

```{r message = FALSE}

 library(colorRamps)
 # NOTE This can return "figure margins too large" error.
 plotfishstd(df=stdQ,legend.title="Standardized Density",plot.title="SpeciesQ")
 
```
![image](https://github.com/h-y-c/fishabundind/assets/90223612/ede2dba8-e6c8-4af3-8f77-072903138a80)


Another example of using `fishstd` using all data (`SeasonEst=FALSE`) for species X.

```{r message = FALSE}

 stdX<-fishstd(dataset=fishX,Year="yr",
              Week="wk",
              RegionName="regname",
              Strata="stra",
              StrataVolume="stravol",
              RegionVolume="regvol",
              VolumeSample="volsamp",
              N_individual="nind",
              SeasonEst=FALSE)

```

To visualize the spatiotemporal distribution of average weekly density for species X.

```{r message = FALSE}

 # NOTE This can return "figure margins too large" error.
 plotfishstd(df=stdX,legend.title="Standardized Density",plot.title="SpeciesX")
 
```
![image](https://github.com/h-y-c/fishabundind/assets/90223612/c20ca514-a001-46c2-a620-1a207e4c8d26)



# References

Chang, H.-Y., Sun, M., Rokosz, K., and Chen, Y..
2023.
Evaluating effects of changing sampling protocol for a long-term Ichthyoplankton monitoring program.
Frontiers in Marine Science.
DOI: 10.3389/fmars.2023.1237549
