# Flux_Compilation
Contains compilation associated with Till et al. submitted manuscript

Here, we provide the data needed to reproduce figures and statistics in the manuscript.

The file "All_Long_Term_Rates_Updated_9_26" contains the compiled values for erupted flux, mantle-derived magma flux, and crustal addition rate. 

Description of variables: 

Citation: abbreviated citation for the study from which the values are compiled

DOI: the doi for the study (as available)

GeneralName: Used to generalize the names of volcanoes reported in the "Location" column

Location: Description of the location, also indicates whether the estimate is a min max or average

Arc: Used to indicate the arc with which the volcano is associated (for example "Cascades" for Middle Sister")

EstimateType: Describes whether the estimate is for an "Active Arc" or "Extinct Arc" for an arc-averaged estimate or from a "Volcanic Field" "Caldera" or "Volcano" for erupted fluxes

TermLength: detailed information on the time period captured by the estimate (erupted fluxes only)

VolcanoType: erupted fluxes only. Discriminates between different types of volcanoes (volcanic field ("Field"), "Stratocone", "Shield", "Caldera", "Dome Field", "Stratocone+periphery" (i.e., includes surrounding field), or "Extinct")

ArcType: Mantle or crustal rates only. "Oceanic" or "Continental"

Duration (ka): erupted fluxes only. The duration captured by the flux estimate

Time averaged: erupted fluxes only. "Yes" or "No"; describes whether or not the estimate is the time averaged flux (as described in-text)

Flux(km3/myr): if reported in this unit, flux in km3/myr (generally erupted fluxes)

Flux(km3/km/myr): if reported in this unit, flux in km3/km/myr (generally mantle or crustal rates)

Notes: descriptive notes about the estimate from the original publication or editorial notes about the estimate (disagreements about the estimate between sources, where the values come from in the publication, etc.)

CrustalorMantleRates: classification of the estimate as a crustal, mantle, or erupted flux ("CrustalRate","MantleRate", or "Erupted")

LoOrHiFlux: Classification of the estimate as representative of periods of average supply ("Average"), flare-up ("Flare-up"), or relative quiescence ("Quiescent"). Crustal and mantle rates only.

Method: General description of the method used to estimate the flux

Dating method: General description of the dating method used

Volume method: General description of the method used to estimate the volume

The file "Cumulative_Volume_Data" contains a subset of data used to construct the cumulative volume plot in the manuscript

The file "Figures_And_Statistics_Sept26.R" is an R code that can be used to analyze eruptive, crustal, and mantle fluxes and reproduce statistics and figures in the publication. To use this code, you must download R and RStudio (need help? Follow steps in this guide: https://docs.posit.co/ide/user/). Download this repository and put it in a folder. Open RStudio, create a new project, and set the directory to the folder where you put the repository items. You should now be able to run the code. This code was developed using R version 4.4.3. If issues arise with this code, please email the corresponding author.

The file "syneruptive_rates" contains all the compiled data for the syn-eruptive decompression sections of the manuscript. The variables are as follows:

Rate (m/s): The calculated decompression rate

LogRate: The logarithm of the calculated decompression rate

Method: The method used to calculate decompression rate, with the different categories explained in the main text

Composition: The bulk composition of the eruption of interest

Style: The eruption style of the eruption of interest, either explosive or effusive

Volcano: The volcano from which the decompression rate was calculated

Notes: Any clarifying information needed to uniquely identify the calculated decompression rate

Reference: The in-text style citation to identify the source of the calculated decompression rate

DOI: The DOI for each reference for ease of identifying specific references
