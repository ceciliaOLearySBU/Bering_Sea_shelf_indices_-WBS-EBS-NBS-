# Bering_Sea_shelf_indices (WBS-EBS-NBS) <br />
model-based indices for the Bering Sea shelf ( western, eastern, and northern Bering Sea) <br />

file '1_create_Bering_extrapolation_grid.R' creates the spatial bounds to interpolate groundfish densities over <br />
file '2_setup_data_for_VAST.R' transforms TINRO and AFSC data to the correct format for input into the VAST spatiotemporal model <br />
file '3_Bering_sea_indices_with_CPI' contains all of the code used to fit models for groundfish biomass that include the Cold Pool Index <br />
file '4_Bering_sea_indices_without_CPI' contains all of the code used to fit models for groundfish biomass that do not include the Cold Pool Index <br />

The plots folder includes the code used to create the figures in the associated manuscript published in ICES. <br />
The adjusted functions folder includes the R code that was adjusted for maps to cross the dateline. <br />
The R folder contains additional code for plots that provide theme settings for R code. <br />

The manuscript with model information and methods can be found [here in ICES JMS](https://academic.oup.com/icesjms/article/79/4/1063/6555703).<br />
Citation: O'Leary, Cecilia A., Lukas B. DeFilippo, James T. Thorson, Stan Kotwicki, Gerald R. Hoff, Vladimir V. Kulik, James N. Ianelli, and André E. Punt. "Understanding transboundary stocks’ availability by combining multiple fisheries-independent surveys and oceanographic conditions in spatiotemporal models." ICES Journal of Marine Science 79, no. 4 (2022): 1063-1074.
