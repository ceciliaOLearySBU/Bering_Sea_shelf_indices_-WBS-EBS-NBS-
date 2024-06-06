# Bering_Sea_shelf_indices (WBS-EBS-NBS)
model-based indices for the Bering Sea shelf ( western, eastern, and northern Bering Sea)

//file '1_create_Bering_extrapolation_grid.R' creates the spatial bounds to interpolate groundfish densities over
file '2_setup_data_for_VAST.R' transforms TINRO and AFSC data to the correct format for input into the VAST spatiotemporal model
file '3_Bering_sea_indices_with_CPI' contains all of the code used to fit models for groundfish biomass that include the Cold Pool Index
file '4_Bering_sea_indices_without_CPI' contains all of the code used to fit models for groundfish biomass that do not include the Cold Pool Index

The plots folder includes the code used to create the figures in the associated manuscript published in ICES.
The adjusted functions folder includes the R code that was adjusted for maps to cross the dateline.
The R folder contains additional code for plots that provide theme settings for R code.
