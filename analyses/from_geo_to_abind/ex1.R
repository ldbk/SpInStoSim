# Download latest release number; its useful for reproducibility to use a specific release number
devtools::install_github("james-thorson/VAST")

# Set local working directory (change for your machine)
setwd( "D:/UW Hideaway (SyncBackFree)/AFSC/2019-03 -- Making helper functions for VAST" )

# Load package
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example = load_example( data_set="EBS_pollock" )

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x=100, 
                          Region=example$Region, 
                          purpose="index", 
                          strata.limits=example$strata.limits, 
                          bias.correct=FALSE )

# Run model
fit = fit_model( settings=settings, 
                 Lat_i=example$sampling_data[,'Lat'], 
                 Lon_i=example$sampling_data[,'Lon'], 
                 t_i=example$sampling_data[,'Year'], 
                 c_i=rep(0,nrow(example$sampling_data)), 
                 b_i=example$sampling_data[,'Catch_KG'], 
                 a_i=example$sampling_data[,'AreaSwept_km2'], 
                 v_i=example$sampling_data[,'Vessel'] )

# Plot results
plot( fit )