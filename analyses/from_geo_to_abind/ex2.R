# Download latest release number; its useful for reproducibility to use a specific release number
#devtools::install_github("james-thorson/VAST")

# Set local working directory (change for your machine)
#setwd( "D:/UW Hideaway (SyncBackFree)/AFSC/2019-03 -- Making helper functions for VAST" )

# Load package


registerDoParallel(cores = 2)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example<-list(load_example( data_set="EBS_pollock" ),load_example( data_set="EBS_pollock" ))
indices<-list(NA,NA)
foreach(i = 1:2) %dopar%
library(FishStatsUtils)
library(VAST)
library(doParallel)
  for(i in 1:2){
    # Make settings (turning off bias.correct to save time for example)
    settings = make_settings( n_x=100, 
                              Region=example[[i]]$Region, 
                              purpose="index2", 
                              strata.limits=example[[i]]$strata.limits, 
                              bias.correct=FALSE )
    
    # Run model
    fit = fit_model( settings=settings, 
                     Lat_i=example[[i]]$sampling_data[,'Lat'], 
                     Lon_i=example[[i]]$sampling_data[,'Lon'], 
                     t_i=example[[i]]$sampling_data[,'Year'], 
                     c_i=rep(0,nrow(example[[i]]$sampling_data)), 
                     b_i=example[[i]]$sampling_data[,'Catch_KG'], 
                     a_i=example[[i]]$sampling_data[,'AreaSwept_km2'], 
                     v_i=example[[i]]$sampling_data[,'Vessel'] )
    
    indices[[i]]<-fit$Report$Index_ctl[1,,1]
  }
stopCluster()

# Plot results
plot( fit )