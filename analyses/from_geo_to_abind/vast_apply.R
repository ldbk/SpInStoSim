
registerDoParallel(cores = 2)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
library(stringr)

vast_format<-vector("list",96)
for (i in 1:96){ #stock 
  
  stock_name<-names(Res.red[i])
  n_ts<-as.numeric(paste(str_sub(stock_name,-2,-1)))
  vast_format[[i]]<-vector("list",25)
 
for ( j in 1:25){ # iteration
  
  iteration<-paste(c("It.",j), collapse="")
 
  example_sphe<-data.frame()
  example_gaus<-data.frame()
  example_cub<-data.frame()
  example_expo<-data.frame()
 
  vast_format[[i]][[j]]<-list(example_sphe,example_gaus,example_cub,example_expo)
  names(vast_format[[i]][[j]])=c("example_sphe", "example_gaus", "example_cub", "example_expo")
  
  vast_format[[i]][[j]][["example_sphe"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["sample.sphe"]][["x1"]],n_ts), 
                      long=rep(Res.red[[stock_name]][[iteration]][["sample.sphe"]][["x2"]],n_ts),
                                 year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts))
  vast_format[[i]][[j]][["example_gaus"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["sample.gaus"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["sample.gaus"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts))
 vast_format[[i]][[j]][["example_cub"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["sample.cub"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["sample.cub"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts))
 vast_format[[i]][[j]][["example_expo"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["sample.expo"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["sample.expo"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts))
  lindex_min<-1
  lindex_max<-60  
for (l in 1:60){ #year 
  simul<-paste(c("Simu.V1.S",l), collapse="")
  vast_format[[i]][[j]][["example_sphe"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.sphe"]][[simul]]
  vast_format[[i]][[j]][["example_gaus"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.gaus"]][[simul]]
  vast_format[[i]][[j]][["example_cub"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.cub"]][[simul]]
  vast_format[[i]][[j]][["example_expo"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.expo"]][[simul]]
lindex_min<-lindex_max+1
lindex<-lindex_max+n_ts

} # year
} #iteration
} #stock 


indices<-list(NA,NA) ## list first level stock, second level iteration, thirs level variograms 

foreach(i = 1:2) %dopar%
library(FishStatsUtils)
library(VAST)
library(doParallel)