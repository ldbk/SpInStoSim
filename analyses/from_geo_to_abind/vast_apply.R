
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
                                 year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
  vast_format[[i]][[j]][["example_gaus"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["sample.gaus"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["sample.gaus"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
 vast_format[[i]][[j]][["example_cub"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["sample.cub"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["sample.cub"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
 vast_format[[i]][[j]][["example_expo"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["sample.expo"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["sample.expo"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
  lindex_min<-1
  lindex_max<-90  
for (l in 1:90){ #year 
  simul<-paste(c("Simu.V1.S",l), collapse="")
  vast_format[[i]][[j]][["example_sphe"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.sphe"]][[simul]]
  vast_format[[i]][[j]][["example_gaus"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.gaus"]][[simul]]
  vast_format[[i]][[j]][["example_cub"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.cub"]][[simul]]
  vast_format[[i]][[j]][["example_expo"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["sample.expo"]][[simul]]
lindex_min<-lindex_max+1
lindex<-lindex_max+90

} # year
} #iteration
} #stock 

t<-seq(0.005,0.995, length.out=100)
tt<-expand.grid(t,t)
times<-c(400,1200,400,1200,3600,1200,400,1200,400)/3
r <- rep(c(1:9), times=c(400,1200,400,1200,3600,1200,400,1200,400))

example<-list(vast_format[[1]][[1]][["example_sphe"]],vast_format[[1]][[1]][["example_gaus"]])
indices<-list(NA,NA)
imax<-2
foreach(i = 1:2) %dopar%
library(FishStatsUtils)
library(VAST)
library(doParallel)
for(i in 1:imax){
  # Make settings (turning off bias.correct to save time for example)
  settings = make_settings( n_x=100, 
                            Region="user", 
                            purpose="index2", 
                            strata.limits= data.frame(STRATA = "All_areas"), 
                            bias.correct=FALSE )
  
  m_ll<-matrix(data=NA, nrow=90, ncol=3)
 # m_ll<-matrix(data=NA, nrow=90, ncol=2)
  m_ll[,1]<-example[[i]][["lat"]][1:90]
  m_ll[,2]<-example[[i]][["lat"]][1:90]
  m_ll[,3]<-rep(c(400,1200,400,1200,3600,1200,400,1200,400)/3, each=10)
  colnames(m_ll)<-c('Lat', 'Lon','Area_km2')
  #colnames(m_ll)<-c('Lat', 'Lon')
  

  # Run model
  fit = fit_model( settings=settings, 
                   Lat_i=example[[i]][["lat"]], 
                   Lon_i=example[[i]][["long"]], 
                   t_i=example[[i]][["year"]], 
                   c_i=rep(0,5400), 
                   b_i=example[[i]][["survey"]], 
                   a_i=example[[i]][["s_a"]]
                   #,observations_LL =m_ll
                   ,input_grid=m_ll
                   #v_i=example[[i]]$sampling_data[,'Vessel'] 
                   ,Aniso=FALSE, 
                   FieldConfig= c("Omega1"=0, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=1))
                 
  
  indices[[i]]<-fit$Report$Index_ctl[1,,1]
}
stopCluster()