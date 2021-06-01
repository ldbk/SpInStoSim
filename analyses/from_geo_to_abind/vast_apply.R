library(foreach)
library(doParallel)
library(FishStatsUtils)
library(VAST)


# load data set
#load("~data/simu_geostat/SamplesDE_red.RData") #adapt the path 
load("C:/git/SpInStoSim/data/simu_geostat/SamplesDE_red.RData")
library(stringr)

fsize<-1 #first stock to 
fsize
size<-96 #last stock to consider
nsize<-96 #total number of stocks

vast_format<-vector("list",nsize)

for (i in fsize:size){ #stock 
  stock_name<-names(Res.red[i])
  n_ts<-as.numeric(paste(str_sub(stock_name,-2,-1)))
  vast_format[[i]]<-vector("list",25)
 
for ( j in 1:25){ # iteration
  
 # iteration<-paste(c("It.",j), collapse="")
  iteration<-j
  example_sphe<-data.frame()
  example_gaus<-data.frame()
  example_cub<-data.frame()
  example_expo<-data.frame()
 
  vast_format[[i]][[j]]<-list(example_sphe,example_gaus,example_cub,example_expo)
  names(vast_format[[i]][[j]])=c("example_sphe", "example_gaus", "example_cub", "example_expo")
  
  vast_format[[i]][[j]][["example_sphe"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["Sample_spherical"]][["x1"]],n_ts), 
                      long=rep(Res.red[[stock_name]][[iteration]][["Sample_spherical"]][["x2"]],n_ts),
                                 year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
  vast_format[[i]][[j]][["example_gaus"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["Sample_gaussian"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["Sample_gaussian"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
 vast_format[[i]][[j]][["example_cub"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["Sample_cubic"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["Sample_cubic"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
 vast_format[[i]][[j]][["example_expo"]]<-data.frame(lat=rep(Res.red[[stock_name]][[iteration]][["Sample_exponential"]][["x1"]],n_ts), 
                              long=rep(Res.red[[stock_name]][[iteration]][["Sample_exponential"]][["x2"]],n_ts),
                              year=rep(1:n_ts, each=90), survey=rep(NA,90*n_ts),s_a=rep(1,90*n_ts))
  lindex_min<-1
  lindex_max<-90  
for (l in 1:n_ts){ #year 
  simul<-paste(c("Simu.V1.S",l), collapse="")
  Res.red[[stock_name]][[iteration]][["Sample_spherical"]][[simul]][order(Res.red[[stock_name]][[iteration]][["Sample_spherical"]][[simul]])[1:10]]<-0
  Res.red[[stock_name]][[iteration]][["Sample_gaussian"]][[simul]][order(Res.red[[stock_name]][[iteration]][["Sample_gaussian"]][[simul]])[1:10]]<-0
  Res.red[[stock_name]][[iteration]][["Sample_cubic"]][[simul]][order(Res.red[[stock_name]][[iteration]][["Sample_cubic"]][[simul]])[1:10]]<-0
  Res.red[[stock_name]][[iteration]][["Sample_exponential"]][[simul]][order(Res.red[[stock_name]][[iteration]][["Sample_exponential"]][[simul]])[1:10]]<-0
  
  vast_format[[i]][[j]][["example_sphe"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["Sample_spherical"]][[simul]]
  vast_format[[i]][[j]][["example_gaus"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["Sample_gaussian"]][[simul]]
  vast_format[[i]][[j]][["example_cub"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["Sample_cubic"]][[simul]]
  vast_format[[i]][[j]][["example_expo"]][["survey"]][lindex_min:lindex_max]<-Res.red[[stock_name]][[iteration]][["Sample_exponential"]][[simul]]
lindex_min<-lindex_max+1
lindex_max<-lindex_max+90

} # year
} #iteration
} #stock 

t<-seq(0.005,0.995, length.out=100)
tt<-expand.grid(t,t)
times<-c(400,1200,400,1200,3600,1200,400,1200,400)/3
r <- rep(c(1:9), times=c(400,1200,400,1200,3600,1200,400,1200,400))


example2<-list()
for( i in 1:nsize){
  example1<-list()
  for( j in 1:25){
example<-list(vast_format[[i]][[j]][["example_sphe"]],
              vast_format[[i]][[j]][["example_gaus"]], 
vast_format[[i]][[j]][["example_cub"]],
vast_format[[i]][[j]][["example_expo"]])

example1<-c(example1, example)
  }
  example2<-c(example2,example1)
 
}



na<-list(NA)
nstocks<-4 # change depending of the number of stocks (stocks*iteration*vario)
indices<-rep(na, nstocks)
imax<-2 # change depending of number of cores
cl=parallel::makeCluster(4,type="PSOCK") 
registerDoParallel(cl)
tictoc::tic()
indices = foreach(i=1:nstocks,.packages=c("VAST","FishStatsUtils")) %dopar% {
  # Make settings (turning off bias.correct to save time for example)
  settings = make_settings( n_x=100, 
                            Region="user", 
                            purpose="index2", 
                            strata.limits= data.frame(STRATA = "All_areas"), 
                            bias.correct=FALSE )
  
  m_ll<-matrix(data=NA, nrow=90, ncol=3)
 # m_ll<-matrix(data=NA, nrow=90, ncol=2)
  m_ll[,1]<-example2[[i+50]][["long"]][1:90]
  m_ll[,2]<-example2[[i+50]][["lat"]][1:90]
  m_ll[,3]<-rep(c(400,1200,400,1200,3600,1200,400,1200,400)/10, each=10)
  m_ll[,3]<-rep(1,90)
  colnames(m_ll)<-c('Lat', 'Lon','Area_km2')
  #colnames(m_ll)<-c('Lat', 'Lon')
  

  # Run model
  fit16 = try(fit_model(settings=settings, 
                   Lat_i=example2[[i+50]][["lat"]], 
                   Lon_i=example2[[i+50]][["long"]], 
                   t_i=example2[[i+50]][["year"]], 
                   c_i=rep(0,90*length(unique(example2[[i+50]][["year"]]))), 
                   b_i=example2[[i+50]][["survey"]], 
                   a_i=example2[[i+50]][["s_a"]]
                   #,observations_LL =m_ll
                   ,input_grid=m_ll
                   #v_i=example[[i]]$sampling_data[,'Vessel'] 
                   ,Aniso=FALSE, 
                   ObsModel=c("PosDist"=2, 'Link'=0),
                   FieldConfig= c("Omega1"=0, "Epsilon1"=0, "Omega2"=0, "Epsilon2"=1)),TRUE)
                 
  #print(i)
  indices<-try(fit16$Report$Index_ctl[1,,1],TRUE)
}
tictoc::toc()
save(indices, file="indices_essais_DE_51-90.Rdata")
#save(indices, file="indices_1_48_DE.Rdata")
#save(indices, file="indices_49_96_DE.Rdata")
#save(indices, file="indices_1_48_LP.Rdata")
#save(indices, file="indices_49_96_LP.Rdata") #choose the good one
stopCluster()