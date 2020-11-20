#Libraries
library(RGeostats)
require(Rfast)

#install.packages("FLCore", repos="http://flr-project.org/R")
library(FLCore)
#devtools::install_github("flr/FLIfe", INSTALL_opts=c("--no-multiarch")) 
library(FLBRP)
#install.packages("FLBRP", repos="http://flr-project.org/R")
library(raster)

#Non conditionnal simulation
data.db <- db.create(data.frame(x1=c(0,0,1,1),x2=c(0,1,1,0)))
grid.db <- db.grid.init(data.db,nodes=c(100,100))

mod.sphe <- model.create("Spherical",range=.2,sill=1)
mod.gaus <- model.create("Gaussian",range=.2,sill=1)
mod.cub <- model.create("Cubic",range=.2,sill=1)
mod.expo <- model.create("Exponential",range=.2,sill=1)

RGeostats::plot(mod.sphe)
RGeostats::plot(mod.gaus)
RGeostats::plot(mod.cub)
RGeostats::plot(mod.expo)


sim.sphe<- simtub(model=mod.sphe,dbout=grid.db,nbsim=60,nbtuba=100)
sim.gaus<- simtub(model=mod.gaus,dbout=grid.db,nbsim=60,nbtuba=100)
sim.cub<- simtub(model=mod.cub,dbout=grid.db,nbsim=60,nbtuba=100)
sim.expo<- simtub(model=mod.expo,dbout=grid.db,nbsim=60,nbtuba=100)

RGeostats::plot(sim.sphe,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))
RGeostats::plot(sim.gaus,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))
RGeostats::plot(sim.cub,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))
RGeostats::plot(sim.expo,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))



#Import biomass values of the 1st stock

load("data/simu_stock/out100/0.6/lh/simsDE202010091026.RData")

biomass <- (as.integer(sims[1][1]$DE_ID0_AR_ED0_SELF_UR0_TS60$biomass))
dim(biomass) <- c(60,100)
biomass <- data.frame(biomass)


# Test for the 1st iteration
sphe <- sim.sphe@items
gaus <- sim.gaus@items
cub <- sim.cub@items
expo <- sim.expo@items

#Generate 1/3rd of 0 values
# for (l in 4:63){
#   limit <- Rfast::nth(sphe[,l], 3334, descending = F) #Create 0 values (1/3rd of total samples)
#   sphe[,l][sphe[,l]<limit] <- 0
#   
#   limit <- Rfast::nth(gaus[,l], 3334, descending = F) #Create 0 values (1/3rd of total samples)
#   gaus[,l][gaus[,l]<limit] <- 0
#   
#   limit <- Rfast::nth(cub[,l], 3334, descending = F) #Create 0 values (1/3rd of total samples)
#   cub[,l][cub[,l]<limit] <- 0
#   
#   limit <- Rfast::nth(expo[,l], 3334, descending = F) #Create 0 values (1/3rd of total samples)
#   expo[,l][expo[,l]<limit] <- 0
# }

#Get only positive values
sphe[,c(4:63)] <- sweep(sphe[,c(4:63)],2,abs(apply(sphe[,c(4:63)],2, min)),"+")
gaus[,c(4:63)] <- sweep(gaus[,c(4:63)],2,abs(apply(gaus[,c(4:63)],2, min)),"+")
cub[,c(4:63)] <- sweep(cub[,c(4:63)],2,abs(apply(cub[,c(4:63)],2, min)),"+")
expo[,c(4:63)] <- sweep(expo[,c(4:63)],2,abs(apply(expo[,c(4:63)],2, min)),"+")

#Get biomass sum on simulations
f.s <-biomass$X1/colSums(sphe[,c(4:63)])
f.g <-biomass$X1/colSums(gaus[,c(4:63)])
f.c <-biomass$X1/colSums(cub[,c(4:63)])
f.e <-biomass$X1/colSums(expo[,c(4:63)])

sphe[,c(4:63)] <- t(t(sphe[,c(4:63)])*f.s)
gaus[,c(4:63)] <- t(t(gaus[,c(4:63)])*f.g)
cub[,c(4:63)] <- t(t(cub[,c(4:63)])*f.c)
expo[,c(4:63)] <- t(t(expo[,c(4:63)])*f.e)



#Sample
r <- raster(ncol=100, nrow=100,xmn=0, xmx=1, ymn=0, ymx=1)
names(r) <- "stratum"
values(r) <- rep(c(1:9),times=c(400,1200,400,1200,3600,1200,400,1200,400))
plot(r)

sample <- sampleStratified(r, size=10)


sample.sphe <- sphe[sample[,1],]
sample.gaus <- gaus[sample[,1],]
sample.cub <- cub[sample[,1],]
sample.expo <- expo[sample[,1],]

ggplot(sphe)+
  geom_tile(aes(x=x1,y=x2, col=Simu.V1.S1, fill=Simu.V1.S1))+scale_fill_viridis_c()

ggplot(sample.sphe)+
  geom_tile(aes(x=x1,y=x2, col=Simu.V1.S1, fill=Simu.V1.S1))+scale_fill_viridis_c()

# Loop

Res = vector('list', length(sims))
Res.red = vector('list', length(sims))
Iter <- 25
#Non conditionnal simulation
data.db <- db.create(data.frame(x=c(0,0,1,1),x2=c(0,1,1,0)))
grid.db <- db.grid.init(data.db,nodes=c(100,100))

mod.sphe <- model.create("Spherical",range=.2,sill=1)
mod.gaus <- model.create("Gaussian",range=.2,sill=1)
mod.cub <- model.create("Cubic",range=.2,sill=1)
mod.expo <- model.create("Exponential",range=.2,sill=1)

#Sample area
r <- raster(ncol=100, nrow=100,xmn=0, xmx=1, ymn=0, ymx=1)
names(r) <- "stratum"
values(r) <- rep(c(1:9),times=c(400,1200,400,1200,3600,1200,400,1200,400))
plot(r)


load("data/simu_stock/out100/0.6/lh/simsDE202010091026.RData")

tic()
for (i in 1:length(sims)){
  
  Iter.list = vector('list', Iter)
  Iter.list.red = vector('list', Iter)
  
  for (j in c(1:Iter)){
    
    #Get biomass 
    biomass <- (as.integer(sims[[1]][[7]]@.Data[,,,,,j]))
    
    #Simulation of the time series
    sim.sphe<- simtub(model=mod.sphe,dbout=grid.db,nbsim=60,nbtuba=100)
    sim.gaus<- simtub(model=mod.gaus,dbout=grid.db,nbsim=60,nbtuba=100)
    sim.cub<- simtub(model=mod.cub,dbout=grid.db,nbsim=60,nbtuba=100)
    sim.expo<- simtub(model=mod.expo,dbout=grid.db,nbsim=60,nbtuba=100)
    
    # Extract from db
    sphe <- sim.sphe@items
    gaus <- sim.gaus@items
    cub <- sim.cub@items
    expo <- sim.expo@items
    
    #Get only positive values
    sphe[,c(4:63)] <- sweep(sphe[,c(4:63)],2,abs(apply(sphe[,c(4:63)],2, min)),"+")
    gaus[,c(4:63)] <- sweep(gaus[,c(4:63)],2,abs(apply(gaus[,c(4:63)],2, min)),"+")
    cub[,c(4:63)] <- sweep(cub[,c(4:63)],2,abs(apply(cub[,c(4:63)],2, min)),"+")
    expo[,c(4:63)] <- sweep(expo[,c(4:63)],2,abs(apply(expo[,c(4:63)],2, min)),"+")
    
    #Standardize with biomass
    f.s <-biomass/colSums(sphe[,c(4:63)])
    f.g <-biomass/colSums(gaus[,c(4:63)])
    f.c <-biomass/colSums(cub[,c(4:63)])
    f.e <-biomass/colSums(expo[,c(4:63)])
    
    sphe[,c(4:63)] <- t(t(sphe[,c(4:63)])*f.s)
    gaus[,c(4:63)] <- t(t(gaus[,c(4:63)])*f.g)
    cub[,c(4:63)] <- t(t(cub[,c(4:63)])*f.c)
    expo[,c(4:63)] <- t(t(expo[,c(4:63)])*f.e)
    
    #Sample
    sample <- sampleStratified(r, size=10)
    sample.sphe <- sphe[sample[,1],]
    sample.gaus <- gaus[sample[,1],]
    sample.cub <- cub[sample[,1],]
    sample.expo <- expo[sample[,1],]
    
    Iter.list[[j]] <- list(Spheric=sphe,Gaussian=gaus,Cubic=cub,Exponential=expo,Sample_spherical=sample.sphe,Sample_gaussian=sample.gaus,Sample_cubic=sample.cub,Sample_exponential=sample.expo)
    Iter.list.red[[j]] <- list(Sample_spherical=sample.sphe,Sample_gaussian=sample.gaus,Sample_cubic=sample.cub,Sample_exponential=sample.expo)
  }
  Res[[i]] <- Iter.list
  Res.red[[i]] <- Iter.list.red

}
toc()
names(Res) <- names(sims)
names(Res.red) <- names(sims)
save(Res, "data/simu_geostat/Samples.RData")
save(Res.red, "data/simu_geostat/Samples_red.RData")

