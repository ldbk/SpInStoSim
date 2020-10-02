#Example of non conditional and conditional simulations in geostatistics with RGeostats

library(RGeostats)

#Non conditionnal simulations with turning bands

# Generate 4 realizations of non conditional simulation
# with a varying number of bands
projec.toggle(0)
data.db <- db.create(data.frame(x1=c(0,0,1,1),x2=c(0,1,1,0)))
grid.db <- db.grid.init(data.db,nodes=c(100,100))
mod <- model.create("Exponential",range=.15,sill=1)
sim1<- simtub(model=mod,dbout=grid.db,nbsim=1,nbtuba=1)
sim2<- simtub(model=mod,dbout=grid.db,nbsim=1,nbtuba=10)
sim3<- simtub(model=mod,dbout=grid.db,nbsim=1,nbtuba=100)
sim4<- simtub(model=mod,dbout=grid.db,nbsim=1,nbtuba=1000)
# Generate figures
plot(sim1,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))
plot(sim2,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))
plot(sim3,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))
plot(sim4,name="Simu.V1.S1",pos.legend=1,zlim=c(-4,4))


#Conditionning data

# Create the simulation grid
projec.toggle(0)
grid.db <- db.grid.init(db.create(data.frame(x1=c(0,0,1,1),x2=c(0,1,1
                                                                ,0))),
                        nodes=c(101,101))
# Create a variogram model
a1 <- model.create(vartype=1,sill=0.01)
mod <- model.create(vartype=3,sill=0.99,range=0.25,model=a1)
# Generate the truth
real <- simtub(model=mod,dbout=grid.db,nbsim=1,nbtuba=1000)
# Sample the true field at random
data.df <- data.frame(x1=round(runif(100,0,1),2),x2=round(runif(100,0
                                                                ,1),2))
data.df <- merge(data.df,real[,2:4],by=c("x1","x2"));names(data.df)[3
]<-"z1"
data.db <- db.create(data.df)
# Perform an ordinary kriging in unique neighbourhood
kri <- kriging(dbin=data.db,dbout=grid.db,model=mod,
               neigh=neigh.create(type=0,ndim=2),uc=NA,mean=0)
# Perform a conditional simulation
sc <- simtub(dbin=data.db,dbout=grid.db,model=mod,
             neigh= neigh.create(type=0,ndim=2),uc=NA,mean=0,
             nbsim=1,nbtuba=1000,seed=232132)
# Generate figures
plot(real,name="Simu.V1.S1",title="Reality",pos.legend=1,zlim=c(-4,4)
)
plot(data.db,title="Sample data")
plot(kri,name="Kriging.z1.estim",title="Kriging",pos.legend=1,zlim=c(
  -4,4))
plot(sc,name="Simu.z1.S1",title="Conditional simulation",
     pos.legend=1,zlim=c(-4,4))


