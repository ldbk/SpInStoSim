#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)
library(raster)
library(RGeostats)

#install.packages("FLCore", repos="http://flr-project.org/R")
library(FLCore)
#devtools::install_github("flr/FLIfe", INSTALL_opts=c("--no-multiarch")) 
library(FLBRP)

#Data formatting for CGFS #####
CGFS <- read.csv("data/simu_geostat/CGFS.csv", head=T)

##Remove first column
CGFS.df <- CGFS %>% dplyr::select(-X)

##Adding rows with 0 densities
ttsel <- CGFS.df %>% dplyr::select(StNo, scientificname, Year, meanLong, meanLat, Density) %>% distinct() 
codif <- ttsel %>% dplyr::select(StNo, Year, meanLong, meanLat) %>% distinct() 
allstrat <- ttsel %>% tidyr::expand(StNo, scientificname) 
CGFS.df0 <- left_join(allstrat, codif) %>% left_join(ttsel) %>% dplyr::select(StNo, meanLong, meanLat, scientificname, Density) %>% mutate(Density = ifelse(is.na(Density), 0, Density))
CGFS.df0 <- left_join(codif, CGFS.df0) %>% dplyr::select(StNo, Year, meanLong, meanLat, scientificname, Density) %>% unique() 

##Filter for targeted species
CGFS.df0 <- CGFS.df0 %>% filter(scientificname %in% c("Limanda limanda",
                                                      "Pleuronectes platessa",
                                                      "Solea solea",
                                                      "Mullus surmuletus",
                                                      "Raja clavata",
                                                      "Raja montagui",
                                                      "Raja undulata",
                                                      "Raja brachyura",
                                                      "Raja microocellata",
                                                      "Leucoraja naevus",
                                                      "Amblyraja radiata",
                                                      "Leucoraja circularis",
                                                      "Leucoraja fullonica"))
CGFS.df0$scientificname <- factor(CGFS.df0$scientificname)

##Add standardized density for each year
CGFS.df0$Std.Density <- 0
for (i in unique(CGFS.df0$Year)){
  for (j in unique(CGFS.df0$scientificname)){
    CGFS.df0[CGFS.df0$scientificname==j & CGFS.df0$Year==i,]$Std.Density <- CGFS.df0[CGFS.df0$scientificname==j & CGFS.df0$Year==i,]$Density/sd(CGFS.df0[CGFS.df0$scientificname==j & CGFS.df0$Year==i,]$Density)
  }
}


coast <- rgdal::readOGR("data/simu_geostat/FR_southENG_coast.gpkg")
coast <- raster::crop(coast, raster::extent(-2.5,3,49,51.5))

#####

## Maps of occurrence, selection of study species#####



#Flatfish
ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Limanda limanda" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Limanda limanda" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of dab") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Pleuronectes platessa" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Pleuronectes platessa" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of plaice") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Solea solea" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Solea solea" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of sole") + xlab("Longitude")+ylab("Latitude")+theme_minimal()


#Red mullet
ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Mullus surmuletus" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Mullus surmuletus" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of mullet") + xlab("Longitude")+ylab("Latitude")+theme_minimal()


#Skates and rays
ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja clavata" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja clavata" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of thornback ray") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja montagui" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja montagui" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Raja montagui") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja undulata" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja undulata" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Raja undulata") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja brachyura" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja brachyura" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Raja brachyura") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja microocellata" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Raja microocellata" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Raja microocellata") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Leucoraja naevus" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Leucoraja naevus" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Leucoraja naevus") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Amblyraja radiata" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Amblyraja radiata" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Amblyraja radiata") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Leucoraja circularis" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Leucoraja circularis" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Leucoraja circularis") + xlab("Longitude")+ylab("Latitude")+theme_minimal()

ggplot()+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Leucoraja fullonica" & CGFS.df0$Density==0,], aes(x=meanLong, y=meanLat), col="black", pch=4)+
  geom_point(data=CGFS.df0[CGFS.df0$scientificname=="Leucoraja fullonica" & CGFS.df0$Density>0,], aes(x=meanLong, y=meanLat), col="red", pch=20)+
  geom_polygon(data = coast, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Occurence of Leucoraja fullonica") + xlab("Longitude")+ylab("Latitude")+theme_minimal()
#####

##Filter for targeted species with enough data#####
colSums(table(CGFS.df0$Year[CGFS.df0$Density>0],CGFS.df0$scientificname[CGFS.df0$Density>0]))

CGFS.df0.red <- CGFS.df0 %>% filter(scientificname %in% c("Limanda limanda",
                                                      "Pleuronectes platessa",
                                                      "Solea solea",
                                                      "Mullus surmuletus",
                                                      "Raja clavata",
                                                      "Raja montagui",
                                                      "Raja undulata",
                                                      "Raja brachyura"))
CGFS.df0.red$scientificname <- factor(CGFS.df0.red$scientificname)
#####

# Formatting data to db class object#####
CGFS.df0.red <- CGFS.df0.red %>% na.omit() %>% mutate(Std.Density=round(Std.Density,5)) %>% distinct()

CGFS.df0.red <- CGFS.df0.red %>% dplyr::select(-Density) %>% distinct() %>% spread(scientificname,Std.Density)

#db object for RGeostats
db <- db.create(CGFS.df0.red)
db <- db.locate(db,c(4,5), "x")
db <- db.locate(db,"Limanda limanda","z")
#####

# Simulation grid and neighborhood#####
grid <- db.grid.init(db, nodes=c(100,100))
#layer to crop
liste <- with(CGFS.df0.red, chull(meanLong, meanLat))
hull <- CGFS.df0.red[liste, c("meanLong", "meanLat")]
poly2 <- Polygon(hull)
p2 <- SpatialPolygons(list(Polygons(list(poly2), "p2")))
buff <- buffer(p2, 0.2)

res <- rgeos::gDifference(buff, coast) #crop
#save(res, "polygon.RData")
hull <- fortify(res)


poly <- polygon.create(x=hull[hull$group=="1.1",1],y=hull[hull$group=="1.1",2]) #RGeostats polygon
grid <- db.polygon(grid, poly)


# Moving or unique neighbouhood?
nei1 <- neigh.create(ndim=2,type=0) #unique
nei2 <- neigh.create(ndim=2,type=2,nmini=10,nmaxi=50,radius=1) # moving
#####

#Simulations###

#Dab#####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Limanda limanda","z")
  
# Mean variogram
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
LIMALIM.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####

#Red mullet#####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Mullus surmuletus","z")

# Mean variogram
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
MULLSUR.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####

#Plaice#####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Pleuronectes platessa","z")

# Mean variogram
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
PLEUPLA.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####

#Raja brachyura####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Raja brachyura","z")

# Mean variogram?
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
RAJABRA.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####

#Raja clavata#####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Raja clavata","z")

# Mean variogram
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
RAJACLA.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####

#Raja montagui#####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Raja montagui","z")

# Mean variogram
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
RAJAMON.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####

#Raja undulata#####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Raja undulata","z")

# Mean variogram
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
RAJAUND.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####

#Sole#####
db.CGFS <- db.locate(db,"Year","code")
db.CGFS <- db.locate(db.CGFS,"Solea sola","z")

# Mean variogram
vg <- vario.calc(db.CGFS, lag=0.05, nlag=40,opt.code=1, tolcode=0) 
#plot(vg,npairdw=T,inches=0.1,las=1,col=2,lwd=2)
vg.mod <- model.auto(vario=vg, struct=c(1,2,3,4,5), npairdw=TRUE, title="", inches=.05)

#Simulations
SOLESOL.Sims <- simtub(dbin=duplicate(db.CGFS),dbout=grid,model=vg.mod,
                       neigh= nei1,uc=NA,mean=0,
                       nbsim=35,nbtuba=1000,seed=42)
#####


#Condition density according to stock simulations

load(file = "data/simu_stock/out/0.6/simsDET1.2202010071545.RData")

SP.tot <- sims[1][1]$SP_ID0_NR_ED0_SELF_UR0_TS60$index
