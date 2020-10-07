#Libraries
require(icesDatras) #Needed for CGFS
require(worms)
require(dplyr)

###Get Channel Ground Fish Survey (CGFS) data from DATRAS #####
#Is time consuming as files are heavy (especially HL data)

view <- getDatrasDataOverview(surveys = "FR-CGFS", long = FALSE) #Check data availability
HH_CGFS <- getDATRAS(record="HH", survey="FR-CGFS", year=colnames(view$`FR-CGFS`),
                     quarters=4) #Haul data
HL_CGFS <- getDATRAS(record="HL", survey="FR-CGFS", year=colnames(view$`FR-CGFS`),
                     quarters=4) #Length-based data
CA_CGFS <- getDATRAS(record="CA", survey="FR-CGFS", year=colnames(view$`FR-CGFS`),
                     quarters=4) #Age-based data


#Keep original data intact
HH <- HH_CGFS
HL <- HL_CGFS
CA <- CA_CGFS

##Data cleaning

HH[which(HH==-9, arr.ind = T)]<- NA #Convert -9 to NA as it is a convention 
HL[which(HL==-9, arr.ind = T)]<- NA
CA[which(CA==-9, arr.ind = T)]<- NA

#Remove variables unused later
CA <- CA[,-c(1,2,3,4,5,6,7,8,9,13,15,16,17,21,25)]
HH<- HH[,-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 16, 18, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,55, 56, 57, 58, 59, 60, 61)] # RecordType, Survey, Quarter, Country, Ship, Gear, SweepLngt, GearExp (NA), DoorType, Mois, Jour, TimeShot, Stratum (NA), DayNight, Depth, HaulVal, HydroStNo, StdSpecRecCode, BycSpecRecCode, DataType, NetOpening, Rigging (Na), Tickler (NA), WarpLgnt, WarpDia, WarpDen (NA), DoorSurface, DoorWgt, DoorSpread, Buoyancy, KiteDim (NA), WgtGroundRope, TowDir, GroundSpeed, SpeedWater, SurCurDir (NA), SurCurSpeed (NA),BotCurDir(NA), BotCurSpeed (NA), WindDir, WindSpeed, SwellDir(NA), SwellHeight(NA), SurTemp, BotTemp, SurSal, BotSal, ThermoCline (NA), ThClineDepth (NA), DateofCalculation. 
HL<- HL[,-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 15, 16, 18, 19, 20, 21, 23, 28)] # RecordType, Survey, Quarter (all 4), Country, Ship (GWD ou THA2), Gear, SweepLngt, GearExp (NA), DoorType (P ou R), SpecCodeType (all W), SpecVal, Sex, CatIdentifier, NoMeas, SubFactor, SubWgt (NA), LngtCode, Date of calculation.
HL<- unique(HL)

#Complete and choose species variable 
HL$Valid_Aphia[is.na(HL$Valid_Aphia)]<-	HL$SpecCode[is.na(HL$Valid_Aphia)] #Replace AphiaID with SpecCode more complete
HL<- HL[!is.na(HL$Valid_Aphia),] #Keep rows with a species code
HL<- HL[, -4] #Remove the redundant variable

#WingSpread
Wing<- read.csv(file= "data/simu_geostat/WingCGFS2015_2019 original.csv", sep=";") #Complete the WingSpread variable. File obtained by IFREMER
Wing<- Wing[, -c(1, 2, 4, 5, 6)]
names(Wing)[1]<- "StNo"
names(Wing)[2]<- "WingSpread"


# Joint tables
J1<- HH %>% left_join(Wing, by= c("StNo"))
J1$WingSpread.x[is.na(J1$WingSpread.x)]<-	J1$WingSpread.y[is.na(J1$WingSpread.x)]
J1<- J1[, -11]
names(J1)[10]<- "WingSpread"
J1$WingSpread[is.na(J1$WingSpread)]<- 10 #Replace NA with the standard value

J2<- HL %>% left_join(J1, by= c("StNo", "HaulNo", "Year"))

J3 <- J2 %>% left_join(CA, by=c("StNo", "HaulNo", "Year","LngtClass","Valid_Aphia"))


J3<- J3 %>%
  mutate(Superficie= Distance*WingSpread) %>%
  mutate(meanLat= (ShootLat+HaulLat)/2) %>%
  mutate(meanLong= (ShootLong+HaulLong)/2)

J3<- J3 %>%
  group_by(Year,meanLong,meanLat,Valid_Aphia) %>%
  mutate(Density=mean(TotalNo*(HaulDur/60)/Superficie)) %>% #TotalNo is number of individuals caught per hour. Scale to get number of individuals caught during the fishing operation 
  ungroup()

Species_name <- wormsbyid(unique(J3$Valid_Aphia)) #Adding species names from WORMS
J3 <- J3 %>% left_join(Species_name[,c(3,9)], by=c("Valid_Aphia"="valid_AphiaID"))

CGFS<- J3 %>% dplyr::select(StNo, Year, Superficie,Distance, meanLat, meanLong, HaulDur, scientificname,LngtClass,HLNoAtLngt, TotalNo, Sex, Age, Maturity, NoAtALK, IndWgt,Density)


# Final dataset
write.csv(CGFS, file= "data/simu_geostat/CGFS.csv")
#####