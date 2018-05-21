library(data.table)

#--irrigation analysis 2018


setwd("/dmine/data/geog401/")

irrigated_percentage <- read.csv("Irrigated_percentage_refined.csv", header=TRUE)
colnames(irrigated_percentage) <- c("NAME", "Irrigated", "Not_Irrigated")


counties <- readShapePoly('/dmine/data/counties/UScounties.shp',
                     proj4string=CRS
                     ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

irrigated_counties <- readShapePoly('boise_irrigated_wheat_counties2.shp',
                                    proj4string=CRS
                                    ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
counties <- subset(counties, STATE_NAME %in% "Idaho")
colnames(counties@data) <- c("NAME", "State", "State_FIPS", "County_FIPS", "FIPS")


plot(counties)

plot(irrigated_counties, col = "lightblue", add=TRUE)


HUC <- readShapePoly('HUC6_Snake.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

irrigated_counties <- readShapePoly('boise_irrigated_wheat_counties2.shp',
                     proj4string=CRS
                     ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#counties <- subset(counties, STATE_NAME %in% "California")
#colnames(counties@data) <- c("County", "State", "State_FIPS", "County_FIPS", "FIPS")

rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
                                space = "rgb")
rgb.palette(17)
irrigated_counties <- merge(irrigated_counties, irrigated_percentage, by = "NAME")

#plot(irrigated_counties, col = gray(irrigated_counties$Not_Irrigated))


rbPal <- colorRampPalette(c('blue', 'lightblue'))
irrigated_counties@data$Col <- rbPal(17)[as.numeric(cut(irrigated_counties@data$Not_Irrigated, seq(0, 1.0, by = .1)))]

plot(counties)
plot(irrigated_counties, col = "red", add=TRUE)

plot(irrigated_counties, col = irrigated_counties@data$Col, add=TRUE)

text(coordinates(irrigated_counties), as.character(round(irrigated_counties$Irrigated, 3)), cex=.6, col = "white") 

irrigated_counties_onlydroughtclais <- subset(irrigated_counties, NAME == "Minidoka" | NAME == "Twin Falls" | NAME == "Fremont" | NAME == "Madison" | NAME == "Bingham" | NAME == "Power" | NAME == "Cassia" | NAME == "Bonneville" | NAME == "Bannock" | NAME == "Teton" | NAME == "Oneida" | NAME == "Franklin" | NAME == "Caribou" | NAME == "Bear Lake")

plot(counties)

plot(irrigated_counties_onlydroughtclais, col = "lightblue", add=TRUE)

#plot(irrigated_counties$Irrigated, col=rgb.palette(17))




setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

statez = c("Idaho")
Idaho_list1 <- paste("Minidoka", "Lincoln", "Jerome", "Jefferson", "Gooding", "Twin Falls", "Fremont", "Madison", "Bingham", "Power", "Cassia", "Bonneville", "Bannock", "Teton", "Oneida", "Franklin", "Caribou", "Bear Lake", sep="|")



setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
palouse_Idaho_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]
kk="Idaho"
#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Idaho_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#--loop list for county by fip
countyfiploop <- counties@data$FIPS

#--data frame of county fip list
countyfiplist <- data.frame(counties@data$FIPS)

#--data frame of county names
countynames <- data.frame(counties@data$NAME)

#combo of county names and fip for this list
countylist <- cbind(countynames, countyfiplist)
colnames(countylist) <- c("county", "countyfips")










setwd(paste("/dmine/data/USDA/agmesh-scenarios/Allstates", sep=""))

files <- list.files(pattern = "Idaho")
myfiles = lapply(files, read.csv, strip.white = TRUE, header = TRUE) 
x <- do.call(rbind, myfiles)
DTall <- data.table(x)

#--all 18 counties for 2007 - 2015

DTall$commodity <- trimws(DTall$commodity)
DTall$county <- trimws(DTall$county)
DTall$damagecause <- trimws(DTall$damagecause)


DTall <- subset(DTall, year >= 2007 & year <= 2015)

DTall <- subset(DTall, county == "Minidoka" | county == "Lincoln" | county == "Jerome" | county == "Jefferson" | county == "Gooding" | county == "Twin Falls" | county == "Fremont" | county == "Madison" | county == "Bingham" | county == "Power" | county == "Cassia" | county == "Bonneville" | county == "Bannock" | county == "Teton" | county == "Oneida" | county == "Franklin" | county == "Caribou" | county == "Bear Lake")
DTall <- subset(DTall, commodity == "WHEAT")
DTall <- subset(DTall, damagecause == "Drought")


#--singluar counties for a singular year, my month

DTall_singlecounty <- data.table(x)

DTall_singlecounty$commodity <- trimws(DTall_singlecounty$commodity)
DTall_singlecounty$county <- trimws(DTall_singlecounty$county)
DTall_singlecounty$damagecause <- trimws(DTall_singlecounty$damagecause)

DTall_singlecounty <- subset(DTall_singlecounty, year >= 2007)

DTall_singlecounty <- subset(DTall_singlecounty, county == "Power")

DTall_singlecounty <- subset(DTall_singlecounty, commodity == "WHEAT")
DTall_singlecounty <- subset(DTall_singlecounty, damagecause == "Drought")


DTall_singlecounty_agg <- aggregate(DTall_singlecounty$loss, by= list(DTall_singlecounty$month), FUN = "sum")
colnames(DTall_singlecounty_agg) <- c("month", "loss")

missingmonths <- as.data.frame(c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
colnames(missingmonths) <- c("month")

DTall_singlecounty_agg <- join(missingmonths, DTall_singlecounty_agg, by = "month")
DTall_singlecounty_agg[is.na(DTall_singlecounty_agg)] <- 0


#--get climate monthly data for Idaho 18 counties

setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries")


files <- list.files(pattern = "Idaho")
myfiles = lapply(files, read.csv, strip.white = TRUE, header = TRUE) 
x <- do.call(rbind, myfiles)
DTall_climate <- data.table(x)
#DTall_climate$countyfips <- factor(DTall_climate$countyfips)


DTall_climate <- subset(DTall_climate, county == "Minidoka" | county == "Lincoln" | county == "Jerome" | county == "Jefferson" | county == "Gooding" | county == "Twin Falls" | county == "Fremont" | county == "Madison" | county == "Bingham" | county == "Power" | county == "Cassia" | county == "Bonneville" | county == "Bannock" | county == "Teton" | county == "Oneida" | county == "Franklin" | county == "Caribou" | county == "Bear Lake")
DTall_climate_irrigated <- subset(DTall_climate, county == "Minidoka" | county == "Lincoln" | county == "Jerome" | county == "Jefferson" | county == "Gooding" | county == "Twin Falls" | county == "Fremont" | county == "Madison" )
DTall_climate_nonirrigated <- subset(DTall_climate, county == "Teton" | county == "Oneida" | county == "Franklin" | county == "Caribou" | county == "Bear Lake" )




DTall_climate <- subset(DTall_climate, commodity == "WHEAT")
DTall_climate <- subset(DTall_climate, damagecause == "Drought")

DTall_climate <- aggregate(DTall_climate$loss, by = list(DTall_climate$pr, DTall_climate$tmmx, DTall_climate$pet, DTall_climate$year, DTall_climate$month, DTall_climate$county), FUN = "sum")
colnames(DTall_climate) <- c("pr", "tmmx", "pet", "year", "month", "county", "loss")

all_lm1 <- lm(loss ~ pr + tmmx +  pet, data = DTall_climate)



DTall_climate_irrigated <- subset(DTall_climate_irrigated, commodity == "WHEAT")
DTall_climate_irrigated <- subset(DTall_climate_irrigated, damagecause == "Drought")

DTall_climate_irrigated <- aggregate(DTall_climate_irrigated$loss, by = list(DTall_climate_irrigated$pr, DTall_climate_irrigated$tmmx, DTall_climate_irrigated$pet, DTall_climate_irrigated$year, DTall_climate_irrigated$month, DTall_climate_irrigated$county), FUN = "sum")
colnames(DTall_climate_irrigated) <- c("pr", "tmmx", "pet", "year", "month", "county", "loss")

all_lm1_irrigated <- lm(loss ~ pr + tmmx +  pet, data = DTall_climate_irrigated)




DTall_climate_nonirrigated <- subset(DTall_climate_nonirrigated, commodity == "WHEAT")
DTall_climate_nonirrigated <- subset(DTall_climate_nonirrigated, damagecause == "Drought")

DTall_climate_nonirrigated <- aggregate(DTall_climate_nonirrigated$loss, by = list(DTall_climate_nonirrigated$pr, DTall_climate_nonirrigated$tmmx, DTall_climate_nonirrigated$pet, DTall_climate_nonirrigated$year, DTall_climate_nonirrigated$month, DTall_climate_nonirrigated$county), FUN = "sum")
colnames(DTall_climate_nonirrigated) <- c("pr", "tmmx", "pet", "year", "month", "county", "loss")

all_lm1_nonirrigated <- lm(loss ~ pr + tmmx +  pet, data = DTall_climate_nonirrigated)

















DTall_sum <- sum(DTall$loss)
DTall_mean <- mean(DTall$loss)
DTall_count <- nrow(DTall)
DTall_values <- paste("Washington", " ", "WHEAT" " all years, all damage causes", sep="") 
DTall2 <- rbind(DTall_values, DTall_sum, DTall_mean, DTall_count)

DTall3 <- as.data.frame(DTall2)
row.names(DTall3) <- c("year_and_state", "loss_summary", "mean_summary", "count_summary")
DTall3$Description <- c("Year and State", "Loss Summary", "Mean Loss", "Total Claim Counts")
colnames(DTall3)[1] <- c("Summary Values")
#colnames(DT21) <- paste("Summary Values", input$year7, " ", input$state7, sep="") 
DTall4 <- DTall3[,c(2,1)]
DTall4



