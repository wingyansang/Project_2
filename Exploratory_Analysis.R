library (data.table)
library (dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

load(bikes_dt, file = "bikes_dt.rda")
load(bikes2_dt, file = "bikes2_dt.rda")
load(census_dt, file = "census_dt.rda")
load(census_perc, file = "census_perc.rda")
#convert all blanks, NAs, and "unknowns" to NA type
#bikes = read.csv("bikes.csv", sep = ",", stringsAsFactors = FALSE, na.strings = c('', 'unknown', 'Unknown', "NA", "-", "?"))
#bikes_dt = as.data.table(bikes)
#--------------------------------------------------------------------------------------------------------------

###Lock_Desc column: combine two of the columns to "chain". Combine U-lock columns. 
bikes_dt[(Lock_desc == "Chain with padlock") | Lock_desc == ("Heavy duty bicycle security chain"), Lock_desc := "Chain"]
bikes_dt[(Lock_desc == "Two U-locks") | Lock_desc == ("U-lock"), Lock_desc := "U-lock"]

#--------------------------------------------------------------------------------------------------------------

### Manufacturer column: change names 
bikes_dt[(Manufacturer == "6 Ku Balance"), Manufacturer := "6KU"]
bikes_dt[(Manufacturer == "Aventon"), Manufacturer := "Aventón"]
bikes_dt[(Manufacturer == "Bob Jakson"), Manufacturer := "Bob Jackson"]
bikes_dt[(Manufacturer == "Burley Design"), Manufacturer := "Burley"]
bikes_dt[(Manufacturer == "Cadillac Beach Cruiser"), Manufacturer := "Cadillac"]
bikes_dt[(Manufacturer == "Canadian tire"), Manufacturer := "Canadian Tire"]
bikes_dt[(Manufacturer == "Carerra"), Manufacturer := "Carrera bicycles"]
bikes_dt[(Manufacturer == "Charger"), Manufacturer := "Charge"]
bikes_dt[(Manufacturer == "Chris King hub/archtype rim front wheel only"), Manufacturer:="Chris King"]
bikes_dt[(Manufacturer == "Christiana"), Manufacturer:="Christiania Bikes"]
bikes_dt[(Manufacturer == "Co-Op"), Manufacturer:="Co-Op(REI)"]
bikes_dt[(Manufacturer == "Co-Op(REI)"), Manufacturer:="Co-Op (REI)"]
bikes_dt[(Manufacturer == "Columba"), Manufacturer:="Columbia"]
bikes_dt[(Manufacturer == "Corsa Elegance"), Manufacturer:="Corsa"]
bikes_dt[(Manufacturer == "Crossroads"), Manufacturer:="Crossroad"]
bikes_dt[(Manufacturer == "Cruiser with Toddler Seat on Back"), Manufacturer:="Cruiser"]
bikes_dt[(Manufacturer == "Cruz"), Manufacturer:="Cruzbike"]
bikes_dt[(grepl("Custom", Manufacturer, ignore.case = TRUE)), Manufacturer:="Custom"]
bikes_dt[(Manufacturer == "Cutom"), Manufacturer:="Custom"]
bikes_dt[(Manufacturer == "Blue"), Manufacturer:="Blue Avenue"]
bikes_dt[(Manufacturer == "Dillinger"), Manufacturer:="Dillenger"]
bikes_dt[(Manufacturer == "Firm Strong") | (Manufacturer == "Firmstrong Prestige"), Manufacturer:="Firmstrong"]
bikes_dt[(Manufacturer == "Fit Bike Co"), Manufacturer:="Fit bike Co."]
bikes_dt[(Manufacturer == "Fly Bikes"), Manufacturer:="Flybikes"]
bikes_dt[(Manufacturer == "Generic steel frame, matte black, covered in stickers"), Manufacturer :="Generic"]
bikes_dt[(Manufacturer == "Genesis V2100"), Manufacturer:="Genesis"]
bikes_dt[(Manufacturer == "Giant Liv"), Manufacturer:="Giant"]
bikes_dt[(Manufacturer == "Gran Royal"), Manufacturer:="Gran Royale"]
bikes_dt[(Manufacturer == "Hampton Cruiser"), Manufacturer:="Hampton"]
bikes_dt[(Manufacturer == "Hand Made") | (Manufacturer == "Handbuilt"), Manufacturer:="Custom"]
bikes_dt[(Manufacturer == "HardRock") | (Manufacturer == "Hard Rock"), Manufacturer:="Specialized"]
bikes_dt[(Manufacturer == "Havok"), Manufacturer:="Havoc"]
bikes_dt[(Manufacturer == "Hero"), Manufacturer:="Hero Cycles Ltd"]
bikes_dt[(Manufacturer == "Hero Honda Splendorplus"), Manufacturer:="Hero Cycles Ltd"]
bikes_dt[(grepl("Home", Manufacturer, ignore.case = TRUE)), Manufacturer:="Custom"]
bikes_dt[Manufacturer== "Hyper Co", Manufacturer:="Hyper"]
bikes_dt[Manufacturer== "I dont know", Manufacturer:= NA]
bikes_dt[Manufacturer== "Ibis Mojo", Manufacturer:= "Ibis"]
bikes_dt[Manufacturer== "Ironhorse", Manufacturer:= "Iron Horse Bicycles"]
bikes_dt[Manufacturer== "Iron Man By Huff", Manufacturer:= "Huffy"]
bikes_dt[(Manufacturer == "J C Higgens") | (Manufacturer == "J.c. Higins"), Manufacturer:="J.C. Higgins"]
bikes_dt[Manufacturer== "Jeep Comanche Classic", Manufacturer:= "Jeep"]
bikes_dt[Manufacturer== "Juliana (Santa Cruz)", Manufacturer := "Juliana Bicycles"]
bikes_dt[(grepl("Kent", Manufacturer, ignore.case = TRUE)), Manufacturer:="Kent"]
bikes_dt[(grepl("Kildemoes", Manufacturer, ignore.case = TRUE)), Manufacturer:="Kildemoes"]
bikes_dt[Manufacturer== "LeMond", Manufacturer := "LeMond Racing Cycles"]
bikes_dt[(grepl("Leader", Manufacturer, ignore.case = TRUE)), Manufacturer:="Leader"]
bikes_dt[Manufacturer== "Lightspeed", Manufacturer := "Litespeed"]
bikes_dt[(grepl("Limit", Manufacturer, ignore.case = TRUE)), Manufacturer:="Limit"]
bikes_dt[(grepl("Look", Manufacturer, ignore.case = TRUE)), Manufacturer:="Look"]
bikes_dt[(grepl("Magna", Manufacturer, ignore.case = TRUE)), Manufacturer:="Magna"]
bikes_dt[(grepl("Mercier", Manufacturer, ignore.case = TRUE)), Manufacturer:="Mercier"]
bikes_dt[Manufacturer== "Miir", Manufacturer:= "MiiR"]
bikes_dt[(grepl("Misfit", Manufacturer, ignore.case = TRUE)), Manufacturer:="Misfit Cycles"]
bikes_dt[Manufacturer== "Mission Bicycle Company", Manufacturer:= "Mission Bicycles"]
bikes_dt[Manufacturer== "Mongoose ledge 2.1", Manufacturer:= "Mongoose"]
bikes_dt[Manufacturer== "Montgomery wards", Manufacturer := "Montgomery Ward"]
bikes_dt[Manufacturer== "Myata", Manufacturer := "Miyata"]
bikes_dt[(Manufacturer == "Nemises Project Steveland V3")|(Manufacturer== "Nemesis Project Proto Type Steveland V2"),
Manufacturer := "Nemesis Project"]
bikes_dt[(grepl("New Belgium", Manufacturer, ignore.case = TRUE)), Manufacturer:="New Belgium"]
bikes_dt[Manufacturer== "Nikishi", Manufacturer := "Nishiki"]
bikes_dt[(Manufacturer== "No logo") | (Manufacturer == "No name listed on frame, likely a local frame builder")| 
         (Manufacturer == "No-name Brand") | (Manufacturer == "Nondescript") | 
           (Manufacturer == "None"), Manufacturer := NA]
bikes_dt[Manufacturer== "One One", Manufacturer := "On-One"]
bikes_dt[(grepl("Pacific", Manufacturer, ignore.case = TRUE)), Manufacturer:="Pacific"]
bikes_dt[(grepl("Peugeot", Manufacturer, ignore.case = TRUE)), Manufacturer:="Peugeot"]
bikes_dt[(grepl("PureFix", Manufacturer, ignore.case = TRUE)), Manufacturer:="Pure Fix"]
bikes_dt[(grepl("REI", Manufacturer, ignore.case = FALSE)), Manufacturer:="REI"]
bikes_dt[(grepl("\\<Rad\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "Rad Power"]
bikes_dt[Manufacturer == "RadPower Bikes", Manufacturer := "Rad Power"]
bikes_dt[Manufacturer == "Raleigh (Talus)", Manufacturer := "Raleigh"]
bikes_dt[(grepl("\\<Reid\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "Reid"]
bikes_dt[Manufacturer == "Retrospective", Manufacturer := "Retrospec"]
bikes_dt[Manufacturer == "Rolfast", Manufacturer := "Rollfast"]
bikes_dt[Manufacturer == "S-Works", Manufacturer := "Specialized"]
bikes_dt[Manufacturer == "S Amp M Fully Aftermarket", Manufacturer := "S and M"]
bikes_dt[(grepl("\\<SE\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "SE"]
bikes_dt[Manufacturer == "SKU", Manufacturer := "6KU"]
bikes_dt[Manufacturer == "SR", Manufacturer := "SR Suntour"]
bikes_dt[Manufacturer == "REL Co-op", Manufacturer := "REI"]
bikes_dt[Manufacturer == "Se Draft", Manufacturer := "SE"]
bikes_dt[Manufacturer == "Sette Razzo (polished AL) frame with Salsa Fork(black)", Manufacturer := "Sette"]
bikes_dt[(Manufacturer == "Shwin") | (Manufacturer == "Shwinn"), Manufacturer := "Schwinn"]
bikes_dt[Manufacturer == "SoftRide", Manufacturer := "Softride"]
bikes_dt[(grepl("\\<Specialized\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "Specialized"]
bikes_dt[(Manufacturer == "Shwin") | (Manufacturer == "Shwinn"), Manufacturer := "Schwinn"]
bikes_dt[Manufacturer == "State Bicycle Co", Manufacturer := "State Bicycle Co."]
bikes_dt[Manufacturer == "Steelman", Manufacturer := "Steelman Cycles"]
bikes_dt[Manufacturer == "Stray Cat", Manufacturer := "Stray Cat Bicycles"]
bikes_dt[Manufacturer == "Street runner Miyata", Manufacturer := "Miyata"]
bikes_dt[(grepl("\\<Sub\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "Subrosa"]
bikes_dt[Manufacturer == "Subrosa shadow united", Manufacturer := "Subrosa"]
bikes_dt[Manufacturer == "Team Fuji", Manufacturer := "Fuji"]
bikes_dt[(grepl("\\<Takara\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "Takara"]
bikes_dt[(grepl("<Terra", Manufacturer, ignore.case = FALSE)), Manufacturer := "Terra Trike"]
bikes_dt[Manufacturer == "Thorn Cycles", Manufacturer := "Throne Cycles"]
bikes_dt[Manufacturer == "Tokyo Bike", Manufacturer := "Tokyobike"]
bikes_dt[Manufacturer == "Trail Mate", Manufacturer := "Trailmate"]
bikes_dt[Manufacturer == "Treck", Manufacturer := "Trek"]
bikes_dt[Manufacturer == "Trek Bicycle Corporation", Manufacturer := "Trek"]
bikes_dt[Manufacturer == "Uknown", Manufacturer := NA]
bikes_dt[(grepl("\\<Unbranded\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := NA]
bikes_dt[(grepl("\\<Unknown\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := NA]
bikes_dt[(grepl("\\<Urban\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "Urban"]
bikes_dt[Manufacturer == "Velovie", Manufacturer := "Velo Vie"]
bikes_dt[Manufacturer == "Wal-Mart", Manufacturer := "Walmart"]
bikes_dt[Manufacturer == "Wilier", Manufacturer := "Wilier Triestina"]
bikes_dt[Manufacturer == "Woman S Trek", Manufacturer := "Trek"] 
bikes_dt[Manufacturer == "Womens Specialized Arial", Manufacturer := "Specialized"]
bikes_dt[Manufacturer == "Worksman", Manufacturer := "Worksman Cycles"]
bikes_dt[Manufacturer == "Xl22 Diamondback", Manufacturer := "Diamondback"]
bikes_dt[(grepl("\\<gary\\>", Manufacturer, ignore.case = FALSE)), Manufacturer := "Gary Fisher"]
bikes_dt[Manufacturer == "hampton", Manufacturer := "Hampton"]
bikes_dt[Manufacturer == "handmade bamboo frame", Manufacturer := "Custom"]
bikes_dt[Manufacturer == "i don't know", Manufacturer := NA]
bikes_dt[Manufacturer == "kabuki", Manufacturer := "Kabuki"]
bikes_dt[Manufacturer == "mongoose", Manufacturer := "Mongoose"]
bikes_dt[Manufacturer == "n/a - Hand Built", Manufacturer := "Custom"]
bikes_dt[Manufacturer == "ritchey", Manufacturer := "Ritchey"]
bikes_dt[(Manufacturer == "shamino") | (Manufacturer == "shimano"), Manufacturer := "Shimano"]
bikes_dt[(Manufacturer == "unbranded"), Manufacturer:= NA]
bikes_dt[(Manufacturer == "walmart"), Manufacturer := "Walmart"]

#--------------------------------------------------------------------------------------------------------------

### Year column: convert those < 1957 and > 2017 to NAs
bikes_dt[(Year < 1957), Year := NA]
bikes_dt[(Year == 0) | (Year > 2017), Year := NA]

#--------------------------------------------------------------------------------------------------------------

### Location column: extract zip code state and create 2 columns (city if state not available)

#Remove white-spaces at beginning of each string element in list
lst <-  strsplit(bikes_dt$Location, split = ",")
for (i in 1:length(bikes_dt$Location)){
  for (j in 1: length(lst[[i]])) {
    lst[[i]][j] = trimws(lst[[i]][j])
    j = j + 1
  }
  i = i +1
  j = 0
}

#grab zip code data and store it in vec
patterns <- c("^[0-9]{5}$", "^[0-9]{5}-[0-9]{4}$")
vec <- c()
i = 1
for (i in 1:length(bikes_dt$Location)){
  if (any(grepl(paste(patterns,collapse = "|"),lst[[i]]))) {
    vec[i] = lst[[i]][grepl(paste(patterns,collapse = "|"),lst[[i]])]
  } else {
    vec[i] = NA
  }
  i = i +1
}
 
#remove last four digits of zip code
pattern <- "^[0-9]{5}"
vec2 <- c()
i = 1
for (i in 1:length(bikes_dt$Location)){
  if (is.na(vec[i])) {
    vec2[i] = NA
  } else {
    vec2[i] = str_extract(vec[i], pattern)
  }
  i = i + 1
}

#add this as a column to bikes_dt
bikes_dt[,"Zip":= as.integer(vec2)]

#read in zip code file to join with bikes_dt
zip_data <- read.csv("zip_code_database.csv", sep = ",", stringsAsFactors = FALSE)
zip_dt <- as.data.table(zip_data)
zip2_dt <- zip_dt[, .(zip, primary_city, state, county, country, latitude, longitude)]

#merge bikes_dt with the zip file
bikes_dt <- merge(bikes_dt, zip2_dt, by.x = "Zip", by.y = "zip", all.x = TRUE)

#--------------------------------------------------------------------------------------------------------------

### Convert Date column to date type
vec <- strsplit(bikes_dt$Date, split = '\\.')
result <- c()
for (i in 1:length(bikes_dt$Date)) {
  if (any(is.na(vec[[i]]))) {
    result[i] = NA
  } else{
    result[i] = paste0(vec[[i]], collapse = "-")
  }
  i = i +1
}
result <- as.Date(result, "%m-%d-%Y")
bikes_dt[,Date := result]

#--------------------------------------------------------------------------------------------------------------

### Convert more than one color in Color column to "multi-colored"
bikes_dt[str_count(bikes_dt$Color,"\\S+")>1, Color := "Multi-colored"]

#--------------------------------------------------------------------------------------------------------------

###Clean several unusual rows with odd state columns: AE, AP, AA
bikes_dt[state == "AP" & Zip == 96603, c("Zip", "state", "county") := c(95603, "CA", "Placer County")]
bikes_dt[Zip == 95603 & Color == "Blue", state:= "CA"]
bikes_dt[Zip == 95603 & Color == "Blue", county:= "Placer County"]
bikes_dt[Zip == 95603 & Color == "Blue", latitude:= 38.92]
bikes_dt[Zip == 95603 & Color == "Blue", longitude:= -121.07]
bikes_dt[Zip == 95603 & Color == "Blue", Zip = 95603]

bikes_dt[state == "AP", state := "OR"]

#--------------------------------------------------------------------------------------------------------------

###Filter out NAs in Zip and county columns
bikes2_dt <- bikes_dt[is.na(Zip) == FALSE & is.na(county) == FALSE & county != '']

#Filter out Puerto Rico and zip code with 0
bikes2_dt <-bikes2_dt[Zip != 617 & Zip != 930 & Zip != 0]

#--------------------------------------------------------------------------------------------------------------
# Add column with full state names
states <- read.csv("states.csv", sep = ",", stringsAsFactors = FALSE)
x <- merge(bikes2_dt, states, by.x = "state", by.y = "Abbreviation")
bikes2_dt <- x

#re-order columns so the geographical location are in one region of data table
reorder_vec <- c("Serial", "Date", "Zip", "Location", "primary_city", "county", "state", "State", "country", "latitude",
                 "longitude", "Manufacturer", "Model", "Year", "Material", "Size", "Color", "Lock_desc",
                 "Lock_circ", "Incident")
setcolorder(bikes2_dt, reorder_vec)


#--------------------------------------------------------------------------------------------------------------
# Change Brooklyn and Bronx to New York

bikes2_dt[primary_city == "Bronx" | primary_city == "Brooklyn", primary_city := "New York"]

#--------------------------------------------------------------------------------------------------------------
#Filter out rows with no dates
bikes2_dt <- bikes2_dt[is.na(Date) == FALSE]

#--------------------------------------------------------------------------------------------------------------
#Add seasons
bikes2_dt[month(Date)>=1 & month(Date) <4, "Season":= "Winter"]
bikes2_dt[month(Date)>=4 & month(Date) <7, "Season":= "Spring"]
bikes2_dt[month(Date)>=7 & month(Date) <10, "Season":= "Summer"]
bikes2_dt[month(Date)>=10 & month(Date) <=12, "Season":= "Fall"]


#--------------------------------------------------------------------------------------------------------------
#Rename values in the Lock_circ column
bikes2_dt[Lock_circ == "Other situation, please describe below.", Lock_circ := "Other"]
bikes2_dt[Lock_circ == "Lock cut and present", Lock_circ := "Lock cut/present"]
bikes2_dt[Lock_circ == "Lock cut and missing", Lock_circ := "Lock cut/missing"]
bikes2_dt[Lock_circ == "Bike was not locked", Lock_circ := "Bike wasn't locked"]
bikes2_dt[Lock_circ == "Object bike locked to was compromised", 
          Lock_circ := "locked to object was compromised"]
bikes2_dt[Lock_circ == "Lock opened and present", 
          Lock_circ := "Lock opened/present"]
#--------------------------------------------------------------------------------------------------------------
# Examine the top 25% zip codes for the top 10 counties
#top ten counties:
x <-  bikes2_dt[, .N, by = .(county)][order(-N)][1:10,]
setkey(bikes2_dt, county)

#create data table of just the top 10 counties and group by county/ zip
county_vec = x[["county"]]
y  <- bikes2_dt[ county %in% county_vec, .N, by = .(county, Zip)][order(county, -N)]

#--------------------------------------------------------------------------------------------------------------
# Read in csv file from US Cenus
census = read.csv("ACS.csv", sep = ",", stringsAsFactors = FALSE)
census_dt = data.table(census)
census_meta <- read.csv("ACS_meta.csv", sep = ",", stringsAsFactors = FALSE)

temp_dt <- census_dt[, c("GEO.id2", "HC01_VC03", "HC01_VC04","HC01_VC05", "HC01_VC08", "HC01_VC09", "HC01_VC10", "HC01_VC11",
              "HC01_VC12", "HC01_VC13", "HC01_VC14", "HC01_VC15","HC01_VC16", "HC01_VC17","HC01_VC18", 
              "HC01_VC19", "HC01_VC20", "HC01_VC23", "HC01_VC77", "HC01_VC78", "HC01_VC79","HC01_VC80",
              "HC01_VC81", "HC01_VC82", "HC01_VC83", "HC01_VC87", "HC01_VC88", "HC01_VC89", "HC01_VC90",
              "HC01_VC91", "HC01_VC92", "HC01_VC93")]
census_dt <- temp_dt

#merge the data table of the top 10 counties with census_dt
census_dt[["GEO.id2"]][1] = 0
census_dt[["GEO.id2"]] <- as.integer(census_dt[["GEO.id2"]])
temp_dt <- merge(y, census_dt, by.x = "Zip", by.y = "GEO.id2" ) #40 rows were not merged. represents 109 bikes
census_dt <- temp_dt

#create a 3 x 10 data table: county, top 25% quantile, % of bikes stolen
#for loop to create the quantiles and % share:
result1 <- c()
result2 <- c()
temp_dt <- NULL
prob <- .75
for (i in 1:10) {
  temp_dt <- census_dt[county == county_vec[i]]
  result1[i] <- quantile(temp_dt$N, prob) #save quantiles in result1
  result2[i] <- round(temp_dt[N >result1[i],sum(N)]/temp_dt[,sum(N)],2) #%share saved in result2
  temp_dt = NULL
  i = i + 1
}

#join the columns to form data table
top25_dt = data.table(county_vec, result1, result2)
colnames(top25_dt) = c("county", "top25_Quantile", "Percent")

#tag each row whether it is in top 25 or not
for (i in 1:10){
  census_dt[county == top25_dt[[1]][i] & N >= top25_dt[[2]][i], "top25" := 1]
  census_dt[county == top25_dt[[1]][i] & N < top25_dt[[2]][i], "top25" := 0]
  i = i +1
}

#convert census_dt character columns to numeric
temp_dt = census_dt[, lapply(.SD, as.numeric), .SDcols = -c("Zip", "county", "N")]
temp_dt[, "HC01_VC23" := NULL]
census_dt[, "HC01_VC23" := NULL]
temp2_dt <- cbind(census_dt$county, census_dt$Zip, census_dt$N, temp_dt)
census_dt <-temp2_dt
colnames(census_dt)[1:3] = c("county", "Zip", "stolen_bikes")
#add across all the columns

temp_dt <- census_dt[, lapply(.SD, sum), by= .(county, top25), .SDcols = -c("Zip", "county", "stolen_bikes")]

#combine some of the age columns
temp_dt[, c("to24") := HC01_VC08 + HC01_VC09+ HC01_VC10+ HC01_VC11+ HC01_VC12]
temp_dt[, c("to64") := HC01_VC16 + HC01_VC17]
temp_dt[, c("over65") := HC01_VC18 + HC01_VC19+ HC01_VC20]

#delete some of the age columns and the HC01_V77, HCO1_VC87
temp_dt[, c("HC01_VC08", "HC01_VC09", "HC01_VC10", "HC01_VC11", "HC01_VC12", "HC01_VC16", "HC01_VC17",
            "HC01_VC18", "HC01_VC19", "HC01_VC20"):= NULL]

temp_dt[, c("HC01_VC77", "HCO1_VC87"):= NULL]

#rename columns
colnames(temp_dt)[3:8] <- c("total_pop", "male", "female", "to34", "to44", "to54")

#add Asian and Pacific Islander
temp_dt[,c("asian_PI") := HC01_VC81 + HC01_VC82]
temp_dt[,c("HC01_VC81", "HC01_VC82") := NULL]

#rename race columns
colnames(temp_dt)[9:12] <- c("white", "black", "native_am", "other")

#delete hispanic columns
temp_dt[, c("HC01_VC87", "HC01_VC88", "HC01_VC89", "HC01_VC90", "HC01_VC91", "HC01_VC92",
            "HC01_VC93"):= NULL]

#re-order the columns
reorder_vec <- c("county","top25","total_pop","male","female","to24", "to34", "to44", "to54", "to64",
                 "over65", "white", "black", "asian_PI", "native_am", "other") 

setcolorder(temp_dt, reorder_vec)
census_dt <- temp_dt

#create a data table of percentages 
result = list()
for (i in 1:13) {
  result[[i]] = round(100*census_dt[[i+3]]/census_dt[[3]],1)
  i = i+1
}

result2 <- as.data.table(result)
census_perc <- cbind(census_dt$county, census_dt$top25, census_dt$total_pop, result2)
colnames(census_perc) <- colnames(census_dt)

# reorder the data table
census_perc <- census_perc[order(county, -top25)]
#--------------------------------------------------------------------------------------------------------------
# Plot the results of Sex

g <- ggplot(census_perc, aes(county, male, fill = factor(top25))) + 
  geom_bar(stat="identity", position = "dodge")

g + coord_flip() + scale_fill_discrete(name = "Stolen Bikes Rank", breaks=c("1", "0"), 
                                       labels = c("Top Quartile", "Bottom 3 Quartile")) + 
  scale_y_discrete(limits = c(0, 25, 50)) + 
  ggtitle("County Comparsion by Sex") + xlab("County") + ylab("Percent Male") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        text = element_text(size=20), axis.title = element_text(face = "bold")) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

#--------------------------------------------------------------------------------------------------------------
# Plot the results of Age
data1 <- census_perc[county == "District of Columbia", .(top25, to24, to34, to44, to54, to64, over65)]
data2 <- melt(data1, id.vars = "top25")

g <- ggplot(data2, aes(variable, value, fill = factor(top25))) + 
  geom_bar(stat="identity", position = "dodge")

g + scale_fill_discrete(name = "Stolen Bikes Rank", breaks=c("1", "0"), 
                                       labels = c("Top Quartile", "Bottom 3 Quartile")) + 
  scale_y_discrete(limits = c(0, 10, 20, 30, 40)) +
  scale_x_discrete(labels = c("0-24", "25-34", "35-44", "45-54", "55-64", "over 65"))+
  ggtitle("Age Distribution Comparison") + xlab("Age Group") + ylab("Percent") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        text = element_text(size=20), axis.title = element_text(face = "bold")) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))


#--------------------------------------------------------------------------------------------------------------
# Plot the results of Race
data1 <- census_perc[county == "District of Columbia", .(top25, white, black, asian_PI, native_am, other)]
data2 <- melt(data1, id.vars = "top25")

g <- ggplot(data2, aes(variable, value, fill = factor(top25))) + 
  geom_bar(stat="identity", position = "dodge")

g + scale_fill_discrete(name = "Stolen Bikes Rank", breaks=c("1", "0"), 
                        labels = c("Top Quartile", "Bottom 3 Quartile")) + 
  scale_y_discrete(limits = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_x_discrete(labels = c("White", "Black", "Asian_PI", "Native_American", "Other"))+
  ggtitle("Race Distribution Comparison") + xlab("Race") + ylab("Percent") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        text = element_text(size=20), axis.title = element_text(face = "bold")) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

#--------------------------------------------------------------------------------------------------------------
# Perform One-Way ANOVA Test to see if the Difference in Averge Bikes Stolen by Season is Stat. Significant

aov_dt <- bikes2_dt[year(Date)>=2012 & year(Date)<=2017  & !(year(Date) == 2017 & Season == "Fall")&
                    is.na(Season) == FALSE, .N, by = .(Season, year(Date))]
#since Fall 2012 was not full season, assumed the first 13 days of the season is same as daily average 
#for rest of season
aov_dt[[3]][17] = 689+90/78

summary(aov(aov_dt$N ~ aov_dt$Season))

sd_season <- aov_dt[, sd(N), by = .(Season)]
avg_season <- aov_dt[,sum(N)/5, by = .(Season)]


#test homoscedastiscity #1: Bartlett
bartlett.test(aov_dt$N ~ aov_dt$Season, data = aov_dt) 

#test homoscedastiscity #2: Levene
library(car)
leveneTest(aov_dt$N ~ as.factor(aov_dt$Season), data = aov_dt) 

#test for normality
qqnorm(aov_dt$N - mean(aov_dt$N)) #image included in Shinyapp
qqline(aov_dt$N - mean(aov_dt$N)) #image included in Shinyapp

#create data frame summarizing average and standard deviation by season

table_2 <- data.frame(Test = c("Bartlett's Test", "Levene's Test", "ANOVA"), Test_Stat = c(1.3913, 0.2975, 3.096),
                      Null_Hypothesis = c("Same Variance", "Same Variance", "Same Mean"),  
                      p_value = c(0.7076, 0.8267, 0.0566), Conclusion = 
                        c("Can't reject null", "Can't reject null", "Can't reject null"))

library(gridExtra)
library(grid)


t1 <- ttheme_default(core=list(
  fg_params=list(fontface=c(rep("plain", 2), "bold.italic")),
  bg_params = list(fill=c(rep(c("grey95", "grey90"),
                              length.out=2), "#6BAED6"),
                   alpha = rep(c(1,0.5), each=5))))

table2 <- grid.table(table_2, theme = t1, rows = NULL) ##image included in Shinyapp
