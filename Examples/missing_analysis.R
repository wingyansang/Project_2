library (data.table)
library (dplyr)
save(bikes_dt, file = "bikes_dt.rda")
save(bikes, file = "bikes.rda")
save(na_miss_unkn_dt, file = "na_miss_unkn_dt.rda")

bikes = read.csv("bikes.csv", sep = ",", stringsAsFactors = FALSE)

#create a data table
bikes_dt <- as.data.table(bikes)
#####################################################################################
######################## Missing Data Analysis ##############################################
#####################################################################################


#calculate the number of NAs in each column and create a data table of summary
na_dt <- as.data.table(colSums(is.na(bikes_dt)))
na_dt <- na_dt[, data.table(t(.SD), keep.rownames=FALSE)]
colnames(na_dt) <- colnames(bikes_dt)

#calculate the number of missing cells in each column and create a data table of summary
missing_dt <- as.data.table(colSums(bikes_dt[, c(1:12)] == '', na.rm = TRUE))
missing_dt <- missing_dt[, data.table(t(.SD), keep.rownames=TRUE)]
missing_dt[, c(1)] <- NULL
colnames(missing_dt) <- colnames(bikes_dt)

#calculate the number of Unkown
unknown_dt <- as.data.table(colSums(bikes_dt[, c(1:12)] == 'Unknown' | bikes_dt[, c(1:12)] == 'unknown', na.rm = TRUE))
unknown_dt <- unknown_dt[, data.table(t(.SD), keep.rownames=TRUE)]
unknown_dt[, c(1)] <- NULL
colnames(unknown_dt) <- colnames(bikes_dt)


#rbind the missing/na/unknown data table and add total to bottom with percentage missing on bottom
#na_missing_dt <- rbind(na_dt, missing_dt)
na_miss_unkn_dt <-rbind(na_dt, missing_dt, unknown_dt, na_dt + missing_dt + unknown_dt)
na_miss_unkn_dt <- rbind(na_miss_unkn_dt, round(na_miss_unkn_dt/dim(bikes_dt)[1]*100,1)[4])
row.names(na_miss_unkn_dt) = c("NA", "Missing", "Unknown", "Total", "Percent")
