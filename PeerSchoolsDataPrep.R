data <- read.csv("K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\SchoolGrades18.csv",colClasses = "character")
v <- c(
 "English.Language.Arts.Achievement", 
 "English.Language.Arts.Learning.Gains", 
 "English.Language.Arts.Learning.Gains.of.the.Lowest.25.",
 "Mathematics.Achievement", 
 "Mathematics.Learning.Gains", 
 "Mathematics.Learning.Gains.of.the.Lowest.25.", 
 "Science.Achievement", 
 "Social.Studies.Achievement", 
 "Middle.School.Acceleration", 
 "Graduation.Rate.2016.17", 
 "College.and.Career.Acceleration.2016.17", 
 "Total.Points.Earned", 
 "Total.Components", 
 "Percent.of.Total.Possible.Points", 
 "Percent.Tested", 
 "Percent.of.Minority.Students", 
 "Percent.of.Economically.Disadvantaged.Students"
)
for(j in v) data[,j] <- as.numeric(data[,j])
data$School <- paste0(data$District.Name,"; ",data$School.Name)
rownames(data) <- data$School
data <- data[order(data$School),]

charter <- data[data$Charter.School == "YES",]
data <- data[data$Charter.School != "YES",]
elementary <- data[data$School.Type == "01",]
middle <- data[data$School.Type == "02",]
high <- data[data$School.Type == "03",]
other <- data[data$School.Type == "04",]


D.elementary <- dist(elementary[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "maximum")
D.middle <- dist(middle[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "maximum")
D.high <- dist(high[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "maximum")
D.charter <- dist(charter[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "maximum")
D.other <- dist(other[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "maximum")

D.elementary <- as.matrix(D.elementary)
D.middle <- as.matrix(D.middle)
D.high <- as.matrix(D.high)
D.charter <- as.matrix(D.charter)
D.other <- as.matrix(D.other)

M.elementary <- D.elementary
M.middle <- D.middle
M.high <- D.high
M.charter <- D.charter
M.other <- D.other

for(j in 1:nrow(M.elementary)){
  v <- colnames(D.elementary)[order(D.elementary[j,])]
  d <- strsplit(x = rownames(M.elementary)[j],split = ";")[[1]][1]
  v <- c(v[!grepl(x = v,pattern = paste0(d,";"))],v[grepl(x = v,pattern = paste0(d,";"))])
  M.elementary[j,] <- v
}
colnames(M.elementary) <- paste0("X",1:ncol(M.elementary))

for(j in 1:nrow(M.middle)){
  v <- colnames(D.middle)[order(D.middle[j,])]
  d <- strsplit(x = rownames(M.middle)[j],split = ";")[[1]][1]
  v <- c(v[!grepl(x = v,pattern = paste0(d,";"))],v[grepl(x = v,pattern = paste0(d,";"))])
  M.middle[j,] <- v
}
colnames(M.middle) <- paste0("X",1:ncol(M.middle))

for(j in 1:nrow(M.high)){
  v <- colnames(D.high)[order(D.high[j,])]
  d <- strsplit(x = rownames(M.high)[j],split = ";")[[1]][1]
  v <- c(v[!grepl(x = v,pattern = paste0(d,";"))],v[grepl(x = v,pattern = paste0(d,";"))])
  M.high[j,] <- v
}
colnames(M.high) <- paste0("X",1:ncol(M.high))

for(j in 1:nrow(M.charter)){
  v <- colnames(D.charter)[order(D.charter[j,])]
  d <- strsplit(x = rownames(M.charter)[j],split = ";")[[1]][1]
  v <- c(v[!grepl(x = v,pattern = paste0(d,";"))],v[grepl(x = v,pattern = paste0(d,";"))])
  M.charter[j,] <- v
}
colnames(M.charter) <- paste0("X",1:ncol(M.charter))

for(j in 1:nrow(M.other)){
  v <- colnames(D.other)[order(D.other[j,])]
  d <- strsplit(x = rownames(M.other)[j],split = ";")[[1]][1]
  v <- c(v[!grepl(x = v,pattern = paste0(d,";"))],v[grepl(x = v,pattern = paste0(d,";"))])
  M.other[j,] <- v
}
colnames(M.other) <- paste0("X",1:ncol(M.other))

elementary <- elementary[,sapply(X = elementary,FUN = function(x) sum(is.na(x))) < nrow(elementary)/2]
middle <- middle[,sapply(X = middle,FUN = function(x) sum(is.na(x))) < nrow(middle)/2]
high <- high[,sapply(X = high,FUN = function(x) sum(is.na(x))) < nrow(high)/2]
charter <- charter[,sapply(X = charter,FUN = function(x) sum(is.na(x))) < nrow(charter)/2]
other <- other[,sapply(X = other,FUN = function(x) sum(is.na(x))) < nrow(other)/2]

elementary <- data.frame(elementary,M.elementary)
middle <- data.frame(middle,M.middle)
high <- data.frame(high,M.high)
charter <- data.frame(charter,M.charter)
other <- data.frame(other,M.other)

write.csv(elementary,"K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\elementary.csv",row.names = FALSE,na = "")
write.csv(middle,"K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\middle.csv",row.names = FALSE,na = "")
write.csv(high,"K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\high.csv",row.names = FALSE,na = "")
write.csv(charter,"K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\charter.csv",row.names = FALSE,na = "")
write.csv(other,"K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\other.csv",row.names = FALSE,na = "")

