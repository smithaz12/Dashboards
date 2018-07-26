data <- as.data.frame(readxl::read_excel("K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\SchoolGrades18.xls",col_types = "text",skip = 3))
data <- data[order(data[,"School Name"]),]
data <- data[order(data[,"District Name"]),]
data <- unique(data[,c("District Name","School Name","Percent of Minority Students","Percent of Economically Disadvantaged Students","Grade 2018")])
data[,"Percent of Minority Students"] <- as.numeric(data[,"Percent of Minority Students"])
data[,"Percent of Economically Disadvantaged Students"] <- as.numeric(data[,"Percent of Economically Disadvantaged Students"])
summary(data)

elementary <- data[grepl(x = data[,"School Name"],pattern = " ELEMENTARY SCHOOL|GOLDSBORO ELEMENTARY MAGNET| ELEMENTARY"),]
middle <- data[grepl(x = data[,"School Name"],pattern = " MIDDLE SCHOOL| MIDDLE MAGNET| MIDDLE ACADEMY"),]
high <- data[grepl(x = data[,"School Name"],pattern = " HIGH SCHOOL|CROOMS ACADEMY OF INFORMATION TECHNOLOGY"),]
other <- data[!grepl(x = data[,"School Name"],pattern = " ELEMENTARY SCHOOL| MIDDLE SCHOOL| HIGH SCHOOL|CROOMS ACADEMY OF INFORMATION TECHNOLOGY|GOLDSBORO ELEMENTARY MAGNET| ELEMENTARY| MIDDLE MAGNET| MIDDLE ACADEMY"),]

seminole <- data[data[,"District Name"] == "SEMINOLE",]
seminole.elementary <- seminole[grepl(x = seminole[,"School Name"],pattern = " ELEMENTARY SCHOOL|GOLDSBORO ELEMENTARY MAGNET| ELEMENTARY"),]
seminole.middle <- seminole[grepl(x = seminole[,"School Name"],pattern = " MIDDLE SCHOOL| MIDDLE MAGNET| MIDDLE ACADEMY"),]
seminole.high <- seminole[grepl(x = seminole[,"School Name"],pattern = " HIGH SCHOOL|CROOMS ACADEMY OF INFORMATION TECHNOLOGY"),]
seminole.other <- seminole[!grepl(x = seminole[,"School Name"],pattern = " ELEMENTARY SCHOOL| MIDDLE SCHOOL| HIGH SCHOOL|CROOMS ACADEMY OF INFORMATION TECHNOLOGY|GOLDSBORO ELEMENTARY MAGNET| ELEMENTARY| MIDDLE MAGNET| MIDDLE ACADEMY"),]

elementary <- unique(dplyr::bind_rows(seminole.elementary,elementary))
middle <- unique(dplyr::bind_rows(seminole.middle,middle))
high <- unique(dplyr::bind_rows(seminole.high,high))

M.elementary <- matrix(NA,nrow = 6,ncol = 8)
rownames(M.elementary) <- c("euclidean","maximum","manhattan","canberra","binary","minkowski")
colnames(M.elementary) <- c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")
M.middle <- M.elementary
M.high <- M.elementary

require(cluster)
for(j in c("euclidean","maximum","manhattan","canberra","binary","minkowski")){
  dist.elementary <- dist(elementary[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],method = j)
  dist.middle <- dist(middle[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],method = j)
  dist.high <- dist(high[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],method = j)
  for(k in c("ward.D","ward.D2","single","complete","average","mcquitty","median","centroid")){
    try({
      hclust.elementary <- hclust(dist.elementary,method = k)
      hclust.elementary$height <- rank(hclust.elementary$height)
      M.elementary[j,k] <- coef.hclust(hclust.elementary)
    })
    try({
      hclust.middle <- hclust(dist.middle,method = k)
      hclust.middle$height <- rank(hclust.middle$height)
      M.middle[j,k] <- coef.hclust(hclust.middle)
    })
    try({
      hclust.high <- hclust(dist.high,method = k)
      hclust.high$height <- rank(hclust.high$height)
      M.high[j,k] <- coef.hclust(hclust.high)
    })
  }
}

dist.elementary <- dist(elementary[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],method = "maximum")
dist.middle <- dist(middle[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],method = "manhattan")
dist.high <- dist(high[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],method = "maximum")
hclust.elementary <- hclust(dist.elementary,method = "ward.D")
hclust.middle <- hclust(dist.middle,method = "ward.D")
hclust.high <- hclust(dist.high,method = "ward.D")
for(j in 777) print(c(j,max(table(elementary[,"District Name"],cutree(hclust.elementary,j))["SEMINOLE",])))
elementary <- dplyr::bind_cols(elementary,cluster = cutree(hclust.elementary,100))
middle <- dplyr::bind_cols(middle,cluster = cutree(hclust.middle,100))
high <- dplyr::bind_cols(high,cluster = cutree(hclust.high,100))


dist.elementary <- as.matrix(dist.elementary)
dist.middle <- as.matrix(dist.middle)
dist.high <- as.matrix(dist.high)
elementary$School <- paste0(elementary[,"District Name"],"; ",elementary[,"School Name"])
middle$School <- paste0(middle[,"District Name"],"; ",middle[,"School Name"])
high$School <- paste0(high[,"District Name"],"; ",high[,"School Name"])

rownames(dist.elementary) <- elementary[,"School"]
rownames(dist.middle) <- middle[,"School"]
rownames(dist.high) <- high[,"School"]
colnames(dist.elementary) <- elementary[,"School"]
colnames(dist.middle) <- middle[,"School"]
colnames(dist.high) <- high[,"School"]

M.elementary <- dist.elementary
for(j in 1:nrow(dist.elementary)) M.elementary[j,] <- names(dist.elementary[j,order(dist.elementary[j,])])
M.middle <- dist.middle
for(j in 1:nrow(dist.middle)) M.middle[j,] <- names(dist.middle[j,order(dist.middle[j,])])
M.high <- dist.high
for(j in 1:nrow(dist.high)) M.high[j,] <- names(dist.high[j,order(dist.high[j,])])

write.csv(M.elementary,file = "K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\comparableSchoolsElementary.csv")
write.csv(M.middle,file = "K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\comparableSchoolsMiddle.csv")
write.csv(M.high,file = "K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\comparableSchoolsHigh.csv")


v <- M.middle[grepl(x = M.middle[,1],pattern = "SEMINOLE;"),2]
v <- M.high[grepl(x = M.high[,1],pattern = "SEMINOLE;"),2]

plot(elementary[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "#80808033",asp = 1)
v <- as.vector(M.elementary[grepl(x = M.elementary[,1],pattern = "SEMINOLE;"),2:6])
points(elementary[elementary$School %in% v,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "#0000FF80")
points(elementary[elementary$District == "SEMINOLE",c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "red")

plot(middle[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "#80808033",asp = 1)
v <- as.vector(M.middle[grepl(x = M.middle[,1],pattern = "SEMINOLE;"),2:6])
points(middle[middle$School %in% v,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "#0000FF80")
points(middle[middle$District == "SEMINOLE",c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "red")

plot(high[,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "#80808033",asp = 1)
v <- as.vector(M.high[grepl(x = M.high[,1],pattern = "SEMINOLE;"),2:6])
points(high[high$School %in% v,c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "#0000FF80")
points(high[high$District == "SEMINOLE",c("Percent of Minority Students","Percent of Economically Disadvantaged Students")],pch = 19,col = "red")

##################################################################################################################################################

data <- read.csv("K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\SchoolGrades18.csv",stringsAsFactors = FALSE)
#data <- as.data.frame(readxl::read_excel("K:\\A&A Docs\\Aaron Smith\\ComparableSchools\\SchoolGrades18.xls",skip = 3))
data <- data[order(data[,"School.Name"]),]
data <- data[order(data[,"District.Name"]),]
data[,"School"] <- paste0(data[,"District.Name"],"; ",data[,"School.Name"])
data <- dplyr::bind_rows(data[data[,"District.Name"] == "SEMINOLE",],data[data[,"District.Name"] != "SEMINOLE",])
rownames(data) <- data[,"School"]

elementary <- data[data[,"School.Type"] == 1 & !grepl(x = rownames(data),pattern = "CHARTER"),]
middle <- data[data[,"School.Type"] == 2 & !grepl(x = rownames(data),pattern = "CHARTER"),]
high <- data[data[,"School.Type"] == 3 & !grepl(x = rownames(data),pattern = "CHARTER"),]
other <- data[data[,"School.Type"] == 4 | grepl(x = rownames(data),pattern = "CHARTER"),]

D.elementary <- as.matrix(dist(elementary[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "manhattan"))
D.elementary <- D.elementary[,!grepl(x = colnames(D.elementary),pattern = "SEMINOLE;")]
M.elementary <- D.elementary
for(j in 1:nrow(M.elementary)) M.elementary[j,] <- colnames(D.elementary)[order(D.elementary[j,])]
colnames(M.elementary) <- 1:ncol(M.elementary)
elementary <- data.frame(elementary,M.elementary)

D.middle <- as.matrix(dist(middle[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "manhattan"))
D.middle <- D.middle[,!grepl(x = colnames(D.middle),pattern = "SEMINOLE;")]
M.middle <- D.middle
for(j in 1:nrow(M.middle)) M.middle[j,] <- colnames(D.middle)[order(D.middle[j,])]
colnames(M.middle) <- 1:ncol(M.middle)
middle <- data.frame(middle,M.middle)

D.high <- as.matrix(dist(high[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "manhattan"))
D.high <- D.high[,!grepl(x = colnames(D.high),pattern = "SEMINOLE;")]
M.high <- D.high
for(j in 1:nrow(M.high)) M.high[j,] <- colnames(D.high)[order(D.high[j,])]
colnames(M.high) <- 1:ncol(M.high)
high <- data.frame(high,M.high)

D.other <- as.matrix(dist(other[,c("Percent.of.Minority.Students","Percent.of.Economically.Disadvantaged.Students")],method = "manhattan"))
D.other <- D.other[,!grepl(x = colnames(D.other),pattern = "SEMINOLE;")]
M.other <- D.other
for(j in 1:nrow(M.other)) M.other[j,] <- colnames(D.other)[order(D.other[j,])]
colnames(M.other) <- 1:ncol(M.other)
other <- data.frame(other,M.other)

seminole <- dplyr::bind_rows(
  elementary[grepl(x = rownames(elementary),pattern = "SEMINOLE;"),],
  middle[grepl(x = rownames(middle),pattern = "SEMINOLE;"),],
  high[grepl(x = rownames(high),pattern = "SEMINOLE;"),],
  other[grepl(x = rownames(other),pattern = "SEMINOLE;"),]
)

nonseminole <- dplyr::bind_rows(
  elementary[!grepl(x = rownames(elementary),pattern = "SEMINOLE;"),],
  middle[!grepl(x = rownames(middle),pattern = "SEMINOLE;"),],
  high[!grepl(x = rownames(high),pattern = "SEMINOLE;"),],
  other[!grepl(x = rownames(other),pattern = "SEMINOLE;"),]
)




