
setwd("D:/NYCDSA/Project 4 - Capstone Project")

library(readr)

### Grab Files
reuters = read_csv("reuters_data_with_location.csv", col_names = T)
library('tidyr')
a = separate(reuters,post,into = c("location", "post","post2"),sep = "\\(Reuters\\) -",remove = TRUE)

b= a[c(7069,7062,7070),]
a[c(7069,7062,7070),]["post"] = a[c(7069,7062,7070),]["post2"]
a[c(7069,7062,7070),]["post"] 
a = a[-8]

write.csv(a, "reuters_data_with_location.csv", row.names = FALSE)

loc123= unique(reuters$location)
loc123= data.frame(unique(reuters$location))
loc123 = separate(loc123,unique.reuters.location.,into = c("location", "post","post2"),sep = "-",remove = TRUE)
#loc123 = separate(c,location,into = c("location", "post","post2"),sep = ",",remove = TRUE)
locl = loc123$location
locl = data.frame(locl)

colnames(locl)= "location"
local = locl

local = local[-c(1,2,3), ,drop = FALSE]

local$location = as.character(local$location)

local = local[-c(8,44,47,81,83,84,86,87,89,98,101,111,134,211,248,254,259,261,278,285,287,288,290,293,298,308,310:313,315,318,332,336,344,348,364,352), ,drop = FALSE]
rownames(local) <- NULL
local = local[-c(105,209,158,68,132,179,271,72,138,143,144,176,140,46,37,35,149,264,273,167,274,180,217,268,174,269,226), ,drop = FALSE]
local = local[-c(185,290,74,111,165,212,140,245,161), ,drop = FALSE]

local2 = local[order(local),, drop=FALSE]
rownames(local2) <- NULL

loc123 = separate(local2,location,into = c("location", "location2","location3"),sep = "/",remove = TRUE)


#write.csv(local2, "reuters_locations_list.csv", row.names = FALSE)

local = local[-c(289,231,193,247,180,297,283,162,36,38,49,153,75,151,156,157,189,295,85,354,145,192,171,217,117), ,drop = FALSE]

local[1,]
###############################################################################################################



