
library(readr)
library(anytime)
library(tidyquant)
library(dplyr)

library(Hmisc)
library(psych)
setwd("D:/NYCDSA/Project 4 - Capstone Project")


reuters_data = read_csv("reuters_data.csv")
eur = read_csv("eurfinal50var.csv")