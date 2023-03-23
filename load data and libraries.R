#Analyse CAF Program surveys data (application, 2021 evaluation, 2022 evaluation)
#created March 7, 2023
#This is a project and linked to a github repository. 
#I am using renv which means that all libraries are loading specifically within this project only
#First I need to import the data

#make a dataframe from the loaded data, preserve headers and put NA into the blank cells
COMBOdata <- read.csv("~/R/CAF-eval-analysis/Combined_dataset.csv", 
                     header=TRUE, sep = ",", na.strings=c("", "NA"))

#remove top header rows with questions and import id descriptions. 
COMBOdata <- COMBOdata[-c(1,2),]

library (tidyverse)
library (ggplot2)
library (patchwork)
library (ggpubr)
library (vtable)

install.packages("MatrixModels")

as_tibble(COMBOdata)
#end script