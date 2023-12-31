library(sf)
library(here)
library(dplyr)
# plot
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
# Updated -> 4.x
library(tmap)
library(tidyverse)
# load data
library(jsonlite)
library(readxl)

# Read in world city shape file and gender inequality data
CountriesShape <- read_sf(here("data/week04", "World_Countries_Generalized/World_Countries_Generalized.shp"))
GenderInequality <- read_excel(here("data/week04", "HDR21-22_Statistical_Annex_HDI_Trends_Table.xlsx"),
                            sheet= 1,
                            na = c("..", " ", "na", "NA", "NULL", "null"))

# Table beautify
col_name = c("HDI_rank", "Country", "gen_1990", "N1","gen_2000", "N2", "gen_2010", "N3", "gen_2015"
             , "N4", "gen_2018", "N5", "gen_2019", "N6", "gen_2020", "N7", "gen_2021")
# Delete top 2 rows !Run once only
GenderInequality <- GenderInequality[-c(1:5),-c(17:ncol(GenderInequality))]
# Change the col name of the table
names(GenderInequality) <- col_name
# Remove Na columns
GenderInequality <- GenderInequality %>%
  select_if(~!all(is.na(.)))


self.colSub <- function(table_, col_1, col_2, one=1, auto_crop=FALSE){
  # table_: The table will be processed
  # col_1: The first column
  # col_2: The second column
  # one: Used contain info column start index
  # auto_crop: whether remove other not used columns
  
  out <- table_
  
  out <- out %>%
    mutate(sub_dif = round(table_[[col_1]] - table_[[col_2]], digits = 3))
  
  
  if(!auto_crop){
    print("No crop")
    return(out)
  }
  
  out <- out %>%
    select(., -all_of(c(one:ncol(out)-1)))
  #This uses the select function to drop columns from the data frame.
  #c(one:ncol(out)-1): This creates a vector of column indices starting from column one up to the second-to-last column (ncol(out)-1).
  #all_of: This is a helper function in dplyr that helps to work with column names or expressions provided as variables.
  # -: The minus sign indicates that the specified columns should be dropped.
  return(out)
}

attr(self.colSub, "comment") <- "Apply subtraction to two specific columns and remove not used columns"
#The code you provided is using the attr function in R to set an attribute named "comment" for an object

# Make a difference between two target columns
new <- self.colSub(GenderInequality, 6, 9, one = 4, auto_crop = TRUE) %>%
  as.data.frame(new)
# GenderInequality: The object on which the subtraction operation is performed.
# 6 and 9: Columns to subtract.
# one = 4: Column to use as one

# Join the difference value
combined_out <- left_join(CountriesShape, new, by = c("COUNTRY" = "Country"))

combined_out
plot(combined_out)
