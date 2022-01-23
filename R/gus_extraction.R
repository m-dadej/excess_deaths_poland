library(readxl)
library(tidyverse)
library(glue)
library(stringr)
library(lubridate)

# in a new data downloaded from GUS :
# https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/zgony-wedlug-tygodni,39,2.html
# the actual data set should start from 10th row
# one have to manually change it

# The data already present in this directory is adjusted

# sorry for including variables in polish (protip: "wiek" is age in polish)

dane_folder <- "gus_data"

wiek <- read_excel(glue("{dane_folder}/Zgony według tygodni w Polsce_2000.xlsx"),
                   skip = 7, sheet = 3) %>%
          .[2:nrow(.),] %>%
          rename("wiek" = '0', "kod_regionu" = '1', "region" = "2")  %>%
          rename_with(~1:52, -c(1:3)) %>%
          pivot_longer(cols = -c(wiek, kod_regionu, region)) %>%
          mutate(year = 2000,
                 sex = "F",
                 kraj = str_sub(kod_regionu, 1,2),
                 makroregion = str_sub(kod_regionu, 3,3),
                 wojewodztwo = str_sub(kod_regionu, 3,4),
                 powiat = str_sub(kod_regionu, 3,5)) %>%
          rename("week" = name) %>%
          mutate_at(vars(makroregion, wojewodztwo, powiat), function(x){ifelse(x == "", NA, x)})

clean_sheet <- function(excel_file, year, sex){
  n_weeks <- ncol(excel_file) - 3
  excel_file[2:nrow(excel_file),] %>%
    rename("wiek" = '0', "kod_regionu" = '1', "region" = "2") %>%
    rename_with(~1:n_weeks, -c(1:3)) %>%
    pivot_longer(cols = -c(wiek, kod_regionu, region)) %>%
    mutate(year = year, sex = sex,
           kraj = str_sub(kod_regionu, 1,2),
           makroregion = str_sub(kod_regionu, 3,3),
           wojewodztwo = str_sub(kod_regionu, 3,4),
           powiat = str_sub(kod_regionu, 3,5)) %>%
    rename("week" = name)
}

read_excel(glue("{dane_folder}/Zgony według tygodni w Polsce_2000.xlsx"),
           skip = 7, sheet = 3) %>%
  clean_sheet(year = 2020, sex = "T")

file_years <- 2000:2021

mortality <- data.frame(wiek = NA, kod_regionu = NA, region = NA, week = NA, value = NA, year = NA, sex = NA,
                        kraj = NA, makroregion = NA, wojewodztwo = NA, powiat = NA)

for (y in file_years) {
  
  path <- glue("{dane_folder}/Zgony według tygodni w Polsce_{y}.xlsx")
  ogol<- read_excel(path,skip = 7, sheet = 1) %>%
          clean_sheet(year = y, sex = "T")
  male <- read_excel(path,skip = 7, sheet = 2) %>%
            clean_sheet(year = y, sex = "M")
  fem <- read_excel(path,skip = 7, sheet = 3) %>%
             clean_sheet(year = y, sex = "F")
  mortality <- rbind(mortality, ogol, male, fem)
  print(paste("downloading files from year: ", y, sep = ""))
}

mortality <- mutate(mortality,
                    makroregion = ifelse(makroregion == "", NA, makroregion),
                    wojewodztwo = ifelse(wojewodztwo == "", NA, wojewodztwo),
                    powiat = ifelse(powiat == "", NA, powiat),
                    geo_unit = nchar(kod_regionu)) %>%
                  .[-1,]

lvls <- c("0 - 4","5 - 9", "10 - 14", "15 - 19","20 - 24","25 - 29","30 - 34", "35 - 39","40 - 44",
          "45 - 49","50 - 54","55 - 59", "60 - 64", "65 - 69" , "70 - 74" , "75 - 79",
          "80 - 84" ,"85 - 89", "90 i więcej", "Ogółem")

mortality$wiek <- factor(mortality$wiek, levels = lvls)

