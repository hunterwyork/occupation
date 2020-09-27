# This is the primary clean and prep code. It takes in all crosswalked data
# and input data, syntesizes it, and outputs standardized census files

library(DescTools)
library(data.table)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(readxl)
library(ggrepel)
library(foreign)

#options
theme_set(theme_bw())

#
setwd("/Users/hyork/Documents/projects/occupation/code")

# read in map. This is DOT1939 to 1940 Census map
dot_cen_map <- read_xlsx("../ref/DOT1939_to_CEN1940.xlsx") %>% 
  data.table()

# read in occ to occ1940 conversion since the data uses the latter
occ_ooc_conv <- read_xls("../ref/occ_occ1940.xls") %>% 
  data.table()

# merge
dot_cen_map <- merge(dot_cen_map, occ_ooc_conv[!is.na(OCC1940),], 
                     by.x = "Census1940", by.y = "OCC1940",
                     all.x = T)
dot_cen_map[,Census1940 := OCC]

# clean up messy vars, for 2 digit codes, insert a 0
dot_cen_map[, clean_dot_code := gsub("[^0-9]", "", DOT1939)]
dot_cen_map[nchar(clean_dot_code) == 2,
            clean_dot_code := paste0("0",clean_dot_code)]
dot_cen_map[, Census1940 := gsub("\n", "", Census1940)]
dot_cen_map[, `Titles of 1940 Census` := gsub("\n", 
                                              "",
                                              `Titles of 1940 Census` )]

# load complete list of dot codes
dot_complete <- read_xlsx("../ref/DOT_1ed_1939.xlsx",col_names = F) %>% 
  data.table() %>% setnames(., c("dot_code", "dot_description"))

# create nested categories using 5 digit dot codes
# and merge onto main file
dot_complete_5 <- dot_complete[nchar(dot_code) == 5]
setnames(dot_complete_5, c("dot_description"), 
         c("dot_parent_desc"))
dot_complete[,first_five_dot := substr(dot_code,1,5)]
dot_complete <- merge(dot_complete, dot_complete_5,
                      by.x = c("first_five_dot"),
                      by.y = c("dot_code"))

# create new var of prefix of dot codes to match map
dot_complete[,clean_dot_code := substr(gsub("[^0-9]", "", dot_code),1,3)]

# do a cartesian merge to allow all combos of of dot codes
# to match potential census codes
mg_cen_dot_map <- merge(dot_complete, dot_cen_map, all.y = T, allow.cartesian = T, by = "clean_dot_code")

# also  only include most granular dot codes
mg_cen_dot_map <- mg_cen_dot_map[nchar(dot_code) != 5]

# drop nas from output. These are header rows in the dot_cen_map, so 
# nothing is really dropped
mg_cen_dot_map <- mg_cen_dot_map[!is.na(clean_dot_code) & 
                                   clean_dot_code != "<NA>" &
                                   clean_dot_code != ""]

# only preserve important columns
mg_cen_dot_map <- mg_cen_dot_map[,.(clean_dot_code, dot_code, dot_description, 
                                    `Titles of 1940 Census`, Census1940, dot_parent_desc, Occupation)]

### THIS CAN BE TOGGLED WITH LENGTH(UNIQUE(DOT DESCRIPTIONS))
# count unique codes by census category
cen_dot_code_counts<- mg_cen_dot_map[,.(num_dot_codes = length(unique(dot_code)),
                                        num_dot_titles = length(unique(dot_description))), 
                                     by = .(Census1940, Occupation)]

# create variable for graphing
cen_dot_code_counts[, var_title := paste0(Census1940, 
                                          substr(Occupation,
                                                 1,30), "...")]

# Remove unmapped
cen_dot_code_counts <- cen_dot_code_counts[Census1940 != "988"]

# sort by freq
cen_dot_code_counts[,var_title := factor(var_title,
                                         
                                         levels = cen_dot_code_counts[order(num_dot_codes),
                                                                      var_title])]

cen_dot_code_counts <- cen_dot_code_counts[order(num_dot_codes)]



# load data and save as r object for faster loading
#census_1940 <- data.table(read.dta13("inputs/usa_00003.dta"))
#saveRDS(census_1940, "inputs/usa_00003.rds")

# load data from saved rds
census_1940 <- readRDS("../inputs/usa_00003.rds")

# only keep relevant vars for now
census_1940 <- census_1940[,.(serial, hhwt, region, statefip, urban, 
                              metro, farm, 
                              multgen, pernum, perwt,sex,age,agemonth, race,hispan, bpl,
                              school, higraded, empstat, empstatd, labforce, occ,
                              occ1950, ind, ind1950, classwkr, classwkrd, wkswork1,
                              wkswork2, hrswork1, hrswork2, durunemp, uocc, uocc95, 
                              uind, uclasswk, incwage, incnonwg, occscore, sei, presgl,
                              erscor50, edscor50, npboss50, migrate5, migrate5d)]

# subset
census_1940 <- census_1940[empstat == "employed" &
                             school == "no, not in school" &
                             age %in% 25:64 &
                             hrswork1 >= 30 &
                             incwage != 999998 &
                             incwage > 0]

# derive a income/hr worked per week var
census_1940[, income_per_hr := incwage/hrswork1]

# derive a 5-year age var
census_1940[, age_cat := paste0(floor(as.numeric(as.character(age))/5)*5,
                                " to ", 
                                floor(as.numeric(as.character(age))/5)*5 + 4)]

# make occ var char
census_1940[, occ := as.character(occ)]

# make first dig var
census_1940[, first_dig := substr(occ, 1,1)]

# make map for first dig categories
map <- data.table(first_dig = as.character(0:9), 
                  occ_categ = c("Professional", "Clerical", "Service",
                                "Agricultural, etc.", "Skilled", "Skilled",
                                "Semiskilled", "Semiskilled", "Unskilled",
                                "Unskilled"))
census_1940 <- merge(census_1940, map, by = "first_dig")

# merge census 1940 file on ind codes
ind_codes <- read_xlsx("../ref/IND1940.xlsx") %>% data.table()
ind_codes[, IND := as.numeric(IND)]
census_1940 <- merge(census_1940, 
                     ind_codes[!is.na(IND) & IND != 996], 
                     by.x = "ind", by.y = "IND", all.x = T)

