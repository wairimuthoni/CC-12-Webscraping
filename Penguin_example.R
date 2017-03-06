---
# An example of web scraping and using `grep`, `gsub` and `mapply` to gather useful information from http://www.iucnredlist.org
# Author: John Godlee
# Date: 06_Mar_2017
---

# Packages
library(rvest)
library(dplyr)
library(knitr)


# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##########################################
# Sing web page scraping #################
##########################################

# Import data for one species
Penguin_html <- readLines("Aptenodytes forsteri (Emperor Penguin).html")

# Find Scientific Name
## Find anchor
grep("Scientific Name:", Penguin_html)  # 132

## Isolate 
Penguin_html[131:135]
Penguin_html[133]

## Store line in new object
species_line <- Penguin_html[133]

## Pipes to grab the text and get rid of unwanted information like html tags
species_name <- species_line %>% 
  gsub("<td class=\"sciName\"><span class=\"notranslate\"><span class=\"sciname\">", "", .) %>%  # Remove leading html
  gsub("</span></span></td>", "", .) %>%  # Remove trailing html
  gsub("^\\s+|\\s+$", "", .)  # Remove whitespace

# Find common name
## Find anchor
grep("Common Name", Penguin_html)  # 146

## Isolate
Penguin_html[130:160]
Penguin_html[150:160]
Penguin_html[151]

## Cut straight to cleaning html
common_name <- Penguin_html[151] %>% 
gsub("<td>", "", .) %>%
  gsub("</td>", "", .) %>%
  gsub("^\\s+|\\s+$", "", .)


# Find Red List Category
## Find anchor
grep("Red List Category", Penguin_html)  # 180

## Anchor
Penguin_html[179:185]  #182
Penguin_html[182]

## Cut straight to cleaning html
red_cat_line <- Penguin_html[182]
red_cat <- gsub("^\\s+|\\s+$", "", red_cat_line)

# Find date of listing
## Find anchor
grep("Date Assessed:", Penguin_html)  # 195

## Isolate
Penguin_html[192:197] #196
Penguin_html[196]

## Store line in new object
date_line <- Penguin_html[196]

## Clean html
date_assess <- date_line %>% 
  gsub("<td>", "",.) %>% 
  gsub("</td>", "",.) %>% 
  gsub("\\s", "",.)


# Combine vectors into a data frame
iucn <- data.frame(species_name, common_name, red_cat, date_assess)

# View iucn
View(iucn)

##########################################
# Multiple web page scraping #############
##########################################

#Download many web pages from list of URLs from search results from iucn:
search_html <- readLines("Search Results.html")

# Search for html tags relating to species URLs
## Create anchor and store locations in vector
line_list <- grep("<a href=\"/details", search_html)
## Use vector to isolate lines and store in vector
link_list <- search_html[line_list]

# Clean html and add IUCN URL prefix
species_list <- link_list %>%
  gsub('<a href=\"', "http://www.iucnredlist.org", .) %>%  # Add IUCN URL prefix
  gsub('\".*', "", .) %>%  # Remove punctuation marks
  gsub('\\s', "",.)  # Remove white space

# Collect species names for each species in the list of links using gsub for use in naming downloaded html files
file_list_grep <- link_list %>%  
  gsub('.*sciname\">', "", .) %>%  # Remove sciname\" at the end of the string
  gsub('</span></a>.*', ".html", .)  # remove trailing html and add .html file suffix

# Download each file and place in wd
mapply(function(x,y) download.file(x,y), species_list, file_list_grep)

# Import each file in list
penguin_html_list <- lapply(file_list_grep, readLines)

# Find scientific name
## Isolate line
sci_name_list_rough <- lapply(penguin_html_list, grep, pattern="Scientific Name:")
sci_name_list_rough  # 132 
penguin_html_list[[2]][133]  # +1

## Create vector of positions of lines for each list entry
sci_name_unlist_rough <- unlist(sci_name_list_rough) + 1

## Retrieve lines containing scientific names from each list entry in turn
sci_name_line <- mapply('[', penguin_html_list, sci_name_unlist_rough)

## Clean html
sci_name <- sci_name_line %>% 
  gsub(pattern = "<td class=\"sciName\"><span class=\"notranslate\"><span class=\"sciname\">", 
         replacement = "") %>%  # Remove leading html
  gsub(pattern = "</span></span></td>", replacement = "") %>%  # Remove trailing html
  gsub(pattern = "^\\s+|\\s+$", replacement = "")  # Remove white space

# Find common name
## Isolate line
common_name_list_rough <- lapply(penguin_html_list, grep, pattern = "Common Name")
common_name_list_rough #146
penguin_html_list[[1]][151]

## Create vector of positions of lines for each list entry
common_name_unlist_rough <- unlist(common_name_list_rough) + 5

## Retrieve lines containing common names from each list entry in turn
common_name_line <- mapply('[', penguin_html_list, common_name_unlist_rough)

## Clean html
common_name <- common_name_line %>%
  gsub(pattern = "<td>", replacement = "") %>%
  gsub(pattern = "</td>", replacement = "") %>%
  gsub(pattern = "^\\s+|\\s+$", replacement = "")

# Find red list category
## Isolate line
red_cat_list_rough <- lapply(penguin_html_list, grep, pattern = "Red List Category")
penguin_html_list[[16]][186]

## Create vector of positions of lines for each list entry
red_cat_unlist_rough <- unlist(red_cat_list_rough) + 2

## Retrieve lines containing common names from each list entry in turn
red_cat_line <- mapply(`[`, penguin_html_list, red_cat_unlist_rough)

## Clean html
red_cat <- gsub("^\\s+|\\s+$", "", red_cat_line)

# Find date of listing
## Isolate line
date_list_rough <- lapply(penguin_html_list, grep, pattern = "Date Assessed:")
penguin_html_list[[18]][200]

## Create vector of positions of lines for each list entry
date_unlist_rough <- unlist(date_list_rough) + 1 

## Retrieve lines containing common names from each list entry in turn
date_line <- mapply('[', penguin_html_list, date_unlist_rough)

## Clean html
date <- date_line %>%
  gsub("<td>", "",.) %>%
  gsub("</td>", "",.) %>%
  gsub("\\s", "",.)


# Create data frame from vectors
penguin_df <- data.frame(sci_name, common_name, red_cat, date)

# Create markdown table from penguin_df
kable(penguin_df, format = "markdown")

