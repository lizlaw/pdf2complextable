# pdf2complextable example of function use

# includes:

# Mallinger et al 2017 Do managed bees have negative effects on wild bees?: A systematic review of the literature

# libraries ---------------------------------------------------------

library(pdftools)
library(tidyverse)
source("pdf2complextable_functions.R")

# Data from table in: =================================================
## Do managed bees have negative effects on wild bees?: A systematic review of the literature
## open access paper https://doi.org/10.1371/journal.pone.0189268
pdf_file <- "Data/mallinger et al 2017 plos one.pdf"
dta <- pdf_data(pdf_file)

# Mallinger table 1 ---------------------------------------------------
# preprocess and clip  
tab1 <- dta[8:13] %>% map(., preprocess_p2df)
tab1[[1]] <- tab1[[1]] %>% 
  clip_section(startrule = function(x) {which(x$text == "Reference")},
               endrule = function(x) {which(x$text == "(Continued)") -1})
tab1[2:5] <- tab1[2:5] %>% 
  map(., ~ clip_section(.x, 
                        startrule = function(x) {which(x$text == "Reference")},
                        endrule = function(x) {which(x$text == "(Continued)")[2] -1}))
tab1[[6]] <- tab1[[6]] %>% 
  clip_section(startrule = function(x) {which(x$text == "Reference")},
               endrule = function(x) {which(x$text == "1") -1})

# set columns  
get_xlocs_fromline(tab1[[1]], 11, out = "text")
get_xlocs_fromline(tab1[[1]], 7, out = "text")

shift <- 2
xbreaks.fromline <- c(get_xlocs_fromline(tab1[[1]], 11, c(1,4,5,6,7,9,11,12)),
                      get_xlocs_fromline(tab1[[1]], 7, 14) 
)
maxx <- tab1 %>% bind_rows() %>% pull(x) %>% max
xbreaks <- c(0, xbreaks.fromline[-1] -shift, maxx + shift)

tab1[1:6] <- tab1[1:6] %>% 
  map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))

# set rows
tab1 <- tab1 %>% 
  map(., ~ set_row_numbers(x = .x, refcol = 9, nheadrow = 2)) 

# tabulate   
tab1.df <- tab1 %>% 
  map(., ~ tabulate_df(.x)) %>% 
  bind_rows()

tab1.df %>% print(n=nrow(tab1.df))

# write_csv(tab1.df, "Data/Mallinger_2017_table1.csv")