# pdf2complextable

# Author: workingconservation@gmail.com
# 22.01.2021

# Aim: extracting complex tables from pdfs. While there are several tools to extract tables from
# pdfs, these often can only deal with single blocks of text (e.g. single line entries). I have a
# bunch of tables that were annoyingly only available in pdf, and had multiple lines per cell. 
# This was my process for dealing with these.

# Status: R script with some functions. This is currently notes for myself, but made available 
# in case anyone would like to help develop it further.

# several alternatives exist to extract text (including tables) from pdf:
# https://cran.r-project.org/web/packages/tabulizer/vignettes/tabulizer.html
# https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/
# for me, tabulizer is failing to load because of java, and I have no admin access to fix this.
# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html
# https://stackoverflow.com/questions/54000691/extracting-tables-from-jpeg-into-a-dataframe-in-r (tesseract)
# tesseract was only ok for single line entries 
# https://ropensci.org/technotes/2018/12/14/pdftools-20/ 
# pdftools keeps the spaces in the data and can be used to reconsruct the table
# but the instructions on this state:  "Converting this pdf data into the original data frame 
# is left as an exercise for the reader :)"
# well, thanks... 

# so here we go...

# libraries and files ---------------------------------------------------------
library(pdftools)
library(tidyverse)

# data is open access paper https://doi.org/10.1371/journal.pone.0189268
pdf_file <- "Data/mallinger et al 2017 plos one.pdf"

# extract data ----------------------------------------------------------------

# test extraction:
# txt <- pdf_text(pdf_file)
# cat(txt[8])  
# seems to be good at pulling out the items, promising!
# so likely can proceed without other processing.

dta <- pdf_data(pdf_file)

# identify (each) section with the table of interest, and compile into a list of tables, 
# arrange so that the data are in rows by columns (y,x), add a rowid
df <- dta[8:13] %>% 
  map(., ~ .x %>% arrange(y,x) %>% rowid_to_column())

# for(i in seq_along(df)){
#   df[[i]] <- df[[i]] %>% add_column(setid = i)
# }

# clean the table sections --------------------------------------------------------
# identify start and end points (by manually finding them), 
# then clip each section to these, and combine to one table

#' Clip a section to the table itself
#' takes a table section, and rules for clipping the rows to remove excess text.
#' note the start and end rules likely nees to be specified manually based on visualisation of the original table
#' 
#' x the data frame
#' startrule a function (of x) which defines the start of the rowids that need to be kept
#' endrule a function (of x) which defines the end of the rowids that need to be kept
#' 
  clip_section <- function(x, startrule, endrule){
    sb <- startrule(x)
    eb <- endrule(x)
    x %>% 
      filter(rowid >= sb & rowid <= eb)
  }

  df[[1]] <- df[[1]] %>% clip_section(startrule = function(x) {which(x$text == "Reference")},
                                      endrule = function(x) {which(x$text == "(Continued)") -1})

  df[2:5] <- df[2:5] %>% 
    map(., ~ clip_section(.x, 
                          startrule = function(x) {which(x$text == "Reference")},
                          endrule = function(x) {which(x$text == "(Continued)")[2] -1}))
  
  df[[6]] <- df[[6]] %>% clip_section(startrule = function(x) {which(x$text == "Reference")},
                           endrule = function(x) {which(x$text == "1") -1})
  

# identify the likely columns ---------------------------------------------------------
## this is challenging in this case, because there is different formatting for each column
## we know that we have 9 columns. we can manually specify the breaks by examining these.
i <- 6
table(df[[i]]$x)
hist(df[[i]]$x, breaks = 1:max(df[[i]]$x))

xbreaks <- c(0, 80, 140, 220, 240, 300, 390, 460, 530, 555)  

df[1:6] <- df[1:6] %>% 
  map(., function(.df) .df %>% mutate(xcol = cut(x, breaks = xbreaks) %>% as.numeric()))

# if the heading boxes were always aligned with the edge (which they are not in this case) AND capitalised
# another approach might be to identify column breaks by 
# headinglines <- 124:151
# headingboxes <- headinglines[str_detect(df %>% filter(rowid %in% c(headinglines)) %>% pull(text), "^[[:upper:]]")]
# ... not developed as not appropriate for my case

# identify the likely rows ---------------------------------------------------------
## luckily the rows generally start on the same line. 
## we can leverage column 9 (which has only one line per entry, so will have gaps between them) 

#' Set row numbers for extracted table
#' Given a refcol which only contains one entry per line
#' We can use this (and the knowledge of how many rows are in the heading) to identify the rows.
#' 
#' x the data frame (each section needs to be seperate)
#' refcol the reference column number (xcol code) derived above for the column with only one entry per line
#' nheadrow the number of rows in the header for this row
#' shift a number to shift the breaks so cutpoints fall between observed y's
#' 
set_row_numbers <- function(x, refcol, nheadrow, shift = 3){
  cref <- x %>% filter(xcol == refcol)
  ybreaks <- c(0,  
               cref$y[(nheadrow+1):nrow(cref)]-shift, 
               max(x$y)+shift) %>% unique()
  x %>% 
    mutate(yrow = cut(y, breaks = ybreaks) %>% as.numeric())
}

df <- df %>% 
  map(., ~ set_row_numbers(x = .x, refcol = 9, nheadrow = 2)) 


# string out into a table, then combine --------------------------------

tabulate_df <- function(x, colname.row = 1){
  nrows <- max(x$yrow)
  ncols <- max(x$xcol)
  
  # string the text together
  tx <- x %>% 
    group_by(xcol, yrow) %>% 
    summarise(stext = str_c(text, collapse = " ")) %>% 
    pull(stext) %>% 
    matrix(., nrow = nrows, ncol = ncols) 
  
  rx <- tx[-colname.row, ] %>% 
    as_tibble(.name_repair = "universal")
  names(rx) <- c(tx[colname.row, ])
  
  return(rx)
}


tab.df <- df %>% 
  map(., ~ tabulate_df(.x)) %>% 
  bind_rows()

tab.df 
