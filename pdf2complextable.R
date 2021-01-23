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

# so here we go...the basic principle is to clip the text to the table (remove titles, etc), then 
# identify the columns and rows, using whatever cues you might have in your data, then 
# to string together the text in those blocks and reconstruct the table.

# libraries and files ---------------------------------------------------------
  
  library(pdftools)
  library(tidyverse)

# data is open access paper https://doi.org/10.1371/journal.pone.0189268

  pdf_file <- "Data/mallinger et al 2017 plos one.pdf"

# extract data ----------------------------------------------------------------
# pdftools::pdf_data() pulls out each page of text as a tibble with x,y locations of each text element

# test extraction using the pdf_text() function which processes it into a contiguous block of text:
# txt <- pdf_text(pdf_file)
# cat(txt[8])  
# seems to be good at pulling out the items, promising!
# so likely can proceed without other processing. If it didn't, there are a number of processes noted in the
# links above to pre-process the image before converting.

  dta <- pdf_data(pdf_file)

# identify (each) section with the table of interest, and compile into a list of tables, 
# preprocess:
# arrange so that the data are in rows by columns (y,x), THIS IS IMPORTANT (we rely on it later)
# add a rowid (of the df), and a line number id (we can do this because text is usually linear across the page) 
  
  preprocess_p2df <- function(x, line = TRUE){ 
    x <- x %>% 
      arrange(y,x) %>% 
      rowid_to_column() 
    if(line) x <- x %>% 
      mutate(lineid = as.numeric(as.factor(y)))
    return(x)
    }

  df <- dta[8:13] %>% 
    map(., preprocess_p2df)

# for(i in seq_along(df)){
#   df[[i]] <- df[[i]] %>% add_column(setid = i)
# }

# clean the table sections --------------------------------------------------------
# identify start and end points (by manually finding cues for this in the table), 
# then clip each section to these, and combine to one table

#' Clip a section to the table itself, based on a start and end function
#' takes a table section, and rules for clipping the rows to remove excess text.
#' note the start and end rules likely need to be specified manually based on visualisation of the original table
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

# in our example, we can identify specific (typically unique) words in every table that 
# indicate start and end positions (we can toggle with extra pointers)

  df[[1]] <- df[[1]] %>% clip_section(startrule = function(x) {which(x$text == "Reference")},
                                      endrule = function(x) {which(x$text == "(Continued)") -1})

  df[2:5] <- df[2:5] %>% 
    map(., ~ clip_section(.x, 
                          startrule = function(x) {which(x$text == "Reference")},
                          endrule = function(x) {which(x$text == "(Continued)")[2] -1}))
  
  df[[6]] <- df[[6]] %>% clip_section(startrule = function(x) {which(x$text == "Reference")},
                           endrule = function(x) {which(x$text == "1") -1})

# alternatives here might be to identify the line numbers that are the start and end of each table 

# identify the likely columns ---------------------------------------------------------
## this is challenging in this case, because there is different formatting for each column
  
## we know that we have 9 columns. we can manually specify the breaks by examining these, seperately or together
  
  plot_xlocs <- function(x, out = "plot"){
    if(class(x)[1] == "list") x <- bind_rows(x)
    .x <- x$x
    if(out == "plot") return(hist(.x, breaks = 1:max(.x)))  else return(table(.x)) 
  }
  
  plot_xlocs(df[[6]])
  plot_xlocs(df)
  
  xbreaks.manual <- c(0, 80, 140, 220, 240, 300, 390, 460, 530, 555)  
  
# alternatively, we could at least semi automate this by finding a row that has the most left justified items.
# for our data, this is the 11th line (of the first page) for cols 1:8, and the 7th line for column 9.
  
# get xlocs (or text) from a lineid
  get_xlocs_fromline <- function(x, .lineid, .selection = "all", out = "xloc"){
    outx <- x %>% filter(lineid == .lineid)
    if (.selection[1] == "all") .selection <- 1:nrow(outx)
    outx <- outx %>% .[.selection, ]
    if(out == "xloc") return(outx %>% pull(x)) else return(outx %>% pull(text))
  }

# check if we have the right lines  
  get_xlocs_fromline(df[[1]], 11, out = "text")
  get_xlocs_fromline(df[[1]], 7, out = "text")

# get xlocs, and convert to xbreaks by bounding, and shifting
  shift <- 2
  xbreaks.fromline <- c(get_xlocs_fromline(df[[1]], 11, c(1,4,5,6,7,9,11,12)),
                        get_xlocs_fromline(df[[1]], 7, 14) 
                        )
  maxx <- df %>% bind_rows() %>% pull(x) %>% max
  xbreaks.fromline <- c(0, xbreaks.fromline[-1] -shift, maxx + shift)
  
# cut into columns:  
  xbreaks <- xbreaks.fromline
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
