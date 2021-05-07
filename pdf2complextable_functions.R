# pdf2compextable functions

#  preprocess------------------------------------------------------------------------------------
#' preprocess
#' 
#' arrange so that the data are in rows by columns (y,x), THIS IS IMPORTANT (we rely on it later)
#' add a rowid (of the df), and a line number id (we can do this because text is usually linear across the page) 
#' 
#' @param x a tibble object derived from pdftools::pdf_data() containing columns (x,y,text,...)
#' @param line logical indicating if the line number should be extracted, defaults to TRUE.
#' 
#' @return returns the input tibble data frame, sorted to aline with human left-right, 
#' top-bottom reading order (y,x) then appended with rowid and lineid
#' 
#' @export
#' @examples
#' 
preprocess_p2df <- function(x, line = TRUE){ 
  x <- x %>% 
    arrange(y,x) %>% 
    rowid_to_column() 
  if(line) x <- x %>% 
      mutate(lineid = as.numeric(as.factor(y)))
  return(x)
}

# clip to table -------------------------------------------------------------------------------
#' Clip a section to the table itself, based on a start and end function
#' takes a table section, and rules for clipping the rows to remove excess text.
#' note the start and end rules likely need to be specified manually based on visualisation of the original table
#' 
#' @param x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, lineid, rowid...)
#' @param startrule a function (of x) which defines the start of the rowids that need to be kept
#' @param endrule a function (of x) which defines the end of the rowids that need to be kept
#' 
#' @return the input tibble data frame, clipped to specified sections 
#' 
#' @export
#' @examples
#' 
clip_section <- function(x, startrule, endrule){
  sb <- startrule(x)
  eb <- endrule(x)
  x %>% 
    filter(rowid >= sb & rowid <= eb)
}

# plot xlocations -----------------------------------------------------------------------
#' plot or table xlocations of table data
#' 
#' @param x a list of tibbles or a single tibble derived from pdftools::pdf_data, clipped to the table itself.
#' @param out character string "plot" to output histogram of xlocations, or "table" to output the frequency table itself
#' 
#' @return returns a plot or frequency table of x locations useful for determining where xbreaks should occur
#' 
#' @export
#' @examples
#' 
plot_xlocs <- function(x, out = "plot"){
  if(class(x)[1] == "list") x <- bind_rows(x)
  .x <- x$x
  if(out == "plot") return(hist(.x, breaks = 1:max(.x)))  else return(table(.x)) 
}

# get xlocs from lineid -----------------------------------------------------------------
#' get xlocs (or text) from a lineid
#' given lineids that have the most left justivief data entries, we can extract 
#' the xlocations of these in order to define the columns of the table
#' 
#' @param x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, lineid, rowid...)
#' @param .lineid numeric indicating the line of interest
#' @param .selection the text entities of interest, either "all" or a vector indicating the text entities to extract locations from
#' @param out character, either "xloc" for the numeric x location, or "text" to check the line/text entities are correct.
#'  
#' @return returns selected x locations or text 
#'  
#' @export
#' @examples
#' 
get_xlocs_fromline <- function(x, .lineid, .selection = "all", out = "xloc"){
  outx <- x %>% filter(lineid == .lineid)
  if (.selection[1] == "all") .selection <- 1:nrow(outx)
  outx <- outx %>% .[.selection, ]
  if(out == "xloc") return(outx %>% pull(x)) else return(outx %>% pull(text))
}

# set columns from xbreaks -------------------------------------------------------------
#' set columns from xbreaks
#' 
#' @param x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, lineid, rowid...)
#' @param xbreaks a vector defining the xlocation breaks should be placed
#' @param ... passed to base::cut()
#' 
#' @return returns the input tibble data frame, appended with "xcol" denoting column number
#' 
#' @export
#' @examples
#' 
set_columns_fromxbreaks <- function(x, xbreaks,...){
  x %>% 
    mutate(xcol = cut(x, breaks = xbreaks,...) %>% as.numeric())
} 

# Set row numbers from reference column -------------------------------------------------------
#' Set row numbers from reference column
#' Given a refcol which only contains one entry per line
#' We can use this (and the knowledge of how many rows are in the heading) to identify the rows.
#' 
#' @param x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, rowid...)
#' @param refcol the reference column number (xcol code) derived above for the column with only one entry per line
#' @param nheadrow the number of rows in the header for this row
#' @param shift a number to shift the breaks so cutpoints fall between observed y's
#' 
#' @return returns the input tibble data frame, appended with "yrow" denoting row number
#' 
#' @export
#' @examples
#' 
set_row_numbers <- function(x, refcol, nheadrow, shift = 3){
  cref <- x %>% filter(xcol == refcol)
  ybreaks <- c(0,  
               cref$y[(nheadrow+1):nrow(cref)]-shift, 
               max(x$y)+shift) %>% unique()
  x %>% 
    mutate(yrow = cut(y, breaks = ybreaks) %>% as.numeric())
}

# plot ylocations  -------------------------------------------------------
#' Plot y locations with potential breaks
#' 
#' @param x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, rowid...)
#' @param focuscol  number of a column to 'focus' on (plots in blue)
#' @param focuscol2 number of a column to 'focus' on (plots in orange)
#' @param ybreaks NULL or a numeric vector to place proposed ybreaks on
#' 
#' @return plots row positions (grey), those of focuscol in blue, and focuscol2 in orange, and proposed y breaks in red.
#' 
#' @export
#' @examples
#' 
plot_ylocs <- function(.x, focuscol, focuscol2, ybreaks = NULL){
  
  p1 <- ggplot(.x) + 
    geom_histogram(aes(x=y), binwidth = 1) + 
    geom_histogram(data = .x %>% filter(xcol==focuscol), aes(x=y), binwidth = 1, fill = 'blue') + 
    geom_histogram(data = .x %>% filter(xcol==focuscol2), aes(x=y), binwidth = 1, fill = 'orange') 
  
  if (!is.null(ybreaks)) {
    ydf <- tibble(y=ybreaks)
    p1 <-  p1 +
      geom_segment(data = ydf, aes(x=y, xend = y, y = 0, yend = Inf), color = 'red') +
      geom_text(data = ydf, aes(x=y,  y = -1, label = y), color = "red")} 
  
  return(p1)
}

# get_ybreaks  -------------------------------------------------------
#' identify potential ybreaks through the gap between unique y positions
#' 
#' @param x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, rowid...)
#' @param gaplimit numberic when gap >= gaplimit, a break is allocated in the middle of the gap.
#' @param .min numeric, places an extra break at .min (default =0)
#' 
#' @return returns a vector of potential ybreaks
#' 
#' @export
#' @examples
#' 

get_ybreaks_bygap <- function(.x, gaplimit, .min = 0){
  cyu <- unique(.x$y)
  cyd <- diff(cyu, 1)
  cyb <- vector("numeric", length = length(cyd))
  for (i in seq(cyd)){
    cyb[i] <- ifelse(cyd[i] >= gaplimit, cyu[i] + 0.5*cyd[i], NA)
  }
  c(.min, cyb[!is.na(cyb)], max(cyu) +1)
}

# tabulate  --------------------------------
#' convert the tibble to a data frame, given column and row codes
#' 
#' @param x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, xcol, yrow...)
#' @param colname.row  defaults to 1, the ycol code for the row containing headings.
#' @param fixcolnames NULL to use colname.row entries, or a character vector supplying desired column names
#' 
#' @return returns a tibble data frame containing the extracted table. missing cells (including in the header) will be replaced by NA
#' 
#' @export
#' @examples
#' 
tabulate_df <- function(x, colname.row = 1, fixcolnames = NULL){
  nrows <- max(x$yrow)
  ncols <- max(x$xcol)
  
  # string the text together
  tx <- x %>% 
    group_by(xcol, yrow) %>% 
    summarise(stext = str_c(text, collapse = " "))
  
  tm <- matrix(data = NA, nrow = nrows, ncol = ncols)
  for (i in 1:nrow(tx)){
    tm[tx$yrow[i], tx$xcol[i]] <- tx$stext[i]
  }
  
  if(colname.row !=0) rx <- tm[-colname.row, ]  else rx <- tm
  rx <- rx %>% as_tibble(.name_repair = "universal")
  
  if(is.null(fixcolnames)) names(rx) <- c(tm[colname.row, ]) else names(rx) <- fixcolnames
  
  return(rx)
}

# str_split_custom ------------------------------
#' str_split_custom
#' function modified from Jakob Gepp's https://www.r-bloggers.com/2018/04/strsplit-but-keeping-the-delimiter/
#' Allows splitting of strings "before", "after" or "around" the delimiter (while keeping it)
#' 
#' @param x character string to split
#' @param pattern regex or character string to split on
#' @param type is "remove" for normal strsplit/str_split function (removing the delimiter), 
#'   "before" to split before the delimiter (keeping the delimiter),
#'   "after" to split after the delimiter (keeping the delimiter), or 
#'   "around" to split before and after the delimiter (keeping the delimiter)
#'   
#' @return returns a list containing a character vector with the split string.
#' 
#' @export
#' @examples
#' 
str_split_custom <- function(x,
                             pattern,
                             type = "remove",
                             perl = FALSE,
                             ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = pattern, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", pattern, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", pattern, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "around") {
    # split before and after the delimiter and keep it
    out1 <- base::strsplit(x = x,
                           split = paste0("(?<=.)(?=", pattern, ")"),
                           perl = TRUE,
                           ...)
    out <- list(unlist(lapply(out1, function(x) {base::strsplit(x = x,
                                                                split = paste0("(?<=", pattern, ")"),
                                                                perl = TRUE,
                                                                ...)} )))
    
  } else {
    # wrong type input
    stop("type must be remove, after, before, or around")
  }
  return(out)
}

# examples
# text <- "[-1-3, 4, 6,8-10,12] but see [22-24]"
# str_split_custom(text, pattern = ",| |\\[|\\]", type = "before")
# str_split_custom(text, pattern = ",| |\\[|\\]", type = "after")
# str_split_custom(text, pattern = ",| |\\[|\\]", type = "around")


# rangeExpand ------------------------------------
#' range Expand a text string containing ranges into a list of numbers
#' 
#' Code is modified from https://rosettacode.org/wiki/Range_expansion#R
#' However, I've split this from splitting the origional string, 
#' so that custom splits can be applied before this function is used.
#' Also, this now works with multiple "-" type signs. 
#' 
#' @param split.text vector of character strings potentially containing ranges to be expanded, 
#'   e.g. from str_split_custom(); each element to be split must not contain other punctuation
#' 
#' @return a text string containing expanded numbers
#' 
#' @export
#' @examples
#' 
range_expand <- function(split.text) {
  # check if element has a hypenated number, if so, expand it and parse the results 
  map(split.text[[1]], function(x) if (grepl("(\\d)-", x)|grepl("(\\d)-", x)) {
    .text <- x %>% str_replace_all(.,"(\\d)-", "\\1:") %>% str_replace_all(., "(\\d)-","\\1:")
    parse(text=.text) %>% 
      eval() %>% 
      as.character() %>% 
      str_c(collapse = ",")
  } else x ) %>% 
    str_c(collapse = "")
}

# examples
# text <- "[-1-3, 4, 6,8-10,12] but see [22-24]"
# rangeExpand(str_split_custom(text, pattern = ",| |\\[|\\]", type = "around"))
# "[8-19] but see [6,20-22]" %>% str_split_custom(pattern = ",| |\\[|\\]", type = "around") %>% range_expand()
# "[8-19][-1-3, 4, 6,8-10,12] but see [22-24]" %>% str_split_custom(pattern = ",| |\\[|\\]", type = "around") %>% range_expand()
