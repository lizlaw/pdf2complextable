# pdf2compextable functions

#  preprocess------------------------------------------------------------------------------------
#' preprocess
#' 
#' arrange so that the data are in rows by columns (y,x), THIS IS IMPORTANT (we rely on it later)
#' add a rowid (of the df), and a line number id (we can do this because text is usually linear across the page) 
#' 
#' x a tibble object derived from pdftools::pdf_data() containing columns (x,y,text,...)
#' line logical indicating if the line number should be extracted, defaults to TRUE.
#' 
#' returns the input tibble data frame, sorted to aline with human left-right, 
#' top-bottom reading order (y,x) then appended with rowid and lineid
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
#' x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, lineid, rowid...)
#' startrule a function (of x) which defines the start of the rowids that need to be kept
#' endrule a function (of x) which defines the end of the rowids that need to be kept
#' 
#' the input tibble data frame, clipped to specified sections 
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
#' x a list of tibbles or a single tibble derived from pdftools::pdf_data, clipped to the table itself.
#' out character string "plot" to output histogram of xlocations, or "table" to output the frequency table itself
#' 
#' returns a plot or frequency table of x locations useful for determining where xbreaks should occur
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
#' x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, lineid, rowid...)
#' .lineid numeric indicating the line of interest
#' .selection the text entities of interest, either "all" or a vector indicating the text entities to extract locations from
#' out character, either "xloc" for the numeric x location, or "text" to check the line/text entities are correct.
#'  
#' returns selected x locations or text 
#'  
get_xlocs_fromline <- function(x, .lineid, .selection = "all", out = "xloc"){
  outx <- x %>% filter(lineid == .lineid)
  if (.selection[1] == "all") .selection <- 1:nrow(outx)
  outx <- outx %>% .[.selection, ]
  if(out == "xloc") return(outx %>% pull(x)) else return(outx %>% pull(text))
}

# set columns from xbreaks -------------------------------------------------------------
#' set columns form xbreaks
#' 
#' x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, lineid, rowid...)
#' xbreaks a vector defining the xlocation breaks should be placed
#' ... passed to base::cut()
#' 
#' returns the input tibble data frame, appended with "xcol" denoting column number
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
#' x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, rowid...)
#' refcol the reference column number (xcol code) derived above for the column with only one entry per line
#' nheadrow the number of rows in the header for this row
#' shift a number to shift the breaks so cutpoints fall between observed y's
#' 
#' returns the input tibble data frame, appended with "yrow" denoting row number
#' 
set_row_numbers <- function(x, refcol, nheadrow, shift = 3){
  cref <- x %>% filter(xcol == refcol)
  ybreaks <- c(0,  
               cref$y[(nheadrow+1):nrow(cref)]-shift, 
               max(x$y)+shift) %>% unique()
  x %>% 
    mutate(yrow = cut(y, breaks = ybreaks) %>% as.numeric())
}

# tabulate  --------------------------------
#' convert the tibble to a data frame, given column and row codes
#' 
#' x a tibble object derived from pdftools::pdf_data() and preprocessed, ie containing columns (x,y,text, xcol, yrow...)
#' colname.row  defaults to 1, the ycol code for the row containing headings.
#' 
#' returns a tibble data frame containing the extracted table
#' 
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
