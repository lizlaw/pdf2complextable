# pdf2complextable example of function use

# Data extraction from:
## 1) Mallinger et al 2017 Do managed bees have negative effects on wild bees?: A systematic review of the literature https://doi.org/10.1371/journal.pone.0189268
## 2) 2016 Positive and Negative Impacts of Non-Native Bee Species around the World INSECTS 10.3390/insects7040069

## 1) demonstrates: extracting the tables using known left-aligned and top-aligned columns. This is a relatively simple example.

## 2) demonsrates: A more complex set of examples. In this case, all columns and rows are centre-aligned, so we extract column and row information using jenks natural breaks. This needs human oversight, and all pages need to be done seperately as they are aligned differently. There are special characters, and several elements needed to be corrected manually. Further, some information was held in formatting (colours, underline, bold, etc.). This is added, and is added manually. Finally, it also demonstrated extraction of citations, which need to be linked to the numbers in the tables. 

# libraries ---------------------------------------------------------

library(pdftools)   # base text extraction from pdf
library(tidyverse)  # programming
library(BAMMtools)  # Jenks natural breaks function
source("pdf2complextable_functions.R")

# all the functions are sourced from the "pdf2complextable_functions.R" file, and are documented in there.

# Mallinger et al 2017 =================================================

## Do managed bees have negative effects on wild bees?: A systematic review of the literature
## Three tables from main text, from open access paper https://doi.org/10.1371/journal.pone.0189268

  pdf_file <- "Data/mallinger et al 2017 plos one.pdf"
  dta <- pdf_data(pdf_file)

# Mallinger table 1 ---------------------------------------------------
  
## This table runs across several pages, contains some left-aligned, and some centre-aligned text, and includes multi-line data and footnotes. 
  
# preprocess and clip
## extract out the three pages of the table, and clip out the table itself based on the starting word of the table, and an indicator of the end (we get these from examining the pdf manually)
  tab1 <- dta[8:13] %>% map(., preprocess_p2df)
  tab1[[1]] <- tab1[[1]] %>%
    clip_section(
      startrule = function(x) {
        which(x$text == "Reference")
      },
      endrule = function(x) {
        which(x$text == "(Continued)") - 1
      }
    )
  tab1[2:5] <- tab1[2:5] %>%
    map(.,
        ~ clip_section(
          .x,
          startrule = function(x) {
            which(x$text == "Reference")
          },
          endrule = function(x) {
            which(x$text == "(Continued)")[2] - 1
          }
        ))
  tab1[[6]] <- tab1[[6]] %>%
    clip_section(
      startrule = function(x) {
        which(x$text == "Reference")
      },
      endrule = function(x) {
        which(x$text == "1") - 1
      }
    )

# set columns
## here we set the columns based on the most left aligned text words (mostly on line 11, some on line 7)
  get_xlocs_fromline(tab1[[1]], 11, out = "text")
  get_xlocs_fromline(tab1[[1]], 7, out = "text")

  shift <- 2
  xbreaks.fromline <-
    c(get_xlocs_fromline(tab1[[1]], 11, c(1, 4, 5, 6, 7, 9, 11, 12)),
      get_xlocs_fromline(tab1[[1]], 7, 14))
  maxx <- tab1 %>% bind_rows() %>% pull(x) %>% max
  xbreaks <- c(0, xbreaks.fromline[-1] - shift, maxx + shift)
  
  tab1[1:6] <- tab1[1:6] %>%
    map(., ~ set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
# set rows
## here we set the rows based on column 9, which has only one line per entry, and can therefore mark the start lines.  
  tab1 <- tab1 %>%
    map(., ~ set_row_numbers(
      x = .x,
      refcol = 9,
      nheadrow = 2
    ))

# tabulate
## now we have the rows and columns identified, we can tabulate these into one table and check output.  
  tab1.df <- tab1 %>%
    map(., ~ tabulate_df(.x)) %>%
    bind_rows()
  
  tab1.df %>% print(n = nrow(tab1.df))

# save to file
# write_csv(tab1.df, "Data/Mallinger_2017_table1.csv")
  
# Mallinger table 2 ---------------------------------------------------
## This table is processed similarly to the table 1, provided here as an additional example.
  # preprocess and clip  
  tab2 <- dta[14:17] %>% map(., preprocess_p2df)
  
  tab2[[1]] <- tab2[[1]] %>% 
    clip_section(startrule = function(x) {which(x$text == "Reference")},
                 endrule = function(x) {which(x$text == "(Continued)") -1})
  
  tab2[2:3] <- tab2[2:3] %>% 
    map(., ~ clip_section(.x, 
                          startrule = function(x) {which(x$text == "Reference")},
                          endrule = function(x) {which(x$text == "(Continued)")[2] -1}))
  tab2[[4]] <- tab2[[4]] %>% 
    clip_section(startrule = function(x) {which(x$text == "Reference")},
                 endrule = function(x) {which(x$text == "*") -1})
  
  # set columns  
  get_xlocs_fromline(tab2[[1]], 12, out = "text", .selection = c(1,4,6,7,8,10,12,14))
  get_xlocs_fromline(tab2[[1]], 8, out = "text", .selection = 13)
  
  shift <- 2
  xbreaks.fromline <- c(get_xlocs_fromline(tab2[[1]], 12, c(1,4,6,7,8,10,12,14)),
                        get_xlocs_fromline(tab2[[1]], 8, 13) 
  )
  maxx <- tab2 %>% bind_rows() %>% pull(x) %>% max
  xbreaks <- c(0, xbreaks.fromline[-1] -shift, maxx + shift)
  
  tab2 <- tab2 %>% 
    map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
  # set rows
  tab2 <- tab2 %>% 
    map(., ~ set_row_numbers(x = .x, refcol = 9, nheadrow = 2)) 
  
  # tabulate   
  tab2.df <- tab2 %>% 
    map(., ~ tabulate_df(.x)) %>% 
    bind_rows()
  
  tab2.df     
  #write_csv(tab2.df, "Data/Mallinger_2017_table2.csv")
  
# Mallinger table 3 -------------------------------------------------------------
## This table is processed similarly to the table 1, provided here as an additional example.
  # preprocess and clip  
  tab3 <- dta[18:19] %>% map(., preprocess_p2df)
  
  tab3[[1]] <- tab3[[1]] %>% 
    clip_section(startrule = function(x) {which(x$text == "Reference")},
                 endrule = function(x) {which(x$text == "(Continued)") -1})
  
  tab3[[2]] <- tab3[[2]] %>% 
    clip_section(startrule = function(x) {which(x$text == "Reference")},
                 endrule = function(x) {which(x$text == "1")[4] -1})
  
  # set columns  
  get_xlocs_fromline(tab3[[1]], 12, out = "text", .selection = c(1,2,3,5,6,8,9,10))
  get_xlocs_fromline(tab3[[1]], 8, out = "text", .selection = 15)
  
  shift <- 2
  xbreaks.fromline <- c(get_xlocs_fromline(tab3[[1]], 12, c(1,2,3,5,6,8,9,10)),
                        get_xlocs_fromline(tab3[[1]], 8, 15) 
  )
  maxx <- tab3 %>% bind_rows() %>% pull(x) %>% max
  xbreaks <- c(0, xbreaks.fromline[-1] -shift, maxx + shift)
  
  tab3 <- tab3 %>% 
    map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
  # set rows
  tab3 <- tab3 %>% 
    map(., ~ set_row_numbers(x = .x, refcol = 9, nheadrow = 2)) 
  
  # tabulate   
  tab3.df <- tab3 %>% 
    map(., ~ tabulate_df(.x)) %>% 
    bind_rows()
  
  tab3.df     
  #write_csv(tab3.df, "Data/Mallinger_2017_table3.csv")  

# Russo 2016 =============================================================
## Russo, 2016 Positive and Negative Impacts of Non-Native Bee Species around the World INSECTS 10.3390/insects7040069
## Three tables from main text, running over several pages each.
## Supplementary Materials: The following are available online at http://www.mdpi.com/2075-4450/7/4/69/s1.
## Two tables and supplementary references.

  pdf_file <- "Data/russo 2016 insects.pdf"
  txt <- pdf_text(pdf_file)
  dta <- pdf_data(pdf_file)
  
# Russo table 1 ---------------------------------------------------
  # pages 3-5
  # table starts on line 6, (line 3, for subsq pages) ends at bottom of page.  
  # 7 columns - number 1 heading incorrect, number 3 missing heading, all centre aligned, all pages are aligned re columns
  # first column is grouping, should be no other entries for that line
  # Rows are all single line EXCEPT column 5 and 6. Rows are centred. 
  # online version is different, good to cross reference, but likely no easier to extract.
  
  # preprocess and clip  
  tab1 <- dta[3:5] %>% map(., preprocess_p2df)
  
  tab1[[1]] <- tab1[[1]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 6) %>% min()},
                 endrule = function(x) {nrow(x)})
  tab1[2:3] <- tab1[2:3] %>% 
    map(., ~ clip_section(.x, 
                          startrule = function(x) {which(x$lineid == 3) %>% min()},
                          endrule = function(x) {nrow(x)}))
  
  # set columns  - these are centre aligned so best using suggestions from jenks natural breaks 
  # (alt is kmeans but doesnt always do very well)
  # All pages are aligned
  plot_xlocs(tab1)
  plot_xlocs(tab1, out = "text")
  
  cx <- tab1 %>% bind_rows() %>% select(x)
  jc <- tibble(x=BAMMtools::getJenksBreaks(cx %>% pull(x), 11))
  
  ggplot()+
    geom_bar(data = cx, aes(x=x))+
    geom_segment(data  = jc, aes(x=x, xend=x, y=0, yend=Inf), col="red")+
    geom_text(data =jc, aes(x=x, y=-2, label = x), col= "red")+
    theme_bw()
  
  # jenks isnt perfect so interpret this, and check
  xbreaks <- c(0, 175, 300, 335, 400, 510, 640, max(cx$x))
  xb <- tibble(x=xbreaks)
  
  ggplot()+
    geom_histogram(data = cx, aes(x=x), binwidth = 1)+
    geom_segment(data  = xb, aes(x=x, xend=x, y=0, yend=Inf), col="red")+
    geom_text(data =xb, aes(x=x, y=-2, label = x), col= "red")+
    theme_bw()
  
  tab1[1:3] <- tab1[1:3] %>% 
    map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
  # set rows. This is more challenging as these are also centred. 
  # all pages will likely need to be done seperately
  
  # for page 1, group headings are in column 1, and column 4 is always single line
  plot_ylocs(tab1[[1]], focuscol = 1, focuscol2 = 4, ybreaks = NULL)
  
  # find potential ybreaks
  ybreaks <- get_ybreaks_bygap(tab1[[1]], gaplimit = 8, .min = 150)
  
  plot_ylocs(tab1[[1]], focuscol = 1, focuscol2 = 4, ybreaks = ybreaks)
  
  # remove the two that need removing - these didnt have big enough gaps but are single row.
  ybreaks <- ybreaks[!ybreaks %in% c(364, 372)]
  
  # set the breaks for section 1
  tab1[[1]] <- tab1[[1]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # section 2
  plot_ylocs(tab1[[2]], focuscol = 1, focuscol2 = 2, ybreaks = NULL) # all the species are one line only (year is missing one)
  ybreaks <- get_ybreaks_bygap(tab1[[2]], gaplimit = 7, .min = 100)
  plot_ylocs(tab1[[2]], focuscol = 1, focuscol2 = 2, ybreaks = ybreaks)
  ybreaks <- ybreaks[!ybreaks %in% c(220, 228, 357, 365)]
  tab1[[2]] <- tab1[[2]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # section 3 
  plot_ylocs(tab1[[3]], focuscol = 1, focuscol2 = 2, ybreaks = NULL) # all the species are one line only (year is missing one)
  ybreaks <- get_ybreaks_bygap(tab1[[3]], gaplimit = 7, .min = 100)
  plot_ylocs(tab1[[3]], focuscol = 1, focuscol2 = 2, ybreaks = ybreaks)
  ybreaks <- ybreaks[!ybreaks %in% c(164, 172)]
  tab1[[3]] <- tab1[[3]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # tabulate   
  # in this case, we need to be wary of the empty cells when we tabulate - so I have updated the tabulate function to accommodate this
  
  t11 <- tabulate_df(tab1[[1]])
  t12 <- tabulate_df(tab1[[2]])
  t13 <- tabulate_df(tab1[[3]])
  
  # we also need to correct the header for all sections, and add the 'Colletidae (8)' subheader to the first section
  t11 <- t11 %>% 
    add_row(`Colletidae (8)`='Colletidae (8)', .before = 1) 
  newnames <- names(t11)
  newnames[1] <- "Family"
  newnames[3] <- "Probable method of introduction"
  names(t11) <- names(t12) <- names(t13) <- newnames
  
  # finally, bind them together, fill the empty family columns, and remove the subheader lines:
  tab1.df <- bind_rows(t11, t12, t13) %>% 
    fill(Family) %>% 
    filter(!is.na(`Non-native Species`))
  
  tab1.df %>% print(n=nrow(tab1.df))
  
  # write_csv(tab1.df, "Data/Russo_2016_table1.csv")  
  
  
# Russo table 2 ---------------------------------------------------
  # pages 8-10
  # table starts on line 7, (line 3, for subsq pages) ends at bottom of page (pg 1,2) or before line containing "oligolectic"  
  # 8 columns - headers are mostly centre aligned, can be treated as left but have multilines (so will be the 1st 5 unique lines)
  # many entries are blank
  # Rows are mostly all single line EXCEPT one in col2 page2. Rows are centred. 
  # additional information contained in the table by highlight color, will need to be added manually
  
  # preprocess and clip  
  tab2 <- dta[8:10] %>% map(., preprocess_p2df)
  
  tab2[[1]] <- tab2[[1]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 7) %>% min()},
                 endrule = function(x) {nrow(x)})
  tab2[[2]] <- tab2[[2]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 3) %>% min()},
                 endrule = function(x) {nrow(x)})
  tab2[[3]] <- tab2[[3]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 3) %>% min()},
                 endrule = function(x) {which(x$text == "**") %>% max() -2})
  
  # set columns  - these can be treated as left aligned, but care with multi rows, all pages are the same
  shift <- 2
  maxx <- bind_rows(tab2) %>% select(x) %>% max()
  xbreaks <- c(
    get_xlocs_fromline(tab2[[1]], .lineid = 7, .selection = 1),
    get_xlocs_fromline(tab2[[1]], .lineid = 8, .selection = 1),
    get_xlocs_fromline(tab2[[1]], .lineid = 9, .selection = c(1,3,5,7,8)),
    get_xlocs_fromline(tab2[[1]], .lineid = 10, .selection = 2)
  ) %>% sort() 
  xbreaks <- c(xbreaks - shift, maxx+shift) 
  
  tab2[1:3] <- tab2[1:3] %>% 
    map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
  # set rows. 
  # all pages will likely need to be done seperately
  
  # for section 1
  plot_ylocs(tab2[[1]], focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab2[[1]], gaplimit = 8, .min = 150)
  plot_ylocs(tab2[[1]], focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  tab2[[1]] <- tab2[[1]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # section 2
  plot_ylocs(tab2[[2]], focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab2[[2]], gaplimit = 8, .min = 100)
  plot_ylocs(tab2[[2]], focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  ybreaks <- ybreaks[-7] # there is one with double line here
  tab2[[2]] <- tab2[[2]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # section 3 
  plot_ylocs(tab2[[3]], focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab2[[3]], gaplimit = 8, .min = 100)
  plot_ylocs(tab2[[3]], focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  ybreaks <- ybreaks[-21] # there is one with double line here
  tab2[[3]] <- tab2[[3]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # tabulate   
  tab2.df <- tab2 %>% 
    map(., ~ tabulate_df(.x)) %>% 
    bind_rows()
  
  tab2.df %>% print(n=nrow(tab2.df))
  
  # now, we have some special characters in column1, and grouping names included in this row too, so we can split these out
  tab2.df <- tab2.df %>% 
    mutate(subheads = map_dbl(`Non-native Species`, str_count, pattern = '\\w+')) %>% 
    mutate(Family = ifelse(subheads == 1, `Non-native Species`, NA)) %>% 
    fill(Family) %>% 
    filter(!Family == `Non-native Species`) %>% 
    relocate(Family, .before = 1) %>% 
    mutate(`Establishment uncertain` = grepl(`Non-native Species`, pattern = "?")) %>% 
    mutate(`oligolectic`= grepl(`Non-native Species`, pattern = "\\*\\*")) %>% 
    relocate(`Establishment uncertain`, .before = 3) %>% 
    relocate(`oligolectic`, .before = 4) %>% 
    select(-subheads) 
  
  tab2.df <- tab2.df %>% 
    mutate(`Non-native Species` = str_replace_all(`Non-native Species`, "[^[:alnum:]]", " ") %>% str_squish() )
  
  tab2.df %>% print(n=67)
  
  # then there are further formatting elements that contain information:
  # Bold and underlined text refers to citations with an empirical component while unbolded text refers to papers that refer to impacts only from a hypothetical standpoint. All the bold, underline references are ALL bold underline 
  empirical <- c(80, 81, 82, 84, 86, 56, 91, 92, 93, 94, 63, 96, 96)
  
  empirical_rows <- tab2.df %>% 
    select(`Nesting Sites`:`Change Pollination`) %>% 
    pmap_chr(paste) %>% 
    map_chr(function(x) empirical[str_detect(x, pattern = as.character(empirical))] %>% unique() %>% str_c(., collapse = ", "))
  
  tab2.df <- tab2.df %>% 
    add_column(EmpiricalRefs = empirical_rows)
  
  # Shading:
  # Light grey (2) shading indicates species for which neither positive nor negative impacts have been recorded, while 
  # dark grey (3) indicates species for which only positive impacts have been recorded. 
  # no color (1) has entries supported by papers: "But see" refers to manuscripts that show evidence or describe the opposite of the effect.
  # We can add these as further columns, manual entry
  
  colcolor <- c(1,1,1,2,2,1,2,1,3,2,2,2,1,3,1,3,
                2,1,2,1,2,2,2,3,2,2,1,1,2,1,1,1,1,2,2,3,2,1,2,1,1,1,2,1,3,3,2,2,
                1,1,1,1,1,2,2,1,1,1,3,2,3,2,1,2,1,2,1) # checked by entering twice
  
  tab2.df <- tab2.df %>% 
    add_column(`General impacts (row color)` = factor(colcolor, 
                                                      levels = 1:3, 
                                                      labels = c('see columns', 'no impacts recorded', 'only positive recorded')),
               .before =5) 
  
  #write_csv(tab2.df, "Data/Russo_2016_table2.csv")  
  
# Russo table 3 ---------------------------------------------------
  # pages 12:14
  # table starts on line 7, (line 3, for subsq pages) ends at bottom of page (pg 1,2) or before line containing "oligolectic"  
  # 7 columns - headers are mostly centre aligned, can be treated as left
  # many entries are blank
  # Rows are mostly all single line EXCEPT one in col2 page2. Rows are centred. 
  # additional information contained in the table by highlight color, will need to be added manually
  
  # preprocess and clip  
  tab3 <- dta[12:14] %>% map(., preprocess_p2df)
  
  tab3[[1]] <- tab3[[1]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 7) %>% min()},
                 endrule = function(x) {nrow(x)})
  tab3[[2]] <- tab3[[2]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 3) %>% min()},
                 endrule = function(x) {nrow(x)})
  tab3[[3]] <- tab3[[3]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 3) %>% min()},
                 endrule = function(x) {which(x$text == "tranquebarorum")})
  
  # set columns  - these can be treated as left aligned, but care with multi rows, all pages are the same
  shift <- 2
  maxx <- bind_rows(tab3) %>% select(x) %>% max()
  xbreaks <- get_xlocs_fromline(tab3[[1]], .lineid = 7, .selection = c(3,5,6,9,11))
  xbreaks <- c(0, xbreaks - shift, maxx+shift) 
  
  tab3[1:3] <- tab3[1:3] %>% 
    map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
  # set rows. 
  # all pages will likely need to be done seperately
  
  # for section 1
  plot_ylocs(tab3[[1]], focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab3[[1]], gaplimit = 8, .min = 150)
  plot_ylocs(tab3[[1]], focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  tab3[[1]] <- tab3[[1]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # section 2
  plot_ylocs(tab3[[2]], focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab3[[2]], gaplimit = 8, .min = 100)
  plot_ylocs(tab3[[2]], focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  ybreaks <- ybreaks[!ybreaks == 347] # there is one with double line here
  tab3[[2]] <- tab3[[2]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # section 3 
  plot_ylocs(tab3[[3]], focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab3[[3]], gaplimit = 8, .min = 100)
  plot_ylocs(tab3[[3]], focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  tab3[[3]] <- tab3[[3]] %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # tabulate   
  tab3.df <- tab3 %>% 
    map(., ~ tabulate_df(.x)) %>% 
    bind_rows()
  
  tab3.df %>% print(n=nrow(tab3.df))  
  
  # again we have some special characters and formatting to deal with
  tab3.df <- tab3.df %>% 
    mutate(subheads = map_dbl(`Non-native Species`, str_count, pattern = '\\w+')) %>% 
    mutate(Family = ifelse(subheads == 1, `Non-native Species`, NA)) %>% 
    fill(Family) %>% 
    filter(!Family == `Non-native Species`) %>% 
    relocate(Family, .before = 1) %>% 
    mutate(`Establishment uncertain` = grepl(`Non-native Species`, pattern = "?")) %>% 
    mutate(`oligolectic`= grepl(`Non-native Species`, pattern = "\\*\\*")) %>% 
    relocate(`Establishment uncertain`, .before = 3) %>% 
    relocate(`oligolectic`, .before = 4) %>% 
    select(-subheads) 
  
  tab3.df <- tab3.df %>% 
    mutate(`Non-native Species` = str_replace_all(`Non-native Species`, "[^[:alnum:]]", " ") %>% str_squish() )
  
  tab3.df %>% print(n=nrow(tab3.df))
  
  # expand out ranges
  tab3.df <- tab3.df %>% 
    mutate(across(`Agricultural Pollination`:`Resilience`, 
                  ~map_chr(.x, function(x) {str_split_custom(x, pattern = ",| |\\[|\\]", type = "around") %>% range_expand() })))
  
  # empirical refs 
  empirical <- c(103, 113, 38,115, 116, 117, 118, 119, 120, 112, 123, 124, 125, 126, 126, 128, 130, 104) %>% sort()
  
  empirical_rows <- tab3.df %>% 
    select(`Agricultural Pollination`:Resilience) %>% 
    pmap_chr(paste) %>% 
    map_chr(function(x) empirical[str_detect(x, pattern = as.character(empirical))] %>% unique() %>% str_c(., collapse = ", "))
  
  tab3.df <- tab3.df %>% 
    add_column(EmpiricalRefs = empirical_rows)
  
  # color indicating impact types
  rowcolor2 <- c(1,1,1,2,2,3,2,3,1,2,2,2,3,1,1,1,
                 2,1,2,1,2,2,2,1,2,2,1,3,2,1,3,1,1,2,2,1,2,1,2,1,1,1,2,1,1,1,2,2,
                 1,1,1,1,1,2,2,1,1,1,1,2,1,1,1,2,1,2,3) # checked by entering twice
  
  tab3.df <- tab3.df %>% 
    add_column(`General impacts (row color)` = factor(rowcolor2, 
                                                      levels = 1:3, 
                                                      labels = c('see columns', 'no impacts recorded', 'only negative recorded')),
               .before =5) 
  
  #write_csv(tab3.df, "Data/Russo_2016_table3.csv")  
  
# Russo_2016 references --------------------------------------------------------------------------
  # pages 16:22 contain references
  # we can treat this also as a table, with the numeric being the top, left indicator of each cell. 
  
  refs <- dta[16:22] %>% map(., preprocess_p2df)
  
  refs[[1]] <- refs[[1]] %>% 
    clip_section(startrule = function(x) {which(x$text == "1.") %>% min()},
                 endrule = function(x) {nrow(x)})
  
  refs[2:6] <- refs[2:6] %>% 
    map(., ~ clip_section(.x, 
                          startrule = function(x) {which(x$lineid == 2) %>% min()},
                          endrule = function(x) {nrow(x)}))
  
  refs[[7]] <- refs[[7]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 2) %>% min()},
                 endrule = function(x) {which(x$text == "301-309.")})
  
  # set columns  - these can be treated as left aligned, but care with multi rows, all pages are the same
  shift <- 2
  maxx <- bind_rows(refs) %>% select(x) %>% max()
  xbreaks <- get_xlocs_fromline(refs[[7]], .lineid = 2, .selection = c(2))
  xbreaks <- c(0, xbreaks - shift, maxx+shift) 
  
  refs <- refs %>% 
    map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
  # set rows - we can do this from col 1
  refs <- refs %>% 
    map(~ set_row_numbers(.x, 1, 1, shift = 1))
  
  # tabulate - no header
  
  refs <- refs %>% 
    map(., ~ tabulate_df(.x, colname.row = 0, fixcolnames = c("RefID","Citation"))) %>% 
    bind_rows() 
  
  # write_csv(refs, "Data/Russo_2016_references.csv")  
  
# Russo 2016 Supplementary information =======================================================================================
  pdf_file <- "Data/Russo 2016 insects SI.pdf"
  txt <- pdf_text(pdf_file)
  dta <- pdf_data(pdf_file)  
  
# Russo 2016 SI table 1  ----------------------------------------------------------------------------------  
  # preprocess and clip  
  tab1 <- dta[[1]] %>% preprocess_p2df()
  
  tab1 <- tab1 %>% 
    clip_section(startrule = function(x) {which(x$text == "Altering")},
                 endrule = function(x) {which(x$text == "Insects") %>% max() - 1})
  
  # set columns  - these likely need to be identified through mapping all of them
  jc <- tibble(x=BAMMtools::getJenksBreaks(tab1 %>% pull(x), 9))
  
  ggplot()+
    geom_bar(data = tab1, aes(x=x))+
    geom_segment(data  = jc, aes(x=x, xend=x, y=0, yend=Inf), col="red")+
    geom_text(data =jc, aes(x=x, y=-2, label = x), col= "red")+
    theme_bw()
  
  tab1 %>% filter(text == "Nesting")
  tab1 %>% filter(text == "florea")
  
  # jenks isnt perfect so interpret this, and check
  shift <- 5
  xbreaks <- c(0, jc$x[-1] + shift)
  xb <- tibble(x=xbreaks)
  
  ggplot()+
    geom_histogram(data = tab1, aes(x=x), binwidth = 1)+
    geom_segment(data  = xb, aes(x=x, xend=x, y=0, yend=Inf), col="red")+
    geom_text(data =xb, aes(x=x, y=-2, label = x), col= "red")+
    theme_bw()
  
  tab1 <- tab1 %>% 
    set_columns_fromxbreaks(., xbreaks = xbreaks)
  
  # set rows. 
  plot_ylocs(tab1, focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab1, gaplimit = 8, .min = 250)
  plot_ylocs(tab1, focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  ybreaks <- ybreaks[!ybreaks %in% c(360.5, 381.5, 492.5)]
  plot_ylocs(tab1, focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  
  tab1 <- tab1 %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # tabulate   
  tab1 <- tab1 %>% tabulate_df()
  
  tab1 %>% print(n=nrow(tab1))  
  
  # several numbers are spread over several rows, and come in with a space. We want to remove these
  tab1$`Invasive Weeds`[which(tab1$`Invasive Weeds` == "[28,58,59,6 9,71,72]")] <- "[28,58,59,69,71,72]" ## correct reference 69 
  tab1$`Pathoens/ Parasites`[which(tab1$`Pathoens/ Parasites` == "[25,26,70,7 6,87-90]")] <- "[25,26,70,76,87-90]" ## correct reference 76 
  tab1$`Invasive Weeds`[which(tab1$`Invasive Weeds` == "[29,58,72,9 1-95] but see [96]")] <- "[29,58,72,91-95] but see [96]" ## correct reference 91 
  
  # expand out the x-x numbers using str_split_custom() and range_expand()
  tab1 <- tab1 %>% 
    mutate(across(`Nesting Sites`:`Decrease Plant Fitness`, 
                  ~map_chr(.x, function(x) {str_split_custom(x, pattern = ",| |\\[|\\]", type = "around") %>% range_expand() })))
  
  # empirical refs per line  
  empirical <- list(NA, NA, NA,
                    c(8:19, 9, 23:26, 27:35, 39:43,44, 37:45, 48,49,50) ,
                    c(51,52:54, 55:57), 
                    c(58, 59), 
                    c(60, 61), 
                    NA, NA, 
                    c(68, 28, 58, 59, 69, 71, 72, 73), 
                    c(59), 
                    c(67,70,74,75,77:84,85,86,25,26,70,76, 87:90, 29,58,72,91:95, 38,39,68,81,97,98,4,76,88,99,100,47,76,49,86,97,101:103)) %>% 
    map_chr(function(x) unique(x) %>% sort() %>% str_c(collapse = ","))
  
  tab1 <- tab1 %>% mutate(`Empirical references` = empirical)
  
  # color indicating impact types
  rowcolor <- c(1,2,1,1,1,1,1,1,2,1,1,1) # checked by entering twice
  
  tab1 <- tab1 %>% 
    add_column(`General impacts (row color)` = factor(rowcolor, 
                                                      levels = 1:2, 
                                                      labels = c('see columns', 'no impacts recorded')),
               .before =2) 
  
  # write_csv(tab1, "Data/Russo_2016_SItable1.csv")  
  
# Russo 2016 SI table 2  ----------------------------------------------------------------------------------
  # preprocess and clip  
  tab2 <- dta[[2]] %>% preprocess_p2df()
  
  tab2 <- tab2 %>% 
    clip_section(startrule = function(x) {which(x$text == "Non-native")},
                 endrule = function(x) {which(x$text == "*") %>% max() - 1})
  
  # set columns  - these likely need to be identified through mapping all of them
  jc <- tibble(x=BAMMtools::getJenksBreaks(tab2 %>% pull(x), 7))
  
  ggplot()+
    geom_histogram(data = tab2, aes(x=x), binwidth = 1) +
    geom_histogram(data = tab2 %>% filter(lineid==9), aes(x=x), fill = "blue", binwidth = 1) +
    geom_histogram(data = tab2 %>% filter(lineid==10), aes(x=x), fill = "orange", binwidth = 1) +
    geom_segment(data  = jc, aes(x=x, xend=x, y=0, yend=Inf), col="red")+
    geom_text(data =jc, aes(x=x, y=-2, label = x), col= "red")+
    theme_bw()
  
  xbreaks <- c(75, 125, 200, 250, 300, 375, 475)
  xc <- tibble(x=xbreaks)
  
  ggplot()+
    geom_histogram(data = tab2, aes(x=x), binwidth = 1) +
    geom_histogram(data = tab2 %>% filter(lineid==9), aes(x=x), fill = "blue", binwidth = 1) +
    geom_histogram(data = tab2 %>% filter(lineid==10), aes(x=x), fill = "orange", binwidth = 1) +
    geom_segment(data  = xc, aes(x=x, xend=x, y=0, yend=Inf), col="red")+
    geom_text(data =xc, aes(x=x, y=-2, label = x), col= "red")+
    theme_bw()
  
  tab2 <- tab2 %>% 
    set_columns_fromxbreaks(., xbreaks = xbreaks)
  
  # set rows. 
  plot_ylocs(tab2, focuscol = 1, focuscol2 = 1, ybreaks = NULL)
  ybreaks <- get_ybreaks_bygap(tab2, gaplimit = 8, .min = 100)
  plot_ylocs(tab2, focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  ybreaks <- ybreaks[!ybreaks %in% c(179.5,  356.5, 422.5,444.5)]
  plot_ylocs(tab2, focuscol = 1, focuscol2 = 1, ybreaks = ybreaks)
  
  tab2 <- tab2 %>% mutate(yrow = as.numeric(cut(y, breaks = ybreaks)))
  
  # tabulate   
  tab2 <- tab2 %>% tabulate_df()
  
  tab2 %>% print(n=nrow(tab2))  
  
  # expand out the x-x numbers using str_split_custom() and range_expand()
  tab2 <- tab2 %>% 
    mutate(across(`Agricultural Pollination`:`Resilience to Human Disturbance and Climate Change`, 
                  ~map_chr(.x, function(x) {str_split_custom(x, pattern = ",| |\\[|\\]", type = "around") %>% range_expand() })))
  
  # empirical refs per line  
  empirical <- list(c(1,104,105,106),
                    NA,
                    c(5,107),
                    c(15,113,114,115:119, 6, 106, 120,121,122, 123,124),
                    127,
                    c(130,131,132,133),
                    60,
                    c(136,137),
                    NA,
                    c(140,141,130,131,132,133,140,141,122),
                    c(132,133),
                    c(131,144,145,132,133,122,146)
  ) %>% 
    map_chr(function(x) unique(x) %>% sort() %>% str_c(collapse = ","))
  
  tab2 <- tab2 %>% mutate(`Empirical references` = empirical)
  
  # color indicating impact types
  rowcolor <- c(1,2,1,1,1,1,1,1,2,1,1,1) # checked by entering twice
  
  tab2 <- tab2 %>% 
    add_column(`General impacts (row color)` = factor(rowcolor, 
                                                      levels = 1:2, 
                                                      labels = c('see columns', 'no impacts recorded')),
               .before =2) 
  
  # write_csv(tab2, "Data/Russo_2016_SItable2.csv")  
  
# Russo_2016 SI references ------------------------------------------------------------------------------------
  # pages 3:8 contain references
  # we can treat this also as a table, with the numeric being the top, left indicator of each cell. 
  
  refs <- dta[3:8] %>% map(., preprocess_p2df)
  
  refs[[1]] <- refs[[1]] %>% 
    clip_section(startrule = function(x) {which(x$text == "1.") %>% min()},
                 endrule = function(x) {nrow(x)})
  
  refs[2:5] <- refs[2:5] %>% 
    map(., ~ clip_section(.x, 
                          startrule = function(x) {which(x$lineid == 2) %>% min()},
                          endrule = function(x) {nrow(x)}))
  
  refs[[6]] <- refs[[6]] %>% 
    clip_section(startrule = function(x) {which(x$lineid == 2) %>% min()},
                 endrule = function(x) {which(x$text == "33.") %>% max()})
  
  # set columns  - these can be treated as left aligned, but care with multi rows, all pages are the same
  shift <- 2
  maxx <- bind_rows(refs) %>% select(x) %>% max()
  xbreaks <- get_xlocs_fromline(refs[[6]], .lineid = 2, .selection = c(2))
  xbreaks <- c(0, xbreaks - shift, maxx+shift) 
  
  refs <- refs %>% 
    map(., ~set_columns_fromxbreaks(.x, xbreaks = xbreaks))
  
  # set rows - we can do this from col 1
  refs <- refs %>% 
    map(~ set_row_numbers(.x, 1, 1, shift = 1))
  
  # tabulate - no header
  
  refs <- refs %>% 
    map(., ~ tabulate_df(.x, colname.row = 0, fixcolnames = c("RefID","Citation"))) %>% 
    bind_rows() 
  
  # write_csv(refs, "Data/Russo_2016_SIreferences.csv")    
  