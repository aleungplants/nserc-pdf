library(dplyr)
library(progress)

###########################
# Parse NSERC awards sheet
###########################

# uncomment to parse names
#
# infiles <- list.files(path = here::here("awards data"),
#                      pattern = "[0-9].xls.xlsx")
# data <- purrr::map(infiles, ~ mutate(readxl::read_xlsx(here::here(.), skip = 3),
#                                             Filename = .)) %>%
#   bind_rows() %>%
#   mutate(Program = stringr::word(Filename, 1, sep = "_"),
#                 CompetitionYear = stringr::word(`Fiscal Year`, 1, sep = "-"),
#                 .keep = "unused") %>%
#   select(Program, CompetitionYear, contains("Name")) %>%
#   mutate(FirstName = stringr::word(Name, 2, sep = ", "),
#          LastName = stringr::word(Name, 1, sep = ", ")) %>%
#   rowwise() %>%
#   mutate(FirstMiddleNames = stringr::str_split_1(FirstName, "(?=[A-Z])")[-1] %>% 
#                   stringr::str_c(collapse = " "),
#          LastNames = stringr::str_split_1(LastName, "(?=[A-Z])")[-1] %>% 
#                   stringr::str_c(collapse = "-")) %>%
#   mutate(ParsedName = stringr::str_c(c(FirstMiddleNames, LastNames), collapse = " ")) %>%
#   ungroup() %>%
#   arrange(desc(CompetitionYear))
# data
# writexl::write_xlsx(data, here::here("parsednames.xlsx"))


#########################
# Get Google Scholar IDs
#########################

# data <- readxl::read_xlsx(here::here("parsednames_checked.xlsx"))
# names <- data %>% pull(ParsedName)
# scholar_ids <- readxl::read_xlsx(here::here("scholars.xlsx"))
# 
# pb <- progress_bar$new(format = ":what [:bar] :percent",
#                        total = length(names))
# for (name in names) {
#   
#   if (name %in% scholar_ids$Name) {
#     pb$tick(tokens = list(what = paste("skipped", name)))
#     Sys.sleep(0.001)
#     next
#   } else {
#     scholar_ids <- scholar_ids %>%
#       add_row(Name = name,
#               ID = scholar::get_scholar_id(last_name = stringr::word(name, 1, sep = " "),
#                                            first_name = stringr::word(name, 2, sep = " ")))
#     set.seed(as.numeric(Sys.time()))
#     time_wait <- sample(50:300, 1)/10 # randomized 5 to 30 second wait, worked well so far with not getting blocked 
#     Sys.sleep(time_wait)
#     
#     if (stringr::str_detect(names(warnings()) %>% tail(n = 1), "code 429")) { # breaks loop if blocked by Google Scholar
#       break
#       print(names(warnings()))
#     }
#     
#     writexl::write_xlsx(scholar_ids, here::here("scholars.xlsx"))
#     pb$tick(tokens = list(what = paste("downloaded", name)))
#   }
# }

#########################################
# Check Google Scholar areas of interest
#########################################

scholar_ids <- readxl::read_xlsx(here::here("scholars.xlsx"))
ids <- scholar_ids %>% 
  na.omit() %>% 
  pull(ID) %>% 
  unique()

scholar_fields <- readxl::read_xlsx(here::here("scholar_fields.xlsx"))

pb <- progress_bar$new(format = ":what [:bar] :percent",
                       total = length(ids))

for (id in ids) {
  
  if (id %in% scholar_fields$ID) {
    pb$tick(tokens = list(what = paste("skipped", id)))
    Sys.sleep(0.01)
    next
  } else {
    
    scholar_fields <- scholar_fields %>%
      add_row(ID = id,
              Fields = stringr::str_c(scholar::get_profile(id)$fields, collapse = " | "))
    set.seed(as.numeric(Sys.time()))
    # time_wait <- sample(10:30, 1)/10
    # Sys.sleep(time_wait)
    
    if (stringr::str_detect(names(warnings()) %>% tail(n = 1), "code 429")) { # breaks loop if blocked by Google Scholar
      break
      print(names(warnings()))
    }
    
    writexl::write_xlsx(scholar_fields, here::here("scholar_fields.xlsx"))
    pb$tick(tokens = list(what = paste("downloaded", id)))
  }
  
}

##########################
# Get Google Scholar pubs
##########################

scholar_ids <- readxl::read_xlsx(here::here("scholars.xlsx"))
ids <- scholar_ids %>% 
  na.omit() %>% 
  pull(ID) %>% 
  unique()

pubs <- readxl::read_xlsx(here::here("pubs.xlsx"))

pb <- progress_bar$new(format = ":what [:bar] :percent",
                       total = length(ids))

for (id in ids) {
  
  if (id %in% pubs$ID) {
    pb$tick(tokens = list(what = paste("skipped", id)))
    Sys.sleep(0.001)
    next
  } else {
    
    nested_pubs <- tibble::tibble(ID = id,
                                  ScholarData = scholar::get_publications(id))
    pubs <- pubs %>%
      bind_rows(nested_pubs %>% tidyr::unnest(cols = "ScholarData"))
    
    time_wait <- sample(50:300, 1)/10 # randomized 5 to 30 second wait, worked well so far with not getting blocked
    Sys.sleep(time_wait)
    
    if (stringr::str_detect(names(warnings()) %>% tail(n = 1), "code 429")) { # breaks loop if blocked by Google Scholar
      break
      print(names(warnings()))
    }
    
    writexl::write_xlsx(pubs, here::here("pubs.xlsx"))
    pb$tick(tokens = list(what = paste("downloaded", id)))
  }
  
}

