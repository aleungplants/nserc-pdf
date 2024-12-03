library(dplyr)
library(progress)

# infiles <- list.files(path = here::here(),
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

data <- readxl::read_xlsx(here::here("parsednames_checked.xlsx"))
names <- data %>% pull(ParsedName)
scholar_ids <- readxl::read_xlsx(here::here("scholars.xlsx"))

pb <- progress_bar$new(format = ":what [:bar] :percent",
                       total = length(names))
for (name in names) {
  
  if (name %in% scholar_ids$Name) {
    # print(paste("> ", name, "'s profile has already been downloaded.", sep = ""))
    pb$tick(tokens = list(what = paste("skipped", name)))
    Sys.sleep(0.01)
    next
  } else {
    scholar_ids <- scholar_ids %>%
      add_row(Name = name,
              ID = scholar::get_scholar_id(last_name = stringr::word(name, 1, sep = " "),
                                           first_name = stringr::word(name, 2, sep = " ")))
    set.seed(as.numeric(Sys.time()))
    time_wait <- sample(50:300, 1)/10
    Sys.sleep(time_wait) # stops getting blocked by Google Scholar
    
    if (stringr::str_detect(names(warnings()) %>% tail(n = 1), "code 429")) {
      break
      print(names(warnings()))
    }
    
    writexl::write_xlsx(scholar_ids, here::here("scholars.xlsx"))
    pb$tick(tokens = list(what = paste("downloaded", name)))
  }
}
