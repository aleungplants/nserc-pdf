library(magrittr)

infiles <- list.files(path = here::here(),
                     pattern = "[0-9].xls.xlsx")

data <- purrr::map(infiles, ~ dplyr::mutate(readxl::read_xlsx(here::here(.), skip = 3),
                                            Filename = .)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Program = stringr::word(Filename, 1, sep = "_"),
                CompetitionYear = stringr::word(`Fiscal Year`, 1, sep = "-"),
                .keep = "unused") %>%
  dplyr::select(Program, CompetitionYear, contains("Name"))
data

names <- data %>% head(3) %>% dplyr::pull(Name)
scholar_ids <- list()
for (name in names) {
  scholar_ids <- list(scholar_ids, 
                      name = scholar::get_scholar_id(last_name = stringr::word(name, 1, sep = ", "),
                                                     first_name = stringr::word(name, 2, sep = ", ")))
}

writexl::write_xlsx(scholars, here::here("scholars.xlsx"))
