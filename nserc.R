library(magrittr)

infiles <- list.files(path = here::here(),
                     pattern = "*.xlsx")

data <- purrr::map(infiles, ~ dplyr::mutate(readxl::read_xlsx(here::here(.), skip = 3),
                                            Filename = .)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(Program = stringr::word(Filename, 1, sep = "_"),
                CompetitionYear = stringr::word(`Fiscal Year`, 1, sep = "-"),
                FirstName = stringr::word(Name, 2, sep = ", "),
                LastName = stringr::word(Name, 1, sep = ", "),
                .keep = "unused") %>%
  dplyr::select(Program, CompetitionYear, contains("Name"))
data

scholars <- data %>%
  dplyr::rowwise() %>%
  dplyr::mutate(ScholarID = scholar::get_scholar_id(LastName, FirstName))
writexl::write_xlsx(scholars, here::here("scholars.xlsx"))
