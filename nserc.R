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
  filter(!is.na(ID)) %>% 
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

scholar_ids_fields <- full_join(scholar_ids, scholar_fields)
writexl::write_xlsx(scholar_ids_fields, here::here("scholar_ids_fields.xlsx"))


##########################
# Get Google Scholar pubs
##########################

scholar_ids <- readxl::read_xlsx(here::here("scholar_ids_fields_checked.xlsx")) %>% 
  bind_rows(readxl::read_xlsx(here::here("scholar_ids_20212024.xlsx"))) %>%
  filter(!is.na(ID))
ids <- scholar_ids %>%
  filter(!is.na(ID)) %>%
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
    
    # time_wait <- sample(50:300, 1)/10 # randomized 5 to 30 second wait, worked well so far with not getting blocked
    # Sys.sleep(time_wait)
    
    if (stringr::str_detect(names(warnings()) %>% tail(n = 1), "code 429")) { # breaks loop if blocked by Google Scholar
      break
      print(names(warnings()))
    }
    
    writexl::write_xlsx(pubs, here::here("pubs.xlsx"))
    pb$tick(tokens = list(what = paste("downloaded", id)))
  }
  
}

length(pubs$ID %>% unique())
length(ids %>% unique())

setdiff(pubs$ID %>% unique(), ids %>% unique())

#############
# Count pubs
#############

library(ggplot2)
library(sjrdata)
journals <- sjr_journals %>%
  mutate(journal = stringr::str_to_lower(title), 
         .keep = "unused") %>%
  filter(year == 2023)

data <- readxl::read_xlsx(here::here("parsednames_checked.xlsx")) %>%
  select(Program, CompetitionYear, ParsedName) %>%
  rename(Name = ParsedName)
scholar_ids <- readxl::read_xlsx(here::here("scholar_ids_fields_checked.xlsx")) %>% 
  bind_rows(readxl::read_xlsx(here::here("scholar_ids_20212024.xlsx"))) %>%
  filter(!is.na(ID))
pubs <- readxl::read_xlsx(here::here("pubs.xlsx")) %>%
  group_by(ID) %>%
  mutate(FirstAuthor = stringr::word(author, 1, sep = ",") %>% 
           stringr::word(2),
         year_paper = year,
         journal = stringr::str_to_lower(journal)) %>%
  left_join(journals, by = "journal", relationship = "many-to-many") %>%
  filter(!is.na(sjr_best_quartile))

scholar_pubs <- right_join(scholar_ids, pubs) %>%
  right_join(data, ., relationship = "many-to-many") %>%
  mutate(Name = stringi::stri_trans_general(Name, "Latin-ASCII"),
         FirstAuthor = stringi::stri_trans_general(FirstAuthor, "Latin-ASCII"),
         IsFirstAuthor = stringr::str_detect(Name, FirstAuthor))

scholar_pubs_precomp <- scholar_pubs %>%
  group_by(CompetitionYear) %>%
  filter(year_paper <= CompetitionYear) %>%
  filter(Name != "Robert Latta" | Name == "Robert Latta" & CompetitionYear > 1971)

metrics <- scholar_pubs_precomp %>%
  group_by(Program, CompetitionYear, Name) %>%
  summarise(nPub = n(),
            nPub_FirstAuthor = sum(IsFirstAuthor == TRUE, na.rm = TRUE)) %>%
  mutate(Program = case_when(Program == "EvoEco" ~ "evol. & ecol.",
                             Program == "PlantBio" ~ "plant & tree biol."),
         CompetitionYear = as.numeric(CompetitionYear)-1)

plot_firstauthor_evol <- metrics %>% 
  filter(Program == "evol. & ecol.") %>% 
  ggplot(aes(x = CompetitionYear, y = nPub_FirstAuthor)) +
  geom_point() +
  facet_wrap(~ Program) +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1991, 2024, 2)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) +
  xlab("competition year") +
  ylab("# of first author publications")
plot_firstauthor_evol

ggsave(here::here("firstauthorpubs_time_evolecol.pdf"), plot_firstauthor_evol,
       height = 4, width = 5)

plot_firstauthor_plant <- metrics %>% 
  filter(Program == "plant & tree biol.") %>% 
  ggplot(aes(x = CompetitionYear, y = nPub_FirstAuthor)) +
  geom_point() +
  facet_wrap(~ Program) +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1991, 2024, 2)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) +
  xlab("competition year") +
  ylab("# of first author publications")
plot_firstauthor_plant

ggsave(here::here("firstauthorpubs_time_plantbiol.pdf"), plot_firstauthor_plant,
       height = 4, width = 5)

plot_firstauthor_all <- ggplot(metrics, aes(x = Program, y = nPub_FirstAuthor)) +
  geom_boxplot(linewidth = 0.25, outliers = FALSE) + # already plotting all points
  ggbeeswarm::geom_quasirandom(color = "black", size = 0.5, varwidth = TRUE) +
  cowplot::theme_cowplot() +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) +
  xlab("research subject") +
  ylab("# of first author publications")
plot_firstauthor_all

ggsave(here::here("firstauthorpubs_all.pdf"), plot_firstauthor_all,
       height = 4, width = 3.5)
