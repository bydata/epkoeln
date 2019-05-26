library(tidyverse)
library(ggthemes)

# Unemployment statistics by district 2010 
alo_2010 <- read_csv("2010_Beschaeftigte_Stadtteil.csv") %>%
  rename(Stadtteil_Nr = `Nr.`) %>%
  mutate(year = 2010)

# # Unemployment statistics by district 2014 (delimiter = semicolon)
alo_2014 <- read_csv2("2014_Beschaeftigte_Stadtteil.csv") %>%
  rename(Stadtteil_Nr = `Nr.`) %>%
  mutate(year = 2014)

alo <- bind_rows(alo_2010, alo_2014) %>%
  arrange(Stadtteil, year) %>%
  select(Stadtteil, year, everything())

alo_2014_change <- alo %>%
  group_by(Stadtteil) %>%
  arrange(year) %>%
  mutate(Arbeitslosenquote_change = (Arbeitslosenquote - lag(Arbeitslosenquote))/lag(Arbeitslosenquote)) %>%
  ungroup() %>%
  filter(year == 2014) %>%
  select(Stadtteil, Stadtteil_Nr, starts_with("Arbeitslosenquote")) %>%
  mutate(
    Stadtteil = case_when(
      Stadtteil == "Altstadt-Nord" ~ "Altstadt/Nord",
      Stadtteil == "Altstadt-Süd" ~ "Altstadt/Süd",
      Stadtteil == "Neustadt-Nord" ~ "Neustadt/Nord",
      Stadtteil == "Neustadt-Süd" ~ "Neustadt/Süd",
      TRUE ~ Stadtteil
      )
    )

rm(alo_2010, alo_2014)





# join election results with unemployment data

election_results_aug <- election_results %>%
  full_join(alo_2014_change, by = "Stadtteil") %>%
  filter(Stadtteil != "Stadt Köln")


(cor_part_unempl <- cor(election_results_aug$Wahlbeteiligung, election_results_aug$Arbeitslosenquote) %>%
  round(2))

election_results_aug %>%
  ggplot(aes(Arbeitslosenquote, Wahlbeteiligung)) +
  geom_point() +
  geom_text(aes(label = Stadtteil), alpha = 0.3, size = 3) +
  labs(
    title = "Wahlbeteiligung nach Arbeitslosenquote in Kölner Stadtteilen",
    subtitle = "Europawahl 2019",
    caption = str_c("Korrelation Wahlbeteiligung - Arbeitslosenquote: ", cor_part_unempl),
    x = "Arbeitslosenquote 2014 (in %)",
    y = "Wahlbeteiligung (in %)"
    ) +
  guides(alpha = FALSE) +
  theme_hc()


cor(election_results_aug$Wahlbeteiligung, election_results_aug$Arbeitslosenquote)
