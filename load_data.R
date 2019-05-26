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

alo <- alo %>%
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


# add boroughs information
alo <- alo %>%
  left_join(districts, by = c("Stadtteil" = "Name")) %>%
  select(-Stadtteil_Nr.y) %>%
  rename(Stadtteil_Nr = Stadtteil_Nr.x)



# join election results with unemployment data

election_results_aug <- election_results %>%
  full_join(alo, by = "Stadtteil") %>%
  filter(Stadtteil != "Stadt Köln")


(cor_part_unempl <- cor(election_results_aug$Wahlbeteiligung, election_results_aug$Arbeitslosenquote) %>%
  round(2))


highlights <- election_results_aug %>%
  top_n(1, Wahlbeteiligung) %>%
  select(Stadtteil, Arbeitslosenquote, Wahlbeteiligung)

highlights <- election_results_aug %>%
  top_n(1, desc(Wahlbeteiligung)) %>%
  select(Stadtteil, Arbeitslosenquote, Wahlbeteiligung) %>%
  bind_rows(highlights)

highlights <- election_results_aug %>%
  filter(Stadtteil %in% c("Fühlingen", "Blumenberg")) %>%
  select(Stadtteil, Arbeitslosenquote, Wahlbeteiligung) %>%
  bind_rows(highlights)


election_results_aug %>%
  ggplot(aes(Arbeitslosenquote, Wahlbeteiligung)) +
  geom_point(aes(col = Stadtbezirk)) +
  ggrepel::geom_text_repel(
    data = highlights, 
    aes(x = Arbeitslosenquote, y = Wahlbeteiligung, label = Stadtteil), 
    alpha = 0.5, size = 3) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Wahlbeteiligung nach Arbeitslosenquote in Kölner Stadtteilen",
    subtitle = "Europawahl 2019",
    caption = str_c("Korrelation Wahlbeteiligung - Arbeitslosenquote: ", cor_part_unempl),
    x = "Arbeitslosenquote 2014 (in %)",
    y = "Wahlbeteiligung (in %)"
    ) +
  guides(alpha = FALSE, col = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "#777777", size = 0.5) +
  theme_hc() +
  scale_color_hc()

