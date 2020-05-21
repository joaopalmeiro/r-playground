library(tidyverse)
library(rvest)
library(here)
library(hrbrthemes)
library(ggtext)

# Font: https://fonts.google.com/specimen/IBM+Plex+Sans

# Source of abbreviations for non-active franchises/teams:
#   Sports Reference LLC.
#   Basketball-Reference.com - Basketball Statistics and History.
#   https://www.basketball-reference.com/.
#   (Thursday, May 21, 2020)

url <-
  "https://en.wikipedia.org/wiki/NBA_Most_Valuable_Player_Award"
css_selector <-
  "table[summary^=\"Season (sortable), Player (sortable), Position (sortable), Nationality (sortable) and Team (sortable)\"]"

url_abbreviations <-
  "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations"

nba_mvp <- url %>%
  read_html() %>%
  html_node(css = css_selector) %>%
  html_table() %>%
  as_tibble()

nba_abbreviations <- url_abbreviations %>%
  read_html() %>%
  html_node(css = "table") %>%
  html_table(header = TRUE) %>%
  as_tibble() %>%
  rename(Team_Abbreviation = `Abbreviation/Acronym`) %>%
  mutate(Team_Active = TRUE)

nba_abbreviations_plus_non_active <- nba_abbreviations %>%
  bind_rows(tibble(
    Team_Abbreviation = c("STL", "PHW", "CIN", "BAL", "BUF"),
    Franchise = c(
      "St. Louis Hawks",
      "Philadelphia Warriors",
      "Cincinnati Royals",
      "Baltimore Bullets",
      "Buffalo Braves"
    ),
    Team_Active = FALSE
  ))

nba_abbreviations_plus_non_active

clean_nba_mvp <- nba_mvp %>%
  mutate(
    Player = str_trim(str_remove_all(
      Player, "[()*^[:digit:]]|\\[[:alpha:]\\]"
    )),
    Team = str_trim(str_remove_all(Team, "\\([:digit:]+\\)")),
    Nationality = str_trim(str_remove_all(Nationality, "\\[[:alpha:]\\]"))
  )

teams <-
  clean_nba_mvp %>%
  group_by(Team) %>%
  tally() %>%
  arrange(desc(n))
print(teams, n = nrow(teams))

clean_nba_mvp <-
  clean_nba_mvp %>%
  left_join(nba_abbreviations_plus_non_active, by = c("Team" = "Franchise"))

clean_nba_mvp <- clean_nba_mvp %>% type.convert()

clean_nba_mvp

nba_mvp_to_plot <-
  clean_nba_mvp %>%
  group_by(Team_Abbreviation) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(
    Team_Abbreviation =
      factor(Team_Abbreviation, levels = Team_Abbreviation)
  )

nba_mvp_to_plot

# break_labels_y <- clean_nba_mvp %>% distinct(n) %>% pull(n)
# break_labels_y

nba_mvp_to_plot %>%
  ggplot(aes(x = Team_Abbreviation, y = n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    expand = c(0.01, 0),
    breaks = function(x) {
      pretty(x)
    }
  ) +
  theme_ipsum_ps() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
