#################################
#### Tidy Tuesday 2025-07-15 ####
#################################


# PACKAGES ====

library(tidyverse)
library(tidytuesdayR)
library(afcharts)



# DATA ====

## SUBJECT: British Library Funding
## SOURCE: https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-07-15



## IMPORT ----


### Option 1: tidytuesdayR R package  ----

tuesdata <-
  tidytuesdayR::tt_load('2025-07-15')
## alternative: tidytuesdayR::tt_load(2025, week = 28)


bl_fund_raw <-
  tuesdata %>%
  pluck("bl_funding")


### Option 2: Read directly from GitHub ---

# bl_funding <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv')



## PROCESSING ----


# difference between nominal nominal_gbp_millions amd sum of gia_gbp_millions:other_gbp_millions
bl_fund_diff <-
  bl_fund_raw %>%
  mutate(tot_fund_src = rowSums(pick(gia_gbp_millions:other_gbp_millions))) %>%
  mutate(nom_tot_diff = nominal_gbp_millions - tot_fund_src) %>% 
  relocate(nom_tot_diff, .after = nominal_gbp_millions ) %>% 
  relocate(tot_fund_src, .after = nominal_gbp_millions)



### convert to long format ----

#### group funding sources into two types "nominal_gbp_millions" ["nominal"] and other sources ["various"]

bl_fund_long <-
  bl_funding %>%
  pivot_longer(
    cols = nominal_gbp_millions:other_gbp_millions,
    values_to = "funding_gbp_millions",
    names_to = "funding_source"
  ) %>% 
  mutate(
    funding_type = case_when(funding_source == "nominal_gbp_millions" ~ "nominal",
                             TRUE ~ "various")) %>% 
  relocate(funding_type, funding_source, funding_gbp_millions, .after = year)



## EXPLORATION ----

bl_fund_long %>%
  ggplot(aes(
    x = funding_type,
    y = funding_gbp_millions,
    fill = as.factor(funding_source),
    label = funding_gbp_millions
  )) +
  geom_col(na.rm = T, position="fill") +
  # geom_label(
  #   colour = "white",
  #   show.legend = F,
  #   label.size = 0.2,
  #   position = position_stack(reverse = F, vjust = 0.5)
  # ) +
  
  facet_grid(cols = vars(year),
             scales = "free_x",
             switch = "x") +
  scale_fill_viridis_d(
    name = "Funding type",
    end = 0.45,
    direction = 1,
    option = "D"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.placement = "outside",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ## uncomment to remove the "Month" labelling
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1)) +

  labs(
    x = "Year",
    y = "Funding [£m]",
    title = "Total annual funding for the Brithish Library",
    subtitle = "Grouped by funding type"
  )
