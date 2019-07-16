library(tidyverse)
library(extrafont)
library(magrittr)
library(gtrendsR)
library(ggpmisc)
library(openair)
library(zoo)
library(ggsci)

source("https://raw.githubusercontent.com/atsyplenkov/caucasus-sediment-yield/master/R/00_own-functions.R")

# 1) Data reading -------------------------------------------------------------
# Read tidytuesday dataset
r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")

# Get time span
timespan <- glue::glue("{r4ds_members$date[1]} {r4ds_members$date[nrow(r4ds_members)]}")

# Read google search engine trends
gtrends("data science",
        time = as.character(timespan),
        onlyInterest = T) %$% 
  interest_over_time %>% 
  as_tibble() %>% 
  select(date, hits) -> gtr

# 2) Plot ---------------------------------------------------------------------
# Timeline
r4ds_members %>% 
  full_join(gtr %>% mutate(date = as.Date(date)), by = "date") %>% 
  mutate(hits = hits * 30,
         hits = zoo::na.approx(hits, rule = 2)) %>% 
  dplyr::select(date, total_membership, hits) %>% 
  gather(what, value, -date) %>% 
  ggplot(aes(x = date, y = value, color = what)) +
  geom_vline(xintercept = as.Date("2018-07-6"),
             color = "grey60") +
  geom_smooth(method = "lm",
              linetype = "dashed",
              se = F) +
  geom_line() +
  geom_curve(data = tibble(x = as.Date("2018-11-17"), y = 750,
                           xend = as.Date("2018-07-10"), yend = 1450),
             aes(x = x, y = y, xend = xend, yend = yend),
             curvature = -.25,
             color = "black",
             inherit.aes = F,
             arrow = arrow(type = "closed", length = unit(.15, "cm"))) +
  annotate("text", x = as.Date("2018-11-17"), y = 750,
           hjust = -.01,
           vjust = .4,
           lineheight = .9,
           label = paste0("July 4-6: 7eme rencontres R \n Rennes, France \n @rencontres_R"),
           size = 4, family = "Ubuntu", color = "black") +
  scale_y_continuous(breaks = c(0, 750, 1500, 2250, 3000),
                     labels = function(...) prettyNum(..., big.mark = " "), 
                     sec.axis = sec_axis(~ . / 30,
                                         name = "Google search hits")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  ggsci::scale_color_startrek(name = "",
                              labels = c("Google Search Hits",
                                         "R4DS Total Membership")) +
  labs(x = "", y = "Total Membership",
       title = "R4DS community through time",
       subtitle = "What is the key for growing?",
       caption = "Made by @atsyplen for #TidyTuesday №29") +
  theme_clean() -> timeline

# Scatterplot
openair::timeAverage(r4ds_members,
                     avg.time = "7 day",
                     statistic = "sum") %>% 
  full_join(gtr, by = "date") %>% 
  ggplot(aes(y = total_membership,
             x = hits)) +
  geom_smooth(se = F,
              method = "lm",
              color = "coral") +
  geom_point(color = "dimgrey") +
  ggpmisc::stat_poly_eq(formula = y ~ x, parse = T) +
  scale_y_continuous(labels = function(...) prettyNum(..., big.mark = " ")) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(y = "Total members", x = "Google search hits",
       caption = "Made by @atsyplen for #TidyTuesday №29") +
  theme_clean() -> scatterplot

# Percent of active members
r4ds_members %>% 
  mutate(da = daily_active_members * 100 / total_membership) %>% 
  ggplot(aes(x = date, y = da)) +
  geom_line(color = "dimgrey") +
  geom_smooth(method = 'loess',
              se = F,
              color = "coral") +
  scale_y_log10() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  labs(x = "", y = "Percent of active members, %",
       title = "R4DS community through time",
       subtitle = "Is the increase in total membership is significant in the end?",
       caption = "Made by @atsyplen for #TidyTuesday №29") +
  theme_clean() -> percent

# 3) Save -------------------------------------------------------------------
ggsave("2019-07-16/figures/timeline.png", timeline,
       dpi = 300, h = 5, w = 10)

ggsave("2019-07-16/figures/percent.png", percent,
       dpi = 300, h = 5, w = 10)

ggsave("2019-07-16/figures/scatterplot.png", scatterplot,
       dpi = 300, h = 5, w = 5)
