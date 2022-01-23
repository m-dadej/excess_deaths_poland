library(tidyverse)
library(modelr)
library(ggforce)
library(hrbrthemes)
library(MetBrewer)
library(lubridate)


# First source("gus_extraction.R") or run it manually

# calculate excess deaths with a x order polynomial trend 
# and weekly seasonality calculated as a mean difference from detrended
# the model above is estimated on pre 2020 period

excess_df <- filter(mortality, kod_regionu == "PL" & sex == "T" &  wiek != "Ogółem") %>%
  select(wiek, week, value, year) %>%
  mutate(training_set = year < 2020) %>%
  group_by(wiek) %>%
  nest() %>%
  mutate(model = map(data, function(x){lm(value ~ poly(year, 3),
                                          data = filter(x, training_set))})) %>%
  mutate(detrend_deaths = map2(data, model, modelr::add_residuals)) %>%
  unnest(detrend_deaths) %>%
  ungroup() %>%
  select(-c(data, model)) %>%
  group_by(week, wiek) %>%
  mutate(week_effect = mean(resid[training_set])) %>%
  ungroup() %>%
  mutate(excess_deaths = resid - week_effect) %>%
  group_by(wiek) %>%
  mutate(model_deaths = value - excess_deaths,
         model_error = quantile(value - model_deaths, 0.95)) %>%
  ungroup()

# excess deaths for total population:

total_df <- group_by(excess_df, year, week) %>%
            summarise(model_deaths = sum(model_deaths),
                      actual_deaths = sum(value)) %>%
            ungroup() %>%
            mutate(wiek = "ogółem",
                   model_error = quantile(actual_deaths[.$year < 2020] - model_deaths[.$year < 2020], 0.95, na.rm = TRUE))

# separate df for zoom part of the plot
zoom_data <- filter(total_df, week < 53 ) %>%
  mutate(date = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u")) %>%
  filter(year(date) > 2018)

total_excess_plot <- filter(total_df, week < 53) %>%
  mutate(date = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u")) %>%
  filter(year > 2009) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = actual_deaths, color = "Faktyczne zgony")) +
  geom_line(aes(y = model_deaths, color = "Oczekiwane zgony")) +
  geom_ribbon(aes(ymin = (model_deaths - model_error), 
                  ymax = (model_deaths + model_error),
                  fill = "Przedział ufności (95%)"), alpha = 0.3) +
  facet_zoom(x = year(date) > 2019) +
  geom_ribbon(data = zoom_data,
              aes(ymin = pmin(model_deaths, actual_deaths), ymax = actual_deaths),
              fill = "coral", alpha = 0.3) +
  labs(title = "Nadmiarowe zgony w trakcie pandemii COVID-19",
       subtitle = "Liczba zgonów oraz ich wartość oczekiwana, oszacowana na danych z okresu 2000-2019.\nOd początku pandemii COVID-19 liczba nadmiernych zgonów wyniosła 154,759.",
       caption = "tt: @mateusz_dadej | dane: GUS 
Model oszacowany oddzielnie dla powyższych grup wiekowych, procedurą ETS z wielomianowym trendem i tygodniowym efektem sezonowym.",
       x = "", y = "Liczba zgonów") +
  scale_x_date() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(size = 7),
        plot.title = element_text(size = 16)) +
  scale_color_manual(values = c("coral3", "steelblue4")) +
  scale_fill_manual(values = "steelblue2")

ggsave("media/total_excess_plot.png", total_excess_plot, width = 10, height = 6)


# excess_deaths per age group:

excess_deaths_age_plot <- select(excess_df, year, week, "actual_deaths" = value, model_deaths, wiek, model_error) %>%
  rbind(total_df) %>%
  filter(week < 53) %>%
  mutate(date = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u")) %>%
  ggplot(aes(x = date)) +
  facet_wrap(~wiek, scales = "free_y") +
  geom_line(aes(y = actual_deaths, color = "Faktyczne zgony")) +
  geom_line(aes(y = model_deaths, color = "Oczekiwane zgony")) +
  labs(title = "Nadmiarowe zgony dla różnych grup wiekowych w Polsce",
       subtitle = "Liczba zgonów w Polsce oraz ich oczekiwana wartość oszacowana na danych z okresu 2000-2019. \nOd początku pandemii COVID-19 suma nadmiarowych zgonów wynosi 154,759.",
       x = "", y = "Liczba zgonów",
       caption = "tt: @mateusz_dadej | dane: GUS 
Model oszacowany dla oddzielnych grup wiekowych, dzięki procedurze ETS z wielomianowym trendem oraz tygodniowym efektem sezonowym.") +
  scale_x_date(date_labels = "'%y") +
  theme_ipsum_es() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = c("coral2", "steelblue4")) +
  geom_ribbon(aes(ymin = (model_deaths - model_error), 
                  ymax = (model_deaths + model_error),
                  fill = "Przedział ufności (95%)"), alpha = 0.4) +
  scale_fill_manual(values = "steelblue2")
  
ggsave(filename = "media/excess_deaths_age_plot.png", 
       excess_deaths_age_plot, width = 14, height = 12)


excess_deviation_plot <- select(excess_df, year, week, "actual_deaths" = value, model_deaths, wiek, model_error) %>%

  mutate(date = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u"),
         wiek2 = case_when(wiek %in% c('0 - 4', '5 - 9',  '10 - 14', '15 - 19', '20 - 24', '25 - 29') ~ '0 - 29',
                           wiek %in% c( '30 - 34', '35 - 39') ~ '30 - 39',
                           wiek %in% c( '40 - 44', '45 - 49') ~ '40 - 49',
                           wiek %in% c( '50 - 54', '55 - 59') ~ '50 - 59',
                           wiek %in% c( '60 - 64', '65 - 69') ~ '60 - 69',
                           wiek %in% c( '70 - 74', '75 - 79') ~ '70 - 79',
                           wiek %in% c( '80 - 84', '85 - 89', '90 i więcej') ~ '80 i więcej')) %>%
  group_by(wiek2, date) %>%
  summarise(actual_deaths = sum(actual_deaths),
            model_deaths = sum(model_deaths),
            date, wiek2) %>%
  ungroup() %>%
  group_by(wiek2) %>%
  mutate(model_error = quantile(actual_deaths[.$date < as.Date("2020-01-01")] - model_deaths[.$date < as.Date("2020-01-01")], 0.95, na.rm = TRUE)) %>%
  mutate(excess_deviation = actual_deaths / model_deaths,
         excess_dev_pmin = pmax(actual_deaths / model_deaths, 1),
         excess_dev_conf = ) %>%
  ungroup() %>%
  filter(year(date) > 2018) %>%
  drop_na() %>%
  ggplot(aes(x = date)) + 
  geom_tile(aes(y = wiek2, fill = excess_dev_pmin), height = 0.9) +
  scale_fill_viridis_c(option = "A", labels = function(x){scales::percent(x - 1)},
                       name = "Odchylenie od oczekiwanego poziomu zgonów", direction = -1) +
  theme_ipsum_es() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.7, "cm"),
        plot.caption = element_text(size = 7)) +
  labs(title = "Tygodniowe nadmierne zgony w Polsce",
       subtitle = "Odchylenie zgonów od ich wartości oczekiwanej dla różnych grup wiekowych.",
       x = "rok", y = "Wiek",
       caption = "twitter: @mateusz_dadej | dane: GUS
Model oszacowany dla oddzielnych grup wiekowych, dzięki procedurze ETS z wielomianowym trendem oraz tygodniowym efektem sezonowym.")

ggsave(filename = "media/excess_deviation.png", 
       excess_deviation_plot, width = 11, height = 7.5)



