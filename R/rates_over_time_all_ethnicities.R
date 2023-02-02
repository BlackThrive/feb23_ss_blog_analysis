rm(list = ls()) # clean environment

# install and library required packages
packages <- c('tidyverse','chron')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

## 1. combine 12 months to nov 22 with older data ##

data <- readRDS("./data/eng_wales_12mths_nov22.rds")
data <- data[["result"]]

data_2 <- read_csv("./data/data_36_months_to_dec_21.csv")

# get just the additional 24 months backwards from dec 21
data_2 <- data_2 %>%
  filter(date >= "2019-12-01" & date < "2021-12-01")

# sort out variable types so that the datasets can be combined
data$time <- chron::as.times(data$time)
data_2$time <- chron::as.times(data_2$time)
data$location.longitude <- as.numeric(data$location.longitude)
data$location.latitude <- as.numeric(data$location.latitude)
data$location.street.id <- as.numeric(data$location.street.id)
data$involved_person <- as.logical(data$involved_person)
data$outcome_linked_to_object_of_search <- as.logical(data$outcome_linked_to_object_of_search)
data$removal_of_more_than_outer_clothing <- as.logical(data$removal_of_more_than_outer_clothing)
data$operation <- as.logical(data$operation)

# combine most recent with older data
combined_data <- bind_rows(data, data_2)

# tidy ups
data <- combined_data
rm(combined_data)
rm(data_2)

## 2. Population estimates ##

# population estimates - - from https://www.nomisweb.co.uk/census/2011/dc2101ew
population_ests <- read.csv("./data/census2011_pop_ests_by_ethn_region.csv")
# correct capitalisation for compatibility
population_ests$Region[which(population_ests$Region == "Yorkshire and The Humber")] <- "Yorkshire and the Humber"

# set region of all Welsh LAs to 'Wales' and of Scottish LAs to 'Scotland'
data$region[which(data$country == "Wales")] <- "Wales"
#data$region[which(data$country == "Scotland")] <- "Scotland"

# get total pop for white and black by summing the regions
pop_ests_total <- data.frame("White" = sum(population_ests$White),
                             "Black" = sum(population_ests$Black),
                             "Mixed" = sum(population_ests$Mixed),
                             "Asian" = sum(population_ests$Asian),
                             "Other" = sum(population_ests$Other))

## 3. Data wrangling ##

# list ethnicity categories in data
# sort(unique(data$self_defined_ethnicity))

# collapse self-defined ethnicity (not necessary for officer-defined)
data$self_defined_ethnicity <- as.factor(data$self_defined_ethnicity)
data$self_defined_ethnicity <-
  forcats::fct_collapse(data$self_defined_ethnicity,
                        Asian = c("Asian/Asian British - Any other Asian background",
                                  "Asian/Asian British - Bangladeshi",
                                  "Asian/Asian British - Chinese",
                                  "Asian/Asian British - Indian",
                                  "Asian/Asian British - Pakistani"),
                        Black = c("Black/African/Caribbean/Black British - African",
                                  "Black/African/Caribbean/Black British - Any other Black/African/Caribbean background",
                                  "Black/African/Caribbean/Black British - Caribbean",
                                  "Mixed/Multiple ethnic groups - White and Black African", # have included mixed in Black category
                                  "Mixed/Multiple ethnic groups - White and Black Caribbean"),
                        Mixed = c("Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background",
                                  "Mixed/Multiple ethnic groups - White and Asian"),
                        Other = c("Other ethnic group - Any other ethnic group",
                                  #"Other ethnic group - Not stated",
                                  "Other ethnic group - Arab"),
                        White = c("White - Any other White background",
                                  "White - English/Welsh/Scottish/Northern Irish/British",
                                  "White - Irish")
  )

data <- data %>%
  rename(
    ethnicity = self_defined_ethnicity
  )



# separate date into separate columns
data$year <- as.numeric(substr(data$date, 1, 4))
data$month <- as.numeric(substr(data$date, 6, 7))
data$day <- as.numeric(substr(data$date, 9, 10))

# make year_month variable for indexing
data$year_month <-
  as.Date(paste(
    as.character(data$year),
    as.character(data$month),"01", sep= "-"))

# determine the year period based on dates - not necessary now that I've just
# by year-month

# data$year_period <- NA

# this is fastest way I've found to do this:
# data$year_period[which(data$date>= "2019-12-01" & data$date < "2020-12-01" )] <- "Dec19-Nov20"
# data$year_period[which(data$date>= "2020-12-01" & data$date < "2021-12-01" )] <- "Dec20-Nov21"
# data$year_period[which(data$date>= "2021-12-01" & data$date < "2022-12-01" )] <- "Dec21-Nov22"

## 4. Calculate stats ##

# collect stats - count number of stops and calculate rates
temp_df <- data %>%
  dplyr::group_by(ethnicity, year_month) %>%
  dplyr::summarise(
    stopped = dplyr::n()
  )

pop_ests_long <- pop_ests_total %>%
  pivot_longer(., everything(), names_to = c("ethnicity"), values_to = "population")

temp_df <- merge(temp_df, pop_ests_long, by = "ethnicity")

temp_df$ethnicity <- factor(temp_df$ethnicity, levels = c("White","Black","Asian", "Mixed","Other"))

temp_df <- temp_df %>%
  dplyr::mutate(
    rate = 1000 * (stopped/population),
    not_stopped = population - stopped
  ) %>%
  as.data.frame()

bt_palette <- c("#F18D21",
                 "#000000",
                 "#7A72BD", #Purple
                 "#369694", #Pastel Turquoise
                 "#85DBD9", #Light Pastel Turquoise
                 "#BB521E", #Red
                 "#FDE9FF", #Light Pastel Pink
                 "#FFD483", #Ochre/Pastel Yellow
                 "#FFF7E7") #Light Pastel Yellow


temp_df$year_month_label <- substr(temp_df$year_month, 1, 7)

# plot the rates

ggplot(temp_df, aes(year_month_label, rate, colour = ethnicity)) +
  geom_point() +
  geom_line(aes(group = ethnicity)) +
  #scale_x_date(breaks = temp_df$year_month) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.minor = element_blank()) +
  scale_colour_manual(values = bt_palette, name = "Ethnicity") +
  ylab("Stops per 1000 population") + xlab("Date") +
  scale_y_continuous(breaks = seq(0, plyr::round_any(max(temp_df$rate), 5, f = ceiling),5), limits = c(0, plyr::round_any(max(temp_df$rate), 5, f = ceiling)))

# ggsave(filename = "./outputs/all_ethnicity_stop_rates_36mths_nov22.png", height = 5, width = 10)

# other category removed

temp_df %>%
  filter(ethnicity != "Other") %>%
ggplot(aes(year_month_label, rate, colour = ethnicity)) +
  geom_point() +
  geom_line(aes(group = ethnicity)) +
  #scale_x_date(breaks = temp_df$year_month) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.minor = element_blank()) +
  scale_colour_manual(values = bt_palette, name = "Ethnicity") +
  ylab("Stops per 1000 population") + xlab("Date") +
  scale_y_continuous(breaks = seq(1,max(temp_df$rate),1))

ggsave(filename = "./outputs/all_ethnicity_stop_rates_36mths_nov22_other_removed.png", height = 5, width = 10)
