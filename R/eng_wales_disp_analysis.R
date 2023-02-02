rm(list = ls()) # clean environment

# install and library required packages
packages <- c('tidyverse','gmodels','epitools')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

source("./R/riskratio_from_df.R") # risk ratio function

## 1. Data wrangling##
# 12 months to nov 22
data <- readRDS("./data/eng_wales_12mths_nov22.rds")
data <- data[["result"]]


# population estimates - - from https://www.nomisweb.co.uk/census/2011/dc2101ew
population_ests <- read.csv("./data/census2011_pop_ests_by_ethn_region.csv")
population_ests$Region[which(population_ests$Region == "Yorkshire and The Humber")] <- "Yorkshire and the Humber"

# set region of all Welsh LAs to 'Wales' and of Scottish LAs to 'Scotland'
data$region[which(data$country == "Wales")] <- "Wales"
#data$region[which(data$country == "Scotland")] <- "Scotland"

# get total pop for white and black by summing the regions
pop_ests_total <- data.frame("white_population" = sum(population_ests$White),
                             "black_population" = sum(population_ests$Black))

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
                                  "Other ethnic group - Not stated"),
                        White = c("White - Any other White background",
                                  "White - English/Welsh/Scottish/Northern Irish/British",
                                  "White - Irish")
  )

# subset to black and white
bw_data <- subset(data, self_defined_ethnicity == "Black" | self_defined_ethnicity == "White")
bw_data$ethnicity <- factor(bw_data$self_defined_ethnicity, levels = c("White","Black"))

## 2. collect stats ##
# count number of stops and calculate rates
temp_df <- bw_data %>%
  dplyr::group_by(ethnicity) %>%
  dplyr::summarise(
    stopped = dplyr::n()
  ) %>%
  dplyr::mutate(
    pop = c(as.numeric(pop_ests_total[, "white_population"]),
            as.numeric(pop_ests_total[, "black_population"])),
    percentage = 100 * (stopped/pop),
    not_stopped = pop - stopped
  ) %>%
  as.data.frame()

row.names(temp_df) <- temp_df$ethnicity

# build crosstab
ethn_1 <- data.frame("white" = c("stopped" = temp_df["White", "stopped"],
                                       "not_stopped" = temp_df["White", "not_stopped"]))

ethn_2 <- data.frame("black" = c("stopped" = temp_df["Black", "stopped"],
                                       "not_stopped" = temp_df["Black", "not_stopped"]))

comp_mat <- as.matrix(cbind(ethn_2, ethn_1)) # matrix for crosstable
comp_df <- as.data.frame(comp_mat) # df for custom rr function

# run stats
xtab <- tryCatch(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T),
                 warning = function(w) return(list(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T), w)))
rr <- riskratio_from_df(comp_df, "12 months to Nov 22") # risk ratio
