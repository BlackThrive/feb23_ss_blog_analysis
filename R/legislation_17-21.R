rm(list = ls()) # clean environment

# install and library required packages
packages <- c('tidyverse','kableExtra')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

data <- readRDS("./data/ss_records_jan17_dec21.rds")

# count number of cases of each legislation and express as percentage
legislation <- data %>%
  group_by(legislation) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    percentage = round(100 * (n / sum(n)),2)
  )

# make into html table and save
kable_table <- kable(legislation,
                     caption = "Table 1: Stop and search records by legislation, Jan 2017-Dec 2021",
                     col.names = c("Legislation","Number of stops","Percentage of stops")) %>%
  kable_styling(full_width = F)

save_kable(kable_table, file = "./outputs/legislation_table.html")
# save_kable(kable_table, file = "./outputs/legislation_table.pdf")
save_kable(kable_table, file = "./outputs/legislation_table.png")

