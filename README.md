# Data and analysis files for statistics in Black Thrive blog 2nd Feb 23

Data and analysis scripts for statistics cited in Black Thrive's blog posted on 2nd February 2023.
Note, the csv file contained in data_36_months_to_dec_21.zip will need to be unzipped to the ./data folder before running.

## Scripts

1. eng_wales_disp_analysis.R calculates the disproporionality in stop and search between 'Black' and 'White' people in England and wales over the 12 months to Nov 2022.

2. rates_over_time_all_ethnicities.R calcualtes the rate of stops of people of each ethncity ('Black', 'White', 'Asian', 'Mixed', 'Other') over the time period Jan 2019 - Nov 2022. Note these broad categories are the result of aggregating the Police 18+1 codes.  Note also that Mixed: White and Black African and Mixed: White and Black Caribbean categories are included within the ‘Black’ grouping, and the White: Gypsy or Irish Traveller is omitted from the ‘White’ grouping.

## Data licences
Stop and search data were aqcuired from the Police API via [extractss](https://github.com/BlackThrive/extractss). Additional population and geographic data were acquired from the Office of National Statistics. All data are available under the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
