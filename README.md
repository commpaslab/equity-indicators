# equity-indicators
Repo for Regional Equity Indicator Reports

## templates
* cPov.R: pulls child poverty data from the American Community Survey 5-year data for selected year and locality, saves resulting data as cPov_[localityfips]_tract_[year].Rds
* ed.R: pulls selected educational degree attainment data (high school, associate degree, bachelors degree) from the American Community Survey 5-year data for selected year and locality, save resulting data as ed_[localityfips]_tract_[year].Rds
* ed_general.Rmd: reads selected ed_[localityfips]_tract_[year].Rds file and creates disparity table, maps, and data table; this is knit to create ed_general.html
* cPov_general.Rmd: reads selected cPov_[localityfips]_tract_[year].Rds file and creates disparity table, maps, and data table; this is knit to create cPov_general.html
* county_codes.csv: contains locality fips codes and descriptive locality names to be used in the R and Rmarkdown files

Currently, we change the locality/year in the R files and run these to create the data frames. Then create a copy of the Rmarkdown files, renmaing them cPov_[fips].Rmd for each additional locality and knit these to create the html. By creating your own county_codes.csv, you could run the R files to get data for selected localities (changing only the state, locality, and year at the beginning of the code, along with educational level for ed.R) and then run the accompanying Rmd file to generate an initial summary report.

## docs

Contains the webpage files; you can see the individual locality Rmarkdown/html files here.
