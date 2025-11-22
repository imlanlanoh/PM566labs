install.packages("rvest")

library(rvest)
library(xml2)

# Reading the HTML table with the function xml2::read_html
covid <- read_html(
  x = "https://en.wikipedia.org/wiki/COVID-19_pandemic_death_rates_by_country"
)

# Let's look at the the output
covid

table <- xml_find_all(covid, xpath = '/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/div[4]/table')
print(table) # collection of tables in XML

table <- html_table(table) 
print(table) # list of tibbles


html_table(covid)[[2]]
