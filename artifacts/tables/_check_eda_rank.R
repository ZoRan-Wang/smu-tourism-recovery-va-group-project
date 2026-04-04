library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
path <- 'data/raw/tourism_update.xlsx'
tour <- read_excel(path, sheet = 'My Series', skip = 29, col_names = FALSE, .name_repair = 'minimal')
names(tour) <- c(
  'Date','Visitor Arrivals','Tourism Receipts: YTD: YoY','Tourist Expenditure Per Capita','Visitor Arrivals: China','Average Length of Stay','Visitor Arrivals: Malaysia','Visitor Arrivals: India','Visitor Arrivals: Indonesia','Visitor Arrivals: Australia','Number of Hotels','Visitor Days','Hotel Room Occupancy Rate','Visitor Arrivals: West Asia','Visitor Arrivals: Taiwan','Total Room Revenue','Number of Hotels.1','Average Length of Stay.1','Visitor Arrivals: Hong Kong SAR (China)','Hotel Revenue: per Available Room: Luxury','Hotel Revenue: per Available Room: Mid-Tier','Visitor Arrivals: ASEAN','Visitor Arrivals: Italy','Visitor Arrivals: Russian Federal (CIS)','Visitor Arrivals: France','Visitor Arrivals: Philippines','Visitor Arrivals: Spain','No of Hotel Room Stock','Visitor Arrivals: Thailand','Visitor Arrivals: Ireland','Visitor Arrivals: United Arab Emirates','Visitor Arrivals: United Kingdom','Visitor Arrivals: Africa','Visitor Arrivals: Bangladesh','Visitor Arrivals: Iran','Visitor Arrivals: New Zealand','Visitor Arrivals: Israel','Visitor Arrivals: North Asia','Visitor Arrivals: 8-10 Days','Visitor Arrivals: 11-14 Days','Visitor Arrivals: 15 Days & Over','Visitor Arrivals: Americas','Visitor Arrivals: Germany','Visitor Arrivals: Scandinavia: Sweden','Visitor Arrivals: Switzerland','Visitor Arrivals: USA','Visitor Arrivals: Canada','Visitor Arrivals: Mauritius','Visitor Arrivals: Kuwait','Visitor Arrivals: Egypt','Visitor Arrivals: Brunei','Visitor Arrivals: Finland','Visitor Arrivals: Japan','Visitor Arrivals: South Korea','Visitor Arrivals: Myanmar','Visitor Arrivals: Netherlands','Visitor Arrivals: Scandinavia: Norway','Visitor Arrivals: Saudi Arabia','Visitor Arrivals: Sri Lanka','Visitor Arrivals: Vietnam','Visitor Arrivals: Pakistan','Visitor Arrivals: Republic of South Africa'
)
tour <- tour %>% mutate(Date = as.Date(Date))
tour_clean <- tour %>% filter(year(Date) >= 2017 & year(Date) <= 2025)
country_cols <- c(
  'Visitor Arrivals: China','Visitor Arrivals: Malaysia','Visitor Arrivals: India','Visitor Arrivals: Indonesia','Visitor Arrivals: Australia','Visitor Arrivals: Taiwan','Visitor Arrivals: Hong Kong SAR (China)','Visitor Arrivals: Italy','Visitor Arrivals: Russian Federal (CIS)','Visitor Arrivals: France','Visitor Arrivals: Philippines','Visitor Arrivals: Spain','Visitor Arrivals: Thailand','Visitor Arrivals: Ireland','Visitor Arrivals: United Arab Emirates','Visitor Arrivals: United Kingdom','Visitor Arrivals: Bangladesh','Visitor Arrivals: Iran','Visitor Arrivals: New Zealand','Visitor Arrivals: Israel','Visitor Arrivals: Germany','Visitor Arrivals: Scandinavia: Sweden','Visitor Arrivals: Switzerland','Visitor Arrivals: USA','Visitor Arrivals: Canada','Visitor Arrivals: Mauritius','Visitor Arrivals: Kuwait','Visitor Arrivals: Egypt','Visitor Arrivals: Brunei','Visitor Arrivals: Finland','Visitor Arrivals: Japan','Visitor Arrivals: South Korea','Visitor Arrivals: Myanmar','Visitor Arrivals: Netherlands','Visitor Arrivals: Scandinavia: Norway','Visitor Arrivals: Saudi Arabia','Visitor Arrivals: Sri Lanka','Visitor Arrivals: Vietnam','Visitor Arrivals: Pakistan','Visitor Arrivals: Republic of South Africa'
)
tour_long <- tour_clean %>%
  select(Date, all_of(country_cols)) %>%
  pivot_longer(cols = -Date, names_to = 'Country', values_to = 'Arrivals') %>%
  mutate(Country = sub('^Visitor Arrivals: ', '', Country))
p1_data <- tour_long %>% filter(year(Date) >= 2017 & year(Date) <= 2019)
p2_data <- tour_long %>% filter(year(Date) >= 2022 & year(Date) <= 2025)
p1_total_market <- tour_clean %>% filter(year(Date) >= 2017 & year(Date) <= 2019) %>% summarise(Total_Market_Avg = round(mean(.data[['Visitor Arrivals']], na.rm = TRUE), 0)) %>% pull(Total_Market_Avg)
p1_rank <- p1_data %>% group_by(Country) %>% summarise(Avg_Monthly_Arrivals = round(mean(Arrivals, na.rm = TRUE), 0), .groups = 'drop') %>% mutate(Overall_Share_Percent = (Avg_Monthly_Arrivals / p1_total_market) * 100) %>% arrange(desc(Avg_Monthly_Arrivals)) %>% mutate(Rank = row_number()) %>% slice_head(n = 5)
p2_total_market <- tour_clean %>% filter(year(Date) >= 2022 & year(Date) <= 2025) %>% summarise(Total_Market_Avg = round(mean(.data[['Visitor Arrivals']], na.rm = TRUE), 0)) %>% pull(Total_Market_Avg)
p2_rank <- p2_data %>% group_by(Country) %>% summarise(Avg_Monthly_Arrivals = round(mean(Arrivals, na.rm = TRUE), 0), .groups = 'drop') %>% mutate(Overall_Share_Percent = (Avg_Monthly_Arrivals / p2_total_market) * 100) %>% arrange(desc(Avg_Monthly_Arrivals)) %>% mutate(Rank = row_number()) %>% slice_head(n = 5)
print(p1_rank)
print(p2_rank)
