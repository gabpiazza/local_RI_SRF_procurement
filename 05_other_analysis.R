# Load the Italian data 

italy<- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele IT.dta",encoding='latin1')

italy<-italy %>% 
  mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z"),
         date_closing = as.Date(closing_date_format))

# I then extract month and year 
italy<-italy %>% 
  mutate(year_orbis = year(closing_date_format),
         month_orbis = month(closing_date_format))

italy<- italy %>% mutate(year = case_when (month_orbis <6 ~ year_orbis -1, month_orbis >5 ~ year_orbis))


italy_data_2012_14<- italy %>% filter(year %in%c(2012, 2013,2014))

rm(italy)

italy_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE IT.csv")
italy_nace<- italy_nace %>% rename(bvd_id_number = bvdidnumber)

italy_2012_14_nace <- left_join(italy_data_2012_14, italy_nace)
italy_2012_14_nace$consolidation_l <- substr(italy_2012_14_nace$consolidation_code,1,1)
italy_2012_14_nace_unconsolidated<- italy_2012_14_nace %>% filter(consolidation_l =="U")
View(italy_2012_14_nace_unconsolidated)
italy_2012_14_nace_unconsolidated_manufacturing <- italy_2012_14_nace_unconsolidated %>% 
  filter(nacerev2mainsection=="C - Manufacturing")
italy_2012_14_nace_unconsolidated_manufacturing<- italy_2012_14_nace_unconsolidated_manufacturing %>% 
  filter(!is.na(number_of_employees) & !is.na(operating_revenue_turnover_)) 
italy_2012_14_ricavo_addetto<-italy_2012_14_nace_unconsolidated_manufacturing %>% 
  select(bvd_id_number, number_of_employees, operating_revenue_turnover_, nacerev2mainsection) %>% distinct()
italy_2012_14_ricavo_addetto<- italy_2012_14_ricavo_addetto %>% mutate(ricavo_addetto = operating_revenue_turnover_/number_of_employees)

# Calculate Q1 and Q3

Q1 <- quantile(italy_2012_14_ricavo_addetto$ricavo_addetto, 0.25)
Q3 <- quantile(italy_2012_14_ricavo_addetto$ricavo_addetto, 0.75)

# Calculate IQR
IQR_value <- IQR(italy_2012_14_ricavo_addetto$ricavo_addetto)

# Define the bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter the data
italy_2012_14_ricavo_addetto_cleaned <- italy_2012_14_ricavo_addetto[italy_2012_14_ricavo_addetto$ricavo_addetto >= lower_bound & italy_2012_14_ricavo_addetto$ricavo_addetto <= upper_bound, ]


italy_2012_14_ricavo_addetto_cleaned<- italy_2012_14_ricavo_addetto_cleaned %>% mutate(ricavo_addetto = operating_revenue_turnover_/number_of_employees)
View(italy_2012_14_ricavo_addetto_cleaned
     )
mean(italy_2012_14_ricavo_addetto_cleaned$ricavo_addetto
     )
40000000/230657.8
40000000/193330.6
42000000