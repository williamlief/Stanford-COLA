library(tidyverse)
# devtools::install_github("williamlief/liefTools")
# devtools::install_github("williamlief/DataVersionControl")


raw <- liefTools::read_excel_all("Raw/Stanford Cost of Living.xlsx", sheet_id = "group", skip = 2, na = "NA")

df <- raw %>% 
  select(-c(Date, wayback_url)) %>% 
  pivot_longer(-c(School_Year, group) , names_to = "item", values_to = "amount", values_drop_na = TRUE) %>% 
  rename(school_year = School_Year)

# Annualize all amounts, clean up names
df2 <- df %>% 
  mutate(
    annual_amount = case_when(
      group == "Stipend" ~ amount * 3, # stipend amount is quarterly, excluding summer
      group == "Meal_Plans" ~ amount * 365, # assuming eating every day
      group == "Insurance_student" ~ (amount * 3) / 2, # summer quarter is free, University pays half
      group == "Insurance_dependent" ~ amount * 12, # rate is monthly, 
      group == "Housing" ~ amount * 12 # rate is monthly
      ), 
    item = if_else(item == "Total_Cost" & group == "Insurance_student", "Student", item),
    group = if_else(grepl("Insurance", group), "Insurance", group)
  ) %>% 
  select(-amount)

DataVersionControl::saveRDS_vc(df2, "Data/col_data.rds")