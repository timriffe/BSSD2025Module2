
library(tidyverse)

library(rvest)
seer_page <- read_html("https://seer.cancer.gov/stdpopulations/stdpop.20ages.html")
html_table(seer_page)%>%'[['(1) |> 
  filter(Age != "Total") |> 
  mutate(age = parse_number(Age)) |> 
  select( -Age) |> 
  pivot_longer(-age, names_to = "standard", values_to = "pop") |> 
  mutate(pop = gsub(",","",pop) |> as.integer()) |> 
  group_by(standard) |> 
  mutate(pop = pop / sum(pop)) |> 
  arrange(standard, age) |> 
  write_csv("data/standards_abridged.csv")


meta <- fwf_widths(widths = c(3,3,8),
              col_names = c("standard","age","pop"))
seer <-
  read_fwf("https://seer.cancer.gov/stdpopulations/stdpop.19ages.txt", col_positions = meta,
           col_types = "cdd") |> 
  group_by(standard) |> 
  mutate(structure = pop / sum(pop),
         age_abr = case_when(age < 2 ~ age,
                         age >= 2 ~ 5 * age - 5))

# 012 = World (WHO 2000-2025) Std Million (single ages to 99)
# 205 = 2000 U.S. Std Population (single ages to 99 - Census P25-1130)
seer |> 
  mutate(n = case_when(age == 0 ~ 1,
                       age == 1 ~ 4,
                       TRUE ~ 5),
         structure = structure / n) |> 
  filter(standard == "010") |> 
  ggplot(aes(x=age,y=structure, color = standard)) +
  geom_line()


