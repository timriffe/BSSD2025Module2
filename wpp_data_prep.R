
# library(devtools)
# options(timeout = 600)
# install_github("PPgp/wpp2024")
library(wpp2024)
data(mx1dt)

mxwpp <-
  mx1dt |> 
  select(un_code = country_code, name, year, age, m = mxM, f = mxF) |> 
  pivot_longer(c(f,m),names_to = "sex", values_to = "mx") |> 
  arrange(un_code,year,sex,age)

write_csv(mxwpp,file = "data/mxwpp.csv.gz")
