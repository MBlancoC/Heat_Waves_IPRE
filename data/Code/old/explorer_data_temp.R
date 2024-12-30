
# Val comunas

comunas  <- rio::import("Data/Input/HW/tx_comunas_stgo_novmar_1980_2020_v2.xlsx")
comunas_total <- names(comunas)[colSums(!is.na(comunas)) > 0][-1]

comunas_con_na <- names(comunas)[colSums(is.na(comunas)) > 0]

list.files("Data/Output")

births <- rio::import("Data/Output/births_1992_2020.RData")

comunas_nac <- unique(births$name_com)

comunas_con_na[comunas_con_na=="San Ramón"] <- "San Ramon"
comunas_con_na[comunas_con_na=="Ñuñoa"] <- "Nunoa"

setdiff(comunas_con_na, comunas_nac)

births2 <- births %>% filter(month_nac %in% c(11,12,1,2,3))

comunas_nac_total <- births2 %>% 
  group_by(name_com) %>% 
  summarise(n=n()) %>% 
  mutate(drop=if_else(name_com %in% comunas_con_na, 1, 0)) %>% 
  arrange(desc(drop))

sum(comunas_nac_total$n[comunas_nac_total$drop==0])

births2 <- births2 %>% filter(!name_com %in% comunas_con_na)

writexl::write_xlsx(comunas_nac_total, "Data/Output/Tabla_comunas_nacimiento.xlsx")

births3 <- births2 %>% filter(year_nac >= 2000 & year_nac <= 2024) 



list.files("Data/Input/HW")
tmean <- rio::import("Data/Input/HW/CR2MET_pr_v2.5_day_COM_TS_1980_2021_mmday.csv")
tmin <- rio::import("Data/Input/HW/CR2MET_tmin_v2.5_day_COM_TS_1980_2021.csv")
tmax <- rio::import("Data/Input/HW/CR2MET_tmax_v2.5_day_COM_TS_1980_2021.csv")

glimpse(tmean)
glimpse(tmin)
glimpse(tmax)

apply(tmean, 2, function(x) sum(is.na(x)))
apply(tmin, 2, function(x) sum(is.na(x)))
apply(tmax, 2, function(x) sum(is.na(x)))


apply(tmean, 2, function(x) sum(x == -9999, na.rm = TRUE))
apply(tmin, 2, function(x) sum(x == -9999, na.rm = TRUE))
apply(tmax, 2, function(x) sum(x == -9999, na.rm = TRUE))


sum(apply(tmean, 2, function(x) grepl("^13", as.character(x[1]))))
sum(apply(tmin, 2, function(x) grepl("^13", as.character(x[1]))))
sum(apply(tmax, 2, function(x) grepl("^13", as.character(x[1]))))
