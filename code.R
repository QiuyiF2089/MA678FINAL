library(dplyr)
library(stringr)
library(readxl)

# 读取 Excel 文件


#1.data cleaning and standardlization
population=read.csv('population.csv')
population <- population %>%
  select(STNAME, CTYNAME, POPESTIMATE2020, POPESTIMATE2021, POPESTIMATE2022)
colnames(population)


df_gaz_counties <- read.delim("2024_Gaz_counties_national.txt")
area <- df_gaz_counties[c("USPS","NAME","ALAND_SQMI","AWATER_SQMI")]
colnames(area)

state_mapping <- data.frame(
  USPS = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
           "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
           "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
           "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
           "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
  STNAME = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
             "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
             "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
             "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
             "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
             "New Hampshire", "New Jersey", "New Mexico", "New York", 
             "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
             "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
             "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
             "West Virginia", "Wisconsin", "Wyoming", "District of Columbia")
)


population <- merge(population, state_mapping, by = "STNAME", all.x = TRUE)

# 合并数据框
merged_data <- merge(
  area,
  population,
  by.x = c("USPS", "NAME"),
  by.y = c("USPS", "CTYNAME"),
  all = TRUE
)

merged_data
colnames(merged_data)
cleaned_data <- merged_data[!is.na(merged_data$ALAND_SQMI), ]

# 计算每年的密度，新增密度列
cleaned_data$density2020 <- cleaned_data$POPESTIMATE2020 / cleaned_data$ALAND_SQMI
cleaned_data$density2021 <- cleaned_data$POPESTIMATE2021 / cleaned_data$ALAND_SQMI
cleaned_data$density2022 <- cleaned_data$POPESTIMATE2022 / cleaned_data$ALAND_SQMI

income <- read_excel("income.xlsx")
income <- income[, 1:4]
income <- income[6:nrow(income), ]
# 重命名列名
colnames(income) <- c("county", "2020income", "2021income", "2022income")

income$state <- ifelse(income$county %in% state_mapping$STNAME, income$county, NA)
income$state <- zoo::na.locf(income$state)
income <- income[income$county != income$state, ]

cleaned_data$NAME <- gsub("county", "", cleaned_data$NAME, ignore.case = TRUE)

# 去掉多余的空格（如果需要）
cleaned_data$NAME <- trimws(cleaned_data$NAME)

merged <- merge(
  cleaned_data, 
  income, 
  by.x = c("STNAME", "NAME"), 
  by.y = c("state", "county"), 
  all.x =  TRUE 
)

merged <- merged[!is.na(merged$STNAME), ]

merged <- merged %>%
  mutate(
    `2020income` = as.numeric(`2020income`),
    `2021income` = as.numeric(`2021income`),
    `2022income` = as.numeric(`2022income`)
  )

# 按 state 分组计算均值并插入 NA 值
merged <- merged %>%
  group_by(STNAME) %>%
  mutate(
    `2020income` = ifelse(is.na(`2020income`), mean(`2020income`, na.rm = TRUE), `2020income`),
    `2021income` = ifelse(is.na(`2021income`), mean(`2021income`, na.rm = TRUE), `2021income`),
    `2022income` = ifelse(is.na(`2022income`), mean(`2022income`, na.rm = TRUE), `2022income`)
  ) %>%
  ungroup()

vaccine=read.csv("vaccine.csv")
vaccine <- vaccine[, c(
  "Date",
  "Recip_State",               # 州
  "Recip_County",              # 县
  "Administered_Dose1_Pop_Pct",# 疫苗覆盖率
  "Series_Complete_Pop_Pct",   # 完全接种概率
  "Booster_Doses_Vax_Pct",     # 加强针概率
  "Census2019"                 # 人口普查
)]

vaccine <- vaccine %>%
  arrange(Date)

vaccine


political<-read.csv("political.csv")




age <-read.csv("county_demographics.csv")

age <- age[, c("County", "State", "Age.Percent.65.and.Older")]


age$County <- gsub(" County$", "", age$County)  # 去掉 "County" 后缀

age

final <- merge(
  merged, 
  age, 
  by.x = c("USPS", "NAME"), 
  by.y = c("State", "County"), 
  all.x =  TRUE 
)

final
death<-read.csv("deaths.csv")

cases<-read.csv("cases.csv")



library(tidyr)

table_dose1 <- vaccine %>%
  select(Date, Recip_State, Recip_County, Administered_Dose1_Pop_Pct) %>%
  pivot_wider(names_from = Date, values_from = Administered_Dose1_Pop_Pct)

# 表格2: Series_Complete_Pop_Pct
table_series_complete <- vaccine %>%
  select(Date, Recip_State, Recip_County, Series_Complete_Pop_Pct) %>%
  pivot_wider(names_from = Date, values_from = Series_Complete_Pop_Pct)

# 表格3: Booster_Doses_Vax_Pct
table_booster <- vaccine %>%
  select(Date, Recip_State, Recip_County, Booster_Doses_Vax_Pct) %>%
  pivot_wider(names_from = Date, values_from = Booster_Doses_Vax_Pct)

table_dose1

colnames(final)

final <- subset(final, select = -c(ALAND_SQMI, AWATER_SQMI, POPESTIMATE2020, POPESTIMATE2021, POPESTIMATE2022))

final


######################################
# 查看数据的基本信息
summary(final)

# 检查缺失值
colSums(is.na(final))

# 检查列类型
str(final)
##################################
# 按州分组并计算65岁及以上人口比例的中位数
age_summary <- final %>%
  group_by(STNAME) %>%
  summarise(Median_Age_65 = median(`Age.Percent.65.and.Older`, na.rm = TRUE))

# 绘制箱线图
ggplot(final, aes(x = reorder(STNAME, `Age.Percent.65.and.Older`), y = `Age.Percent.65.and.Older`)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of Age Percent 65 and Older by State",
       x = "State",
       y = "Age Percent 65 and Older")

###################################

# 按州分组计算平均值和中位数
state_summary <- final %>%
  group_by(STNAME) %>%
  summarise(
    Avg_Density_2022 = mean(density2022, na.rm = TRUE),
    Median_Income_2022 = median(`2022income`, na.rm = TRUE),
    Avg_Age_65 = mean(`Age.Percent.65.and.Older`, na.rm = TRUE)
  )

# 查看汇总数据
print(state_summary)
##############################
# 按州分组计算收入中位数
income_summary <- final %>%
  group_by(STNAME) %>%
  summarise(Median_Income_2022 = median(`2022income`, na.rm = TRUE))

# 绘制收入分布箱线图
ggplot(final, aes(x = reorder(STNAME, `2022income`), y = `2022income`)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of Income by State (2022)",
       x = "State",
       y = "Income (2022)")
##############################
# 计算人口密度变化
final <- final %>%
  mutate(Density_Change = density2022 - density2020)

# 按州绘制人口密度变化的箱线图
ggplot(final, aes(x = reorder(STNAME, Density_Change), y = Density_Change)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Population Density Change (2020-2022) by State",
       x = "State",
       y = "Density Change (2022 - 2020)")
########################################
# 创建人口密度分组
final <- final %>%
  mutate(Density_Group = cut(density2022, breaks = c(-Inf, 500, 1000, Inf), 
                             labels = c("Low", "Medium", "High")))

# 绘制分组的箱线图
ggplot(final, aes(x = Density_Group, y = `Age.Percent.65.and.Older`, fill = Density_Group)) +
  geom_boxplot() +
  labs(title = "Age Percent 65 and Older by Population Density Group (2022)",
       x = "Density Group",
       y = "Age Percent 65 and Older") +
  theme(legend.position = "none")





table_dose1
colnames(table_dose1)


############################################

# 转换数据为长格式
table_long <- table_dose1 %>%
  pivot_longer(
    cols = starts_with("01/") | starts_with("02/") | starts_with("03/") | starts_with("12/"),
    names_to = "Date",
    values_to = "Vaccination_Rate"
  ) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# 查看长格式数据
head(table_long)



# 按日期汇总每日平均接种率
daily_summary <- table_long %>%
  group_by(Date) %>%
  summarise(Avg_Vaccination_Rate = mean(Vaccination_Rate, na.rm = TRUE))

# 绘制每日接种率趋势图
ggplot(daily_summary, aes(x = Date, y = Avg_Vaccination_Rate)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Daily Vaccination Rate Trend",
       x = "Date",
       y = "Average Vaccination Rate") +
  theme_minimal()
#######################################

# 按州和日期汇总接种率
state_summary <- table_long %>%
  group_by(Recip_State, Date) %>%
  summarise(Avg_Vaccination_Rate = mean(Vaccination_Rate, na.rm = TRUE))

# 绘制各州接种率趋势图
ggplot(state_summary, aes(x = Date, y = Avg_Vaccination_Rate, color = Recip_State)) +
  geom_line(alpha = 0.7) +
  labs(title = "Vaccination Rate Trends by State",
       x = "Date",
       y = "Average Vaccination Rate") +
  theme_minimal() +
  theme(legend.position = "none")
############################
# 准备数据
heatmap_data <- table_long %>%
  group_by(Recip_State, Date) %>%
  summarise(Avg_Vaccination_Rate = mean(Vaccination_Rate, na.rm = TRUE))

# 绘制热力图
ggplot(heatmap_data, aes(x = Date, y = Recip_State, fill = Avg_Vaccination_Rate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Vaccination Rate Heatmap by State",
       x = "Date",
       y = "State",
       fill = "Rate") +
  theme_minimal()

#######################3
# 转换数据为长格式
data_long <- death %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Date",
    values_to = "Cumulative_Deaths"
  ) %>%
  mutate(
    Date = as.Date(gsub("X", "", Date), format = "%m.%d.%y"),
    Cumulative_Deaths = as.numeric(Cumulative_Deaths)
  )

# 计算每日新增死亡人数
data_long <- data_long %>%
  group_by(Admin2, Province_State) %>%
  arrange(Date) %>%
  mutate(New_Deaths = Cumulative_Deaths - lag(Cumulative_Deaths, default = 0))

# 计算死亡率（每百万人死亡人数）
data_long <- data_long %>%
  mutate(Death_Rate_Per_Million = (Cumulative_Deaths / Population) * 1e6)

# 按日期汇总全国每日新增死亡人数
daily_deaths <- data_long %>%
  group_by(Date) %>%
  summarise(Total_New_Deaths = sum(New_Deaths, na.rm = TRUE))

# 绘制总体每日新增死亡人数趋势
ggplot(daily_deaths, aes(x = Date, y = Total_New_Deaths)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Daily New Deaths Trend",
       x = "Date",
       y = "New Deaths") +
  theme_minimal()

# 按州和日期汇总每日新增死亡人数
state_daily_deaths <- data_long %>%
  group_by(Province_State, Date) %>%
  summarise(Total_New_Deaths = sum(New_Deaths, na.rm = TRUE))

# 绘制各州每日新增死亡人数趋势
ggplot(state_daily_deaths, aes(x = Date, y = Total_New_Deaths, color = Province_State)) +
  geom_line(alpha = 0.7) +
  labs(title = "Daily New Deaths by State",
       x = "Date",
       y = "New Deaths") +
  theme_minimal() +
  theme(legend.position = "none")

# 计算各州的累计死亡率
state_death_rates <- data_long %>%
  group_by(Province_State) %>%
  summarise(Avg_Death_Rate = mean(Death_Rate_Per_Million, na.rm = TRUE))

# 绘制死亡率分布（按州）
ggplot(data_long, aes(x = reorder(Province_State, Death_Rate_Per_Million), y = Death_Rate_Per_Million)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Death Rate Distribution by State",
       x = "State",
       y = "Death Rate (per million)") +
  theme_minimal()


# 绘制州的每日新增死亡人数趋势
ggplot(state_data, aes(x = Date, y = New_Deaths, color = Admin2)) +
  geom_line(alpha = 0.7) +
  labs(title = paste("Daily New Deaths in", specific_state),
       x = "Date",
       y = "New Deaths") +
  theme_minimal()



# 准备热力图数据
heatmap_data <- data_long %>%
  group_by(Province_State, Date) %>%
  summarise(Avg_Death_Rate = mean(Death_Rate_Per_Million, na.rm = TRUE))

# 绘制热力图
ggplot(heatmap_data, aes(x = Date, y = Province_State, fill = Avg_Death_Rate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Death Rate Heatmap by State",
       x = "Date",
       y = "State",
       fill = "Death") +
  theme_minimal()
#######################################
# 转换数据为长格式
cases_long <- cases %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Date",
    values_to = "Cumulative_Cases"
  ) %>%
  mutate(
    Date = as.Date(gsub("X", "", Date), format = "%m.%d.%y"),
    Cumulative_Cases = as.numeric(Cumulative_Cases)
  )

# 计算每日新增病例数
cases_long <- cases_long %>%
  group_by(Admin2, Province_State) %>%
  arrange(Date) %>%
  mutate(New_Cases = Cumulative_Cases - lag(Cumulative_Cases, default = 0))

# 按日期汇总全国每日新增病例数
daily_cases <- cases_long %>%
  group_by(Date) %>%
  summarise(Total_New_Cases = sum(New_Cases, na.rm = TRUE))

# 绘制总体每日新增病例趋势
ggplot(daily_cases, aes(x = Date, y = Total_New_Cases)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Daily New Cases Trend",
       x = "Date",
       y = "New Cases") +
  theme_minimal()

# 按州和日期汇总每日新增病例数
state_daily_cases <- cases_long %>%
  group_by(Province_State, Date) %>%
  summarise(Total_New_Cases = sum(New_Cases, na.rm = TRUE))

# 绘制各州每日新增病例趋势
ggplot(state_daily_cases, aes(x = Date, y = Total_New_Cases, color = Province_State)) +
  geom_line(alpha = 0.7) +
  labs(title = "Daily New Cases by State",
       x = "Date",
       y = "New Cases") +
  theme_minimal() +
  theme(legend.position = "none")

# 计算各州的平均每日新增病例数
state_avg_cases <- cases_long %>%
  group_by(Province_State) %>%
  summarise(Avg_Daily_Cases = mean(New_Cases, na.rm = TRUE))

# 绘制每日新增病例数分布（按州）
ggplot(state_avg_cases, aes(x = reorder(Province_State, Avg_Daily_Cases), y = Avg_Daily_Cases)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Average Daily Cases by State",
       x = "State",
       y = "Average Daily Cases") +
  theme_minimal()


# 绘制州的每日新增病例趋势
ggplot(state_cases, aes(x = Date, y = New_Cases, color = Admin2)) +
  geom_line(alpha = 0.7) +
  labs(title = paste("Daily New Cases in", specific_state),
       x = "Date",
       y = "New Cases") +
  theme_minimal()

# 准备热力图数据
heatmap_cases <- cases_long %>%
  group_by(Province_State, Date) %>%
  summarise(Avg_Cases = mean(New_Cases, na.rm = TRUE))

# 绘制热力图
ggplot(heatmap_cases, aes(x = Date, y = Province_State, fill = Avg_Cases)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "New Cases Heatmap by State",
       x = "Date",
       y = "State",
       fill = "New Cases") +
  theme_minimal()

#######################


death_long <- pivot_longer(death, cols = starts_with("X"), 
                           names_to = "Date", 
                           values_to = "DeathNumber")%>%
  select(Admin2, Province_State, Date, DeathNumber)
colnames(death_long)


cases_long<- pivot_longer(cases, cols = starts_with("X"), 
                          names_to = "Date", 
                          values_to = "CaseNumber")%>%
  select(Admin2, Province_State, Date, CaseNumber)



combined_long <- left_join(death_long, cases_long, by = c("Admin2", "Province_State", "Date"))



# 修改 vaccine 数据集的列名
vaccine <- vaccine %>%
  rename(State = Recip_State, County = Recip_County) 

vaccine$County <- gsub("County$", "", vaccine$County)  # 去掉 "County" 后缀


state_mapping <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
           "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
           "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
           "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
           "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
  STNAME = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
             "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
             "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
             "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
             "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
             "New Hampshire", "New Jersey", "New Mexico", "New York", 
             "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
             "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
             "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
             "West Virginia", "Wisconsin", "Wyoming", "District of Columbia")
)

new_vaccine <- merge(vaccine, state_mapping, by = "State", all.x = TRUE)

combined_long <- combined_long %>%
  mutate(Date = as.Date(gsub("X", "", Date), format = "%m.%d.%y"))

combined_long <- combined_long %>%
  rename(County = Admin2, State = Province_State)

new_vaccine <- new_vaccine %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

new_vaccine <- new_vaccine %>%
  select(-State)

new_vaccine <- new_vaccine %>%
  rename(State = STNAME)


new_vaccine <- new_vaccine %>%
  arrange(State, County, Date)


new_vaccine <- new_vaccine %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

combined_long <- combined_long %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))



combined_long <- combined_long %>%
  mutate(State = trimws(State),
         County = trimws(County))

new_vaccine <- new_vaccine %>%
  mutate(State = trimws(State),
         County = trimws(County))

combined_data <- combined_long %>%
  left_join(new_vaccine, by = c("State", "County", "Date"))

final

final <- final %>%
  rename(County = NAME, State = STNAME)

combined_final_data <- combined_data %>%
  left_join(final, by = c("State", "County"))





# 预览合并后的数据
write.csv(combined_final_data, "data.csv", row.names = FALSE)

