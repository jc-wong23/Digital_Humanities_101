df <- readxl::read_xls("DataForTable2.1WHR2023.xls")

str(df)
range(df$year)

library(tidyverse)

# 10 years data from 2012 to 2022
df_10 <- df %>% 
  rename("Happiness Score" = colnames(df)[3]) %>% 
  mutate(year = sprintf("%04d", year)) %>% 
  filter(year >= 2012)

unique(df_10$`Country name`)
df_10[df_10$`Country name` == "Somaliland region", "Country name"] <- "Somalia"

######

library(ggplot2)

# Compute the proportion of missing values
NAs_prop <- (colSums(is.na(df_10)) / nrow(df_10))

NAs_df <- data.frame(variable = names(NAs_prop), proportion = NAs_prop)

# Bar Chart of the proportion of missing values by variable
ggplot(NAs_df[-c(1, 2), ], aes(x = variable, y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = scales::percent(proportion), y = proportion),
            position = position_stack(vjust = 0.5), color = "black") +
  labs(title = "Proportion of Missing Values by Variable",
       x = "Variable",
       y = "Proportion of Missing Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######

names(df_10)

# Compute the missing values for each variables except the Country name and
# the Happiness Score
na_count_by_country <- df_10 %>%
  group_by(`Country name`) %>%
  summarise(across(c(`Log GDP per capita`, `Social support`,
                     `Healthy life expectancy at birth`, `Freedom to make life choices`,
                     `Generosity`, `Perceptions of corruption`, `Positive affect`,
                     `Negative affect`), ~sum(is.na(.)), .names = "na_count_{.col}"))

# Validating the df
sapply(na_count_by_country[, -1], function(x) range(x, na.rm = TRUE))

which(na_count_by_country$`na_count_Healthy life expectancy at birth` == 11)

na_count_by_country$`Country name`[c(75, 140)]

# Observed both countries Kosovo and Taiwan Province of China do not have
# the Healthy life expectancy at birth data
life_exp_NAs <- df_10[df_10$`Country name` %in% c("Kosovo", "Taiwan Province of China"), ]

# Filtered countries if any value in a single column is greater than 4
filtered_na_count <- na_count_by_country %>%
  filter_at(vars(-`Country name`), any_vars(. > 4))

# long_table <- filtered_na_count %>%
#   select(-c("na_count_Positive affect", "na_count_Negative affect")) %>% 
#   pivot_longer(cols = -c("Country name"), 
#                names_to = "Variable", 
#                values_to = "Value")

######

# Creating correlation heatmap for 10 years data
names(df_10)
copy_df_10 <- na.omit(df_10)
cor_mat <- round(cor(copy_df_10[c(3:9)]), 2)
corr_df <- cor_mat
corr_df <- reshape2::melt(cor_mat)
# cor_mat[upper.tri(cor_mat)] <- NA
# cor_df <- reshape2::melt(cor_mat)

ggplot(data = corr_df,
       aes(x = Var1, y = Var2, fill = value, label = value)) +
  geom_tile() +
  geom_text(color = "black", size = 4) +
  labs(x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c",
                      guide = "colorbar") +
  ggtitle("Correlation Heatmap Between Key Variables") +
  theme(plot.title = element_text(size = 15))

######

# Extracting top 10 and bottom 10 countries based on happiness score from 2022
df_2022 <- read.csv("Appendix_2_Data_for_Figure_2.1.csv")
df_2022$Country[c(1:10, 137:146)]
members_20 <- df_10[df_10$`Country name` %in% df_2022$Country[c(1:10, 137:146)], ]

ggplot(members_20, aes(x = `year`, y = `Happiness Score`, group = `Country name`, color = `Country name`)) +
  geom_line() +
  labs(title = "Happiness Score Over Time by Top & Bottom 10 Countries",
       x = "Year",
       y = "Happiness Score") +
  theme_minimal()

# Creating correlation heatmap for 2022
any(is.na(df_2022))
names(df_2022)
cor_mat_2022 <- round(cor(df_2022[-147, c(3, 7:12)]), 2)
corr_df_2022 <- reshape2::melt(cor_mat_2022)

ggplot(data = corr_df_2022,
       aes(x = Var1, y = Var2, fill = value, label = value)) +
  geom_tile() +
  geom_text(color = "black", size = 4) +
  labs(x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "#86ebc9",
                      high = "#09855c",
                      guide = "colorbar") +
  ggtitle("Correlation Heatmap Between Key Variables") +
  theme(plot.title = element_text(size = 15))

######

# Creating continent variable for overall view for 10 years data
country_continent_list <- list(
  Africa = c(
    "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
    "Central African Republic", "Chad", "Comoros", "Congo (Brazzaville)", "Congo (Kinshasa)", 
    "Egypt", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Ivory Coast", 
    "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", 
    "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
    "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", 
    "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
  ),
  Asia = c(
    "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "China", 
    "Georgia", "Hong Kong S.A.R. of China", "India", "Indonesia", "Iran", "Iraq", 
    "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Laos", 
    "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", "Nepal", "North Korea", 
    "Oman", "Pakistan", "Qatar", "Saudi Arabia", "Singapore", "South Korea", 
    "Sri Lanka", "Syria", "Taiwan Province of China", "Tajikistan", "Thailand", 
    "Turkmenistan", "Turkey", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen",
    "Cambodia", "Philippines", "State of Palestine", "Turkiye"
  ),
  Europe = c(
    "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
    "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", 
    "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", 
    "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova", "Montenegro", 
    "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", 
    "Russia", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
    "Ukraine", "United Kingdom"
  ),
  North_America = c(
    "Belize", "Canada", "Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", 
    "Haiti", "Honduras", "Mexico", "Nicaragua", "Panama", "Trinidad and Tobago", "United States",
    "Jamaica"
  ),
  Oceania = c(
    "Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", "Nauru", 
    "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu"
  ),
  South_America = c(
    "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", 
    "Peru", "Suriname", "Uruguay", "Venezuela"
  )
)



df_10$Continent <- NA

df_10$Continent <- sapply(df_10$`Country name`, function(country) {
  for (continent in names(country_continent_list)) {
    if (country %in% country_continent_list[[continent]]) {
      return(continent)
    }
  }
  # If the country is not found in any continent, return NA or a default value
  return(NA)  # You can replace NA with a default continent if needed
})

which(is.na(df_10$Continent))

df_10 <- df_10 %>%
  select("Country name", "Continent", everything())


######
# write.csv(df_10, "10_years_df.csv", row.names = FALSE)
# write.csv(NAs_df, "NAs_prop_df.csv", row.names = FALSE)
# write.csv(corr_df, "corr_df.csv", row.names = FALSE)
# write.csv(corr_df_2022, "corr_df_2022.csv", row.names = FALSE)
# write.csv(filtered_na_count, "NAs_count_df.csv", row.names = FALSE)
# write.csv(members_20, "members_20.csv", row.names = FALSE)
