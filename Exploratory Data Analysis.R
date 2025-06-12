library(tidyverse)
library(knitr)
library(hrbrthemes)
library(sf)
library(gt)
library(gtExtras)
library(data.tree)
library(circlepackeR)
library(htmlwidgets) 
library(scales)
library(writexl)
library(cowplot)
library(treemapify)
library(RColorBrewer)
library(tigris)
options(tigris_use_cache = TRUE)


# ============================================================
# Statistics
# ============================================================

PED_Clean <- read_csv("PED_Clean.csv")
df <- PED_Clean

cat("Кількість рядків:", nrow(df), "\n")
cat("Кількість змінних:", ncol(df), "\n")

missing <- sapply(df, function(x) sum(is.na(x)))
missing <- sort(missing[missing > 0], decreasing = TRUE)

if (length(missing) > 0) {
  kable(as.data.frame(missing), col.names = c("Кількість пропусків"), caption = "Пропущені значення по змінних")
} else {
  cat("Пропущених значень не виявлено.")
}

numeric_vars <- df %>% select(where(is.numeric))
summary_stats <- summary(numeric_vars)
kable(as.data.frame(summary_stats), caption = "Cтатистика числових змінних")

iqr_hampel_outliers <- function(x, k = 1.5) {
  m   <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  sum(x < (m - k * iqr) | x > (m + k * iqr), na.rm = TRUE)
}

outliers <- sapply(numeric_vars, iqr_hampel_outliers)
kable(
  as.data.frame(outliers),
  caption = "Кількість потенційних викидів за правилом [M ± 1.5·IQR]"
)


# ============================================================
# QUESTION 1
# ============================================================


data <- PED_Clean %>%
  mutate(
    BORO = factor(BORO,
                  levels = 1:5,
                  labels = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")),
    PERIOD = factor(PERIOD,
                    levels = c(0, 1, 3),
                    labels = c("Prior", "Tentative", "Final")),
    CUREXMPTOT = as.numeric(CUREXMPTOT)
  )

exempt_sums <- data %>%
  filter(PERIOD == "Final", startsWith(PSTATUS, "A")) %>%
  group_by(YEAR, BORO) %>%
  summarise(total_exempt = sum(CUREXMPTOT, na.rm = TRUE), .groups = "drop")

print(
  ggplot(exempt_sums, aes(x = YEAR, y = total_exempt, color = BORO, group = BORO)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = comma(total_exempt)),
            vjust = -0.5,
            show.legend = FALSE) +
  scale_x_continuous(breaks = sort(unique(exempt_sums$YEAR))) +
  scale_y_continuous() +
  labs(
    title = "Динаміка сум податкових пільг за районами",
    x = "Рік",
    y = "Сума пільг, $",
    color = "Район (BORO)"
  ) +
  theme_light(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )
)


bldg_descriptions <- c(
  "A" = "A - ONE FAMILY DWELLINGS",
  "B" = "B - TWO FAMILY DWELLINGS",
  "C" = "C - WALK UP APARTMENTS",
  "D" = "D - ELEVATOR APARTMENTS",
  "E" = "E - WAREHOUSES",
  "F" = "F - FACTORIES AND INDUSTRIAL BUILDINGS",
  "G" = "G - GARAGES",
  "H" = "H - HOTELS",
  "I" = "I - HOSPITALS AND HEALTH FACILITIES",
  "J" = "J - THEATRES",
  "K" = "K - STORE BUILDINGS",
  "L" = "L - LOFTS",
  "M" = "M - RELIGIOUS FACILITIES",
  "N" = "N - ASYLUMS AND HOMES",
  "O" = "O - OFFICE BUILDINGS",
  "P" = "P - INDOOR PUBLIC ASSEMBLY & CULT. FACILITIES",
  "Q" = "Q - OUTDOOR RECREATIONAL FACILITIES",
  "R" = "R - CONDOMINIUMS",
  "S" = "S - PRIMARILY RES. - MIXED USE",
  "T" = "T - TRANSPORTATION FACILITIES",
  "U" = "U - UTILITY BUREAU PROPERTIES",
  "V" = "V - VACANT LAND",
  "W" = "W - EDUCATIONAL FACILITIES",
  "Y" = "Y - GOVERNMENT/CITY DEPARTMENTS",
  "Z" = "Z - MISC. BUILDING CLASSIFICATIONS"
)


summary_data <- data %>%
  mutate(BLDG_TYPE = substr(`BLDG-CLASS`, 1, 1)) %>%
  filter(grepl("^[A-Za-z]$", BLDG_TYPE), YEAR == 2025, PERIOD == "Final") %>%
  group_by(BLDG_TYPE) %>%
  summarise(TOTAL_EXEMPTION = sum(CUREXMPTOT, na.rm = TRUE)) %>%
  mutate(
    BLDG_DESC = recode(
      BLDG_TYPE,
      !!!bldg_descriptions,
      .default = "Unknown"
    )
  )

max_ex <- max(summary_data$TOTAL_EXEMPTION, na.rm = TRUE)

plot_bldg_type <- ggplot(summary_data, aes(
  x = TOTAL_EXEMPTION,
  y = reorder(BLDG_TYPE, TOTAL_EXEMPTION),
  fill = BLDG_TYPE
)) +
  geom_col() +
  geom_text(aes(label = scales::comma(TOTAL_EXEMPTION)),
            nudge_x = max_ex * 0.02,
            hjust = 0,
            size = 5) +
  labs(
    title = "Загальна сума пільг за типом нерухомості станом на 2025 рік",
    x = "Сума пільг, $",
    y = "Клас будівлі",
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(margin = margin(r = 5)),
    plot.margin = margin(t = 5, r = 20, b = 5, l = 5)
  )

print(plot_bldg_type)

prop_exempt <- data %>%
  mutate(BLDG_TYPE = substr(`BLDG-CLASS`, 1, 1)) %>%
  filter(
    PERIOD == "Final",
    YEAR == 2025,
    startsWith(PSTATUS, "A"),
    grepl("^[A-Za-z]$", BLDG_TYPE)
  ) %>%
  group_by(PARID, BLDG_TYPE) %>%
  summarise(prop_exempt = sum(CUREXMPTOT, na.rm = TRUE), .groups = "drop")

outliers <- prop_exempt %>% 
  filter(prop_exempt > 1e+9)

hampel_stats_by_type <- prop_exempt %>%
  group_by(BLDG_TYPE) %>%
  summarise(
    med = median(prop_exempt, na.rm = TRUE),
    mad = mad(prop_exempt, constant = 1, na.rm = TRUE),
    cutoff_lo = med - k * mad,
    cutoff_hi = med + k * mad,
    .groups = "drop"
  )

print(hampel_stats_by_type)

cleaned <- prop_exempt %>%
  left_join(hampel_stats_by_type, by = "BLDG_TYPE") %>%
  filter(prop_exempt >= cutoff_lo, prop_exempt <= cutoff_hi)

print(
  ggplot(prop_exempt, aes(x = BLDG_TYPE, y = prop_exempt, color = BLDG_TYPE)) +
  geom_jitter(width = 0.2, alpha = 0.8, size = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "З усіма даними: розподіл сум пільг по ділянках за типом будівлі",
    x = "Тип будівлі",
    y = "Сума пільг на ділянку, $"
  ) +
  theme_light(base_size = 16) +
  theme(
    axis.text.x = element_text(hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
)

print(
  ggplot(cleaned, aes(x = BLDG_TYPE, y = prop_exempt, color = BLDG_TYPE)) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Після використання фільтра Гампеля по типах",
    x = "Тип будівлі",
    y = "Сума пільг на ділянку, $"
  ) +
  theme_light(base_size = 16) +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
)

lot_attrs <- sf::st_set_geometry(lots, NULL) %>%
  mutate(PARID = as.character(BBL)) %>%
  select(PARID, Address, Borough)


outliers_info <- prop_exempt %>%
  filter(prop_exempt > 1e+9) %>%
  left_join(
    data %>% select(PARID, `BLDG-CLASS`, EXNAME, EXMP_CODE),
    by = "PARID"
  ) %>%
  left_join(lot_attrs, by = "PARID") %>%
  group_by(PARID, prop_exempt, Address, Borough) %>%
  summarise(
    BLDG_CLASS = paste(unique(`BLDG-CLASS`), collapse = ", "),
    EXNAME = paste(unique(EXNAME), collapse = ", "),
    EXMP_CODES = paste(unique(EXMP_CODE), collapse = ", "),
    .groups = "drop"
  ) %>%
  select(
    PARID,
    Borough,
    Address,
    BLDG_CLASS,
    EXMP_CODES,
    EXNAME,
    total_exempt = prop_exempt
  )

print(outliers_info)
write_xlsx(outliers_info, path = "outliers_info.xlsx")

lots <- st_read("nyc_mappluto_25v1_1_shp/MapPLUTO.shp")
ped_sum <- PED_Clean %>%
  filter(PERIOD == 3, YEAR == 2025, startsWith(PSTATUS, "A")) %>% 
  group_by(PARID) %>%
  summarise(total_exempt = sum(CUREXMPTOT, na.rm=TRUE), .groups="drop")

lots2 <- lots %>%
  mutate(PARID = as.character(BBL)) %>%
  left_join(ped_sum, by="PARID")

nyc_boro <- tigris::counties(state="36", cb=TRUE, year=2021) %>%
  filter(NAME %in% c("Bronx","Kings","New York","Queens","Richmond")) %>%
  st_transform(st_crs(lots2)) %>%
  mutate(
    BoroName = case_when(
      NAME=="Bronx" ~ "Bronx",
      NAME=="Kings" ~ "Brooklyn",
      NAME=="New York" ~ "Manhattan",
      NAME=="Queens" ~ "Queens",
      NAME=="Richmond" ~ "Staten Island"
    )
  )


lots2 <- lots2 %>%
  mutate(
    fill_val = case_when(
      total_exempt <= 1e9 ~ total_exempt,
      total_exempt >  1e9 ~ 1e9
    )
  )

print(
  ggplot() +
  geom_sf(data = filter(lots2, is.na(total_exempt)), 
          fill = "lightgrey", 
          color =NA) +
  geom_sf(data = filter(lots2, !is.na(total_exempt)), 
          aes(fill=fill_val), 
          color=NA) +
  geom_sf(data=nyc_boro, fill=NA, color="black", size=0.6) +
  geom_sf_text(data=nyc_boro, aes(label=BoroName),
               size=5, fontface="bold", color="darkred") +
  
  scale_fill_gradientn(
    colours = c("darkgrey","green","darkgreen"),
    values = scales::rescale(c(0,    5e8,   1e9)),
    limits = c(0,    1e9),
    breaks = c(0,    5e8,   1e9),
    labels = c("Без пільг", "500 млн", ">= 1 млрд"),
    name = "Сума\nпільг, $"
  ) +
  
  coord_sf(expand=FALSE) +
  theme_void() +
  theme(
    legend.position = c(1.02,0.5),
    legend.justification = c(0,0.5),
    plot.title.position = "plot",
    plot.title = element_text(hjust=0.5)
  ) +
  labs(title="NYC лоти за розміром пільг")
)

# ============================================================
# Question 2
# ============================================================

# Рисунок 4.2.1
# Heatmap що візуалізує рік створення, та рік розгляду

data <- PED_Clean %>%
  mutate(CREATION_DATE = as.Date(`Create date`, format = "%m/%d/%Y")) %>%
  mutate(CREATION_YEAR = as.numeric(format(CREATION_DATE, "%Y")))

data_accepted <- data %>%
  filter(substr(PSTATUS, 1, 1) == "A") %>%
  filter(PERIOD == 3) %>%
  filter(!is.na(YEAR), !is.na(CREATION_YEAR))

heatmap_data <- data_accepted %>%
  group_by(CREATION_YEAR, YEAR) %>%
  summarise(count = n(), .groups = "drop")

print(
  ggplot(heatmap_data, aes(x = CREATION_YEAR, y = factor(YEAR), fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c", name = "Кількість") +
  theme_minimal() +
  labs(
    title = "Залежність року створення, та року схвалення завки",
    x = "Рік створення заявки",
    y = "Рік розгляду заявки"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    panel.grid = element_blank()
  )
)
# ============================================================
# Рисунок 4.2.2
# Heatmap, що візуалізує залежність кількості вперше ухвалених заявок від року подання

data <- PED_Clean %>%
  mutate(CREATION_DATE = as.Date(`Create date`, format = "%m/%d/%Y")) %>%
  mutate(CREATION_YEAR = as.numeric(format(CREATION_DATE, "%Y")))

data_accepted <- data %>%
  filter(substr(PSTATUS, 1, 1) == "A") %>%
  filter(PERIOD == 3) %>%
  filter(!is.na(YEAR), !is.na(CREATION_YEAR))

data_unique <- data_accepted %>%
  group_by(PARID) %>%
  filter(YEAR == min(YEAR)) %>%
  ungroup()

heatmap_data <- data_unique %>%
  group_by(CREATION_YEAR, YEAR) %>%
  summarise(count = n(), .groups = "drop")

print(ggplot(heatmap_data, aes(x = CREATION_YEAR, y = factor(YEAR), fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c", name = "Кількість") +
  theme_minimal() +
  labs(
    title = "Кількість вперше ухвалених заявок залежить від року подання",
    x = "Рік створення заявки",
    y = "Рік розгляду заявки"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    panel.grid = element_blank()
  )
)
# ============================================================
# Рисунок 4.2.3
# Підрахунок кількості заявок по різним статусам
df <- PED_Clean
df %>%
  filter(!is.na(PSTATUS)) %>%
  count(PSTATUS, name = "Count")
# ============================================================
# Рисунок 4.2.4
#Графік з візуалізацією розподілів статусів(створюється html файл)

df <- PED_Clean %>% 
  filter(!is.na(PSTATUS) & PSTATUS != "")

df <- df %>%
  mutate(StatusGroup = case_when(
    startsWith(PSTATUS, "A") ~ "Approved",
    startsWith(PSTATUS, "P") ~ "Pending",
    TRUE ~ "Denied"
  ))

status_counts <- df %>%
  group_by(StatusGroup, PSTATUS) %>%
  summarise(Count = n(), .groups = "drop")

status_counts <- status_counts %>%
  mutate(pathString = paste("All_Statuses", StatusGroup, PSTATUS, sep = "/"))

tree <- as.Node(status_counts)

tree_list <- ToListExplicit(tree, unname = TRUE)

p <- circlepackeR(tree_list, size = "Count")

saveWidget(p, "circle.html", selfcontained = TRUE)
# ============================================================
# Рисунок 4.2.5
#Стовбчикова діаграмма кількості статусів пільг по роках

data <- PED_Clean
df <- data %>% filter(PERIOD == 3)

data <- df %>% 
  mutate(BLDG_TYPE = substr(`BLDG-CLASS`, 1, 1)) %>%
  mutate(PSTATUS_TYPE = substr(`PSTATUS`, 1, 1)) %>%
  filter(!is.na(PSTATUS_TYPE)) %>%
  mutate(PSTATUS_LABEL = case_when(
    PSTATUS_TYPE == "A" ~ "Approved",
    PSTATUS_TYPE == "P" ~ "Pending",
    TRUE ~ "Denied"
  ))

data_count_year <- data %>%
  group_by(YEAR, PSTATUS_LABEL) %>%
  summarise(count = n(), .groups = "drop")

print(ggplot(data_count_year, aes(x = factor(YEAR), y = count, fill = PSTATUS_LABEL)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c(
    "Approved" = "#a1d99b",  # зелений
    "Denied" = "#fc9272",    # червоний
    "Pending" = "#ADD8E6"    # світло-синій
  )) +
  theme_minimal() +
  labs(title = "Кількість статусів пільг по роках",
       x = "Рік",
       y = "Кількість",
       fill = "Статус пільги") +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 40)
  )
)
# ============================================================
# Рисунок 4.2.6
#Графік що показує які статуси набули заявки, які в першому періоді були на розгляді

data <- PED_Clean

pending_period1 <- data %>%
  filter(PERIOD == 1) %>%
  mutate(PSTATUS_TYPE = substr(PSTATUS, 1, 1)) %>%
  filter(PSTATUS_TYPE == "P") %>%
  select(PARID, YEAR)

status_period3 <- data %>%
  filter(PERIOD == 3, PARID %in% pending_period1$PARID) %>%
  mutate(PSTATUS_TYPE = substr(PSTATUS, 1, 1),
         PSTATUS_LABEL = case_when(
           PSTATUS_TYPE == "A" ~ "Approved",
           PSTATUS_TYPE == "P" ~ "Pending",
           TRUE ~ "Denied"
         ))

status_period3 <- status_period3 %>%
  left_join(pending_period1, by = "PARID", suffix = c("", "_Period1"))

status_summary_year <- status_period3 %>%
  group_by(YEAR_Period1, PSTATUS_LABEL) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(YEAR_Period1) %>%
  mutate(percent = count / sum(count) * 100)

print(
  ggplot(status_summary_year, aes(x = factor(YEAR_Period1), y = percent, fill = PSTATUS_LABEL)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "black", fontface = "bold") +
  scale_fill_manual(values = c(
    "Approved" = "#a1d99b", # світло-зелений
    "Denied" = "#fc9272", # світло-червоний
    "Pending" = "#ADD8E6" # світло-блакитний
  )) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  labs(title = "Статуси заявок (які були Pending у Period 1) по роках",
       x = "Рік (заявки в Period 1)",
       y = "Відсоток",
       fill = "Новий статус у Period 3") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
)

# ============================================================
# QUESTION 3
# ============================================================

df <- PED_Clean

df <- df %>%
  mutate(across(1, as.character))
df <- df %>%
  filter(PERIOD == 3)

multi_exemptions <- df %>%
  group_by(PARID, YEAR, BORO) %>%
  summarise(n_exemptions = n_distinct(EXMP_CODE), .groups = "drop") %>%
  filter(n_exemptions > 1)

plot_data <- multi_exemptions %>%
  group_by(YEAR, BORO) %>%
  summarise(count = n(), .groups = "drop")

plot_data <- plot_data %>%
  mutate(BORO = recode_factor(
    factor(BORO),
    `1` = "Манхеттен",
    `2` = "Бронкс",
    `3` = "Бруклін",
    `4` = "Квінс",
    `5` = "Стейтен-Айленд"
  ))

p <- ggplot(plot_data, aes(x = factor(YEAR), y = count, color = factor(BORO), group = BORO)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = count), vjust = -0.8, size = 3.5) + 
  labs(title = "Динаміка кількості об'єктів з кількома пільгами",
       x = "Рік", y = "Кількість об'єктів",
       color = "Район (BORO)") +
  theme_minimal(base_size = 13) +
  theme(aspect.ratio = 0.6, plot.margin = margin(5, 5, 5, 5, unit = "pt"))

ggsave("exemptions_plot.jpg", plot = p, width = 10, height = 4, dpi = 300)


multi_exemptions <- df %>%
  group_by(PARID, YEAR, BORO) %>%
  summarise(n_exemptions = n_distinct(EXMP_CODE), .groups = "drop") %>%
  filter(n_exemptions > 1)

average_exemptions <- multi_exemptions %>%
  summarise(середнє = mean(n_exemptions))

print(average_exemptions)

combinations_by_year <- df %>%
  filter(PERIOD == 3) %>%
  group_by(PARID, YEAR, BORO) %>%
  summarise(combination = paste(sort(unique(EXMP_CODE)), collapse = ", "), .groups = "drop") %>%
  filter(str_detect(combination, ","))  

top_combinations_by_year <- combinations_by_year %>%
  group_by(YEAR, combination) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(YEAR, desc(count))

years <- sort(unique(top_combinations_by_year$YEAR))

for (year in years) {
  cat(paste0("\nТоп-5 комбінацій для ", year, " року:\n"))
  top_combinations_by_year %>%
    filter(YEAR == year) %>%
    slice_max(count, n = 5) %>%
    select(combination, count) %>%
    print()
}


# ============================================================
# QUESTION 4
# ============================================================


df <- PED_Clean

df <- df %>%
  rename(Exemption_Classification_Codes = EXMP_CODE, 
         Status = STATUS, 
         Building_Class = 'BLDG-CLASS') %>%
  filter(PERIOD == 3) %>%
  filter(Status %in% c("A", "AC", "AI", "AK", "AM", "AR", "AS", "AT")) %>%
  mutate(across(Building_Class, as.character),
         Exemption_Classification_Codes = as.factor(Exemption_Classification_Codes),
         Status = as.factor(Status),
         Building_Class_Grouped = str_sub(Building_Class, 1, 1),
         Building_Class = as.factor(Building_Class),
         Building_Class_Grouped = as.factor(Building_Class_Grouped))      


df_counts <- df %>%
  group_by(Building_Class_Grouped) %>%
  summarize(Count = n())

df_counts <- df_counts %>%
  arrange(Count) %>%
  mutate(Building_Class_Grouped = fct_reorder(Building_Class_Grouped, Count, .desc = FALSE)) %>% # Reorder factor levels 
  mutate(BLDG_DESC = recode(Building_Class_Grouped, !!!bldg_descriptions, .default="Unknown"))

df_with_exemption_codes <- df %>% 
  select(Building_Class_Grouped, Exemption_Classification_Codes) %>%
  mutate(Grouped_Exemption_Classification_Codes = case_when(
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1010 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1019 ~ "Vulnerable",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1021 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1200 ~ "Religion",
    as.numeric(as.character(Exemption_Classification_Codes)) == 1301 ~ "Vulnerable",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1401 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1404 ~ "Med Centers",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1501 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1504 ~ "Charitable",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1505 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1572 ~ "NFP/Public Associations",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1601 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1604 ~ "University/Education",
    as.numeric(as.character(Exemption_Classification_Codes)) == 1605 ~ "Museum",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1606 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1660 ~ "Arts and Culture",
    as.numeric(as.character(Exemption_Classification_Codes)) == 1700 ~ "Private Cemetery",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1840 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1891 ~ "Socio-Cultural significance",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1901 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1961 ~ "Infrastructure",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1963 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1965 ~ "Green Energy",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1971 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1992 ~ "Assets",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 2120 & as.numeric(as.character(Exemption_Classification_Codes)) <= 2231 ~ "Departments",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 2232 & as.numeric(as.character(Exemption_Classification_Codes)) <= 2234 ~ "Public places",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 2251 & as.numeric(as.character(Exemption_Classification_Codes)) <= 3360 ~ "State and municipal inst.",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 3380 & as.numeric(as.character(Exemption_Classification_Codes)) <= 3800 ~ "On State payroll",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 4500 & as.numeric(as.character(Exemption_Classification_Codes)) <= 4650 ~ "Under Federal Controll",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 5090 & as.numeric(as.character(Exemption_Classification_Codes)) <= 5130 ~ "Federal Authority",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 6110 & as.numeric(as.character(Exemption_Classification_Codes)) <= 6800 ~ "Ports/Power plants",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 7120 & as.numeric(as.character(Exemption_Classification_Codes)) <= 7170 ~ "International organisations",
    TRUE ~ as.character(Exemption_Classification_Codes)
  ),
  Grouped_Exemption_Classification_Codes = as.factor(Grouped_Exemption_Classification_Codes)
  )

main_plot_quantity_bldgclass <- ggplot(df_counts, aes(x = Building_Class_Grouped, y = Count, fill = Count)) +
  geom_col(color = "black") +
  scale_fill_gradient(low = "lightpink", high = "indianred") +
  coord_flip() + 
  geom_text(aes(label = Count), hjust = -0.1) +
  labs(x = "Клас нерухомості", y =  "Кількість об'єктів", title = "Кількість об'єктів відповідного класу нерухомості", fill = "Кількість об'єктів") + 
  theme(axis.text.x = element_text(hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none",
        text = element_text(color = "gray12", family = "serif"))

legend_df <- df_counts %>%
  select(Building_Class_Grouped, BLDG_DESC)

legend_plot <- ggplot(legend_df, aes(x = Building_Class_Grouped, y = 1, color = BLDG_DESC)) +
  geom_point(alpha = 0) + 
  scale_color_discrete(name = "Опис класу нерухомості:") +
  theme(
    legend.position = "right",
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key = element_rect(fill = "lightgrey"),
    legend.key.width = unit(0, "cm"),   
    legend.key.height = unit(0.5, "cm"),
    text = element_text(color = "gray12", family = "serif"))

legend_gradient <- get_legend(main_plot_quantity_bldgclass)
legend_desc <- get_legend(legend_plot)

combined_legend <- plot_grid(legend_desc, ncol = 1, rel_widths = c(2, 1))
final_plot_with_both_legends <- plot_grid(main_plot_quantity_bldgclass, combined_legend, rel_widths = c(2, 1))

plt <- ggplot(df_counts) +
  geom_col(
    aes(
      x = Building_Class_Grouped, 
      y = Count, 
      fill = Count
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .95
  ) +
  geom_text(
    aes(
      x = Building_Class_Grouped, 
      y = Count, 
      label = Count),
    position = position_dodge2(width = 1, preserve = "single"), 
    vjust = -0.5,
    hjust = 0.5,
    size = 3
  ) + 
  scale_y_log10(
    breaks = trans_breaks(log10, function(x) round(10^x)), 
    labels = trans_format(log10, function(x) format(round(10^x),
                                                    scientific = FALSE))
  ) + 
  coord_polar()

plt <- plt +
  annotate(
    "point",
    x = 0,
    y = 0,
    size = 28,
    colour = "white"
  ) + 
  scale_fill_gradientn(
    "Quantity",
    colours = c( "lightpink","indianred")
  ) +
  guides(
    fill = guide_colorsteps(
      barwidth = 35, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
  )

plt <- plt + 
  # Add labels
  labs(
    title = "\nКількість об'єктів відповідно до класу нерухомості",
    # subtitle = paste(
    #   "\nЦя візуалізація відображає", 
    #   "  кількість об'єктів нерухомості для кожного класу.\n",
    #   " Графік подано у вигляді полярних координат,",
    #   " як один з варіантів представлення гістограми.",
    #   sep = "\n"
    # ),
    caption = "\n\nData Visualisation by Valerii Hryhorenko\n"
  ) +
  theme(
    text = element_text(color = "gray12", family = "serif"),
    
    plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "grey45", linewidth = 0.5, linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
    
    legend.position = "none"
  )

final_plt_with_both_legends <- plot_grid(plt, combined_legend, rel_widths = c(2, 1))

contingency_table <- df_with_exemption_codes %>%
  count(Building_Class_Grouped, Grouped_Exemption_Classification_Codes) %>%
  group_by(Building_Class_Grouped) %>%
  arrange(desc(n), .by_group = TRUE)

building_class_counts <- contingency_table %>%
  group_by(Building_Class_Grouped) %>%
  summarise(total_n = sum(n)) %>%
  arrange(desc(total_n))

total_by_building_class <- contingency_table %>%
  group_by(Building_Class_Grouped) %>%
  summarise(Total = sum(n))

contingency_table_with_percentage <- contingency_table %>%
  left_join(total_by_building_class, by = "Building_Class_Grouped") %>%
  mutate(Percentage = round((n / Total) * 100, 2))

building_class_order <- building_class_counts$Building_Class_Grouped

main_plt_heatmap <- ggplot(contingency_table_with_percentage, aes(
  x = factor(Building_Class_Grouped, levels = building_class_order), 
  y = Grouped_Exemption_Classification_Codes, 
  fill = Percentage)
) +
  geom_tile(
    colour = "white", 
    linewidth = 3
  ) +
  geom_text(aes(
    label = paste0(Percentage, "%")
    # label = n
  ), 
  vjust = 0.5, 
  hjust = 0.5,
  size = 4
  ) +
  scale_fill_gradientn(colours = c("lightpink", "orange", "coral", "tomato", "sienna", "orangered", "indianred", "firebrick", "brown")) + 
  labs(
    title = "\nТеплова карта розподілу категорій податкових пільг за класами будівель",
    # subtitle = paste(
    #   "\nЦя візуалізація відображає кількість", 
    #   " об'єктів кожного класу будівлі,", 
    #   "які мають відповідну податкову пільгу.\n",
    #   " На графіку по осі X дані впорядковані за спаданням",
    #   "кількості об'єктів відповідного класу.",
    #   sep = "\n"
    # ),
    caption = "\n\nData Visualisation by Valerii Hryhorenko\n",
    fill = "Кількість об'єктів",
    x = "Клас будівлі",
    y = "Категорія податкової пільги"
  ) +
  theme(
    text = element_text(color = "gray12", family = "aria"),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.025),
    plot.caption = element_text(size = 10, hjust = .5),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    legend.position = "none",
  )

treemap_plot <- ggplot(contingency_table_with_percentage,
                       aes(area = n, 
                           fill = Grouped_Exemption_Classification_Codes, 
                           label = paste0(Grouped_Exemption_Classification_Codes, " (", Percentage, "%)"),
                           subgroup = Building_Class_Grouped)) + 
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 2) + 
  geom_treemap_subgroup_text(place = "topleft", size = 20, colour = "black", family = "serif", fontface = "bold", hjust = 0.1, vjust = 0.1, alpha = 0.7) + 
  geom_treemap_text(colour = "black", size = 20, place = "centre", alpha = 0.8, fontface = "bold") + 
  scale_fill_manual(name = "Категорія пільги", values = c(
    "lightpink",
    "orange",
    "coral",
    "tomato",
    "sienna",
    "orangered",
    "indianred",
    "firebrick",
    "brown",
    "chocolate",
    "tan",
    "salmon",
    "darkorange",
    "lightcoral",
    "red",
    "maroon",
    "rosybrown",
    "sandybrown",
    "peachpuff",
    "bisque",
    "burlywood"
  )
  ) + 
  labs(title = "Розподіл категорій податкови пільг за класами будівель ") +
  theme(
    text = element_text(color = "gray12", family = "aria"),
    axis.title = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.025),
    plot.caption = element_text(size = 10, hjust = .5),
    legend.position = "none",
  )

print(final_plot_with_both_legends)
print(main_plt_heatmap)
print(treemap_plot)
print(final_plt_with_both_legends)


# ============================================================
# QUESTION 5
# ============================================================

df <- PED_Clean

df_filtered <- df %>%
  filter(
    FINEXMPTOT != 0 & TENEXMPTOT != 0,
    STATUS %in% c("A", "AC", "AI", "AR", "AM", "AS", "AT"),
    PERIOD == 3
  ) %>%
  mutate(
    percent_diff = round((FINEXMPTOT - TENEXMPTOT) / FINEXMPTOT * 100),
    percent_bin = round(percent_diff / 5) * 5
  )

outliers <- df_filtered %>%
  filter(percent_diff < -100)

df_remaining <- df_filtered %>%
  filter(percent_diff >= -100)
#-------------------------------------------------

line_data <- outliers %>%
  group_by(EXMP_CODE) %>%
  summarise(
    percent_diffs = list(percent_diff),
    count = n()
  ) %>%
  unnest(cols = c(percent_diffs)) %>%
  mutate(
    distance_to_zero = abs(percent_diffs)
  ) %>%
  group_by(EXMP_CODE) %>%
  mutate(
    closest_to_zero = ifelse(distance_to_zero == min(distance_to_zero), count, NA)
  ) %>%
  arrange(desc(count))

ggplot(line_data, aes(x = factor(EXMP_CODE), y = percent_diffs, group = EXMP_CODE)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.2, color = "orange") +
  geom_text(aes(label = ifelse(!is.na(closest_to_zero), closest_to_zero, "")), vjust = -1, size = 3, color = "black") + 
  labs(title = "Групування об'єктів по EXMP_CODE",
       x = "Пільгові коди",
       y = "Відсоткові зміни") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))
#-------------------------------------------------
heatmap_data <- df_remaining %>%
  count(YEAR = factor(YEAR), percent_bin)

ggplot(heatmap_data, aes(x = percent_bin, y = YEAR, fill = log10(n))) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("blue", "lightblue", "yellow", "orange", "red"),
    values = scales::rescale(log10(c(1, 50, 500, 5000, 210000))),
    limits = c(0, 5.4),
    name = "Кількість",
    breaks = log10(c(1, 50, 500, 5000, 210000)),  # Позиції для підписів
    labels = c(1, 50, 500, 5000,210000)
  ) +
  labs(
    title = "Відсоткова різниця між TENEXMPTOT та FINEXMPTOT",
    x = "Відсоткова різниця (округлена до 5%)",
    y = "Рік"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

