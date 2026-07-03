setwd("C:/Users/Omen/Desktop/R studio")
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
df <- read_excel("harbidata.xlsx")
# Tarihi Date yap
df$date <- as.Date(df$date, format = "%d.%m.%Y")

# SEASON’ı sıralı factor yap
df$SEASON <- factor(df$SEASON,
                    levels = c("Winter", "Spring", "Summer", "Autumn"))
str(df)
### =======================================================
### 1) SIMPLE REGRESSION — HER POLLUTANT İÇİN
### pollutant ~ wind
### =======================================================
pollutants <- c("NO2", "O3", "PM2_5", "PM10")
for (p in pollutants) {
  
  form <- as.formula(paste(p, "~ wind"))
  fm <- lm(form, data = df)
  
  cat("\n\n==============================\n")
  cat("SIMPLE REGRESSION for", p, "\n")
  print(summary(fm))
  # Plot
  print(
    ggplot(df, aes(x = wind, y = .data[[p]])) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE,
                  linetype = "dashed", color = "red") +
      labs(
        title = paste("SIMPLE REGRESSION:", p, "vs Wind"),
        x = "Wind Speed",
        y = p
      ) +
      theme_minimal()
  )
}
### =======================================================
### 2) MULTIPLE REGRESSION FOR ALL POLLUTANTS
### pollutant ~ wind + temperature + precipitation
### + Influence Strength Bar Chart
### =======================================================

weather_terms <- c("temperature", "wind", "precipitation")
multi_coef_list <- list()

for (p in pollutants) {
  
  form_multi <- as.formula(
    paste(p, "~", paste(weather_terms, collapse = " + "))
  )
  
  fm <- lm(form_multi, data = df)
  
  cat("\n\n==============================\n")
  cat("MULTIPLE REGRESSION for", p, "\n")
  print(summary(fm))
  
  multi_coef_list[[p]] <- coef(fm)[weather_terms]
}

# Influence chart — HATASIZ VERSİYON

# 1) list → matrix/data.frame
influence_df <- do.call(rbind, multi_coef_list)
influence_df <- as.data.frame(influence_df)

# 2) pollutant isimlerini satır isimlerinden al
influence_df$pollutant <- rownames(influence_df)

# 3) Sütun sırasını düzenle (pollutant en başa)
influence_df <- influence_df[, c("pollutant", weather_terms)]

# 4) long formata çevir
library(tidyr)

influence_long <- pivot_longer(
  influence_df,
  cols = all_of(weather_terms),
  names_to = "term",
  values_to = "beta"
)

# 5) |beta| kolonu
influence_long$abs_beta <- abs(influence_long$beta)

# 6) Grafik
ggplot(influence_long,
       aes(x = pollutant, y = abs_beta, fill = term)) +
  geom_col(position = "dodge") +
  labs(
    title = "FINAL RESULT: Influence Strength of Weather Factors on Each Pollutant",
    x = "Pollutant",
    y = "Absolute Regression Coefficient"
  ) +
  theme_minimal()


### =======================================================
### 3) POLYNOMIAL (ARTIFICIAL) REGRESSION
### Quadratic terms + interactions
### =======================================================

poly_models <- list()

for (p in pollutants) {
  
  form_poly <- as.formula(paste(
    p, "~ temperature + wind + precipitation + humidity +
        I(temperature^2) + I(wind^2) + I(humidity^2) +
        temperature:humidity +
        temperature:wind +
        wind:precipitation +
        wind:humidity"
  ))
  
  fm_poly <- lm(form_poly, data = df)
  poly_models[[p]] <- fm_poly
  
  cat("\n\n==============================\n")
  cat("POLYNOMIAL (ARTIFICIAL) MODEL for", p, "\n")
  print(summary(fm_poly))
}


### =======================================================
### 4 & 5) TÜM POLLUTANT’LAR İÇİN
### 3 MODELİN OLS TABLOSU + FINAL MODEL COMPARISON GRAFİĞİ
### SIMPLE, MULTIPLE, POLYNOMIAL
### =======================================================

pollutants <- c("NO2", "O3", "PM2_5", "PM10")

for (p in pollutants) {
  
  cat("\n\n========================================\n")
  cat(">>> MODELS FOR", p, "\n")
  cat("========================================\n")
  
  # 1) Simple model: pollutant ~ wind
  fm_simple <- lm(as.formula(paste(p, "~ wind")), data = df)
  
  # 2) Multiple model: pollutant ~ temperature + wind + precipitation
  fm_multiple <- lm(
    as.formula(paste(p, "~ temperature + wind + precipitation")),
    data = df
  )
  
  # 3) Polynomial (artificial) model
  fm_poly <- poly_models[[p]]
  
  ## --- OLS ÖZETLERİ (3 tablo) ---
  cat("\n=== ", p, " SIMPLE OLS ===\n", sep = "")
  print(summary(fm_simple))
  
  cat("\n=== ", p, " MULTIPLE OLS ===\n", sep = "")
  print(summary(fm_multiple))
  
  cat("\n=== ", p, " POLYNOMIAL OLS ===\n", sep = "")
  print(summary(fm_poly))
}


### =======================================================
### 5) FINAL MODEL COMPARISON (NO2)
  ## --- FINAL MODEL COMPARISON PLOT (Actual vs Predicted) ---
  pred_simple  <- predict(fm_simple,   df)
  pred_multi   <- predict(fm_multiple, df)
  pred_poly    <- predict(fm_poly,     df)
  
  plot_df <- data.frame(
    index      = seq_len(nrow(df)),
    Actual     = df[[p]],
    Simple     = pred_simple,
    Multiple   = pred_multi,
    Polynomial = pred_poly
  )
  
  plot_df_long <- tidyr::pivot_longer(
    plot_df,
    cols      = c("Actual", "Simple", "Multiple", "Polynomial"),
    names_to  = "Model",
    values_to = "Value"
  )
  
  print(
    ggplot(plot_df_long,
           aes(x = index, y = Value,
               color = Model, linetype = Model)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = paste("FINAL MODEL COMPARISON: Actual vs Predicted (", p, ")", sep = ""),
        x = "Observation Index",
        y = p
      )
  )



### =======================================================
### 6) K-MEANS CLUSTERING
### Tüm pollutant + weather → scale()
### Her pollutant için: pollutant vs wind (cluster colors)
### =======================================================

cluster_vars <- c("NO2", "O3", "PM2_5", "PM10",
                  "temperature", "wind", "precipitation")

clust_scaled <- scale(df[, cluster_vars])

set.seed(123)
km <- kmeans(clust_scaled, centers = 3, nstart = 25)

df$cluster <- factor(km$cluster)

# Her pollutant için pollutant vs wind
for (p in pollutants) {
  print(
    ggplot(df, aes(x = wind, y = .data[[p]], color = cluster)) +
      geom_point(alpha = 0.7) +
      labs(
        title = paste("FINAL CLUSTER RESULT:", p, "vs Wind"),
        x = "Wind",
        y = p,
        color = "Cluster"
      ) +
      theme_minimal()
  )
}
### =======================================================
### 6) SEASON CLUSTERING (Winter / Spring / Summer / Autumn)
### Her pollutant için: pollutant vs wind (renk = SEASON)
### =======================================================

df$SEASON <- factor(df$SEASON,
                    levels = c("Winter", "Spring", "Summer", "Autumn"))

season_cols <- c(
  "Winter" = "#1f77b4",  # mavi
  "Spring" = "#2ca02c",  # yeşil
  "Summer" = "#ff7f0e",  # turuncu
  "Autumn" = "#d62728"   # kırmızı
)

for (p in pollutants) {
  print(
    ggplot(df, aes(x = wind, y = .data[[p]], color = SEASON)) +
      geom_point(alpha = 0.55, size = 1.2) +          
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.4) +  # ← size yerine linewidth
      scale_color_manual(values = season_cols) +
      labs(
        title = paste("FINAL CLUSTER RESULT:", p, "vs Wind Speed"),
        x = "Wind Speed",
        y = p,
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank()
      )
  )
}
### =======================================================
### 7) O3 – DATA-DRIVEN TEMPERATURE-BASED SEASONAL CLUSTERS
### =======================================================

# 0) GERÇEK MEVSİMLERE GÖRE SICAKLIK ORTALAMALARI
season_temp_means <- tapply(df$temperature, df$SEASON, mean, na.rm = TRUE)
season_order <- names(sort(season_temp_means))  # en soğuk → en sıcak

# 1) Sadece temperature ile k-means (4 cluster)
temp_scaled_O3 <- scale(df$temperature)

set.seed(123)
km_O3 <- kmeans(temp_scaled_O3, centers = 4, nstart = 50)

# 2) Clusterların ortalama sıcaklıkları
cluster_means_O3 <- tapply(df$temperature, km_O3$cluster, mean, na.rm = TRUE)
cluster_order_O3 <- names(sort(cluster_means_O3))  # en soğuk → en sıcak

# 3) Coldest cluster → coldest real season, ... , hottest → hottest
#    Yani sırayla eşleştiriyoruz.
map_O3 <- setNames(season_order, cluster_order_O3)

df$O3_SEASON <- factor(
  map_O3[as.character(km_O3$cluster)],
  levels = season_order
)

season_cols <- c(
  "Winter" = "#1f77b4",
  "Spring" = "#2ca02c",
  "Summer" = "#ff7f0e",
  "Autumn" = "#d62728"
)

# 4) O3 vs Temperature (renk = data-driven season)
ggplot(df, aes(x = temperature, y = O3, color = O3_SEASON)) +
  geom_point(alpha = 0.55, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.3) +
  scale_color_manual(values = season_cols) +
  labs(
    title = "DATA-DRIVEN TEMPERATURE-BASED SEASONS: O3 vs Temperature",
    x = "Temperature",
    y = "O3",
    color = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


### =======================================================
### 8) AVG(NO2, PM2_5, PM10) – DATA-DRIVEN TEMP SEASONS
### =======================================================

# 1) Ortalama pollution index
df$AVG_POLLUTION <- rowMeans(df[, c("NO2", "PM2_5", "PM10")], na.rm = TRUE)

# 2) Yine sadece temperature ile k-means
temp_scaled_avg <- scale(df$temperature)

set.seed(123)
km_avg <- kmeans(temp_scaled_avg, centers = 4, nstart = 50)

cluster_means_avg <- tapply(df$temperature, km_avg$cluster, mean, na.rm = TRUE)
cluster_order_avg <- names(sort(cluster_means_avg))  # en soğuk → en sıcak

# 3) Cluster → season mapping (yine coldest→coldest, ... hottest→hottest)
map_avg <- setNames(season_order, cluster_order_avg)

df$AVG_SEASON <- factor(
  map_avg[as.character(km_avg$cluster)],
  levels = season_order
)

# 4) AVG_POLLUTION vs Temperature (renk = data-driven season)
ggplot(df, aes(x = temperature, y = AVG_POLLUTION, color = AVG_SEASON)) +
  geom_point(alpha = 0.55, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.3) +
  scale_color_manual(values = season_cols) +
  labs(
    title = "DATA-DRIVEN TEMPERATURE-BASED SEASONS: AVG(NO2, PM2.5, PM10) vs Temperature",
    x = "Temperature",
    y = "Average Pollution",
    color = "Season"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

