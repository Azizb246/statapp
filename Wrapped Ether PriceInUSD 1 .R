# 📌 Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(gridExtra)
library(tseries)   # Pour le test de stationnarité ADF
library(lmtest)    # Pour le test de Granger
library(vars)      # Pour sélectionner le lag optimal

# 📌 Définir le chemin des fichiers
folder_path <- "C:/Users/Aziz/Downloads/"

# 📌 Charger et fusionner les fichiers de prix
price_files <- paste0("hourly_prices_", 
                      rep(2023:2024, each=12), "_", 
                      rep(1:12, 2), ".csv")

df_price <- do.call(rbind, lapply(price_files, function(file) {
  read.csv(paste0(folder_path, file), stringsAsFactors = FALSE)
}))

# 📌 Charger et fusionner les fichiers des variables explicatives
reserve_files <- paste0("reserves_history_hourly_completed_", 
                        rep(2023:2024, each=12), "-", 
                        rep(1:12, 2), ".csv")

df_reserve <- do.call(rbind, lapply(reserve_files, function(file) {
  read.csv(paste0(folder_path, file), stringsAsFactors = FALSE)
}))

# 📌 Convertir la date et filtrer pour Wrapped Ether
df_price <- df_price %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")) %>%
  filter(reserve_name == "Wrapped Ether")

df_reserve <- df_reserve %>%
  mutate(regular_datetime = as.POSIXct(regular_datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")) %>%
  filter(reserve_name == "Wrapped Ether")

# 📌 Fusionner les données sur datetime
merged_df <- df_price %>%
  dplyr::select(datetime, inputTokenPriceUSD) %>%
  inner_join(df_reserve %>% rename(datetime = regular_datetime) %>%
               dplyr::select(datetime, totalATokenSupply, 
                             totalCurrentVariableDebt, totalLiquidity, availableLiquidity ), by = "datetime")

# 📌 Visualisation du prix 
ggplot(df_eth, aes(x = datetime, y = inputTokenPriceUSD)) +
  geom_line(color = "blue") +
  labs(title = "ETH Price Evolution (in USD)", x = "Time", y = "Price in USD") +
  theme_minimal()

# 📌 Visualisation du prix
plot_price <- ggplot(merged_df, aes(x = datetime, y = inputTokenPriceUSD)) +
  geom_line(color = "blue") +
  labs(title = "ETH Price Evolution (in USD)", x = "Time", y = "Price in USD") +
  theme_minimal()

# 📌 Visualisation des variables explicatives
plot_list <- list(
  plot_price,
  ggplot(merged_df, aes(x = datetime, y = totalATokenSupply)) + 
    geom_line(color = "blue") + labs(title = "totalATokenSupply Evolution", x = "Time", y = "Rate") + theme_minimal(),
  
  ggplot(merged_df, aes(x = datetime, y = totalCurrentVariableDebt)) + 
    geom_line(color = "red") + labs(title = "totalCurrentVariableDebt Evolution", x = "Time", y = "Rate") + theme_minimal(),
  
  ggplot(merged_df, aes(x = datetime, y = totalLiquidity)) + 
    geom_line(color = "green") + labs(title = "totalLiquidity Evolution", x = "Time", y = "Rate") + theme_minimal(),
  
  ggplot(merged_df, aes(x = datetime, y = availableLiquidity)) + 
    geom_line(color = "purple") + labs(title = "availableLiquidity Evolution", x = "Time", y = "Debt") + theme_minimal()
  
)

grid.arrange(grobs = plot_list, ncol = 2)


# 📌 Calcul des variations en pourcentage
merged_df <- merged_df %>%
  arrange(datetime) %>%  # S'assurer que les données sont bien triées
  mutate(
    pct_change_price = (inputTokenPriceUSD - lag(inputTokenPriceUSD)) / inputTokenPriceUSD,
    pct_change_totalATokenSupply = (totalATokenSupply - lag(totalATokenSupply)) / totalATokenSupply,
    pct_change_totalCurrentVariableDebt = (totalCurrentVariableDebt - lag(totalCurrentVariableDebt)) / totalCurrentVariableDebt,
    pct_change_totalLiquidity = (totalLiquidity - lag(totalLiquidity)) / totalLiquidity,
    pct_change_availableLiquidity = (availableLiquidity - lag(availableLiquidity)) / availableLiquidity,
  ) 


# 📌 Supprimer les NA correctement
merged_df_clean <- merged_df %>%
  filter(!is.na(pct_change_price) & !is.na(pct_change_totalATokenSupply) & !is.na(pct_change_totalCurrentVariableDebt) & !is.na(pct_change_totalLiquidity) & !is.na(pct_change_availableLiquidity))

# 📌 Liste des variables explicatives
variables <- c("pct_change_totalATokenSupply", "pct_change_totalCurrentVariableDebt", "pct_change_totalLiquidity", "pct_change_availableLiquidity")

# 📌 Appliquer la sélection du lag optimal et le test de Granger sur chaque variable explicative
results <- list()
for (var in variables) {
  # Sélection du lag optimal avec AIC
  lag_selection <- VARselect(merged_df_clean[, c("pct_change_price", var)], lag.max = 100, type = "const")
  optimal_lag <- lag_selection$selection["SC(n)"]
  print(paste("Lag optimal pour", var, ":", optimal_lag))
  
  # Test de causalité de Granger avec le lag optimal
  test_result <- grangertest(pct_change_price ~ merged_df_clean[[var]], order = optimal_lag, data = merged_df_clean)
  results[[var]] <- test_result
}

# 📌 Afficher les résultats des tests
test_summary <- sapply(results, function(res) res$`Pr(>F)`[2])
names(test_summary) <- variables
print(test_summary)  # Si p-value < 0.05, la variable est significative

# 📌 Explorer l'évolution des p-values pour différents lags
lag_range <- 1:20  # Ajustable selon l'analyse
p_values_df <- data.frame(Lag = integer(), Variable = character(), P_Value = numeric())

for (var in variables) {
  p_values <- sapply(lag_range, function(l) {
    test_res <- grangertest(pct_change_price ~ merged_df_clean[[var]], order = l, data = merged_df_clean)
    return(test_res$`Pr(>F)`[2])  # Extraire la p-value du test
  })
  
  temp_df <- data.frame(Lag = lag_range, Variable = var, P_Value = p_values)
  p_values_df <- rbind(p_values_df, temp_df)
}

# 📌 Visualisation des p-values en fonction du lag
ggplot(p_values_df, aes(x = Lag, y = P_Value, color = Variable)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +  # Seuil de significativité
  labs(title = "Évolution des p-values du test de Granger", x = "Lag", y = "P-Value") +
  theme_minimal()