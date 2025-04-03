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

# 📌 Convertir la date et filtrer pour Wrapped BTC
df_price <- df_price %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")) %>%
  filter(reserve_name == "Wrapped BTC")

df_reserve <- df_reserve %>%
  mutate(regular_datetime = as.POSIXct(regular_datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")) %>%
  filter(reserve_name == "Wrapped BTC")

# 📌 Fusionner les données sur datetime
merged_df <- df_price %>%
  dplyr::select(datetime, inputTokenPriceUSD) %>%
  inner_join(df_reserve %>% rename(datetime = regular_datetime) %>%
               dplyr::select(datetime, liquidityRate, variableBorrowRate, 
                             totalScaledVariableDebt, utilizationRate), by = "datetime")

# 📌 Visualisation du prix 
ggplot(df_eth, aes(x = datetime, y = inputTokenPriceUSD)) +
  geom_line(color = "blue") +
  labs(title = "WBTC Price Evolution (in USD)", x = "Time", y = "Price in USD") +
  theme_minimal()

# 📌 Visualisation du prix
plot_price <- ggplot(merged_df, aes(x = datetime, y = inputTokenPriceUSD)) +
  geom_line(color = "blue") +
  labs(title = "WBTC Price Evolution (in USD)", x = "Time", y = "Price in USD") +
  theme_minimal()

# 📌 Visualisation des variables explicatives
plot_list <- list(
  plot_price,
  ggplot(merged_df, aes(x = datetime, y = liquidityRate)) + 
    geom_line(color = "blue") + labs(title = "Liquidity Rate Evolution", x = "Time", y = "Rate") + theme_minimal(),
  
  ggplot(merged_df, aes(x = datetime, y = variableBorrowRate)) + 
    geom_line(color = "red") + labs(title = "Variable Borrow Rate Evolution", x = "Time", y = "Rate") + theme_minimal(),
  
  ggplot(merged_df, aes(x = datetime, y = totalScaledVariableDebt)) + 
    geom_line(color = "purple") + labs(title = "Total Scaled Variable Debt Evolution", x = "Time", y = "Debt") + theme_minimal(),
  
  ggplot(merged_df, aes(x = datetime, y = utilizationRate)) + 
    geom_line(color = "orange") + labs(title = "Utilization Rate Evolution", x = "Time", y = "Rate") + theme_minimal()
)

grid.arrange(grobs = plot_list, ncol = 2)

# 📌 Calcul des variations en pourcentage
merged_df <- merged_df %>%
  arrange(datetime) %>%
  mutate(
    pct_change_price = (inputTokenPriceUSD - lag(inputTokenPriceUSD)) / inputTokenPriceUSD,
    pct_change_liquidityRate = (liquidityRate - lag(liquidityRate)) / liquidityRate ,
    pct_change_variableBorrowRate = (variableBorrowRate - lag(variableBorrowRate)) / variableBorrowRate,
    pct_change_totalScaledVariableDebt = (totalScaledVariableDebt - lag(totalScaledVariableDebt)) / totalScaledVariableDebt,
    pct_change_utilizationRate = (utilizationRate - lag(utilizationRate)) / utilizationRate
  ) 

# 📌 Supprimer les NA, NaN et Inf après transformation
merged_df_clean <- merged_df %>%
  filter(complete.cases(.)) %>%
  filter_all(all_vars(!is.nan(.))) %>%
  filter_all(all_vars(!is.infinite(.)))

# 📌 Vérifier la taille de l’échantillon après nettoyage
print(nrow(merged_df_clean))

# 📌 Vérifier la stationnarité avec le test ADF
adf_results <- lapply(merged_df_clean[, -1], function(x) adf.test(x, alternative = "stationary")$p.value)
names(adf_results) <- colnames(merged_df_clean)[-1]
print(adf_results)  # Si p-value > 0.05, la série n'est pas stationnaire

# 📌 Différencier les séries non stationnaires
merged_df_clean <- merged_df_clean %>%
  mutate(across(-datetime, ~ . - lag(.), .names = "diff_{.col}")) %>%
  filter(complete.cases(.))

# 📌 Liste des variables explicatives
variables <- c("diff_pct_change_liquidityRate", "diff_pct_change_variableBorrowRate", "diff_pct_change_totalScaledVariableDebt", "diff_pct_change_utilizationRate")

# 📌 Test de Granger avec sélection du lag optimal
results <- list()
for (var in variables) {
  lag_selection <- VARselect(merged_df_clean[, c("diff_pct_change_price", var)], lag.max = 100, type = "const")
  optimal_lag <- lag_selection$selection["SC(n)"]
  print(paste("Lag optimal pour", var, ":", optimal_lag))
  
  # Vérifier si le lag optimal donne assez d'observations
  if (nrow(merged_df_clean) > optimal_lag) {
    test_result <- grangertest(as.formula(paste("diff_pct_change_price ~", var)), order = optimal_lag, data = merged_df_clean)
    results[[var]] <- test_result
  } else {
    print(paste("Pas assez d'observations pour", var))
  }
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