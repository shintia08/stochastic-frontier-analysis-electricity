# install & load package
#install.packages("frontier")
library(frontier)
library("readxl")
data <- read_excel("C:/SKRIPSI/DOKUMEN dan DATA/SFA + CLUSTERING/DATA/Data Baru.xlsx", sheet = 'Efesiensi', skip = 1)
data


num_data <- data[, 3:13]

Y<- as.numeric(data[[11]])
X1<- as.numeric(data[[3]])
X2<- as.numeric(data[[4]])
X3<- as.numeric(data[[5]])
X4<- as.numeric(data[[6]])
X5<- as.numeric(data[[7]])
X6<- as.numeric(data[[8]])
X7 <- as.numeric(data[[9]])
X8 <- as.numeric(data[[10]])
#X9 <- as.numeric(data[[11]])

library(car)
vif(lm(log(Y) ~ log(X1) +log(X2) + log(X3) + log(X4) + log(X5) + log(X6) + log(X8), data=data))

# === MODEL SFA (Cobb-Douglas) Tanpa X3 ===
sfa_model <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X4) + log(X5) + log(X6) + log(X8) ,
  data = data
)
summary(sfa_model)

# 6️⃣ Ambil efisiensi teknis tiap DMU
eff <- efficiencies(sfa_model)  # nilai antara 0-1
eff




# === MODEL SFA (Cobb-Douglas) Tanpa X2 ===
sfa_model_1 <- sfa(
  log(Y) ~ log(X1) + log (X3) + log(X4) + log(X5) + log(X6) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas)  ===
sfa_model_2 <- sfa(
  log(Y) ~ log(X1) + log(X4) + log(X5) +  log(X6) + log(X8) ,
  data = data
)

summary(sfa_model); summary(sfa_model_1); summary(sfa_model_2)

# MODEL TANPA X3
# === MODEL SFA (Cobb-Douglas) Tanpa X1 ===
sfa_model_tx1 <- sfa(
  log(Y) ~ log(X2) + log(X4) + log(X5) + log(X6) + log(X8),
  data = data
)
# === MODEL SFA (Cobb-Douglas) Tanpa X4 ===
sfa_model_tx4 <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X5) + log(X6) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X5 ===
sfa_model_tx5 <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X4) + log(X6) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X6 ===
sfa_model_tx6 <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X4) + log(X5) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X8 ===
sfa_model_tx8 <- sfa(
  log(Y) ~ log(X1)  + log(X4) + log(X5) + log(X6),
  data = data
)

summary(sfa_model_tx1); summary(sfa_model_tx4); summary(sfa_model_tx5); summary(sfa_model_tx6); summary(sfa_model_tx8)


# MODEL TANPA X2
# === MODEL SFA (Cobb-Douglas) Tanpa X1 ===
sfa_model_tx1 <- sfa(
  log(Y) ~ log(X3) + log(X4) + log(X5) + log(X6) + log(X8),
  data = data
)
# === MODEL SFA (Cobb-Douglas) Tanpa X4 ===
sfa_model_tx4 <- sfa(
  log(Y) ~ log(X1) + log(X3) + log(X5) + log(X6) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X5 ===
sfa_model_tx5 <- sfa(
  log(Y) ~ log(X1) + log(X3) + log(X4) + log(X6) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X6 ===
sfa_model_tx6 <- sfa(
  log(Y) ~ log(X1) + log(X3) + log(X4) + log(X5) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X8 ===
sfa_model_tx8 <- sfa(
  log(Y) ~ log(X1) + log(X3) + log(X4) + log(X5) + log(X6),
  data = data
)

summary(sfa_model_tx1); summary(sfa_model_tx4); summary(sfa_model_tx5); summary(sfa_model_tx6); summary(sfa_model_tx8)

# MODEL TANPA X3
# === MODEL SFA (Cobb-Douglas) Tanpa X1 ===
sfa_model_tx1 <- sfa(
  log(Y) ~ log(X2) + log(X4) + log(X5) + log(X6) + log(X8),
  data = data
)
# === MODEL SFA (Cobb-Douglas) Tanpa X4 ===
sfa_model_tx4 <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X5) + log(X6) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X5 ===
sfa_model_tx5 <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X4) + log(X6) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X6 ===
sfa_model_tx6 <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X4) + log(X5) + log(X8),
  data = data
)

# === MODEL SFA (Cobb-Douglas) Tanpa X8 ===
sfa_model_tx8 <- sfa(
  log(Y) ~ log(X1) + log(X2) + log(X4) + log(X5) + log(X6),
  data = data
)

summary(sfa_model_tx1); summary(sfa_model_tx4); summary(sfa_model_tx5); summary(sfa_model_tx6); summary(sfa_model_tx8)

# Model frontier normal Tanpa X3
sfa_hn <- sfa(log(Y) ~ log(X1) + log(X2) + log(X4) + log(X5) + log(X6) + log(X8),
              data = data,
              ineffDecrease = TRUE,
              truncNorm = FALSE)

# Model frontier normal Tanpa X2
sfa_hn_1 <- sfa(log(Y) ~ log(X1) + log(X3) + log(X4) + log(X5) + log(X6) + log(X8),
              data = data,
              ineffDecrease = TRUE,
              truncNorm = FALSE)

# Model frontier normal Tanpa X2 dan X3 
sfa_hn_2 <- sfa(log(Y) ~ log(X1) + log(X4) + log(X5) + log(X6) + log(X8),
                data = data,
                ineffDecrease = TRUE,
                truncNorm = FALSE)

summary(sfa_hn);summary(sfa_hn_1);summary(sfa_hn_2)

# 6️⃣ Ambil efisiensi teknis tiap DMU
eff <- efficiencies(sfa_model_tx1)  # nilai antara 0-1
eff


# 7️⃣ Tambahkan ke data asli
data$efficiency <- eff







