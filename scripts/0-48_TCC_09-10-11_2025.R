# ---------------------------------------------------------------------------------
# 1. ΒΑΣΙΚΕΣ ΡΥΘΜΙΣΕΙΣ (ΩΡΕΣ, ΜΗΝΕΣ ΚΑΙ ΦΑΚΕΛΟΙ)
# ---------------------------------------------------------------------------------

library(here)

# Ορισμός των σχετικών διαδρομών (paths) με βάση το φάκελο 'scripts'
DIR_DATA  <- here("data")
DIR_DOCS  <- here("output", "docs")
DIR_PLOTS <- here("output", "plots")

# Automatic folder creation
if (!dir.exists(DIR_DOCS))  dir.create(DIR_DOCS,  recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_PLOTS)) dir.create(DIR_PLOTS, recursive = TRUE, showWarnings = FALSE)

# Ώρες πρόγνωσης
TARGET_HOURS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48)

# Ορίζουμε τους μήνες και τις ετικέτες τους για τους τίτλους των γραφημάτων
MONTHS <- c("09", "10", "11")
MONTH_LABELS <- c("09" = "Σεπ 2025", "10" = "Οκτ 2025", "11" = "Νοε 2025")

# ΕΝΑΡΞΗ ΤΗΣ ΜΕΓΑΛΗΣ ΕΠΑΝΑΛΗΨΗΣ ΓΙΑ ΚΑΘΕ ΜΗΝΑ
for (mo in MONTHS) {
  
  print("######################################################")
  print(paste("Ξεκινάει η ανάλυση για τον μήνα:", MONTH_LABELS[mo]))
  print("######################################################")
  
  # --- Ανάγνωση αρχείων ανάλογα με τον μήνα (mo) ---
  
  # Στην ανάγνωση των τριών παρακάτω αρχείων, ο κώδικας εντοπίζει 
  # τις στήλες με βάση τον αριθμό της εκάστοτε στήλης
  
  # OBS
  obs_file  <- paste0('TCC_OBS_2025_', mo, '.txt')  
  obs_table_data <- read.table(
    file  = file.path(DIR_DATA, obs_file),
    skip  = 8,
    sep   = "\t", 
    fill  = TRUE, 
    quote = ""
  )
  
  obs_cor_full <- data.frame(
    obs_codes         = trimws(obs_table_data[, 2]),
    obs_dates         = as.Date(trimws(obs_table_data[, 3])), # Μετατροπή σε Date κατευθείαν
    obs_tcc_table_h   = as.numeric(obs_table_data[, 5]),
    obs_tcc_table     = as.numeric(obs_table_data[, 7]),
    obs_station_names = trimws(obs_table_data[, 10])
  )
  
  # ECMWF
  ecmwf_file <- paste0('TCC_ECMWF_2025_', mo, '.txt')
  ecmwf_table_data <- read.table(
    file  = file.path(DIR_DATA, ecmwf_file), 
    skip  = 9, 
    sep   = "\t", 
    fill  = TRUE, 
    quote = ""
  )
  
  ecmwf_cor_full <- data.frame(
    ecmwf_codes         = trimws(ecmwf_table_data[, 3]),
    ecmwf_dates         = as.Date(trimws(ecmwf_table_data[, 5])),
    ecmwf_tcc_table_h   = as.numeric(ecmwf_table_data[, 6]),
    ecmwf_tcc_table     = as.numeric(ecmwf_table_data[, 8]),
    ecmwf_station_names = trimws(ecmwf_table_data[, 11])
  )
  
  # ICON
  icon_file <- paste0('TCC_ICON_2025_', mo, '.txt')
  icon_table_data <- read.table(
    file  = file.path(DIR_DATA, icon_file), 
    skip  = 9, 
    sep   = "\t", 
    fill  = TRUE, 
    quote = ""
  )
  
  icon_cor_full <- data.frame(
    icon_codes         = trimws(icon_table_data[, 3]),
    icon_dates         = as.Date(trimws(icon_table_data[, 5])),
    icon_tcc_table_h   = as.numeric(icon_table_data[, 6]),
    icon_tcc_table     = as.numeric(icon_table_data[, 8]),
    icon_station_names = trimws(icon_table_data[, 11])
  )
  
  # ---------------------------------------------------------------------------------
  # 2. Η ΕΣΩΤΕΡΙΚΗ ΕΠΑΝΑΛΗΨΗ ΓΙΑ ΤΙΣ ΩΡΕΣ (0-48)
  # ---------------------------------------------------------------------------------
  
  # Μηδενισμός των πινάκων για τον νέο μήνα
  metrics_ecmwf <- data.frame(Hour = integer(), RMSE = numeric(), ME = numeric())
  metrics_icon  <- data.frame(Hour = integer(), RMSE = numeric(), ME = numeric())
  
  for (i in TARGET_HOURS) {
    # Πραγματική ώρα παρατήρησης και ημέρες μπροστά
    obs_h <- i %% 24
    days_ahead <- i %/% 24
    
    # Φιλτράρισμα παρατηρήσεων
    obs_cor <- obs_cor_full[obs_cor_full$obs_tcc_table_h == obs_h, ]
    
    # ---------- Υπολογισμοί για ECMWF ----------
    ecmwf_cor <- ecmwf_cor_full[ecmwf_cor_full$ecmwf_tcc_table_h == i, ]
    ecmwf_cor$valid_date <- ecmwf_cor$ecmwf_dates + days_ahead
    
    final_ecmwf <- merge(x = obs_cor, y = ecmwf_cor, 
                         by.x = c("obs_codes", "obs_dates", "obs_station_names"), 
                         by.y = c("ecmwf_codes", "valid_date", "ecmwf_station_names"))
    
    final_ecmwf[final_ecmwf == -999 | final_ecmwf == -99.9] <- NA
    final_ecmwf <- na.omit(final_ecmwf)
    
    if (nrow(final_ecmwf) > 0) {
      rmse_ecmwf <- sqrt(mean((final_ecmwf$ecmwf_tcc_table - final_ecmwf$obs_tcc_table)^2, na.rm = TRUE))
      me_ecmwf   <- mean(final_ecmwf$ecmwf_tcc_table - final_ecmwf$obs_tcc_table, na.rm = TRUE)
      
      metrics_ecmwf <- rbind(metrics_ecmwf, data.frame(Hour = i, RMSE = rmse_ecmwf, ME = me_ecmwf))
      
      # Αποθήκευση αρχείου με το όνομα του μήνα στον φάκελο docs
      doc_filename_ecmwf <- paste0("apotelesma_sygkrisis_tcc_", mo, "_ecmwf_", i, ".txt")
      write.table(final_ecmwf, 
                  file = file.path(DIR_DOCS, doc_filename_ecmwf), 
                  sep = "\t", row.names = FALSE, quote = FALSE)
    }
    
    # ---------- Υπολογισμοί για ICON ----------
    icon_cor <- icon_cor_full[icon_cor_full$icon_tcc_table_h == i, ]
    icon_cor$valid_date <- icon_cor$icon_dates + days_ahead
    
    final_icon <- merge(x = obs_cor, y = icon_cor, 
                        by.x = c("obs_codes", "obs_dates", "obs_station_names"), 
                        by.y = c("icon_codes", "valid_date", "icon_station_names"))
    
    final_icon[final_icon == -999 | final_icon == -99.9] <- NA
    final_icon <- na.omit(final_icon)
    
    if (nrow(final_icon) > 0) {
      rmse_icon <- sqrt(mean((final_icon$icon_tcc_table - final_icon$obs_tcc_table)^2, na.rm = TRUE))
      me_icon   <- mean(final_icon$icon_tcc_table - final_icon$obs_tcc_table, na.rm = TRUE)
      
      metrics_icon <- rbind(metrics_icon, data.frame(Hour = i, RMSE = rmse_icon, ME = me_icon))
      
      # Αποθήκευση αρχείου με το όνομα του μήνα στον φάκελο docs
      doc_filename_icon <- paste0("apotelesma_sygkrisis_tcc_", mo, "_icon_", i, ".txt")
      write.table(final_icon, 
                  file = file.path(DIR_DOCS, doc_filename_icon), 
                  sep = "\t", row.names = FALSE, quote = FALSE)
    }
  }
  
  # ---------------------------------------------------------------------------------
  # 3. ΔΗΜΙΟΥΡΓΙΑ ΚΑΙ ΑΠΟΘΗΚΕΥΣΗ ΓΡΑΦΗΜΑΤΟΣ (ΓΙΑ ΤΟΝ ΣΥΓΚΕΚΡΙΜΕΝΟ ΜΗΝΑ)
  # ---------------------------------------------------------------------------------
  
  # Το όνομα της εικόνας αλλάζει αυτόματα και σώζεται στον φάκελο plots
  plot_filename <- paste0("plot_icon_ecmwf_tcc_", mo, "_0_48.png")
  png(filename = file.path(DIR_PLOTS, plot_filename), width = 1200, height = 900, res = 100)
  
  par(mfrow = c(2, 1))
  
  ylim_rmse <- range(c(metrics_ecmwf$RMSE, metrics_icon$RMSE), na.rm = TRUE)
  ylim_me   <- range(c(metrics_ecmwf$ME, metrics_icon$ME), na.rm = TRUE)
  
  # --- ΠΑΝΩ ΣΕΙΡΑ: Γράφημα RMSE ---
  plot_title_rmse <- paste0("Εξέλιξη RMSE Νεφοκάλυψης (Total Cloud Cover) (0h - 48h) \n(IFS ECMWF - ICON-GR, ", MONTH_LABELS[mo], ")")
  
  plot(metrics_ecmwf$Hour, metrics_ecmwf$RMSE, type = "b", col = "red", pch = 16,
       xlab = "Ώρα Πρόγνωσης (Lead Time)", ylab = "RMSE (%)", 
       main = plot_title_rmse,
       xaxt = "n", ylim = c(ylim_rmse[1]*0.9, ylim_rmse[2]*1.15)) 
  
  lines(metrics_icon$Hour, metrics_icon$RMSE, type = "b", col = "blue", pch = 17)
  axis(1, at = TARGET_HOURS)  
  
  legend("topleft", legend = c("IFS ECMWF", "ICON-GR"), col = c("red", "blue"), 
         pch = c(16, 17), lty = 1, bg = "white", cex = 0.75)
  
  # --- ΚΑΤΩ ΣΕΙΡΑ: Γράφημα Mean Error (ME) ---
  plot_title_me <- paste0("Εξέλιξη Mean Error Νεφοκάλυψης (Total Cloud Cover) (0h - 48h) \n(IFS ECMWF - ICON-GR, ", MONTH_LABELS[mo], ")")
  
  plot(metrics_ecmwf$Hour, metrics_ecmwf$ME, type = "b", col = "red", pch = 16,
       xlab = "Ώρα Πρόγνωσης (Lead Time)", ylab = "Mean Error (%)", 
       main = plot_title_me,
       xaxt = "n", ylim = c(ylim_me[1]-2, ylim_me[2]+2))
  
  lines(metrics_icon$Hour, metrics_icon$ME, type = "b", col = "blue", pch = 17)
  axis(1, at = TARGET_HOURS)
  abline(h = 0, col = "gray", lty = 2, lwd = 2) 
  
  legend("topleft", legend = c("IFS ECMWF", "ICON-GR"), col = c("red", "blue"), 
         pch = c(16, 17), lty = 1, bg = "white", cex = 0.75)
  
  dev.off()
  par(mfrow = c(1, 1))
}