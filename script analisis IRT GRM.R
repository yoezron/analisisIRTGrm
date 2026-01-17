# =============================================================================
#                    ANALISIS PROPERTI PSIKOMETRIK INSTRUMEN
#              DENGAN PENDEKATAN ITEM RESPONSE THEORY (IRT)
#                        GRADED RESPONSE MODEL (GRM)
# =============================================================================
# Penulis      : Analisis Otomatis
# Tanggal      : Sys.Date()
# Deskripsi    : Script komprehensif untuk analisis properti psikometrik
#                instrumen skala Likert 1-5 dengan pendekatan GRM
# =============================================================================

# -----------------------------------------------------------------------------
# 1. PERSIAPAN ENVIRONMENT DAN INSTALASI PACKAGE
# -----------------------------------------------------------------------------

# Fungsi untuk instalasi package jika belum tersedia
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(paste0("Menginstall package: ", pkg, "\n"))
      install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
    }
  }
}

# Daftar package yang diperlukan
required_packages <- c(
  "mirt",        # IRT analysis (GRM)
  "psych",       # Reliability (omega), descriptive stats
  "lavaan",      # CFA untuk uji unidimensionalitas
  "ggplot2",     # Visualisasi
  "dplyr",       # Manipulasi data
  "tidyr",       # Reshape data
  "gridExtra",   # Arrange multiple plots
  "corrplot",    # Correlation plot
  "knitr",       # Tabel
  "kableExtra",  # Format tabel
  "RColorBrewer",# Palet warna
  "scales",      # Format axis
  "cowplot",     # Plot arrangement
  "reshape2",    # Melt data
  "moments",     # Skewness & kurtosis
  "rmarkdown"   # Generate report
)

# -----------------------------------------------------------------------------
# CONSTANTS DAN THRESHOLDS ANALISIS
# -----------------------------------------------------------------------------

# Thresholds untuk interpretasi
THETA_LIMITS <- c(-4, 4)
THETA_STEP <- 0.1

# Cutoffs untuk evaluasi item
CUTOFF_ITEM_TOTAL_COR <- 0.30
CUTOFF_EFA_LOADING <- 0.40
CUTOFF_CFA_LOADING <- 0.50

# Parameter diskriminasi
DISCRIM_BREAKPOINTS <- c(0.5, 0.9, 1.3, 1.7)
DISCRIM_LABELS <- c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi")

# Section separator
SECTION_SEPARATOR_WIDTH <- 70
SECTION_SEPARATOR_CHAR <- "="

# Precision untuk rounding
PRECISION_CORRELATION <- 3
PRECISION_PERCENTAGE <- 2
PRECISION_PARAMETER <- 3

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# Helper untuk print section headers (digunakan 40+ kali)
print_section_header <- function(title, width = SECTION_SEPARATOR_WIDTH,
                                 char = SECTION_SEPARATOR_CHAR) {
  separator <- paste(rep(char, width), collapse = "")
  cat(separator, "\n", sep = "")
  cat(title, "\n")
  cat(separator, "\n", sep = "")
}

# Helper untuk save ggplot plots (digunakan 38+ kali)
save_ggplot <- function(plot_obj, filename, plot_num = NULL,
                       width = PLOT_WIDTH, height = PLOT_HEIGHT,
                       dpi = PLOT_DPI, scale = 1) {
  full_path <- paste0(PLOT_DIR, "/", filename)
  ggsave(full_path, plot_obj,
         width = width * scale, height = height * scale, dpi = dpi)

  if (!is.null(plot_num)) {
    cat(sprintf("Plot %d tersimpan: %s\n", plot_num, filename))
  } else {
    cat(sprintf("Plot tersimpan: %s\n", filename))
  }
}

# Helper untuk save base R plots dengan print() eksplisit
save_base_plot <- function(filename, plot_expr, plot_num = NULL,
                          width = PLOT_WIDTH, height = PLOT_HEIGHT,
                          dpi = PLOT_DPI, scale = 1) {
  full_path <- paste0(PLOT_DIR, "/", filename)
  png(full_path, width = width * scale, height = height * scale,
      units = "in", res = dpi)
  print(plot_expr)
  dev.off()

  if (!is.null(plot_num)) {
    cat(sprintf("Plot %d tersimpan: %s\n", plot_num, filename))
  } else {
    cat(sprintf("Plot tersimpan: %s\n", filename))
  }
}

# Helper untuk kategorisasi diskriminasi
categorize_discrimination <- function(values) {
  cut(values,
      breaks = c(-Inf, DISCRIM_BREAKPOINTS, Inf),
      labels = DISCRIM_LABELS)
}

# Install packages
print_section_header("PERSIAPAN ENVIRONMENT")
cat("\nMemeriksa dan menginstall package yang diperlukan...\n\n")
install_if_missing(required_packages)

# Load packages
suppressPackageStartupMessages({
  library(mirt)
  library(psych)
  library(lavaan)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gridExtra)
  library(corrplot)
  library(knitr)
  library(kableExtra)
  library(RColorBrewer)
  library(scales)
  library(cowplot)
  library(reshape2)
  library(moments)
  library(rmarkdown)
})

cat("Semua package berhasil dimuat!\n\n")

# -----------------------------------------------------------------------------
# 2. KONFIGURASI DAN PARAMETER
# -----------------------------------------------------------------------------

# Konfigurasi file
DATA_FILE <- "data_skala.csv"
OUTPUT_DIR <- "output_analisis_psikometrik"
PLOT_DIR <- paste0(OUTPUT_DIR, "/plots")

# Buat direktori output
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

# Konfigurasi skala
MIN_SCALE <- 1
MAX_SCALE <- 5
SCALE_LABELS <- c("Sangat Tidak Sesuai", "Tidak Sesuai", "Netral",
                  "Sesuai", "Sangat Sesuai")

# Konfigurasi plot
PLOT_WIDTH <- 10
PLOT_HEIGHT <- 8
PLOT_DPI <- 300

# Theme untuk ggplot
theme_report <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 11, face = "bold")
    )
}

# Palet warna
colors_main <- brewer.pal(8, "Set2")
colors_gradient <- colorRampPalette(c("#3498db", "#e74c3c"))(5)

# Konfigurasi grafis - mencegah plot ke device default
options(device = function(...) png(filename = tempfile(), ...))
pdf(NULL)  # Disable default PDF device

# -----------------------------------------------------------------------------
# 3. IMPORT DAN PERSIAPAN DATA
# -----------------------------------------------------------------------------

print_section_header("IMPORT DAN PERSIAPAN DATA")

# Import data
cat(paste0("\nMembaca data dari: ", DATA_FILE, "\n"))

tryCatch({
  raw_data <- read.csv(DATA_FILE, sep = ";", header = TRUE, stringsAsFactors = FALSE)
  cat(paste0("Data berhasil diimport!\n"))
  cat(paste0("Dimensi data: ", nrow(raw_data), " responden x ", ncol(raw_data), " kolom\n\n"))
}, error = function(e) {
  stop(paste0("Error membaca file: ", e$message,
              "\nPastikan file '", DATA_FILE, "' ada di working directory."))
})

# Ekstrak data item (mulai dari kolom kedua)
item_data <- raw_data[, -1]  # Hapus kolom pertama (identitas)
n_items <- ncol(item_data)
n_respondents <- nrow(item_data)

# Beri nama item jika belum ada
if (is.null(colnames(item_data)) || all(colnames(item_data) == "")) {
  colnames(item_data) <- paste0("Item_", 1:n_items)
}
item_names <- colnames(item_data)

cat(paste0("Jumlah item: ", n_items, "\n"))
cat(paste0("Jumlah responden: ", n_respondents, "\n"))
cat(paste0("Nama item: ", paste(item_names[1:min(5, n_items)], collapse = ", "),
           ifelse(n_items > 5, ", ...", ""), "\n\n"))

# Konversi ke numerik dan cek missing
item_data <- as.data.frame(lapply(item_data, as.numeric))

# Cek missing values
missing_count <- sum(is.na(item_data))
missing_pct <- (missing_count / (n_items * n_respondents)) * 100

cat("Pemeriksaan Missing Values:\n")
cat(paste0("  Total missing: ", missing_count, " (", round(missing_pct, 2), "%)\n"))

if (missing_count > 0) {
  cat("  Menghapus baris dengan missing values...\n")
  item_data <- na.omit(item_data)
  n_respondents <- nrow(item_data)
  cat(paste0("  Jumlah responden setelah penghapusan: ", n_respondents, "\n"))
}

# Validasi rentang skala
out_of_range <- sum(item_data < MIN_SCALE | item_data > MAX_SCALE, na.rm = TRUE)
if (out_of_range > 0) {
  warning(paste0("Terdapat ", out_of_range, " nilai di luar rentang skala (",
                 MIN_SCALE, "-", MAX_SCALE, ")"))
}

cat("\n")

# -----------------------------------------------------------------------------
# 4. STATISTIK DESKRIPTIF
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("STATISTIK DESKRIPTIF\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Hitung statistik deskriptif per item
desc_stats <- data.frame(
  Item = item_names,
  N = apply(item_data, 2, function(x) sum(!is.na(x))),
  Mean = apply(item_data, 2, mean, na.rm = TRUE),
  SD = apply(item_data, 2, sd, na.rm = TRUE),
  Min = apply(item_data, 2, min, na.rm = TRUE),
  Max = apply(item_data, 2, max, na.rm = TRUE),
  Skewness = apply(item_data, 2, skewness, na.rm = TRUE),
  Kurtosis = apply(item_data, 2, kurtosis, na.rm = TRUE)
)

# Distribusi frekuensi per kategori
freq_dist <- sapply(item_data, function(x) {
  tbl <- table(factor(x, levels = MIN_SCALE:MAX_SCALE))
  as.numeric(tbl)
})
freq_dist <- t(freq_dist)
colnames(freq_dist) <- paste0("Kategori_", MIN_SCALE:MAX_SCALE)
freq_df <- cbind(Item = item_names, as.data.frame(freq_dist))

# Proporsi per kategori
prop_dist <- freq_dist / rowSums(freq_dist) * 100
prop_df <- cbind(Item = item_names, as.data.frame(round(prop_dist, 2)))

cat("\nStatistik Deskriptif Item:\n")
print(round(desc_stats[, -1], 3))

# Simpan ke CSV
write.csv(desc_stats, paste0(OUTPUT_DIR, "/01_statistik_deskriptif.csv"), row.names = FALSE)
write.csv(freq_df, paste0(OUTPUT_DIR, "/02_distribusi_frekuensi.csv"), row.names = FALSE)
write.csv(prop_df, paste0(OUTPUT_DIR, "/03_proporsi_kategori.csv"), row.names = FALSE)

# --- PLOT 1: Distribusi Skor Total ---
total_scores <- rowSums(item_data)

p1 <- ggplot(data.frame(Score = total_scores), aes(x = Score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = colors_main[1], color = "white", alpha = 0.8) +
  geom_density(color = colors_main[2], linewidth = 1.2) +
  geom_vline(aes(xintercept = mean(Score)),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribusi Skor Total Instrumen",
    subtitle = paste0("N = ", n_respondents, " | Mean = ", round(mean(total_scores), 2),
                      " | SD = ", round(sd(total_scores), 2)),
    x = "Skor Total",
    y = "Densitas"
  ) +
  theme_report() +
  annotate("text", x = mean(total_scores), y = Inf,
           label = paste0("M = ", round(mean(total_scores), 2)),
           vjust = 2, hjust = -0.1, color = "red")

save_ggplot(p1, "01_distribusi_skor_total.png", plot_num = 1)

# --- PLOT 2: Boxplot Item ---
item_long <- item_data %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Response") %>%
  mutate(Item = factor(Item, levels = item_names))

p2 <- ggplot(item_long, aes(x = Item, y = Response, fill = Item)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkblue") +
  scale_fill_manual(values = rep(colors_main, length.out = n_items)) +
  labs(
    title = "Distribusi Respons per Item",
    subtitle = "Titik biru menunjukkan nilai mean",
    x = "Item",
    y = "Respons"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(breaks = MIN_SCALE:MAX_SCALE)

save_ggplot(p2, "02_boxplot_item.png", plot_num = 2)

# --- PLOT 3: Mean per Item dengan Error Bar ---
p3 <- ggplot(desc_stats, aes(x = reorder(Item, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = colors_main[3], alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = 0.2, color = "gray30") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Nilai Rata-rata Item dengan Standard Deviation",
    subtitle = "Garis merah putus-putus menunjukkan titik tengah skala",
    x = "Item",
    y = "Mean ± SD"
  ) +
  theme_report() +
  scale_y_continuous(limits = c(0, 5.5), breaks = 0:5)

save_ggplot(p3, "03_mean_item_errorbar.png", plot_num = 3)

# --- PLOT 4: Heatmap Distribusi Kategori ---
prop_long <- prop_df %>%
  pivot_longer(cols = -Item, names_to = "Kategori", values_to = "Proporsi") %>%
  mutate(Item = factor(Item, levels = rev(item_names)),
         Kategori = factor(Kategori, levels = paste0("Kategori_", MIN_SCALE:MAX_SCALE)))

p4 <- ggplot(prop_long, aes(x = Kategori, y = Item, fill = Proporsi)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Proporsi)), size = 3) +
  scale_fill_gradient2(low = "#3498db", mid = "#f1c40f", high = "#e74c3c",
                       midpoint = 50, name = "Proporsi (%)") +
  labs(
    title = "Heatmap Distribusi Kategori Respons",
    subtitle = "Proporsi responden per kategori dalam persen",
    x = "Kategori Respons",
    y = "Item"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(PLOT_DIR, "/04_heatmap_distribusi_kategori.png"), p4,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 4 tersimpan: 04_heatmap_distribusi_kategori.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 5. ANALISIS KORELASI INTER-ITEM
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("ANALISIS KORELASI INTER-ITEM\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Hitung matriks korelasi
cor_matrix <- cor(item_data, use = "pairwise.complete.obs")

# Statistik korelasi
cor_values <- cor_matrix[lower.tri(cor_matrix)]
cat(paste0("\nStatistik Korelasi Inter-Item:\n"))
cat(paste0("  Range: ", round(min(cor_values), 3), " - ", round(max(cor_values), 3), "\n"))
cat(paste0("  Mean : ", round(mean(cor_values), 3), "\n"))
cat(paste0("  SD   : ", round(sd(cor_values), 3), "\n"))

# Item-total correlation
item_total_cor <- sapply(1:n_items, function(i) {
  rest_score <- rowSums(item_data[, -i])
  cor(item_data[, i], rest_score)
})
names(item_total_cor) <- item_names

cat(paste0("\nKorelasi Item-Total (corrected):\n"))
print(round(item_total_cor, 3))

# Simpan
write.csv(cor_matrix, paste0(OUTPUT_DIR, "/04_matriks_korelasi.csv"))
cor_summary <- data.frame(
  Item = item_names,
  Item_Total_r = round(item_total_cor, 3)
)
write.csv(cor_summary, paste0(OUTPUT_DIR, "/05_korelasi_item_total.csv"), row.names = FALSE)

# --- PLOT 5: Correlation Matrix ---
png(paste0(PLOT_DIR, "/05_matriks_korelasi.png"),
    width = PLOT_WIDTH, height = PLOT_HEIGHT, units = "in", res = PLOT_DPI)
par(mar = c(1, 1, 2, 1))  # Set proper margins
corrplot(cor_matrix, method = "color", type = "upper",
         order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("#3498db", "white", "#e74c3c"))(200),
         title = "Matriks Korelasi Inter-Item",
         mar = c(0, 0, 2, 0))
dev.off()
cat("\nPlot 5 tersimpan: 05_matriks_korelasi.png\n")

# --- PLOT 6: Item-Total Correlation ---
p6 <- ggplot(cor_summary, aes(x = reorder(Item, Item_Total_r), y = Item_Total_r)) +
  geom_bar(stat = "identity", aes(fill = Item_Total_r > 0.3), width = 0.7) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "red") +
  geom_text(aes(label = sprintf("%.3f", Item_Total_r)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#e74c3c", "TRUE" = colors_main[4]),
                    name = "r > 0.30") +
  labs(
    title = "Korelasi Item-Total (Corrected)",
    subtitle = "Garis merah menunjukkan cut-off 0.30",
    x = "Item",
    y = "Korelasi Item-Total"
  ) +
  theme_report() +
  scale_y_continuous(limits = c(0, max(item_total_cor) + 0.1))

ggsave(paste0(PLOT_DIR, "/06_korelasi_item_total.png"), p6,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 6 tersimpan: 06_korelasi_item_total.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 6. UJI ASUMSI UNIDIMENSIONALITAS
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("UJI ASUMSI UNIDIMENSIONALITAS\n")
cat("=" , rep("=", 70), "\n", sep = "")

# 6.1 Exploratory Factor Analysis (EFA)
cat("\n--- Exploratory Factor Analysis ---\n")

# Parallel analysis untuk menentukan jumlah faktor
# Jalankan dengan plot=FALSE dulu untuk mendapatkan hasil, lalu plot ke file
fa_parallel <- fa.parallel(item_data, fm = "ml", fa = "fa", n.iter = 100,
                           plot = FALSE)  # Disable plotting dulu

# Simpan hasil parallel analysis
parallel_results <- data.frame(
  Factor = 1:length(fa_parallel$fa.values),
  Actual_Eigenvalue = fa_parallel$fa.values,
  Simulated_Eigenvalue = fa_parallel$fa.sim,
  Suggested_Factors = fa_parallel$nfact
)
write.csv(parallel_results, paste0(OUTPUT_DIR, "/06_parallel_analysis.csv"), row.names = FALSE)

cat(paste0("\nJumlah faktor yang disarankan (Parallel Analysis): ", fa_parallel$nfact, "\n"))

# EFA dengan 1 faktor (unidimensional)
efa_result <- fa(item_data, nfactors = 1, fm = "ml", rotate = "none")

cat("\nEFA Unidimensional - Factor Loadings:\n")
print(round(efa_result$loadings[], 3))

cat(paste0("\nVariance Explained: ", round(efa_result$Vaccounted[2, 1] * 100, 2), "%\n"))

# Simpan hasil EFA
efa_summary <- data.frame(
  Item = item_names,
  Factor_Loading = round(as.numeric(efa_result$loadings), 3),
  Communality = round(efa_result$communality, 3),
  Uniqueness = round(efa_result$uniquenesses, 3)
)
write.csv(efa_summary, paste0(OUTPUT_DIR, "/07_efa_loadings.csv"), row.names = FALSE)

# 6.2 Confirmatory Factor Analysis (CFA)
cat("\n--- Confirmatory Factor Analysis ---\n")

# Model CFA unidimensional
cfa_model <- paste0("F1 =~ ", paste(item_names, collapse = " + "))

cfa_fit <- cfa(cfa_model, data = item_data, ordered = TRUE, std.lv = TRUE)

# Fit indices
cfa_fitm <- fitmeasures(cfa_fit, c("chisq", "df", "pvalue", "cfi", "tli",
                                   "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                                   "srmr", "aic", "bic"))

cat("\nCFA Model Fit Indices:\n")
cat(paste0("  Chi-square = ", round(cfa_fitm["chisq"], 3),
           ", df = ", cfa_fitm["df"],
           ", p = ", round(cfa_fitm["pvalue"], 4), "\n"))
cat(paste0("  CFI = ", round(cfa_fitm["cfi"], 3), " (kriteria >= 0.90)\n"))
cat(paste0("  TLI = ", round(cfa_fitm["tli"], 3), " (kriteria >= 0.90)\n"))
cat(paste0("  RMSEA = ", round(cfa_fitm["rmsea"], 3),
           " [", round(cfa_fitm["rmsea.ci.lower"], 3), " - ",
           round(cfa_fitm["rmsea.ci.upper"], 3), "] (kriteria < 0.08)\n"))
cat(paste0("  SRMR = ", round(cfa_fitm["srmr"], 3), " (kriteria < 0.08)\n"))

# Interpretasi
unidim_ok <- cfa_fitm["cfi"] >= 0.90 & cfa_fitm["tli"] >= 0.90 &
  cfa_fitm["rmsea"] < 0.08 & cfa_fitm["srmr"] < 0.08

cat(paste0("\nKesimpulan Unidimensionalitas: ",
           ifelse(unidim_ok, "TERPENUHI", "TIDAK TERPENUHI"), "\n"))

# Standardized loadings dari CFA
cfa_loadings <- standardizedSolution(cfa_fit)
cfa_loadings <- cfa_loadings[cfa_loadings$op == "=~", ]

# Simpan hasil CFA
cfa_summary <- data.frame(
  Item = cfa_loadings$rhs,
  Std_Loading = round(cfa_loadings$est.std, 3),
  SE = round(cfa_loadings$se, 3),
  z = round(cfa_loadings$z, 3),
  p = round(cfa_loadings$pvalue, 4)
)
write.csv(cfa_summary, paste0(OUTPUT_DIR, "/08_cfa_loadings.csv"), row.names = FALSE)

fit_indices <- data.frame(
  Index = c("Chi-square", "df", "p-value", "CFI", "TLI", "RMSEA",
            "RMSEA CI Lower", "RMSEA CI Upper", "SRMR", "AIC", "BIC"),
  Value = c(round(cfa_fitm["chisq"], 3), cfa_fitm["df"], round(cfa_fitm["pvalue"], 4),
            round(cfa_fitm["cfi"], 3), round(cfa_fitm["tli"], 3),
            round(cfa_fitm["rmsea"], 3), round(cfa_fitm["rmsea.ci.lower"], 3),
            round(cfa_fitm["rmsea.ci.upper"], 3), round(cfa_fitm["srmr"], 3),
            round(cfa_fitm["aic"], 2), round(cfa_fitm["bic"], 2)),
  Kriteria = c(NA, NA, "> 0.05", ">= 0.90", ">= 0.90", "< 0.08",
               NA, NA, "< 0.08", "Smaller is better", "Smaller is better")
)
write.csv(fit_indices, paste0(OUTPUT_DIR, "/09_cfa_fit_indices.csv"), row.names = FALSE)

# --- PLOT 7: Scree Plot / Parallel Analysis ---
png(paste0(PLOT_DIR, "/07_parallel_analysis.png"),
    width = PLOT_WIDTH, height = PLOT_HEIGHT, units = "in", res = PLOT_DPI)
# Re-run dengan plot=TRUE ke PNG device
fa.parallel(item_data, fm = "ml", fa = "fa", n.iter = 100,
            main = "Parallel Analysis Scree Plot")
dev.off()
cat("\nPlot 7 tersimpan: 07_parallel_analysis.png\n")

# --- PLOT 8: Factor Loadings dari EFA ---
p8 <- ggplot(efa_summary, aes(x = reorder(Item, Factor_Loading), y = Factor_Loading)) +
  geom_bar(stat = "identity", fill = colors_main[5], alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "red") +
  geom_text(aes(label = sprintf("%.3f", Factor_Loading)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Factor Loadings (EFA Unidimensional)",
    subtitle = "Garis merah menunjukkan cut-off 0.40",
    x = "Item",
    y = "Factor Loading"
  ) +
  theme_report() +
  scale_y_continuous(limits = c(0, 1))

ggsave(paste0(PLOT_DIR, "/08_efa_loadings.png"), p8,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 8 tersimpan: 08_efa_loadings.png\n")

# --- PLOT 9: Standardized Loadings dari CFA ---
p9 <- ggplot(cfa_summary, aes(x = reorder(Item, Std_Loading), y = Std_Loading)) +
  geom_bar(stat = "identity", fill = colors_main[6], alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = Std_Loading - 1.96*SE, ymax = Std_Loading + 1.96*SE),
                width = 0.2, color = "gray30") +
  geom_text(aes(label = sprintf("%.3f", Std_Loading)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Standardized Factor Loadings (CFA)",
    subtitle = "Error bars menunjukkan 95% CI | Garis merah = cut-off 0.50",
    x = "Item",
    y = "Standardized Loading"
  ) +
  theme_report() +
  scale_y_continuous(limits = c(0, 1.1))

ggsave(paste0(PLOT_DIR, "/09_cfa_loadings.png"), p9,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 9 tersimpan: 09_cfa_loadings.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 7. ESTIMASI GRADED RESPONSE MODEL (GRM)
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("ESTIMASI GRADED RESPONSE MODEL (GRM)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Fit GRM model
cat("\nMengestimasi model GRM...\n")
grm_model <- mirt(item_data, model = 1, itemtype = "graded", verbose = FALSE)

cat("Model GRM berhasil diestimasi!\n")

# Ekstrak parameter item
item_params <- coef(grm_model, IRTpars = TRUE, simplify = TRUE)$items
cat("\nParameter Item GRM:\n")
print(round(item_params, 3))

# Simpan parameter
param_df <- data.frame(
  Item = item_names,
  a = round(item_params[, 1], 3),  # discrimination
  b1 = round(item_params[, 2], 3), # threshold 1
  b2 = round(item_params[, 3], 3), # threshold 2
  b3 = round(item_params[, 4], 3), # threshold 3
  b4 = round(item_params[, 5], 3)  # threshold 4
)
write.csv(param_df, paste0(OUTPUT_DIR, "/10_grm_item_parameters.csv"), row.names = FALSE)

# Interpretasi diskriminasi
cat("\nInterpretasi Parameter Diskriminasi (a):\n")
cat("  a < 0.5  : Sangat Rendah\n")
cat("  0.5-0.9  : Rendah\n")
cat("  0.9-1.3  : Sedang\n")
cat("  1.3-1.7  : Tinggi\n")
cat("  a > 1.7  : Sangat Tinggi\n")

discrim_cat <- cut(item_params[, 1],
                   breaks = c(-Inf, 0.5, 0.9, 1.3, 1.7, Inf),
                   labels = c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi"))
cat(paste0("\nKategori Diskriminasi:\n"))
print(table(discrim_cat))

# --- PLOT 10: Parameter Diskriminasi ---
p10 <- ggplot(param_df, aes(x = reorder(Item, a), y = a)) +
  geom_bar(stat = "identity", aes(fill = a), alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = c(0.5, 0.9, 1.3, 1.7),
             linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.2f", a)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient2(low = "#e74c3c", mid = "#f1c40f", high = "#27ae60",
                       midpoint = 1.3, name = "Diskriminasi") +
  labs(
    title = "Parameter Diskriminasi Item (a)",
    subtitle = "Garis putus-putus menunjukkan batas kategori interpretasi",
    x = "Item",
    y = "Diskriminasi (a)"
  ) +
  theme_report() +
  scale_y_continuous(limits = c(0, max(item_params[, 1]) + 0.5))

ggsave(paste0(PLOT_DIR, "/10_parameter_diskriminasi.png"), p10,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("\nPlot 10 tersimpan: 10_parameter_diskriminasi.png\n")

# --- PLOT 11: Threshold Parameters ---
thresh_long <- param_df %>%
  select(Item, b1, b2, b3, b4) %>%
  pivot_longer(cols = -Item, names_to = "Threshold", values_to = "Value") %>%
  mutate(Threshold = factor(Threshold, levels = c("b1", "b2", "b3", "b4"),
                            labels = c("b1 (1→2)", "b2 (2→3)", "b3 (3→4)", "b4 (4→5)")))

p11 <- ggplot(thresh_long, aes(x = Item, y = Value, color = Threshold, group = Threshold)) +
  geom_point(size = 3) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Parameter Threshold Item (b)",
    subtitle = "Lokasi pada skala theta di mana probabilitas transisi = 0.5",
    x = "Item",
    y = "Threshold (θ)"
  ) +
  theme_report() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(PLOT_DIR, "/11_parameter_threshold.png"), p11,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 11 tersimpan: 11_parameter_threshold.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 8. MODEL FIT - KETEPATAN MODEL
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("EVALUASI KETEPATAN MODEL (MODEL FIT)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# 8.1 Overall Model Fit
cat("\n--- Overall Model Fit ---\n")

# M2 statistic (limited-information fit)
m2_fit <- M2(grm_model, type = "C2")
cat("\nM2 Statistic:\n")
print(m2_fit)

# Ekstrak nilai dari m2_fit (yang merupakan data frame)
m2_val <- m2_fit$M2[1]
df_val <- m2_fit$df[1]
p_val <- m2_fit$p[1]
rmsea_val <- m2_fit$RMSEA[1]
rmsea_lo <- m2_fit$RMSEA_5[1]
rmsea_hi <- m2_fit$RMSEA_95[1]
srmsr_val <- m2_fit$SRMSR[1]
tli_val <- m2_fit$TLI[1]
cfi_val <- m2_fit$CFI[1]

# Log-likelihood dan information criteria
ll_stats <- data.frame(
  Statistic = c("Log-Likelihood", "AIC", "BIC", "SABIC"),
  Value = c(grm_model@Fit$logLik,
            grm_model@Fit$AIC,
            grm_model@Fit$BIC,
            grm_model@Fit$SABIC)
)
cat("\nInformation Criteria:\n")
print(ll_stats)

# Simpan model fit
overall_fit <- data.frame(
  Statistic = c("M2", "df", "p-value", "RMSEA_M2", "RMSEA_M2_CI_Lower",
                "RMSEA_M2_CI_Upper", "SRMSR", "TLI_M2", "CFI_M2",
                "Log-Likelihood", "AIC", "BIC", "SABIC"),
  Value = c(round(m2_val, 3), df_val, round(p_val, 4),
            round(rmsea_val, 4), round(rmsea_lo, 4),
            round(rmsea_hi, 4), round(srmsr_val, 4),
            round(tli_val, 3), round(cfi_val, 3),
            round(grm_model@Fit$logLik, 2), round(grm_model@Fit$AIC, 2),
            round(grm_model@Fit$BIC, 2), round(grm_model@Fit$SABIC, 2))
)
write.csv(overall_fit, paste0(OUTPUT_DIR, "/11_overall_model_fit.csv"), row.names = FALSE)

# Interpretasi Model Fit
cat("\nInterpretasi Model Fit:\n")
cat(paste0("  RMSEA_M2 = ", round(rmsea_val, 4),
           " (kriteria < 0.05 = good, < 0.08 = acceptable)\n"))
cat(paste0("  CFI_M2 = ", round(cfi_val, 3), " (kriteria >= 0.95)\n"))
cat(paste0("  TLI_M2 = ", round(tli_val, 3), " (kriteria >= 0.95)\n"))
cat(paste0("  SRMSR = ", round(srmsr_val, 4), " (kriteria < 0.08)\n"))

model_fit_ok <- rmsea_val < 0.08 & cfi_val >= 0.90 &
  tli_val >= 0.90 & srmsr_val < 0.08
cat(paste0("\nKesimpulan Model Fit: ",
           ifelse(model_fit_ok, "MODEL FIT BAIK", "MODEL FIT PERLU PERHATIAN"), "\n"))

# 8.2 Item Fit
cat("\n--- Item Fit ---\n")

item_fit <- itemfit(grm_model, fit_stats = c("S_X2", "infit"))
cat("\nItem Fit Statistics:\n")

# Round hanya kolom numerik
item_fit_display <- item_fit
numeric_cols <- sapply(item_fit_display, is.numeric)
item_fit_display[numeric_cols] <- round(item_fit_display[numeric_cols], 4)
print(item_fit_display)

# Simpan item fit
# Cek nama kolom yang tersedia
cat("\nKolom dalam item_fit:", paste(names(item_fit), collapse = ", "), "\n")

# Buat data frame dengan penanganan nama kolom yang fleksibel
item_fit_df <- data.frame(
  Item = item_names
)

# Tambahkan S_X2 jika ada
if ("S_X2" %in% names(item_fit)) {
  item_fit_df$S_X2 <- round(item_fit$S_X2, 3)
}
if ("df.S_X2" %in% names(item_fit)) {
  item_fit_df$df_S_X2 <- item_fit$df.S_X2
}
if ("p.S_X2" %in% names(item_fit)) {
  item_fit_df$p_S_X2 <- round(item_fit$p.S_X2, 4)
}
if ("outfit" %in% names(item_fit)) {
  item_fit_df$Outfit <- round(item_fit$outfit, 3)
}
if ("infit" %in% names(item_fit)) {
  item_fit_df$Infit <- round(item_fit$infit, 3)
}

# Tentukan fit status berdasarkan kolom yang tersedia
if (all(c("p.S_X2", "outfit", "infit") %in% names(item_fit))) {
  item_fit_df$Fit_Status <- ifelse(item_fit$p.S_X2 > 0.01 &
                                     item_fit$outfit > 0.5 & item_fit$outfit < 1.5 &
                                     item_fit$infit > 0.5 & item_fit$infit < 1.5,
                                   "Good", "Check")
} else if (all(c("outfit", "infit") %in% names(item_fit))) {
  item_fit_df$Fit_Status <- ifelse(item_fit$outfit > 0.5 & item_fit$outfit < 1.5 &
                                     item_fit$infit > 0.5 & item_fit$infit < 1.5,
                                   "Good", "Check")
} else {
  item_fit_df$Fit_Status <- "N/A"
}

write.csv(item_fit_df, paste0(OUTPUT_DIR, "/12_item_fit.csv"), row.names = FALSE)

cat("\nKriteria Item Fit:\n")
cat("  S_X2 p-value > 0.01 (dengan koreksi Bonferroni lebih ketat)\n")
cat("  Infit/Outfit: 0.5 - 1.5 (produktif untuk pengukuran)\n")

if ("Fit_Status" %in% names(item_fit_df)) {
  misfit_items <- item_fit_df$Item[item_fit_df$Fit_Status == "Check"]
  if (length(misfit_items) > 0) {
    cat(paste0("\nItem dengan potensi misfit: ", paste(misfit_items, collapse = ", "), "\n"))
  } else {
    cat("\nSemua item memiliki fit yang baik.\n")
  }
} else {
  cat("\nInformasi fit status tidak tersedia.\n")
}

# 8.3 Person Fit
cat("\n--- Person Fit ---\n")

person_fit <- personfit(grm_model)

# Ekstrak Zh values
if ("Zh" %in% names(person_fit)) {
  zh_for_plot <- person_fit$Zh
} else {
  # Coba ambil kolom pertama yang numerik
  numeric_cols <- sapply(person_fit, is.numeric)
  if (any(numeric_cols)) {
    zh_for_plot <- person_fit[[which(numeric_cols)[1]]]
  } else {
    zh_for_plot <- rep(0, n_respondents)
  }
}

# Ringkasan person fit
pf_summary <- data.frame(
  Statistic = c("Mean Zh", "SD Zh", "Min Zh", "Max Zh",
                "N Aberrant (|Zh| > 2)", "% Aberrant"),
  Value = c(round(mean(zh_for_plot, na.rm = TRUE), 3),
            round(sd(zh_for_plot, na.rm = TRUE), 3),
            round(min(zh_for_plot, na.rm = TRUE), 3),
            round(max(zh_for_plot, na.rm = TRUE), 3),
            sum(abs(zh_for_plot) > 2, na.rm = TRUE),
            round(sum(abs(zh_for_plot) > 2, na.rm = TRUE) / n_respondents * 100, 2))
)
cat("\nPerson Fit Summary (Zh statistic):\n")
print(pf_summary)

write.csv(pf_summary, paste0(OUTPUT_DIR, "/13_person_fit_summary.csv"), row.names = FALSE)

# --- PLOT 12: Item Fit Plot ---
# Cek apakah kolom Infit dan Outfit tersedia
if (all(c("Infit", "Outfit") %in% names(item_fit_df))) {
  p12 <- ggplot(item_fit_df, aes(x = Infit, y = Outfit, color = Fit_Status)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text(aes(label = Item), vjust = -1, size = 3) +
    geom_hline(yintercept = c(0.5, 1.5), linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = c(0.5, 1.5), linetype = "dashed", color = "gray50") +
    geom_rect(aes(xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 1.5),
              fill = NA, color = "green", linetype = "solid", alpha = 0.2) +
    scale_color_manual(values = c("Good" = colors_main[4], "Check" = "#e74c3c", "N/A" = "gray50")) +
    labs(
      title = "Item Fit: Infit vs Outfit",
      subtitle = "Area hijau menunjukkan rentang fit yang produktif (0.5-1.5)",
      x = "Infit",
      y = "Outfit"
    ) +
    theme_report() +
    coord_cartesian(xlim = c(0, max(2, max(item_fit_df$Infit, na.rm = TRUE) + 0.2)),
                    ylim = c(0, max(2, max(item_fit_df$Outfit, na.rm = TRUE) + 0.2)))

  ggsave(paste0(PLOT_DIR, "/12_item_fit_infit_outfit.png"), p12,
         width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  cat("\nPlot 12 tersimpan: 12_item_fit_infit_outfit.png\n")
} else {
  cat("\nPlot 12 dilewati: Kolom Infit/Outfit tidak tersedia\n")
}

# --- PLOT 13: Person Fit Distribution ---
p13 <- ggplot(data.frame(Zh = zh_for_plot), aes(x = Zh)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = colors_main[1], color = "white", alpha = 0.8) +
  geom_density(color = colors_main[2], linewidth = 1.2) +
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", color = "red") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "blue", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Distribusi Person Fit (Zh Statistic)",
    subtitle = paste0("Garis merah: batas aberrant (|Zh| > 2) | ",
                      "Aberrant: ", sum(abs(zh_for_plot) > 2, na.rm = TRUE), " responden (",
                      round(sum(abs(zh_for_plot) > 2, na.rm = TRUE)/n_respondents*100, 1), "%)"),
    x = "Zh Statistic",
    y = "Densitas"
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/13_person_fit_distribution.png"), p13,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 13 tersimpan: 13_person_fit_distribution.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 9. ITEM CHARACTERISTIC CURVES (ICC)
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("ITEM CHARACTERISTIC CURVES (ICC)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Generate ICC untuk setiap item
theta_range <- seq(-4, 4, by = 0.1)

# --- PLOT 14: ICC Grid (semua item) ---
png(paste0(PLOT_DIR, "/14_icc_all_items.png"),
    width = PLOT_WIDTH * 1.5, height = PLOT_HEIGHT * 1.5, units = "in", res = PLOT_DPI)
p <- plot(grm_model, type = "trace", which.items = 1:n_items,
     theta_lim = c(-4, 4), facet_items = TRUE,
     main = "Item Characteristic Curves (ICC) - All Items")
print(p)
dev.off()
cat("\nPlot 14 tersimpan: 14_icc_all_items.png\n")

# ICC untuk setiap item individual
for (i in 1:n_items) {
  png(paste0(PLOT_DIR, "/14_icc_item_", i, ".png"),
      width = PLOT_WIDTH, height = PLOT_HEIGHT, units = "in", res = PLOT_DPI)
  p <- plot(grm_model, type = "trace", which.items = i,
       theta_lim = c(-4, 4),
       main = paste0("ICC - ", item_names[i]))
  print(p)
  dev.off()
}
cat(paste0("Plot ICC individual tersimpan: 14_icc_item_1.png sampai 14_icc_item_", n_items, ".png\n"))

# --- PLOT 15: Category Response Curves ---
png(paste0(PLOT_DIR, "/15_category_response_curves.png"),
    width = PLOT_WIDTH * 1.5, height = PLOT_HEIGHT * 1.5, units = "in", res = PLOT_DPI)
p <- plot(grm_model, type = "trace", which.items = 1:min(6, n_items),
     theta_lim = c(-4, 4), facet_items = TRUE,
     main = "Category Response Curves (6 Item Pertama)")
print(p)
dev.off()
cat("Plot 15 tersimpan: 15_category_response_curves.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 10. ITEM INFORMATION FUNCTION (IIF)
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("ITEM INFORMATION FUNCTION (IIF)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Hitung item information
item_info <- data.frame(
  Theta = theta_range,
  sapply(1:n_items, function(i) {
    iteminfo(extract.item(grm_model, i), Theta = theta_range)
  })
)
colnames(item_info)[-1] <- item_names

# Simpan
write.csv(item_info, paste0(OUTPUT_DIR, "/14_item_information.csv"), row.names = FALSE)

# Hitung max information per item
max_info <- sapply(item_names, function(item) {
  max(item_info[[item]])
})
theta_at_max <- sapply(item_names, function(item) {
  theta_range[which.max(item_info[[item]])]
})

info_summary <- data.frame(
  Item = item_names,
  Max_Information = round(max_info, 3),
  Theta_at_Max = round(theta_at_max, 2)
)
write.csv(info_summary, paste0(OUTPUT_DIR, "/15_item_info_summary.csv"), row.names = FALSE)

cat("\nRingkasan Item Information:\n")
print(info_summary)

# --- PLOT 16: IIF All Items ---
item_info_long <- item_info %>%
  pivot_longer(cols = -Theta, names_to = "Item", values_to = "Information") %>%
  mutate(Item = factor(Item, levels = item_names))

p16 <- ggplot(item_info_long, aes(x = Theta, y = Information, color = Item)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  scale_color_manual(values = rep(colors_main, length.out = n_items)) +
  labs(
    title = "Item Information Functions (IIF)",
    subtitle = "Informasi yang diberikan setiap item sepanjang kontinum theta",
    x = expression(theta~"(Trait Level)"),
    y = "Information"
  ) +
  theme_report() +
  theme(legend.position = "right")

ggsave(paste0(PLOT_DIR, "/16_iif_all_items.png"), p16,
       width = PLOT_WIDTH * 1.2, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("\nPlot 16 tersimpan: 16_iif_all_items.png\n")

# --- PLOT 17: IIF Individual ---
png(paste0(PLOT_DIR, "/17_iif_individual.png"),
    width = PLOT_WIDTH * 1.5, height = PLOT_HEIGHT * 1.5, units = "in", res = PLOT_DPI)
p <- plot(grm_model, type = "infotrace", which.items = 1:n_items,
     theta_lim = c(-4, 4), facet_items = TRUE,
     main = "Item Information Functions (Individual)")
print(p)
dev.off()
cat("Plot 17 tersimpan: 17_iif_individual.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 11. TEST INFORMATION FUNCTION (TIF) DAN STANDARD ERROR
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("TEST INFORMATION FUNCTION (TIF)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Hitung test information
test_info <- testinfo(grm_model, Theta = theta_range)
se_theta <- 1 / sqrt(test_info)

tif_data <- data.frame(
  Theta = theta_range,
  Information = test_info,
  SE = se_theta
)
write.csv(tif_data, paste0(OUTPUT_DIR, "/16_test_information.csv"), row.names = FALSE)

# Ringkasan TIF
cat("\nRingkasan Test Information:\n")
cat(paste0("  Max Information: ", round(max(test_info), 3),
           " at theta = ", round(theta_range[which.max(test_info)], 2), "\n"))
cat(paste0("  Min SE: ", round(min(se_theta), 3), "\n"))
cat(paste0("  Information > 10 dalam rentang theta: [",
           round(min(theta_range[test_info > 10]), 2), ", ",
           round(max(theta_range[test_info > 10]), 2), "]\n"))

# --- PLOT 18: Test Information Function ---
p18 <- ggplot(tif_data, aes(x = Theta)) +
  geom_line(aes(y = Information), color = colors_main[3], linewidth = 1.5) +
  geom_area(aes(y = Information), fill = colors_main[3], alpha = 0.3) +
  geom_vline(xintercept = theta_range[which.max(test_info)],
             linetype = "dashed", color = "red") +
  labs(
    title = "Test Information Function (TIF)",
    subtitle = paste0("Max Information = ", round(max(test_info), 2),
                      " at θ = ", round(theta_range[which.max(test_info)], 2)),
    x = expression(theta~"(Trait Level)"),
    y = "Information"
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/18_test_information_function.png"), p18,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("\nPlot 18 tersimpan: 18_test_information_function.png\n")

# --- PLOT 19: TIF dengan Standard Error ---
p19 <- ggplot(tif_data, aes(x = Theta)) +
  geom_line(aes(y = Information, color = "Information"), linewidth = 1.2) +
  geom_line(aes(y = SE * max(Information) / max(SE), color = "SE (scaled)"),
            linewidth = 1.2, linetype = "dashed") +
  scale_y_continuous(
    name = "Information",
    sec.axis = sec_axis(~ . * max(tif_data$SE) / max(tif_data$Information),
                        name = "Standard Error")
  ) +
  scale_color_manual(values = c("Information" = colors_main[3],
                                "SE (scaled)" = colors_main[2])) +
  labs(
    title = "Test Information Function dan Standard Error of Measurement",
    subtitle = "Hubungan terbalik antara informasi dan standard error",
    x = expression(theta~"(Trait Level)"),
    color = "Metric"
  ) +
  theme_report() +
  theme(legend.position = "bottom")

ggsave(paste0(PLOT_DIR, "/19_tif_with_se.png"), p19,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 19 tersimpan: 19_tif_with_se.png\n")

# --- PLOT 20: Conditional SE ---
p20 <- ggplot(tif_data, aes(x = Theta, y = SE)) +
  geom_line(color = colors_main[2], linewidth = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = SE), fill = colors_main[2], alpha = 0.3) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "red") +
  labs(
    title = "Conditional Standard Error of Measurement",
    subtitle = "Garis merah menunjukkan SE = 0.30 (reliability marginal ~0.90)",
    x = expression(theta~"(Trait Level)"),
    y = "Standard Error"
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/20_conditional_se.png"), p20,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 20 tersimpan: 20_conditional_se.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 12. RELIABILITAS (OMEGA)
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("ANALISIS RELIABILITAS (OMEGA)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Hitung omega dengan error handling
omega_result <- tryCatch({
  suppressWarnings({
    omega(item_data, nfactors = 1, fm = "ml", poly = TRUE, plot = FALSE)
  })
}, error = function(e) {
  cat("  [Error pada omega(), menggunakan estimasi alternatif]\n")
  # Fallback: gunakan omega tanpa polychoric
  tryCatch({
    omega(item_data, nfactors = 1, fm = "ml", poly = FALSE, plot = FALSE)
  }, error = function(e2) {
    # Jika masih error, return nilai default
    list(omega.tot = NA, omega_h = NA)
  })
})

cat("\nHasil Analisis Omega:\n")
cat(paste0("  Omega Total (ω_t)      : ", round(omega_result$omega.tot, 3), "\n"))
cat(paste0("  Omega Hierarchical (ω_h): ", round(omega_result$omega_h, 3), "\n"))

# Alpha untuk perbandingan (meskipun tidak direkomendasikan)
# Gunakan tryCatch untuk menangani error potential dari alpha()
alpha_result <- tryCatch({
  # Coba dengan check.keys=FALSE untuk menghindari plotting issues
  alpha(item_data, check.keys = FALSE)
}, error = function(e) {
  # Jika error, hitung manual menggunakan formula Cronbach's alpha
  cat("  [Info: Menghitung alpha secara manual karena error pada fungsi alpha()]\n")

  # Formula: α = (k/(k-1)) * (1 - Σσ²ᵢ/σ²ₜ)
  k <- ncol(item_data)
  item_var <- apply(item_data, 2, var, na.rm = TRUE)
  total_var <- var(rowSums(item_data, na.rm = TRUE), na.rm = TRUE)
  alpha_manual <- (k / (k - 1)) * (1 - sum(item_var) / total_var)

  # Return struktur yang mirip dengan alpha()
  list(total = list(raw_alpha = alpha_manual))
})

cat(paste0("  Alpha Cronbach (referensi): ", round(alpha_result$total$raw_alpha, 3), "\n"))

# Reliabilitas marginal dari IRT
marginal_rel <- tryCatch({
  marginal_rxx(grm_model)
}, error = function(e) {
  cat("  [Info: Menghitung marginal reliability secara manual]\n")
  # Fallback: hitung manual menggunakan formula
  # Rumus: Var(θ) / (Var(θ) + Mean(SE²))
  # theta_eap akan didefinisikan di section berikutnya, jadi kita hitung di sini
  theta_temp <- fscores(grm_model, method = "EAP", full.scores = TRUE,
                        full.scores.SE = TRUE)
  var_theta <- var(theta_temp[, 1])
  mean_sem_sq <- mean(theta_temp[, 2]^2)
  var_theta / (var_theta + mean_sem_sq)
})
cat(paste0("  Marginal Reliability (IRT): ", round(marginal_rel, 3), "\n"))

# Empirical reliability
emp_rel <- tryCatch({
  # Dapatkan fscores dengan SE
  theta_scores <- fscores(grm_model, method = "EAP", full.scores = TRUE,
                          full.scores.SE = TRUE)
  empirical_rxx(theta_scores)
}, error = function(e) {
  cat("  [Info: Empirical reliability dihitung dari correlation theta dengan total score]\n")
  # Alternatif: hitung empirical reliability sebagai squared correlation
  theta_eap_alt <- fscores(grm_model, method = "EAP", full.scores = TRUE)
  cor(theta_eap_alt[, 1], rowSums(item_data, na.rm = TRUE))^2
})
cat(paste0("  Empirical Reliability     : ", round(emp_rel, 3), "\n"))

# Interpretasi
cat("\nInterpretasi Reliabilitas:\n")
cat("  ω < 0.60  : Tidak dapat diterima\n")
cat("  0.60-0.70 : Kurang baik\n")
cat("  0.70-0.80 : Dapat diterima\n")
cat("  0.80-0.90 : Baik\n")
cat("  ω > 0.90  : Sangat Baik\n")

rel_cat <- ifelse(is.na(omega_result$omega.tot), "Tidak tersedia",
                  ifelse(omega_result$omega.tot < 0.60, "Tidak dapat diterima",
                         ifelse(omega_result$omega.tot < 0.70, "Kurang baik",
                                ifelse(omega_result$omega.tot < 0.80, "Dapat diterima",
                                       ifelse(omega_result$omega.tot < 0.90, "Baik", "Sangat Baik")))))
cat(paste0("\nKategori Reliabilitas: ", rel_cat, "\n"))

# Simpan hasil reliabilitas
rel_summary <- data.frame(
  Metric = c("Omega Total", "Omega Hierarchical", "Alpha Cronbach (referensi)",
             "Marginal Reliability (IRT)", "Empirical Reliability"),
  Value = round(c(omega_result$omega.tot, omega_result$omega_h,
                  alpha_result$total$raw_alpha, marginal_rel, emp_rel), 3),
  Interpretation = c(rel_cat, NA, NA, NA, NA)
)
write.csv(rel_summary, paste0(OUTPUT_DIR, "/17_reliability.csv"), row.names = FALSE)

# --- PLOT 21: Omega Diagram ---
png(paste0(PLOT_DIR, "/21_omega_diagram.png"),
    width = PLOT_WIDTH, height = PLOT_HEIGHT, units = "in", res = PLOT_DPI)
par(mar = c(2, 2, 2, 2))  # Set margins
tryCatch({
  omega.diagram(omega_result, main = "Omega Factor Structure")
}, error = function(e) {
  plot.new()
  text(0.5, 0.5, "Omega Diagram\n(lihat hasil numerik)", cex = 1.5)
})
dev.off()
cat("\nPlot 21 tersimpan: 21_omega_diagram.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 13. ESTIMASI THETA (ABILITY/TRAIT LEVEL)
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("ESTIMASI THETA (TRAIT LEVEL)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Estimasi theta dengan berbagai metode
theta_eap <- fscores(grm_model, method = "EAP", full.scores = TRUE,
                     full.scores.SE = TRUE)
theta_map <- fscores(grm_model, method = "MAP", full.scores = TRUE)
theta_ml <- fscores(grm_model, method = "ML", full.scores = TRUE)

# Gabungkan hasil
theta_estimates <- data.frame(
  Respondent = 1:n_respondents,
  Theta_EAP = round(theta_eap[, 1], 3),
  SE_EAP = round(theta_eap[, 2], 3),
  Theta_MAP = round(theta_map[, 1], 3),
  Theta_ML = round(theta_ml[, 1], 3),
  Total_Score = total_scores
)
write.csv(theta_estimates, paste0(OUTPUT_DIR, "/18_theta_estimates.csv"), row.names = FALSE)

# Ringkasan theta
cat("\nRingkasan Estimasi Theta (EAP):\n")
cat(paste0("  Mean : ", round(mean(theta_eap[, 1]), 3), "\n"))
cat(paste0("  SD   : ", round(sd(theta_eap[, 1]), 3), "\n"))
cat(paste0("  Range: ", round(min(theta_eap[, 1]), 3), " - ",
           round(max(theta_eap[, 1]), 3), "\n"))
cat(paste0("  Mean SE: ", round(mean(theta_eap[, 2]), 3), "\n"))

# --- PLOT 22: Distribusi Theta ---
p22 <- ggplot(theta_estimates, aes(x = Theta_EAP)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = colors_main[4], color = "white", alpha = 0.8) +
  geom_density(color = colors_main[2], linewidth = 1.2) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(theta_eap[, 1]), color = "red",
             linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribusi Estimasi Theta (EAP)",
    subtitle = paste0("Mean = ", round(mean(theta_eap[, 1]), 3),
                      " | SD = ", round(sd(theta_eap[, 1]), 3),
                      " | Garis biru: N(0,1)"),
    x = expression(theta~"(Trait Level)"),
    y = "Densitas"
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/22_distribusi_theta.png"), p22,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("\nPlot 22 tersimpan: 22_distribusi_theta.png\n")

# --- PLOT 23: Theta vs Total Score ---
p23 <- ggplot(theta_estimates, aes(x = Total_Score, y = Theta_EAP)) +
  geom_point(alpha = 0.5, color = colors_main[5]) +
  geom_smooth(method = "loess", color = colors_main[2], se = TRUE, fill = colors_main[2], alpha = 0.2) +
  labs(
    title = "Hubungan Skor Total dengan Estimasi Theta",
    subtitle = paste0("Korelasi = ", round(cor(theta_estimates$Total_Score,
                                               theta_estimates$Theta_EAP), 3)),
    x = "Skor Total",
    y = expression(theta~"(EAP)")
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/23_theta_vs_total_score.png"), p23,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 23 tersimpan: 23_theta_vs_total_score.png\n")

# --- PLOT 24: Theta dengan Confidence Interval ---
theta_sorted <- theta_estimates[order(theta_estimates$Theta_EAP), ]
theta_sorted$Index <- 1:nrow(theta_sorted)

p24 <- ggplot(theta_sorted, aes(x = Index, y = Theta_EAP)) +
  geom_ribbon(aes(ymin = Theta_EAP - 1.96 * SE_EAP,
                  ymax = Theta_EAP + 1.96 * SE_EAP),
              fill = colors_main[1], alpha = 0.3) +
  geom_line(color = colors_main[3], linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Estimasi Theta dengan 95% Confidence Interval",
    subtitle = "Diurutkan berdasarkan theta (terendah ke tertinggi)",
    x = "Responden (diurutkan)",
    y = expression(theta~"± 95% CI")
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/24_theta_confidence_interval.png"), p24,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 24 tersimpan: 24_theta_confidence_interval.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 13b. PENORMAAN BERDASARKAN THETA
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("PENORMAAN BERDASARKAN THETA\n")
cat("=" , rep("=", 70), "\n", sep = "")

# --- Konversi Theta ke Berbagai Skala Norma ---

# 1. Z-Score (theta sudah dalam skala z dengan mean~0, sd~1)
z_scores <- theta_eap[, 1]

# 2. T-Score (mean = 50, SD = 10)
t_scores <- (z_scores * 10) + 50

# 3. Scaled Score (mean = 10, SD = 3) - seperti subtes IQ
scaled_scores <- (z_scores * 3) + 10

# 4. Stanine (1-9, mean = 5, SD = 2)
stanine_scores <- round((z_scores * 2) + 5)
stanine_scores[stanine_scores < 1] <- 1
stanine_scores[stanine_scores > 9] <- 9

# 5. Sten Score (1-10, mean = 5.5, SD = 2)
sten_scores <- round((z_scores * 2) + 5.5)
sten_scores[sten_scores < 1] <- 1
sten_scores[sten_scores > 10] <- 10

# 6. Percentile Rank
percentile_ranks <- round(pnorm(z_scores) * 100, 1)

# 7. IQ-like Score (mean = 100, SD = 15)
iq_scores <- (z_scores * 15) + 100

# Buat tabel norma lengkap per responden
norma_table <- data.frame(
  Respondent = 1:n_respondents,
  Total_Score = total_scores,
  Theta = round(theta_eap[, 1], 3),
  SE_Theta = round(theta_eap[, 2], 3),
  Z_Score = round(z_scores, 3),
  T_Score = round(t_scores, 1),
  Scaled_Score = round(scaled_scores, 1),
  IQ_Scale = round(iq_scores, 1),
  Stanine = stanine_scores,
  Sten = sten_scores,
  Percentile = percentile_ranks
)

# Simpan tabel norma
write.csv(norma_table, paste0(OUTPUT_DIR, "/21_tabel_norma_individu.csv"), row.names = FALSE)

cat("\nTabel Norma Individu (10 responden pertama):\n")
print(head(norma_table, 10))

# --- Kategori Interpretasi Berdasarkan Theta/Percentile ---

# Fungsi untuk kategorisasi
kategorisasi_theta <- function(theta) {
  cut(theta,
      breaks = c(-Inf, -2, -1, -0.5, 0.5, 1, 2, Inf),
      labels = c("Sangat Rendah", "Rendah", "Agak Rendah",
                 "Sedang", "Agak Tinggi", "Tinggi", "Sangat Tinggi"))
}

kategorisasi_percentile <- function(pct) {
  cut(pct,
      breaks = c(-Inf, 5, 10, 25, 50, 75, 90, 95, Inf),
      labels = c("Sangat Rendah (<P5)", "Rendah (P5-P10)", "Di Bawah Rata-rata (P10-P25)",
                 "Rata-rata Bawah (P25-P50)", "Rata-rata Atas (P50-P75)",
                 "Di Atas Rata-rata (P75-P90)", "Tinggi (P90-P95)", "Sangat Tinggi (>P95)"))
}

kategorisasi_stanine <- function(stanine) {
  labels <- c("Sangat Rendah", "Rendah", "Di Bawah Rata-rata", "Sedikit Di Bawah Rata-rata",
              "Rata-rata", "Sedikit Di Atas Rata-rata", "Di Atas Rata-rata", "Tinggi", "Sangat Tinggi")
  labels[stanine]
}

# Tambahkan kategori ke tabel
norma_table$Kategori_Theta <- kategorisasi_theta(norma_table$Theta)
norma_table$Kategori_Percentile <- kategorisasi_percentile(norma_table$Percentile)
norma_table$Kategori_Stanine <- kategorisasi_stanine(norma_table$Stanine)

# Update file CSV dengan kategori
write.csv(norma_table, paste0(OUTPUT_DIR, "/21_tabel_norma_individu.csv"), row.names = FALSE)

# --- Tabel Konversi Skor (Lookup Table) ---

# Buat tabel konversi dari skor total ke theta dan norma
score_to_theta <- data.frame(
  Total_Score = sort(unique(total_scores))
)

# Hitung mean theta untuk setiap skor total
score_to_theta$Mean_Theta <- sapply(score_to_theta$Total_Score, function(s) {
  mean(theta_eap[total_scores == s, 1])
})

score_to_theta$SD_Theta <- sapply(score_to_theta$Total_Score, function(s) {
  if(sum(total_scores == s) > 1) {
    sd(theta_eap[total_scores == s, 1])
  } else {
    NA
  }
})

score_to_theta$N <- sapply(score_to_theta$Total_Score, function(s) {
  sum(total_scores == s)
})

# Konversi ke skala norma
score_to_theta$T_Score <- round((score_to_theta$Mean_Theta * 10) + 50, 1)
score_to_theta$Scaled_Score <- round((score_to_theta$Mean_Theta * 3) + 10, 1)
score_to_theta$IQ_Scale <- round((score_to_theta$Mean_Theta * 15) + 100, 1)
score_to_theta$Percentile <- round(pnorm(score_to_theta$Mean_Theta) * 100, 1)
score_to_theta$Stanine <- pmin(9, pmax(1, round((score_to_theta$Mean_Theta * 2) + 5)))
score_to_theta$Kategori <- kategorisasi_theta(score_to_theta$Mean_Theta)

# Format theta
score_to_theta$Mean_Theta <- round(score_to_theta$Mean_Theta, 3)
score_to_theta$SD_Theta <- round(score_to_theta$SD_Theta, 3)

write.csv(score_to_theta, paste0(OUTPUT_DIR, "/22_tabel_konversi_skor.csv"), row.names = FALSE)

cat("\n\nTabel Konversi Skor Total ke Theta dan Norma:\n")
print(score_to_theta)

# --- Tabel Norma Teoretis (untuk semua kemungkinan theta) ---

theta_teoretis <- seq(-3, 3, by = 0.1)
norma_teoretis <- data.frame(
  Theta = theta_teoretis,
  T_Score = round((theta_teoretis * 10) + 50, 1),
  Scaled_Score = round((theta_teoretis * 3) + 10, 1),
  IQ_Scale = round((theta_teoretis * 15) + 100, 1),
  Percentile = round(pnorm(theta_teoretis) * 100, 1),
  Stanine = pmin(9, pmax(1, round((theta_teoretis * 2) + 5))),
  Sten = pmin(10, pmax(1, round((theta_teoretis * 2) + 5.5))),
  Kategori = kategorisasi_theta(theta_teoretis)
)

write.csv(norma_teoretis, paste0(OUTPUT_DIR, "/23_tabel_norma_teoretis.csv"), row.names = FALSE)

cat("\n\nTabel Norma Teoretis (rentang theta -3 sampai 3):\n")
print(norma_teoretis[seq(1, nrow(norma_teoretis), by = 5), ])

# --- Statistik Distribusi per Kategori ---

distribusi_kategori <- as.data.frame(table(norma_table$Kategori_Theta))
colnames(distribusi_kategori) <- c("Kategori", "Frekuensi")
distribusi_kategori$Persentase <- round(distribusi_kategori$Frekuensi / n_respondents * 100, 2)

cat("\n\nDistribusi Responden per Kategori (berdasarkan Theta):\n")
print(distribusi_kategori)

write.csv(distribusi_kategori, paste0(OUTPUT_DIR, "/24_distribusi_kategori.csv"), row.names = FALSE)

# --- Statistik Deskriptif Skor Norma ---

norma_deskriptif <- data.frame(
  Skala = c("Theta", "T-Score", "Scaled Score", "IQ Scale", "Stanine", "Sten", "Percentile"),
  Mean = round(c(mean(norma_table$Theta), mean(norma_table$T_Score),
                 mean(norma_table$Scaled_Score), mean(norma_table$IQ_Scale),
                 mean(norma_table$Stanine), mean(norma_table$Sten),
                 mean(norma_table$Percentile)), 2),
  SD = round(c(sd(norma_table$Theta), sd(norma_table$T_Score),
               sd(norma_table$Scaled_Score), sd(norma_table$IQ_Scale),
               sd(norma_table$Stanine), sd(norma_table$Sten),
               sd(norma_table$Percentile)), 2),
  Min = round(c(min(norma_table$Theta), min(norma_table$T_Score),
                min(norma_table$Scaled_Score), min(norma_table$IQ_Scale),
                min(norma_table$Stanine), min(norma_table$Sten),
                min(norma_table$Percentile)), 2),
  Max = round(c(max(norma_table$Theta), max(norma_table$T_Score),
                max(norma_table$Scaled_Score), max(norma_table$IQ_Scale),
                max(norma_table$Stanine), max(norma_table$Sten),
                max(norma_table$Percentile)), 2),
  Teoretis_Mean = c(0, 50, 10, 100, 5, 5.5, 50),
  Teoretis_SD = c(1, 10, 3, 15, 2, 2, NA)
)

cat("\n\nStatistik Deskriptif Skor Norma:\n")
print(norma_deskriptif)

write.csv(norma_deskriptif, paste0(OUTPUT_DIR, "/25_statistik_norma.csv"), row.names = FALSE)

# --- PLOT: Distribusi Theta dengan Kategori ---
p_theta_cat <- ggplot(norma_table, aes(x = Theta, fill = Kategori_Theta)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  geom_vline(xintercept = c(-2, -1, -0.5, 0.5, 1, 2),
             linetype = "dashed", color = "gray40", alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1, name = "Kategori") +
  labs(
    title = "Distribusi Theta dengan Kategori Interpretasi",
    subtitle = "Garis putus-putus menunjukkan batas kategori",
    x = expression(theta~"(Trait Level)"),
    y = "Frekuensi"
  ) +
  theme_report() +
  theme(legend.position = "right")

ggsave(paste0(PLOT_DIR, "/29_distribusi_theta_kategori.png"), p_theta_cat,
       width = PLOT_WIDTH * 1.2, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("\nPlot 29 tersimpan: 29_distribusi_theta_kategori.png\n")

# --- PLOT: Distribusi T-Score ---
p_tscore <- ggplot(norma_table, aes(x = T_Score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = colors_main[5], color = "white", alpha = 0.8) +
  geom_density(color = colors_main[2], linewidth = 1.2) +
  stat_function(fun = dnorm, args = list(mean = 50, sd = 10),
                color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 50, color = "red", linetype = "dashed") +
  labs(
    title = "Distribusi T-Score",
    subtitle = paste0("Mean = ", round(mean(norma_table$T_Score), 1),
                      " | SD = ", round(sd(norma_table$T_Score), 1),
                      " | Garis biru: Distribusi Teoretis N(50,10)"),
    x = "T-Score",
    y = "Densitas"
  ) +
  theme_report() +
  scale_x_continuous(breaks = seq(20, 80, by = 10))

ggsave(paste0(PLOT_DIR, "/30_distribusi_tscore.png"), p_tscore,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 30 tersimpan: 30_distribusi_tscore.png\n")

# --- PLOT: Distribusi Percentile ---
p_percentile <- ggplot(norma_table, aes(x = Percentile)) +
  geom_histogram(bins = 20, fill = colors_main[6], color = "white", alpha = 0.8) +
  geom_vline(xintercept = c(25, 50, 75), linetype = "dashed", color = "gray40") +
  labs(
    title = "Distribusi Percentile Rank",
    subtitle = "Garis putus-putus: Q1 (25), Median (50), Q3 (75)",
    x = "Percentile Rank",
    y = "Frekuensi"
  ) +
  theme_report() +
  scale_x_continuous(breaks = seq(0, 100, by = 10))

ggsave(paste0(PLOT_DIR, "/31_distribusi_percentile.png"), p_percentile,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 31 tersimpan: 31_distribusi_percentile.png\n")

# --- PLOT: Distribusi Stanine ---
p_stanine <- ggplot(norma_table, aes(x = factor(Stanine))) +
  geom_bar(fill = colors_main[3], color = "white", alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Distribusi Stanine Score",
    subtitle = "Stanine: 1-9 (Mean = 5, SD = 2)",
    x = "Stanine",
    y = "Frekuensi"
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/32_distribusi_stanine.png"), p_stanine,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 32 tersimpan: 32_distribusi_stanine.png\n")

# --- PLOT: Pie Chart Kategori ---
pie_data <- distribusi_kategori
pie_data$Label <- paste0(pie_data$Kategori, "\n", pie_data$Frekuensi, " (", pie_data$Persentase, "%)")

p_pie <- ggplot(pie_data, aes(x = "", y = Frekuensi, fill = Kategori)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(
    title = "Proporsi Responden per Kategori",
    subtitle = "Berdasarkan Theta Score"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )

ggsave(paste0(PLOT_DIR, "/33_pie_chart_kategori.png"), p_pie,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 33 tersimpan: 33_pie_chart_kategori.png\n")

# --- PLOT: Perbandingan Distribusi dengan Teoretis ---
comparison_data <- data.frame(
  Score = c(norma_table$Theta, rnorm(n_respondents, 0, 1)),
  Type = rep(c("Empiris", "Teoretis N(0,1)"), each = n_respondents)
)

p_compare <- ggplot(comparison_data, aes(x = Score, fill = Type, color = Type)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_fill_manual(values = c("Empiris" = colors_main[4], "Teoretis N(0,1)" = colors_main[2])) +
  scale_color_manual(values = c("Empiris" = colors_main[4], "Teoretis N(0,1)" = colors_main[2])) +
  labs(
    title = "Perbandingan Distribusi Theta: Empiris vs Teoretis",
    subtitle = "Distribusi empiris dibandingkan dengan distribusi normal standar",
    x = expression(theta),
    y = "Densitas"
  ) +
  theme_report() +
  theme(legend.position = "bottom")

ggsave(paste0(PLOT_DIR, "/34_perbandingan_distribusi.png"), p_compare,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 34 tersimpan: 34_perbandingan_distribusi.png\n")

# --- PLOT: Heatmap Konversi Skor ---
konversi_long <- score_to_theta %>%
  select(Total_Score, T_Score, Percentile, Stanine) %>%
  pivot_longer(cols = -Total_Score, names_to = "Skala", values_to = "Nilai")

p_konversi <- ggplot(konversi_long, aes(x = Skala, y = factor(Total_Score), fill = Nilai)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Nilai, 0)), size = 3) +
  scale_fill_gradient2(low = "#3498db", mid = "#f1c40f", high = "#e74c3c",
                       midpoint = median(konversi_long$Nilai, na.rm = TRUE)) +
  labs(
    title = "Heatmap Konversi Skor Total ke Skor Norma",
    x = "Skala Norma",
    y = "Skor Total"
  ) +
  theme_report() +
  theme(axis.text.y = element_text(size = 8))

ggsave(paste0(PLOT_DIR, "/35_heatmap_konversi.png"), p_konversi,
       width = PLOT_WIDTH, height = PLOT_HEIGHT * 1.2, dpi = PLOT_DPI)
cat("Plot 35 tersimpan: 35_heatmap_konversi.png\n")

# --- PLOT: Kurva Konversi Skor Total ke Theta ---
p_kurva_konversi <- ggplot(score_to_theta, aes(x = Total_Score, y = Mean_Theta)) +
  geom_line(color = colors_main[3], linewidth = 1.2) +
  geom_point(color = colors_main[3], size = 3) +
  geom_ribbon(aes(ymin = Mean_Theta - 1.96 * ifelse(is.na(SD_Theta), 0, SD_Theta),
                  ymax = Mean_Theta + 1.96 * ifelse(is.na(SD_Theta), 0, SD_Theta)),
              fill = colors_main[3], alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Kurva Konversi: Skor Total ke Theta",
    subtitle = "Dengan 95% confidence band",
    x = "Skor Total",
    y = expression(theta~"(Mean ± 95% CI)")
  ) +
  theme_report()

ggsave(paste0(PLOT_DIR, "/36_kurva_konversi.png"), p_kurva_konversi,
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 36 tersimpan: 36_kurva_konversi.png\n")

# --- Ringkasan Penormaan ---
cat("\n")
cat("--- RINGKASAN PENORMAAN ---\n")
cat(paste0("Total Responden: ", n_respondents, "\n"))
cat(paste0("Rentang Skor Total: ", min(total_scores), " - ", max(total_scores), "\n"))
cat(paste0("Rentang Theta: ", round(min(norma_table$Theta), 2), " - ",
           round(max(norma_table$Theta), 2), "\n"))
cat(paste0("Rentang T-Score: ", round(min(norma_table$T_Score), 1), " - ",
           round(max(norma_table$T_Score), 1), "\n"))
cat(paste0("Rentang Percentile: ", min(norma_table$Percentile), " - ",
           max(norma_table$Percentile), "\n\n"))

cat("Distribusi Kategori:\n")
for (i in 1:nrow(distribusi_kategori)) {
  cat(paste0("  ", distribusi_kategori$Kategori[i], ": ",
             distribusi_kategori$Frekuensi[i], " (",
             distribusi_kategori$Persentase[i], "%)\n"))
}

cat("\n")

# -----------------------------------------------------------------------------
# 14. WRIGHT MAP (ITEM-PERSON MAP)
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("WRIGHT MAP (ITEM-PERSON MAP)\n")
cat("=" , rep("=", 70), "\n", sep = "")

# --- PLOT 25: Wright Map ---
# Persiapkan data untuk Wright Map
theta_hist <- hist(theta_eap[, 1], breaks = 30, plot = FALSE)

# Threshold data
thresh_data <- data.frame(
  Item = rep(item_names, each = 4),
  Threshold = rep(c("b1", "b2", "b3", "b4"), n_items),
  Value = as.vector(t(item_params[, 2:5]))
)

# Membuat Wright Map manual
png(paste0(PLOT_DIR, "/25_wright_map.png"),
    width = PLOT_WIDTH, height = PLOT_HEIGHT * 1.5, units = "in", res = PLOT_DPI)

tryCatch({
  layout(matrix(c(1, 2), ncol = 2), widths = c(1, 2))
  par(mar = c(5, 4, 4, 0))

  # Panel kiri: distribusi theta
  barplot(theta_hist$counts, horiz = TRUE, space = 0,
          col = colors_main[1], border = "white",
          main = "Person Distribution",
          xlab = "Frequency", ylab = expression(theta))
  axis(2, at = seq(0, length(theta_hist$counts), length.out = 5),
       labels = round(seq(min(theta_hist$breaks), max(theta_hist$breaks), length.out = 5), 1))

  # Panel kanan: threshold items
  par(mar = c(5, 0, 4, 4))
  plot(NULL, xlim = c(0, n_items + 1), ylim = range(theta_hist$breaks),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n",
       main = "Item Thresholds")

  for (i in 1:n_items) {
    thresholds <- item_params[i, 2:5]
    points(rep(i, 4), thresholds, pch = c(1, 2, 3, 4), col = colors_gradient, cex = 1.5)
  }
  axis(1, at = 1:n_items, labels = item_names, las = 2, cex.axis = 0.7)
  axis(4)
  legend("topright", legend = c("b1", "b2", "b3", "b4"),
         pch = c(1, 2, 3, 4), col = colors_gradient, title = "Threshold")
}, error = function(e) {
  par(mfrow = c(1, 1))
  plot.new()
  text(0.5, 0.5, paste("Wright Map Error:", e$message), cex = 1)
})

dev.off()
cat("\nPlot 25 tersimpan: 25_wright_map.png\n")

# Alternatif Wright Map dengan ggplot
p25b <- ggplot() +
  # Threshold points
  geom_point(data = thresh_data, aes(x = Item, y = Value, color = Threshold, shape = Threshold),
             size = 3, alpha = 0.8) +
  # Person distribution sebagai violin
  geom_violin(data = data.frame(x = "Persons", y = theta_eap[, 1]),
              aes(x = x, y = y), fill = colors_main[1], alpha = 0.5, width = 0.5) +
  scale_color_brewer(palette = "Set1") +
  coord_flip() +
  labs(
    title = "Wright Map (Item-Person Map)",
    subtitle = "Distribusi person dan lokasi threshold item pada skala theta yang sama",
    x = "",
    y = expression(theta~"(Trait Level)")
  ) +
  theme_report() +
  theme(axis.text.y = element_text(size = 8))

ggsave(paste0(PLOT_DIR, "/25b_wright_map_ggplot.png"), p25b,
       width = PLOT_WIDTH * 1.2, height = PLOT_HEIGHT, dpi = PLOT_DPI)
cat("Plot 25b tersimpan: 25b_wright_map_ggplot.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 15. EXPECTED SCORE CURVE
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("EXPECTED SCORE CURVES\n")
cat("=" , rep("=", 70), "\n", sep = "")

# --- PLOT 26: Expected Score Curve (Test) ---
png(paste0(PLOT_DIR, "/26_expected_score_curve.png"),
    width = PLOT_WIDTH, height = PLOT_HEIGHT, units = "in", res = PLOT_DPI)
p <- plot(grm_model, type = "score", theta_lim = c(-4, 4),
     main = "Test Expected Score Curve")
print(p)
dev.off()
cat("\nPlot 26 tersimpan: 26_expected_score_curve.png\n")

# --- PLOT 27: Expected Score per Item ---
png(paste0(PLOT_DIR, "/27_expected_score_items.png"),
    width = PLOT_WIDTH * 1.5, height = PLOT_HEIGHT * 1.5, units = "in", res = PLOT_DPI)
p <- plot(grm_model, type = "itemscore", which.items = 1:n_items,
     theta_lim = c(-4, 4), facet_items = TRUE,
     main = "Expected Score per Item")
print(p)
dev.off()
cat("Plot 27 tersimpan: 27_expected_score_items.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 16. LOCAL INDEPENDENCE CHECK
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("UJI LOCAL INDEPENDENCE\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Residual correlation matrix
residuals_grm <- residuals(grm_model, type = "Q3")

cat("\nQ3 Residual Correlations:\n")
cat("Kriteria: |Q3| > 0.20 mengindikasikan local dependence\n\n")

# Identifikasi pasangan item dengan LD
q3_matrix <- residuals_grm
diag(q3_matrix) <- NA
q3_values <- q3_matrix[lower.tri(q3_matrix)]
q3_pairs <- which(abs(q3_matrix) > 0.20, arr.ind = TRUE)
q3_pairs <- q3_pairs[q3_pairs[, 1] < q3_pairs[, 2], ]

if (nrow(q3_pairs) > 0) {
  cat("Pasangan item dengan potensial Local Dependence (|Q3| > 0.20):\n")
  for (i in 1:nrow(q3_pairs)) {
    cat(paste0("  ", item_names[q3_pairs[i, 1]], " - ", item_names[q3_pairs[i, 2]],
               " : Q3 = ", round(q3_matrix[q3_pairs[i, 1], q3_pairs[i, 2]], 3), "\n"))
  }
} else {
  cat("Tidak ada pasangan item dengan Q3 > 0.20. Asumsi local independence terpenuhi.\n")
}

# Simpan residual matrix
write.csv(round(residuals_grm, 3), paste0(OUTPUT_DIR, "/19_q3_residuals.csv"))

# --- PLOT 28: Q3 Residual Heatmap ---
png(paste0(PLOT_DIR, "/28_q3_residual_heatmap.png"),
    width = PLOT_WIDTH, height = PLOT_HEIGHT, units = "in", res = PLOT_DPI)
par(mar = c(1, 1, 2, 1))  # Set proper margins
corrplot(residuals_grm, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.6,
         col = colorRampPalette(c("#3498db", "white", "#e74c3c"))(200),
         title = "Q3 Residual Correlations (Local Independence Check)",
         mar = c(0, 0, 2, 0))
dev.off()
cat("\nPlot 28 tersimpan: 28_q3_residual_heatmap.png\n")

cat("\n")

# -----------------------------------------------------------------------------
# 17. DIFFERENTIAL ITEM FUNCTIONING (DIF) - OPTIONAL
# -----------------------------------------------------------------------------

# Bagian ini opsional, memerlukan variabel grouping
# Jika ada variabel grouping di data, bisa diaktifkan

cat("=" , rep("=", 70), "\n", sep = "")
cat("DIFFERENTIAL ITEM FUNCTIONING (DIF)\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("\nNOTE: Analisis DIF memerlukan variabel grouping.\n")
cat("Jika tersedia, tambahkan kode analisis DIF di bagian ini.\n")

cat("\n")

# -----------------------------------------------------------------------------
# 18. RINGKASAN KOMPREHENSIF
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("RINGKASAN ANALISIS KOMPREHENSIF\n")
cat("=" , rep("=", 70), "\n", sep = "")

# -----------------------------------------------------------------------------
# NOTE: marginal_rel dan rel_cat sudah dihitung di section Reliabilitas (section 12)
# Section ini tidak perlu menghitung ulang
# -----------------------------------------------------------------------------

# Verifikasi bahwa variabel sudah ada dan valid
if (!exists("marginal_rel") || is.na(marginal_rel)) {
  cat("  [Warning: marginal_rel belum terdefinisi, menghitung ulang...]\n")
  var_theta <- var(theta_eap[, 1])
  mean_sem_sq <- mean(theta_eap[, 2]^2)
  marginal_rel <- var_theta / (var_theta + mean_sem_sq)
}

# Verifikasi rel_cat (berdasarkan omega_total, bukan marginal_rel)
if (!exists("rel_cat") || is.na(rel_cat)) {
  cat("  [Warning: rel_cat belum terdefinisi, membuat kategori...]\n")
  rel_cat <- ifelse(is.na(omega_result$omega.tot), "Tidak tersedia",
                    ifelse(omega_result$omega.tot < 0.60, "Tidak dapat diterima",
                           ifelse(omega_result$omega.tot < 0.70, "Kurang baik",
                                  ifelse(omega_result$omega.tot < 0.80, "Dapat diterima",
                                         ifelse(omega_result$omega.tot < 0.90, "Baik", "Sangat Baik")))))
}

# Print untuk verifikasi
cat(paste0("\n[Verifikasi] Marginal Reliability: ", round(marginal_rel, 3), "\n"))
cat(paste0("[Verifikasi] Kategori Reliabilitas: ", rel_cat, "\n\n"))

# Buat ringkasan
summary_report <- list(
  # Info Dasar
  data_info = list(
    n_items = n_items,
    n_respondents = n_respondents,
    scale_range = paste0(MIN_SCALE, "-", MAX_SCALE)
  ),

  # Statistik Deskriptif
  descriptive = list(
    mean_total = round(mean(total_scores), 2),
    sd_total = round(sd(total_scores), 2),
    min_item_mean = round(min(desc_stats$Mean), 2),
    max_item_mean = round(max(desc_stats$Mean), 2)
  ),

  # Unidimensionalitas
  unidimensionality = list(
    suggested_factors = fa_parallel$nfact,
    variance_explained = round(efa_result$Vaccounted[2, 1] * 100, 2),
    cfa_cfi = round(cfa_fitm["cfi"], 3),
    cfa_rmsea = round(cfa_fitm["rmsea"], 3),
    conclusion = ifelse(unidim_ok, "Terpenuhi", "Perlu perhatian")
  ),

  # Model Fit
  model_fit = list(
    m2_rmsea = round(rmsea_val, 4),
    m2_cfi = round(cfi_val, 3),
    m2_srmsr = round(srmsr_val, 4),
    conclusion = ifelse(model_fit_ok, "Model fit baik", "Model fit perlu perhatian")
  ),

  # Parameter Item
  item_params = list(
    mean_discrimination = round(mean(item_params[, 1]), 2),
    sd_discrimination = round(sd(item_params[, 1]), 2),
    min_discrimination = round(min(item_params[, 1]), 2),
    max_discrimination = round(max(item_params[, 1]), 2)
  ),

  # Item Fit
  item_fit = list(
    n_misfit = if("Fit_Status" %in% names(item_fit_df)) sum(item_fit_df$Fit_Status == "Check") else 0,
    misfit_items = if("Fit_Status" %in% names(item_fit_df))
      paste(item_fit_df$Item[item_fit_df$Fit_Status == "Check"], collapse = ", ") else ""
  ),

  # Reliabilitas
  reliability = list(
    omega_total = round(omega_result$omega.tot, 3),
    omega_hierarchical = round(omega_result$omega_h, 3),
    marginal_reliability = round(marginal_rel, 3),
    interpretation = rel_cat
  ),

  # Test Information
  test_info = list(
    max_information = round(max(test_info), 2),
    theta_at_max = round(theta_range[which.max(test_info)], 2),
    info_range = paste0("[", round(min(theta_range[test_info > max(test_info)*0.5]), 2),
                        ", ", round(max(theta_range[test_info > max(test_info)*0.5]), 2), "]")
  ),

  # Local Independence
  local_independence = list(
    n_ld_pairs = nrow(q3_pairs),
    conclusion = ifelse(nrow(q3_pairs) == 0, "Terpenuhi", "Perlu perhatian")
  ),

  # Penormaan
  norming = list(
    theta_range = paste0(round(min(theta_eap[, 1]), 2), " - ", round(max(theta_eap[, 1]), 2)),
    tscore_range = paste0(round(min(t_scores), 1), " - ", round(max(t_scores), 1)),
    percentile_range = paste0(round(min(percentile_ranks), 1), " - ", round(max(percentile_ranks), 1))
  )
)

# Print ringkasan
cat("\n1. INFORMASI DATA\n")
cat(paste0("   - Jumlah item: ", summary_report$data_info$n_items, "\n"))
cat(paste0("   - Jumlah responden: ", summary_report$data_info$n_respondents, "\n"))
cat(paste0("   - Rentang skala: ", summary_report$data_info$scale_range, "\n"))

cat("\n2. STATISTIK DESKRIPTIF\n")
cat(paste0("   - Mean skor total: ", summary_report$descriptive$mean_total, "\n"))
cat(paste0("   - SD skor total: ", summary_report$descriptive$sd_total, "\n"))
cat(paste0("   - Rentang mean item: ", summary_report$descriptive$min_item_mean, " - ",
           summary_report$descriptive$max_item_mean, "\n"))

cat("\n3. UNIDIMENSIONALITAS\n")
cat(paste0("   - Faktor disarankan (PA): ", summary_report$unidimensionality$suggested_factors, "\n"))
cat(paste0("   - Variance explained: ", summary_report$unidimensionality$variance_explained, "%\n"))
cat(paste0("   - CFA CFI: ", summary_report$unidimensionality$cfa_cfi, "\n"))
cat(paste0("   - CFA RMSEA: ", summary_report$unidimensionality$cfa_rmsea, "\n"))
cat(paste0("   - Kesimpulan: ", summary_report$unidimensionality$conclusion, "\n"))

cat("\n4. MODEL FIT GRM\n")
cat(paste0("   - M2 RMSEA: ", summary_report$model_fit$m2_rmsea, "\n"))
cat(paste0("   - M2 CFI: ", summary_report$model_fit$m2_cfi, "\n"))
cat(paste0("   - SRMSR: ", summary_report$model_fit$m2_srmsr, "\n"))
cat(paste0("   - Kesimpulan: ", summary_report$model_fit$conclusion, "\n"))

cat("\n5. PARAMETER DISKRIMINASI\n")
cat(paste0("   - Mean: ", summary_report$item_params$mean_discrimination, "\n"))
cat(paste0("   - SD: ", summary_report$item_params$sd_discrimination, "\n"))
cat(paste0("   - Range: ", summary_report$item_params$min_discrimination, " - ",
           summary_report$item_params$max_discrimination, "\n"))

cat("\n6. ITEM FIT\n")
cat(paste0("   - Jumlah item misfit: ", summary_report$item_fit$n_misfit, "\n"))
cat(paste0("   - Item misfit: ", ifelse(summary_report$item_fit$misfit_items == "",
                                        "Tidak ada", summary_report$item_fit$misfit_items), "\n"))

cat("\n7. RELIABILITAS\n")
cat(paste0("   - Omega Total: ", summary_report$reliability$omega_total, "\n"))
cat(paste0("   - Omega Hierarchical: ", summary_report$reliability$omega_hierarchical, "\n"))
cat(paste0("   - Marginal Reliability: ", summary_report$reliability$marginal_reliability, "\n"))
cat(paste0("   - Interpretasi: ", summary_report$reliability$interpretation, "\n"))

cat("\n8. TEST INFORMATION\n")
cat(paste0("   - Max Information: ", summary_report$test_info$max_information, "\n"))
cat(paste0("   - Theta at Max: ", summary_report$test_info$theta_at_max, "\n"))
cat(paste0("   - Rentang info tinggi: ", summary_report$test_info$info_range, "\n"))

cat("\n9. LOCAL INDEPENDENCE\n")
cat(paste0("   - Pasangan dengan LD: ", summary_report$local_independence$n_ld_pairs, "\n"))
cat(paste0("   - Kesimpulan: ", summary_report$local_independence$conclusion, "\n"))

cat("\n10. PENORMAAN\n")
cat(paste0("   - Rentang Theta: ", summary_report$norming$theta_range, "\n"))
cat(paste0("   - Rentang T-Score: ", summary_report$norming$tscore_range, "\n"))
cat(paste0("   - Rentang Percentile: ", summary_report$norming$percentile_range, "\n"))

# Simpan ringkasan
saveRDS(summary_report, paste0(OUTPUT_DIR, "/20_summary_report.rds"))

cat("\n")

# -----------------------------------------------------------------------------
# Export Resume Hasil Analisis ke File TXT
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("EXPORT RESUME HASIL ANALISIS\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Buat file teks resume
resume_file <- paste0(OUTPUT_DIR, "/00_RESUME_HASIL_ANALISIS.txt")

# Fungsi untuk membuat garis pembatas
make_line <- function(char = "=", length = 80) {
  paste(rep(char, length), collapse = "")
}

# Tulis resume ke file
cat("Membuat file resume hasil analisis...\n")
sink(resume_file)

# Header Institusi dan Identitas Akademis
cat(make_line("="), "\n")
cat(make_line("="), "\n")
cat("                                                                                \n")
cat("              LAPORAN HASIL ANALISIS PROPERTI PSIKOMETRIK INSTRUMEN            \n")
cat("               DENGAN PENDEKATAN ITEM RESPONSE THEORY (IRT)                    \n")
cat("                      GRADED RESPONSE MODEL (GRM)                              \n")
cat("                                                                                \n")
cat(make_line("="), "\n")
cat(make_line("="), "\n\n")

# Informasi Akademis
cat(make_line("-"), "\n")
cat("INFORMASI AKADEMIS\n")
cat(make_line("-"), "\n")
cat("Program Studi     : Psikologi\n")
cat("Universitas       : Universitas Muhammadiyah Bandung\n")
cat("Mata Kuliah       : Psikometrika Lanjut\n")
cat("Dosen Pengampu    : Isman Rahmani Yusron, M.A\n")
cat("Tanggal Analisis  :", format(Sys.time(), "%d %B %Y, %H:%M:%S WIB"), "\n")
cat(make_line("-"), "\n\n")

# Informasi Teknis
cat(make_line("-"), "\n")
cat("INFORMASI TEKNIS\n")
cat(make_line("-"), "\n")
cat("File Data         :", DATA_FILE, "\n")
cat("Metode Analisis   : Item Response Theory (IRT)\n")
cat("Model IRT         : Graded Response Model (GRM)\n")
cat("Software          : R Programming Language\n")
cat("Package Utama     : mirt, psych, lavaan\n")
cat(make_line("-"), "\n\n\n")

cat(make_line("="), "\n")
cat("                          RINGKASAN EKSEKUTIF                                  \n")
cat(make_line("="), "\n\n")

cat("Laporan ini menyajikan hasil analisis komprehensif terhadap properti psikometrik\n")
cat("instrumen pengukuran menggunakan pendekatan Item Response Theory (IRT) dengan\n")
cat("Graded Response Model (GRM). Analisis mencakup evaluasi unidimensionalitas,\n")
cat("reliabilitas, parameter item, ketepatan model, dan norma interpretasi skor.\n\n")

cat("Instrumen yang dianalisis terdiri dari", summary_report$data_info$n_items, "item dengan skala Likert\n")
cat(summary_report$data_info$scale_range, "yang diujikan kepada", summary_report$data_info$n_respondents, "responden.\n\n")

cat(make_line("-"), "\n\n\n")

cat(make_line("="), "\n")
cat("                     BAGIAN I: DESKRIPSI DATA DAN ASUMSI                       \n")
cat(make_line("="), "\n\n")

# 1. INFORMASI DATA
cat(make_line("-"), "\n")
cat("1. INFORMASI DATA\n")
cat(make_line("-"), "\n")
cat(sprintf("   Jumlah Item       : %d item\n", summary_report$data_info$n_items))
cat(sprintf("   Jumlah Responden  : %d responden\n", summary_report$data_info$n_respondents))
cat(sprintf("   Rentang Skala     : %s\n", summary_report$data_info$scale_range))
cat("\n")

# 2. STATISTIK DESKRIPTIF
cat(make_line("-"), "\n")
cat("2. STATISTIK DESKRIPTIF\n")
cat(make_line("-"), "\n")
cat(sprintf("   Mean Skor Total   : %.2f\n", summary_report$descriptive$mean_total))
cat(sprintf("   SD Skor Total     : %.2f\n", summary_report$descriptive$sd_total))
cat(sprintf("   Rentang Mean Item : %.2f - %.2f\n",
    summary_report$descriptive$min_item_mean,
    summary_report$descriptive$max_item_mean))
cat("\n")
cat("   Interpretasi:\n")
cat(sprintf("   - Rata-rata responden mendapat skor %.2f dari maksimal %d\n",
    summary_report$descriptive$mean_total, summary_report$data_info$n_items * MAX_SCALE))
cat(sprintf("   - Variabilitas skor cukup (SD = %.2f)\n", summary_report$descriptive$sd_total))
cat("\n")

# 3. KORELASI ANTAR ITEM
cat(make_line("-"), "\n")
cat("3. KORELASI ANTAR ITEM\n")
cat(make_line("-"), "\n")
cat(sprintf("   Range Korelasi    : %.3f - %.3f\n", min(cor_values), max(cor_values)))
cat(sprintf("   Mean Korelasi     : %.3f\n", mean(cor_values)))
cat(sprintf("   SD Korelasi       : %.3f\n", sd(cor_values)))
cat("\n")
cat("   Item-Total Correlation:\n")
for (i in 1:min(5, length(item_total_cor))) {
  cat(sprintf("   - %-15s: r = %.3f\n", names(item_total_cor)[i], item_total_cor[i]))
}
if (length(item_total_cor) > 5) {
  cat(sprintf("   ... dan %d item lainnya (lihat file CSV)\n", length(item_total_cor) - 5))
}
cat("\n")
cat("   Interpretasi:\n")
cat("   - Korelasi positif menunjukkan konsistensi internal yang baik\n")
cat("   - Item dengan r < 0.30 perlu dievaluasi lebih lanjut\n")
cat("\n")

# 4. UJI ASUMSI UNIDIMENSIONALITAS
cat(make_line("-"), "\n")
cat("4. UJI ASUMSI UNIDIMENSIONALITAS\n")
cat(make_line("-"), "\n")
cat("   Parallel Analysis:\n")
cat(sprintf("   - Faktor Disarankan   : %d faktor\n", summary_report$unidimensionality$suggested_factors))
cat(sprintf("   - Variance Explained  : %.2f%%\n", summary_report$unidimensionality$variance_explained))
cat("\n")
cat("   Confirmatory Factor Analysis (CFA):\n")
cat(sprintf("   - CFI                 : %.3f (kriteria >= 0.90)\n", summary_report$unidimensionality$cfa_cfi))
cat(sprintf("   - RMSEA               : %.3f (kriteria < 0.08)\n", summary_report$unidimensionality$cfa_rmsea))
cat("\n")
cat(sprintf("   Kesimpulan            : %s\n", summary_report$unidimensionality$conclusion))
cat("\n")
cat("   Interpretasi:\n")
if (summary_report$unidimensionality$conclusion == "Terpenuhi") {
  cat("   - Asumsi unidimensionalitas terpenuhi\n")
  cat("   - Data cocok untuk analisis IRT dengan model unidimensional\n")
} else {
  cat("   - Asumsi unidimensionalitas perlu perhatian\n")
  cat("   - Pertimbangkan model multidimensional atau revisi instrumen\n")
}
cat("\n")

# 5. RELIABILITAS
cat(make_line("-"), "\n")
cat("5. RELIABILITAS INSTRUMEN\n")
cat(make_line("-"), "\n")
cat(sprintf("   Omega Total (ω_t)         : %.3f\n", summary_report$reliability$omega_total))
cat(sprintf("   Omega Hierarchical (ω_h)  : %.3f\n", summary_report$reliability$omega_hierarchical))
cat(sprintf("   Marginal Reliability (IRT): %.3f\n", summary_report$reliability$marginal_reliability))
cat("\n")
cat(sprintf("   Kategori                  : %s\n", summary_report$reliability$interpretation))
cat("\n")
cat("   Kriteria Interpretasi:\n")
cat("   - ω < 0.60  : Tidak dapat diterima\n")
cat("   - 0.60-0.70 : Kurang baik\n")
cat("   - 0.70-0.80 : Dapat diterima\n")
cat("   - 0.80-0.90 : Baik\n")
cat("   - ω > 0.90  : Sangat Baik (Istimewa)\n")
cat("\n")
cat("   Interpretasi:\n")
if (summary_report$reliability$omega_total >= 0.80) {
  cat("   - Instrumen memiliki reliabilitas yang baik\n")
  cat("   - Konsistensi internal tinggi, hasil pengukuran dapat dipercaya\n")
} else if (summary_report$reliability$omega_total >= 0.70) {
  cat("   - Instrumen memiliki reliabilitas yang dapat diterima\n")
  cat("   - Masih ada ruang untuk perbaikan\n")
} else {
  cat("   - Reliabilitas instrumen perlu ditingkatkan\n")
  cat("   - Pertimbangkan revisi item atau penambahan item\n")
}
cat("\n")

# 6. MODEL FIT GRM
cat(make_line("-"), "\n")
cat("6. KETEPATAN MODEL (MODEL FIT) - GRM\n")
cat(make_line("-"), "\n")
cat(sprintf("   M2 RMSEA  : %.4f (kriteria < 0.08)\n", summary_report$model_fit$m2_rmsea))
cat(sprintf("   M2 CFI    : %.3f (kriteria >= 0.95)\n", summary_report$model_fit$m2_cfi))
cat(sprintf("   SRMSR     : %.4f (kriteria < 0.08)\n", summary_report$model_fit$m2_srmsr))
cat("\n")
cat(sprintf("   Kesimpulan: %s\n", summary_report$model_fit$conclusion))
cat("\n")
cat("   Interpretasi:\n")
if (summary_report$model_fit$conclusion == "Model fit baik") {
  cat("   - Model GRM sesuai dengan data empiris\n")
  cat("   - Estimasi parameter dapat dipercaya\n")
} else {
  cat("   - Model fit perlu perhatian\n")
  cat("   - Evaluasi item-level fit untuk identifikasi masalah spesifik\n")
}
cat("\n")

# 7. PARAMETER ITEM - DISKRIMINASI
cat(make_line("-"), "\n")
cat("7. PARAMETER DISKRIMINASI ITEM (a)\n")
cat(make_line("-"), "\n")
cat(sprintf("   Mean  : %.2f\n", summary_report$item_params$mean_discrimination))
cat(sprintf("   SD    : %.2f\n", summary_report$item_params$sd_discrimination))
cat(sprintf("   Range : %.2f - %.2f\n",
    summary_report$item_params$min_discrimination,
    summary_report$item_params$max_discrimination))
cat("\n")
cat("   Kriteria Interpretasi:\n")
cat("   - a < 0.5  : Sangat Rendah (item lemah)\n")
cat("   - 0.5-0.9  : Rendah\n")
cat("   - 0.9-1.3  : Sedang\n")
cat("   - 1.3-1.7  : Tinggi\n")
cat("   - a > 1.7  : Sangat Tinggi (item sangat diskriminatif)\n")
cat("\n")
cat("   Distribusi Kategori Diskriminasi:\n")
discrim_table <- table(discrim_cat)
for (i in 1:length(discrim_table)) {
  cat(sprintf("   - %-15s: %d item\n", names(discrim_table)[i], discrim_table[i]))
}
cat("\n")
cat("   Top 5 Item Diskriminasi Tertinggi:\n")
top_discrim <- head(param_df[order(-param_df$a), ], 5)
for (i in 1:nrow(top_discrim)) {
  cat(sprintf("   %d. %-15s: a = %.3f\n", i, top_discrim$Item[i], top_discrim$a[i]))
}
cat("\n")
cat("   Interpretasi:\n")
cat("   - Parameter 'a' menunjukkan kemampuan item membedakan responden\n")
cat("   - Item dengan a tinggi lebih berguna untuk pengukuran yang presisi\n")
if (summary_report$item_params$mean_discrimination >= 1.0) {
  cat("   - Secara rata-rata, item memiliki daya diskriminasi yang baik\n")
} else {
  cat("   - Pertimbangkan revisi item dengan diskriminasi rendah\n")
}
cat("\n")

# 8. ITEM FIT
cat(make_line("-"), "\n")
cat("8. KESESUAIAN ITEM (ITEM FIT)\n")
cat(make_line("-"), "\n")
cat(sprintf("   Jumlah Item Misfit: %d item\n", summary_report$item_fit$n_misfit))
if (summary_report$item_fit$misfit_items != "") {
  cat(sprintf("   Item Misfit       : %s\n", summary_report$item_fit$misfit_items))
} else {
  cat("   Item Misfit       : Tidak ada\n")
}
cat("\n")
cat("   Interpretasi:\n")
if (summary_report$item_fit$n_misfit == 0) {
  cat("   - Semua item fit dengan model GRM\n")
  cat("   - Tidak ada item yang perlu revisi dari perspektif fit\n")
} else {
  cat("   - Item misfit menunjukkan pola respons yang tidak sesuai model\n")
  cat("   - Evaluasi lebih lanjut diperlukan untuk item tersebut\n")
  cat("   - Pertimbangkan revisi atau penghapusan item misfit\n")
}
cat("\n")

# 9. TEST INFORMATION FUNCTION
cat(make_line("-"), "\n")
cat("9. FUNGSI INFORMASI TES (TEST INFORMATION)\n")
cat(make_line("-"), "\n")
cat(sprintf("   Informasi Maksimum : %.2f\n", summary_report$test_info$max_information))
cat(sprintf("   Theta at Max Info  : %.2f\n", summary_report$test_info$theta_at_max))
cat(sprintf("   Rentang Info Tinggi: %s\n", summary_report$test_info$info_range))
cat("\n")
cat("   Interpretasi:\n")
cat(sprintf("   - Tes paling informatif pada theta = %.2f\n", summary_report$test_info$theta_at_max))
if (abs(summary_report$test_info$theta_at_max) <= 1.0) {
  cat("   - Tes cocok untuk mengukur kemampuan rata-rata populasi\n")
} else if (summary_report$test_info$theta_at_max > 1.0) {
  cat("   - Tes lebih cocok untuk mengukur individu dengan trait tinggi\n")
} else {
  cat("   - Tes lebih cocok untuk mengukur individu dengan trait rendah\n")
}
cat(sprintf("   - Informasi tinggi tersebar di rentang %s\n", summary_report$test_info$info_range))
cat("\n")

# 10. LOCAL INDEPENDENCE
cat(make_line("-"), "\n")
cat("10. ASUMSI LOCAL INDEPENDENCE\n")
cat(make_line("-"), "\n")
cat(sprintf("   Pasangan dengan LD: %d pasangan\n", summary_report$local_independence$n_ld_pairs))
cat(sprintf("   Kesimpulan        : %s\n", summary_report$local_independence$conclusion))
cat("\n")
cat("   Interpretasi:\n")
if (summary_report$local_independence$n_ld_pairs == 0) {
  cat("   - Asumsi local independence terpenuhi\n")
  cat("   - Tidak ada item yang saling tergantung setelah mengontrol trait\n")
} else {
  cat("   - Ada pasangan item dengan local dependence (Q3 > 0.20)\n")
  cat("   - Evaluasi apakah item mengukur subfaktor yang sama\n")
  cat("   - Pertimbangkan testlet model atau penghapusan salah satu item\n")
}
cat("\n")

# 11. PENORMAAN DAN INTERPRETASI SKOR
cat(make_line("-"), "\n")
cat("11. PENORMAAN DAN INTERPRETASI SKOR\n")
cat(make_line("-"), "\n")
cat(sprintf("   Rentang Theta      : %s\n", summary_report$norming$theta_range))
cat(sprintf("   Rentang T-Score    : %s\n", summary_report$norming$tscore_range))
cat(sprintf("   Rentang Percentile : %s\n", summary_report$norming$percentile_range))
cat("\n")
cat("   Interpretasi T-Score:\n")
cat("   - T-Score < 30  : Sangat Rendah\n")
cat("   - T-Score 30-40 : Rendah\n")
cat("   - T-Score 40-60 : Rata-rata\n")
cat("   - T-Score 60-70 : Tinggi\n")
cat("   - T-Score > 70  : Sangat Tinggi\n")
cat("\n")

# 12. RINGKASAN KESIMPULAN UMUM
cat(make_line("="), "\n")
cat("12. RINGKASAN KESIMPULAN UMUM\n")
cat(make_line("="), "\n\n")

# Evaluasi keseluruhan
cat("EVALUASI KUALITAS INSTRUMEN:\n\n")

# Status unidimensionalitas
cat(sprintf("✓ Unidimensionalitas    : %s\n",
    ifelse(summary_report$unidimensionality$conclusion == "Terpenuhi", "TERPENUHI", "PERLU PERHATIAN")))

# Status reliabilitas
cat(sprintf("✓ Reliabilitas          : %s (ω = %.3f)\n",
    summary_report$reliability$interpretation,
    summary_report$reliability$omega_total))

# Status model fit
cat(sprintf("✓ Model Fit GRM         : %s\n",
    toupper(summary_report$model_fit$conclusion)))

# Status item fit
cat(sprintf("✓ Item Fit              : %d/%d item fit dengan model\n",
    summary_report$data_info$n_items - summary_report$item_fit$n_misfit,
    summary_report$data_info$n_items))

# Status local independence
cat(sprintf("✓ Local Independence    : %s\n",
    toupper(summary_report$local_independence$conclusion)))

cat("\n")

# Rekomendasi
cat(make_line("-"), "\n")
cat("REKOMENDASI:\n")
cat(make_line("-"), "\n\n")

recommendations <- c()

if (summary_report$unidimensionality$conclusion != "Terpenuhi") {
  recommendations <- c(recommendations,
    "1. Evaluasi struktur faktor instrumen - pertimbangkan model multidimensional")
}

if (summary_report$reliability$omega_total < 0.80) {
  recommendations <- c(recommendations,
    sprintf("%d. Tingkatkan reliabilitas dengan menambah item atau merevisi item lemah",
            length(recommendations) + 1))
}

if (summary_report$item_fit$n_misfit > 0) {
  recommendations <- c(recommendations,
    sprintf("%d. Evaluasi dan revisi item misfit: %s",
            length(recommendations) + 1,
            summary_report$item_fit$misfit_items))
}

if (summary_report$item_params$min_discrimination < 0.5) {
  recommendations <- c(recommendations,
    sprintf("%d. Revisi item dengan diskriminasi sangat rendah (a < 0.5)",
            length(recommendations) + 1))
}

if (summary_report$local_independence$n_ld_pairs > 0) {
  recommendations <- c(recommendations,
    sprintf("%d. Evaluasi pasangan item dengan local dependence",
            length(recommendations) + 1))
}

if (length(recommendations) == 0) {
  cat("✓ Instrumen memiliki kualitas psikometrik yang baik\n")
  cat("✓ Tidak ada rekomendasi perbaikan major\n")
  cat("✓ Instrumen siap digunakan untuk pengukuran\n")
} else {
  for (rec in recommendations) {
    cat(rec, "\n")
  }
}

cat("\n")

# Kekuatan instrumen
cat(make_line("-"), "\n")
cat("KEKUATAN INSTRUMEN:\n")
cat(make_line("-"), "\n\n")

strengths <- c()

if (summary_report$reliability$omega_total >= 0.80) {
  strengths <- c(strengths, "✓ Reliabilitas baik/sangat baik")
}

if (summary_report$unidimensionality$conclusion == "Terpenuhi") {
  strengths <- c(strengths, "✓ Struktur unidimensional terkonfirmasi")
}

if (summary_report$model_fit$conclusion == "Model fit baik") {
  strengths <- c(strengths, "✓ Model GRM fit dengan data")
}

if (summary_report$item_fit$n_misfit == 0) {
  strengths <- c(strengths, "✓ Semua item fit dengan model")
}

if (summary_report$local_independence$conclusion == "Terpenuhi") {
  strengths <- c(strengths, "✓ Asumsi local independence terpenuhi")
}

if (summary_report$item_params$mean_discrimination >= 1.0) {
  strengths <- c(strengths, "✓ Daya diskriminasi item rata-rata baik")
}

for (strength in strengths) {
  cat(strength, "\n")
}

cat("\n\n")
cat(make_line("="), "\n")
cat("                               PENUTUP                                          \n")
cat(make_line("="), "\n\n")

# Kesimpulan Akhir
cat("KESIMPULAN AKHIR:\n\n")

cat("Berdasarkan analisis komprehensif yang telah dilakukan, instrumen pengukuran\n")
cat("ini menunjukkan karakteristik psikometrik yang dapat dievaluasi dari berbagai\n")
cat("aspek Item Response Theory. Hasil analisis memberikan informasi penting tentang\n")
cat("kualitas item, reliabilitas pengukuran, dan informasi yang diberikan instrumen\n")
cat("pada berbagai tingkat trait yang diukur.\n\n")

# Implikasi Praktis
cat(make_line("-"), "\n")
cat("IMPLIKASI PRAKTIS:\n")
cat(make_line("-"), "\n\n")

cat("1. UNTUK PENGEMBANGAN INSTRUMEN:\n")
cat("   - Item dengan diskriminasi rendah (a < 0.5) perlu direvisi atau diganti\n")
cat("   - Item misfit menunjukkan pola respons yang tidak konsisten dengan model\n")
cat("   - Pertimbangkan penambahan item untuk meningkatkan reliabilitas\n\n")

cat("2. UNTUK PENGGUNAAN PRAKTIS:\n")
cat("   - Instrumen paling informatif pada rentang theta tertentu\n")
cat("   - Gunakan norma T-Score dan Percentile untuk interpretasi skor\n")
cat("   - Pertimbangkan Conditional SEM untuk estimasi presisi pengukuran\n\n")

cat("3. UNTUK PENELITIAN LANJUTAN:\n")
cat("   - Validasi hasil dengan sampel yang berbeda\n")
cat("   - Evaluasi invariansi pengukuran antar kelompok\n")
cat("   - Pertimbangkan analisis Differential Item Functioning (DIF)\n\n")

# Keterbatasan
cat(make_line("-"), "\n")
cat("KETERBATASAN STUDI:\n")
cat(make_line("-"), "\n\n")

cat("1. Ukuran sampel dan representativitas perlu dipertimbangkan\n")
cat("2. Asumsi unidimensionalitas dan local independence perlu dipenuhi\n")
cat("3. Model GRM mengasumsikan ordered categories yang konsisten\n")
cat("4. Hasil berlaku untuk populasi dengan karakteristik serupa dengan sampel\n\n")

# Referensi Teoritis
cat(make_line("-"), "\n")
cat("REFERENSI TEORITIS:\n")
cat(make_line("-"), "\n\n")

cat("Analisis ini mengacu pada prinsip-prinsip Item Response Theory (IRT):\n\n")
cat("- Samejima, F. (1969). Estimation of latent ability using a response pattern\n")
cat("  of graded scores. Psychometrika Monograph Supplement.\n\n")
cat("- Embretson, S. E., & Reise, S. P. (2000). Item Response Theory for\n")
cat("  Psychologists. Lawrence Erlbaum Associates.\n\n")
cat("- Chalmers, R. P. (2012). mirt: A Multidimensional Item Response Theory\n")
cat("  Package for the R Environment. Journal of Statistical Software.\n\n")

# Informasi Tambahan
cat(make_line("="), "\n")
cat("INFORMASI TAMBAHAN\n")
cat(make_line("="), "\n\n")

cat("Dokumentasi Lengkap:\n")
cat("- Laporan lengkap tersedia dalam format HTML interaktif\n")
cat("- Data numerik tersimpan dalam file CSV di folder output\n")
cat("- Visualisasi tersimpan sebagai file PNG beresolusi tinggi (300 DPI)\n\n")

cat("Lokasi File Output:\n")
cat("- Direktori        :", OUTPUT_DIR, "\n")
cat("- Plot             :", PLOT_DIR, "\n")
cat("- File CSV         : Lihat file bernomor 01-20_*.csv\n")
cat("- Laporan HTML     : laporan_analisis_psikometrik.html\n\n")

cat(make_line("="), "\n")
cat(make_line("="), "\n")
cat("                        AKHIR LAPORAN HASIL ANALISIS                            \n")
cat("                    Program Studi Psikologi - UMB                               \n")
cat("                      Psikometrika Lanjut - 2024                                \n")
cat(make_line("="), "\n")
cat(make_line("="), "\n\n")

cat("Laporan dibuat otomatis pada:", format(Sys.time(), "%d %B %Y, %H:%M:%S WIB"), "\n")
cat("Disusun menggunakan: R Programming Language & mirt package\n")
cat("\n")
cat("Untuk pertanyaan lebih lanjut, hubungi dosen pengampu:\n")
cat("Isman Rahmani Yusron, M.A\n")
cat("Program Studi Psikologi\n")
cat("Universitas Muhammadiyah Bandung\n")
cat("\n")

sink()

cat(paste0("Resume hasil analisis berhasil disimpan: ", resume_file, "\n"))
cat("\n")

# -----------------------------------------------------------------------------
# 19. GENERATE LAPORAN HTML
# -----------------------------------------------------------------------------

cat("=" , rep("=", 70), "\n", sep = "")
cat("GENERATE LAPORAN\n")
cat("=" , rep("=", 70), "\n", sep = "")

# Buat file RMarkdown untuk laporan
rmd_content <- '---
title: "LAPORAN ANALISIS PROPERTI PSIKOMETRIK INSTRUMEN"
subtitle: "Pendekatan Item Response Theory - Graded Response Model (IRT-GRM)"
author: |
  | **Program Studi Psikologi**
  | Universitas Muhammadiyah Bandung
  |
  | Mata Kuliah: Psikometrika Lanjut
  | Dosen Pengampu: Isman Rahmani Yusron, M.A
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: true
    toc_depth: 4
    theme: cosmo
    highlight: tango
    code_folding: hide
    df_print: paged
    number_sections: true
    css: |
      body {
        font-family: "Segoe UI", Arial, sans-serif;
        line-height: 1.6;
      }
      h1, h2, h3 {
        color: #2C3E50;
      }
      .main-container {
        max-width: 1400px;
      }
      .alert {
        padding: 15px;
        margin: 20px 0;
        border-radius: 5px;
      }
      .alert-info {
        background-color: #D9EDF7;
        border-left: 5px solid #31708F;
      }
      .alert-success {
        background-color: #DFF0D8;
        border-left: 5px solid #3C763D;
      }
      .alert-warning {
        background-color: #FCF8E3;
        border-left: 5px solid #8A6D3B;
      }
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 12, fig.height = 8, dpi = 300)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
```

```{r load-data}
summary_report <- readRDS("20_summary_report.rds")
```

<div class="alert alert-info">
<h3 style="margin-top:0">📋 Tentang Laporan Ini</h3>
Laporan ini merupakan hasil analisis komprehensif terhadap properti psikometrik instrumen pengukuran menggunakan pendekatan **Item Response Theory (IRT)** dengan **Graded Response Model (GRM)**. Analisis dilakukan sebagai bagian dari tugas mata kuliah Psikometrika Lanjut di Program Studi Psikologi, Universitas Muhammadiyah Bandung.
</div>

---

# Ringkasan Eksekutif

## Gambaran Umum

Analisis properti psikometrik telah dilakukan terhadap instrumen pengukuran psikologis dengan karakteristik sebagai berikut:

- **Jumlah Item**: `r summary_report$data_info$n_items` item
- **Jumlah Responden**: `r summary_report$data_info$n_respondents` responden
- **Skala Pengukuran**: Skala Likert `r summary_report$data_info$scale_range`
- **Metode Analisis**: Item Response Theory (IRT)
- **Model IRT**: Graded Response Model (GRM)

## Highlight Hasil Utama

<div class="alert alert-success">
<h4>✅ Kualitas Unidimensionalitas</h4>
<strong>Status:</strong> `r summary_report$unidimensionality$conclusion`<br>
<strong>CFI:</strong> `r summary_report$unidimensionality$cfa_cfi` |
<strong>RMSEA:</strong> `r summary_report$unidimensionality$cfa_rmsea`<br>
<strong>Interpretasi:</strong> `r if(summary_report$unidimensionality$conclusion == "Terpenuhi") "Instrumen mengukur satu konstruk laten (unidimensional)" else "Perlu evaluasi lebih lanjut"`
</div>

<div class="alert alert-`r if(summary_report$reliability$omega_total >= 0.80) "success" else if(summary_report$reliability$omega_total >= 0.70) "warning" else "danger"`">
<h4>`r if(summary_report$reliability$omega_total >= 0.80) "✅" else if(summary_report$reliability$omega_total >= 0.70) "⚠️" else "❌"` Reliabilitas Instrumen</h4>
<strong>Omega Total (ω):</strong> `r summary_report$reliability$omega_total`<br>
<strong>Kategori:</strong> `r summary_report$reliability$interpretation`<br>
<strong>Interpretasi:</strong> `r if(summary_report$reliability$omega_total >= 0.80) "Reliabilitas sangat baik - instrumen memberikan pengukuran yang konsisten" else if(summary_report$reliability$omega_total >= 0.70) "Reliabilitas dapat diterima - masih ada ruang untuk perbaikan" else "Reliabilitas kurang memadai - perlu perbaikan signifikan"`
</div>

<div class="alert alert-info">
<h4>📊 Karakteristik Pengukuran</h4>
<strong>Informasi Maksimum:</strong> `r summary_report$test_info$max_information` pada θ = `r summary_report$test_info$theta_at_max`<br>
<strong>Interpretasi:</strong> Instrumen paling presisi mengukur responden dengan tingkat trait sekitar `r summary_report$test_info$theta_at_max` (skala standar). Semakin tinggi informasi, semakin presisi pengukuran.
</div>

---

### Temuan Utama:

| Aspek | Hasil | Kesimpulan |
|:------|:------|:-----------|
| Unidimensionalitas | CFI = `r summary_report$unidimensionality$cfa_cfi`, RMSEA = `r summary_report$unidimensionality$cfa_rmsea` | `r summary_report$unidimensionality$conclusion` |
| Model Fit | RMSEA = `r summary_report$model_fit$m2_rmsea`, CFI = `r summary_report$model_fit$m2_cfi` | `r summary_report$model_fit$conclusion` |
| Reliabilitas | ω = `r summary_report$reliability$omega_total` | `r summary_report$reliability$interpretation` |
| Item Fit | Misfit = `r summary_report$item_fit$n_misfit` item | `r ifelse(summary_report$item_fit$n_misfit == 0, "Semua item fit", "Perlu review")` |
| Local Independence | LD pairs = `r summary_report$local_independence$n_ld_pairs` | `r summary_report$local_independence$conclusion` |

---

# 1. Deskripsi Data

## 1.1 Informasi Umum

- **Jumlah Item**: `r summary_report$data_info$n_items`
- **Jumlah Responden**: `r summary_report$data_info$n_respondents`
- **Skala Respons**: `r summary_report$data_info$scale_range` (Likert 5-point)

## 1.2 Statistik Deskriptif

```{r}
desc_stats <- read.csv("01_statistik_deskriptif.csv")
kable(desc_stats, digits = 3, caption = "Statistik Deskriptif per Item") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

## 1.3 Visualisasi Deskriptif

### Distribusi Skor Total
![](plots/01_distribusi_skor_total.png)

### Distribusi per Item
![](plots/02_boxplot_item.png)

### Mean Item
![](plots/03_mean_item_errorbar.png)

### Distribusi Kategori
![](plots/04_heatmap_distribusi_kategori.png)

---

# 2. Analisis Korelasi

## 2.1 Matriks Korelasi Inter-Item
![](plots/05_matriks_korelasi.png)

## 2.2 Korelasi Item-Total
```{r}
cor_summary <- read.csv("05_korelasi_item_total.csv")
kable(cor_summary, digits = 3, caption = "Korelasi Item-Total (Corrected)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

![](plots/06_korelasi_item_total.png)

---

# 3. Uji Unidimensionalitas

## 3.1 Parallel Analysis

![](plots/07_parallel_analysis.png)

**Hasil**: Parallel Analysis menyarankan **`r summary_report$unidimensionality$suggested_factors` faktor**.

## 3.2 Exploratory Factor Analysis (EFA)

```{r}
efa_loadings <- read.csv("07_efa_loadings.csv")
kable(efa_loadings, digits = 3, caption = "Factor Loadings (EFA Unidimensional)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

**Variance Explained**: `r summary_report$unidimensionality$variance_explained`%

![](plots/08_efa_loadings.png)

## 3.3 Confirmatory Factor Analysis (CFA)

```{r}
fit_indices <- read.csv("09_cfa_fit_indices.csv")
kable(fit_indices, caption = "CFA Model Fit Indices") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

```{r}
cfa_loadings <- read.csv("08_cfa_loadings.csv")
kable(cfa_loadings, digits = 3, caption = "Standardized Factor Loadings (CFA)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

![](plots/09_cfa_loadings.png)

**Kesimpulan**: `r summary_report$unidimensionality$conclusion`

---

# 4. Graded Response Model (GRM)

## 4.1 Parameter Item

```{r}
item_params <- read.csv("10_grm_item_parameters.csv")
kable(item_params, digits = 3, caption = "Parameter Item GRM (a = diskriminasi, b = threshold)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

### Interpretasi Parameter Diskriminasi (a):
- a < 0.5: Sangat Rendah
- 0.5 - 0.9: Rendah
- 0.9 - 1.3: Sedang
- 1.3 - 1.7: Tinggi
- a > 1.7: Sangat Tinggi

![](plots/10_parameter_diskriminasi.png)

## 4.2 Threshold Parameters

![](plots/11_parameter_threshold.png)

---

# 5. Evaluasi Model Fit

## 5.1 Overall Model Fit

```{r}
model_fit <- read.csv("11_overall_model_fit.csv")
kable(model_fit, caption = "Overall Model Fit Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

**Kesimpulan**: `r summary_report$model_fit$conclusion`

## 5.2 Item Fit

```{r}
item_fit <- read.csv("12_item_fit.csv")
kable(item_fit, digits = 3, caption = "Item Fit Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

**Kriteria Item Fit:**
- S_X2 p-value > 0.01
- Infit/Outfit: 0.5 - 1.5

![](plots/12_item_fit_infit_outfit.png)

## 5.3 Person Fit

```{r}
person_fit <- read.csv("13_person_fit_summary.csv")
kable(person_fit, caption = "Person Fit Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

![](plots/13_person_fit_distribution.png)

---

# 6. Item Characteristic Curves (ICC)

![](plots/14_icc_all_items.png)

---

# 7. Item Information Function (IIF)

```{r}
info_summary <- read.csv("15_item_info_summary.csv")
kable(info_summary, digits = 3, caption = "Item Information Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

![](plots/16_iif_all_items.png)

![](plots/17_iif_individual.png)

---

# 8. Test Information Function (TIF)

![](plots/18_test_information_function.png)

![](plots/19_tif_with_se.png)

**Maximum Information**: `r summary_report$test_info$max_information` at θ = `r summary_report$test_info$theta_at_max`

## Standard Error of Measurement

![](plots/20_conditional_se.png)

---

# 9. Reliabilitas

```{r}
reliability <- read.csv("17_reliability.csv")
kable(reliability, caption = "Reliability Estimates") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

![](plots/21_omega_diagram.png)

**Kesimpulan**: Reliabilitas instrumen **`r summary_report$reliability$interpretation`** dengan Omega Total = `r summary_report$reliability$omega_total`

---

# 10. Estimasi Theta

![](plots/22_distribusi_theta.png)

![](plots/23_theta_vs_total_score.png)

![](plots/24_theta_confidence_interval.png)

---

# 11. Penormaan Berdasarkan Theta

## 11.1 Tabel Konversi Skor

Tabel berikut menunjukkan konversi dari skor total ke berbagai skala norma:

```{r}
konversi <- read.csv("22_tabel_konversi_skor.csv")
kable(konversi, digits = 2, caption = "Tabel Konversi Skor Total ke Theta dan Norma") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  scroll_box(width = "100%", height = "400px")
```

## 11.2 Tabel Norma Teoretis

```{r}
norma_teor <- read.csv("23_tabel_norma_teoretis.csv")
kable(norma_teor, digits = 2, caption = "Tabel Norma Teoretis (Theta -3 sampai 3)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  scroll_box(width = "100%", height = "400px")
```

## 11.3 Statistik Skor Norma

```{r}
stat_norma <- read.csv("25_statistik_norma.csv")
kable(stat_norma, digits = 2, caption = "Statistik Deskriptif Skor Norma") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

## 11.4 Distribusi Kategori

```{r}
dist_kat <- read.csv("24_distribusi_kategori.csv")
kable(dist_kat, caption = "Distribusi Responden per Kategori") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
```

## 11.5 Visualisasi Penormaan

### Distribusi Theta dengan Kategori
![](plots/29_distribusi_theta_kategori.png)

### Distribusi T-Score
![](plots/30_distribusi_tscore.png)

### Distribusi Percentile
![](plots/31_distribusi_percentile.png)

### Distribusi Stanine
![](plots/32_distribusi_stanine.png)

### Proporsi Kategori
![](plots/33_pie_chart_kategori.png)

### Perbandingan Distribusi Empiris vs Teoretis
![](plots/34_perbandingan_distribusi.png)

### Kurva Konversi Skor Total ke Theta
![](plots/36_kurva_konversi.png)

### Heatmap Konversi Skor
![](plots/35_heatmap_konversi.png)

---

# 12. Wright Map

![](plots/25b_wright_map_ggplot.png)

---

# 13. Local Independence

```{r}
q3_residuals <- read.csv("19_q3_residuals.csv", row.names = 1)
kable(round(q3_residuals, 3), caption = "Q3 Residual Correlations") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  scroll_box(width = "100%", height = "400px")
```

**Kriteria**: |Q3| > 0.20 mengindikasikan local dependence

![](plots/28_q3_residual_heatmap.png)

**Kesimpulan**: `r summary_report$local_independence$conclusion`

---

# 14. Kesimpulan dan Rekomendasi

## Kesimpulan

Berdasarkan analisis komprehensif menggunakan Graded Response Model, instrumen ini menunjukkan:

1. **Unidimensionalitas**: `r summary_report$unidimensionality$conclusion`
2. **Model Fit**: `r summary_report$model_fit$conclusion`
3. **Reliabilitas**: `r summary_report$reliability$interpretation` (ω = `r summary_report$reliability$omega_total`)
4. **Item Quality**: `r ifelse(summary_report$item_fit$n_misfit == 0, "Semua item memiliki kualitas baik", paste0(summary_report$item_fit$n_misfit, " item perlu direview"))`

## Rekomendasi

```{r results="asis"}
if (summary_report$item_fit$n_misfit > 0) {
  cat("- Pertimbangkan untuk mereview item: ", summary_report$item_fit$misfit_items, "\n")
}
if (summary_report$local_independence$n_ld_pairs > 0) {
  cat("- Terdapat indikasi local dependence yang perlu diinvestigasi lebih lanjut\n")
}
if (summary_report$reliability$omega_total < 0.80) {
  cat("- Pertimbangkan untuk menambah item guna meningkatkan reliabilitas\n")
}
```

---

# Lampiran

## Daftar File Output

1. `01_statistik_deskriptif.csv` - Statistik deskriptif per item
2. `02_distribusi_frekuensi.csv` - Distribusi frekuensi kategori
3. `03_proporsi_kategori.csv` - Proporsi per kategori
4. `04_matriks_korelasi.csv` - Matriks korelasi inter-item
5. `05_korelasi_item_total.csv` - Korelasi item-total
6. `06_parallel_analysis.csv` - Hasil parallel analysis
7. `07_efa_loadings.csv` - Factor loadings EFA
8. `08_cfa_loadings.csv` - Standardized loadings CFA
9. `09_cfa_fit_indices.csv` - CFA fit indices
10. `10_grm_item_parameters.csv` - Parameter item GRM
11. `11_overall_model_fit.csv` - Overall model fit
12. `12_item_fit.csv` - Item fit statistics
13. `13_person_fit_summary.csv` - Person fit summary
14. `14_item_information.csv` - Item information values
15. `15_item_info_summary.csv` - Item information summary
16. `16_test_information.csv` - Test information function
17. `17_reliability.csv` - Reliability estimates
18. `18_theta_estimates.csv` - Theta estimates per responden
19. `19_q3_residuals.csv` - Q3 residual correlations
20. `20_summary_report.rds` - Summary report object
21. `21_tabel_norma_individu.csv` - Tabel norma per individu
22. `22_tabel_konversi_skor.csv` - Tabel konversi skor total ke theta
23. `23_tabel_norma_teoretis.csv` - Tabel norma teoretis
24. `24_distribusi_kategori.csv` - Distribusi responden per kategori
25. `25_statistik_norma.csv` - Statistik deskriptif skor norma

## Daftar Plot

Total 36 visualisasi tersimpan di folder `plots/`

---

*Laporan ini dihasilkan secara otomatis menggunakan R Script Analisis Psikometrik GRM*
'

# Simpan RMarkdown
writeLines(rmd_content, paste0(OUTPUT_DIR, "/Laporan_Analisis_Psikometrik.Rmd"))

# Render laporan HTML
cat("\nMembuat laporan HTML...\n")
setwd(OUTPUT_DIR)
tryCatch({
  rmarkdown::render("Laporan_Analisis_Psikometrik.Rmd", quiet = TRUE)
  cat("Laporan HTML berhasil dibuat!\n")
}, error = function(e) {
  cat(paste0("Error saat render laporan: ", e$message, "\n"))
  cat("File RMarkdown tersimpan, dapat dirender manual.\n")
})
setwd("..")

# -----------------------------------------------------------------------------
# 20. SELESAI
# -----------------------------------------------------------------------------

cat("\n")
cat("=" , rep("=", 70), "\n", sep = "")
cat("ANALISIS SELESAI\n")
cat("=" , rep("=", 70), "\n", sep = "")

cat(paste0("\nSemua output tersimpan di: ", OUTPUT_DIR, "/\n"))
cat(paste0("Total file CSV: 25 files\n"))
cat(paste0("Total visualisasi: 36 plots\n"))
cat(paste0("Laporan HTML: Laporan_Analisis_Psikometrik.html\n"))

cat("\n--- Daftar Plot yang Dihasilkan ---\n")
cat("01. Distribusi Skor Total\n")
cat("02. Boxplot Item\n")
cat("03. Mean Item dengan Error Bar\n")
cat("04. Heatmap Distribusi Kategori\n")
cat("05. Matriks Korelasi\n")
cat("06. Korelasi Item-Total\n")
cat("07. Parallel Analysis\n")
cat("08. EFA Factor Loadings\n")
cat("09. CFA Standardized Loadings\n")
cat("10. Parameter Diskriminasi\n")
cat("11. Parameter Threshold\n")
cat("12. Item Fit (Infit vs Outfit)\n")
cat("13. Person Fit Distribution\n")
cat("14. ICC All Items + Individual\n")
cat("15. Category Response Curves\n")
cat("16. IIF All Items\n")
cat("17. IIF Individual\n")
cat("18. Test Information Function\n")
cat("19. TIF dengan Standard Error\n")
cat("20. Conditional SE\n")
cat("21. Omega Diagram\n")
cat("22. Distribusi Theta\n")
cat("23. Theta vs Total Score\n")
cat("24. Theta Confidence Interval\n")
cat("25. Wright Map\n")
cat("26. Expected Score Curve\n")
cat("27. Expected Score per Item\n")
cat("28. Q3 Residual Heatmap\n")
cat("29. Distribusi Theta dengan Kategori\n")
cat("30. Distribusi T-Score\n")
cat("31. Distribusi Percentile\n")
cat("32. Distribusi Stanine\n")
cat("33. Pie Chart Kategori\n")
cat("34. Perbandingan Distribusi Empiris vs Teoretis\n")
cat("35. Heatmap Konversi Skor\n")
cat("36. Kurva Konversi Skor Total ke Theta\n")

cat("\n")
cat("Terima kasih telah menggunakan script ini!\n")
cat("=" , rep("=", 70), "\n", sep = "")
