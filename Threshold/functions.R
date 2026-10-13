library(tidyverse)
library(MASS)      # Negative Binomial
library(mgcv)      # GAM
library(forecast)  # SARIMA
library(gt)
library(patchwork)
library(googlesheets4)
library(googledrive)
library(surveillance)

load_data <- function(url, sheet) {
  gs4_deauth()
  df <- read_sheet(url, sheet = sheet)
  return(df)
}

theme_hcdc_report <- function() {
  theme_bw(base_size = 14) +
    theme(
      text = element_text(color = "black"),
      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(color = "black", size = 14, face = "bold"),
      plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "black", size = 13, hjust = 0.5, margin = margin(b = 10)),
      legend.text = element_text(color = "black", size = 12),
      legend.title = element_text(color = "black", size = 12, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(color = "white", size = 14, face = "bold"),
      strip.background = element_rect(fill = "#1D3557", color = NA) 
    )
}

# Hàm xử lý dữ liệu đầu vào thành chuỗi thời gian liên tục 520 tuần
prep_continuous_ts <- function(df_cases, disease_name) {
  df_cases %>%
    pivot_longer(cols = -Week, names_to = "Year", values_to = "Cases") %>%
    mutate(
      Year = as.numeric(Year),
      Disease = disease_name
    ) %>%
    filter(Year != 2026) %>%
    arrange(Year, Week) %>%
    # BƯỚC QUAN TRỌNG CHUẨN BỊ CHO TIME-SERIES:
    mutate(
      Time_Index = row_number(), # Trục t chạy liên tục từ 1 -> 520
      # Giả lập cột Date (Ngày đầu tiên của tuần) để mớm cho Prophet
      Date = as.Date(paste0(Year, "-01-01")) + (Week - 1) * 7 
    ) %>%
    # Bọc lót loại bỏ hoàn toàn NA để mô hình không báo lỗi
    mutate(Cases = replace_na(Cases, 0))
}

calc_serfling <- function(df_h, df_t) {
  tryCatch({
    df_h <- mutate(df_h, time = row_number())
    mod <- lm(cases ~ time + sin(2*pi*week/52) + cos(2*pi*week/52), data = df_h)
    df_t <- mutate(df_t, time = max(df_h$time) + row_number())
    as.numeric(predict(mod, df_t, interval="prediction", level=0.95)[,"upr"])
  }, error = function(e) rep(NA, nrow(df_t)))
}

calc_mem <- function(df_h, df_t) {
  peaks <- df_h %>% group_by(year) %>% summarise(m = max(cases, na.rm=T)) %>% pull(m)
  peaks <- peaks[peaks > 0]
  if(length(peaks) < 2) return(rep(NA, nrow(df_t)))
  thr <- exp(mean(log(peaks)) + 1.96 * sd(log(peaks)))
  rep(thr, nrow(df_t))
}

calc_cdc <- function(df_hist, df_target) {
  stats <- df_hist %>%
    group_by(week) %>%
    summarise(
      mean_val = mean(cases, na.rm = TRUE),
      sd_val = sd(cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(threshold = mean_val + 2 * sd_val)
  
  df_merged <- df_target %>%
    left_join(stats, by = "week")
  
  return(df_merged$threshold)
}
