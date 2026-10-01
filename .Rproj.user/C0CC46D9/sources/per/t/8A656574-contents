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

