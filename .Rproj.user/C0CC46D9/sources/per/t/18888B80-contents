library(tidyverse)
library(lubridate)
library(Metrics)
library(flextable)

# ==========================================
# 1. ĐỌC VÀ CHUẨN BỊ DỮ LIỆU CÓ KHOẢNG TIN CẬY
# ==========================================
hcm1 <- read_csv("data/HCM-1-xgboost-allVars-2016_06_07.csv") %>%
  mutate(date = ymd(date), fcst_date = ymd(fcst_date), year = year(date))

hcm2 <- read_csv("data/HCM-2-xgboost-allVars-2016_06_07.csv") %>%
  mutate(date = ymd(date), fcst_date = ymd(fcst_date), year = year(date))

# Hàm xử lý xoay dữ liệu: Dùng round(quantile_level, 3) để tránh lỗi dấu phẩy động
prepare_ci_data <- function(df) {
  df %>%
    filter(fh == 1) %>%
    mutate(
      q_label = case_when(
        round(quantile_level, 3) == 0.025 ~ "lower",
        round(quantile_level, 3) == 0.500 ~ "median",
        round(quantile_level, 3) == 0.975 ~ "upper",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(q_label)) %>%
    select(date, observed, fcst_date, year, any_of("VARNAME_2"), q_label, predicted) %>%
    pivot_wider(names_from = q_label, values_from = predicted)
}

hcm1_fh1 <- prepare_ci_data(hcm1)
hcm2_fh1 <- prepare_ci_data(hcm2)


# ==========================================
# 2. TÍNH TOÁN SAI SỐ (Sử dụng đường Median)
# ==========================================
metrics_by_fh <- hcm1 %>%
  # Chỗ này cũng phải làm tròn để lấy đúng đường phân vị 0.5
  filter(round(quantile_level, 3) == 0.500) %>% 
  group_by(fh) %>%
  summarise(
    MAE = round(mae(observed, predicted), 2),
    RMSE = round(rmse(observed, predicted), 2)
  ) %>%
  ungroup()

tbl_metrics_fh <- flextable(metrics_by_fh) %>%
  set_header_labels(fh = "Tuần dự báo (fh)", MAE = "MAE", RMSE = "RMSE") %>%
  theme_vanilla() %>% autofit() %>% align(align = "center", part = "all") %>% bold(part = "header")

metrics_by_year <- hcm1_fh1 %>%
  group_by(year) %>%
  summarise(
    MAE = round(mae(observed, median), 2),
    RMSE = round(rmse(observed, median), 2)
  ) %>%
  ungroup()

tbl_metrics_year <- flextable(metrics_by_year) %>%
  set_header_labels(year = "Năm", MAE = "MAE", RMSE = "RMSE") %>%
  theme_vanilla() %>% autofit() %>% align(align = "center", part = "all") %>% bold(part = "header")


# ==========================================
# 3. VẼ BIỂU ĐỒ (GGPLOT2) THEME_BW + CI RIBBON
# ==========================================
color_obs <- "#1f77b4"   
color_pred <- "#d62728"  
fill_ci <- "#d62728"     

# --- BIỂU ĐỒ 1: HCM-1 TOÀN THÀNH PHỐ ---
p_hcm1_facet_year <- ggplot(hcm1_fh1, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Khoảng tin cậy 95%"), alpha = 0.25) +
  geom_line(aes(y = observed, color = "Số ca thực tế"), linewidth = 1.2) +
  geom_line(aes(y = median, color = "Dự báo (Trung vị)"), linetype = "dashed", linewidth = 1.2) +
  
  facet_wrap(~ year, scales = "free", ncol = 1) +
  scale_color_manual(values = c("Số ca thực tế" = color_obs, "Dự báo (Trung vị)" = color_pred)) +
  scale_fill_manual(values = c("Khoảng tin cậy 95%" = fill_ci)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  
  theme_bw(base_size = 12) +
  labs(
    title = "Khớp mô hình Toàn Thành Phố qua các năm (Tầm nhìn fh = 1)",
    x = "Thời gian", y = "Số ca mắc"
  ) +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "lightblue", color = "black"),
    panel.grid.minor = element_blank()
  )

# --- BIỂU ĐỒ 2: HCM-2 PHƯỜNG XÃ ---
hcm2_chanhhung <- hcm2_fh1 %>% filter(VARNAME_2 == "Chánh Hưng")
hcm2_choquan <- hcm2_fh1 %>% filter(VARNAME_2 == "Chợ Quán")

plot_ward <- function(df, ward_name) {
  ggplot(df, aes(x = date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Khoảng tin cậy 95%"), alpha = 0.25) +
    geom_line(aes(y = observed, color = "Thực tế"), linewidth = 1.2) +
    geom_line(aes(y = median, color = "Dự báo (Trung vị)"), linetype = "dashed", linewidth = 1.2) +
    
    facet_wrap(~ year, scales = "free", ncol = 1) +
    scale_color_manual(values = c("Thực tế" = color_obs, "Dự báo (Trung vị)" = color_pred)) +
    scale_fill_manual(values = c("Khoảng tin cậy 95%" = fill_ci)) +
    scale_x_date(date_labels = "%b", date_breaks = "2 months") +
    
    theme_bw(base_size = 12) +
    labs(
      title = paste("Khớp mô hình tại Phường", ward_name, "(Tầm nhìn fh = 1)"), 
      x = "Thời gian", y = "Số ca"
    ) +
    theme(
      axis.text = element_text(size = 12, color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "lightblue", color = "black"),
      panel.grid.minor = element_blank()
    )
}

p_hcm2_chanhhung <- plot_ward(hcm2_chanhhung, "Chánh Hưng")
p_hcm2_choquan <- plot_ward(hcm2_choquan, "Chợ Quán")