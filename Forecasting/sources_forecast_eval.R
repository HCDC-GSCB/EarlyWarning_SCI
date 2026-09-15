library(tidyverse)
library(lubridate)
library(Metrics)
library(flextable)

# ==========================================
# 1. ĐỌC VÀ CHUẨN BỊ DỮ LIỆU CÓ KHOẢNG TIN CẬY
# ==========================================
# 1.1 Xử lý HCM-1 (File cũ, phải pivot từ quantile_level)
hcm1 <- read_csv("data/HCM-1-xgboost-allVars-2016_06_07.csv") %>%
  mutate(date = ymd(date), fcst_date = ymd(fcst_date), year = year(date))

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

hcm3 <- read_delim("data/fcst_1-12was_xgb_allVars_gid2_2016-06-27_slurm.csv", 
                   delim = ";", 
                   locale = locale(decimal_mark = ","),
                   escape_double = FALSE, 
                   trim_ws = TRUE) %>%
  mutate(date = ymd(date), year = year(date))

hcm3_fh1 <- hcm3 %>%
  filter(fh == 1) %>%
  # Nếu cột interval có nhiều mức (ví dụ 0.8, 0.95), nhớ filter đúng 0.95. 
  # Nếu chỉ có 1 mức thì bỏ qua dòng filter(interval == 0.95) này cũng được.
  filter(interval == 0.95) %>% 
  # Đổi tên cột cho khớp với format vẽ biểu đồ của ggplot
  rename(
    observed = truth,
    median = response
  ) %>%
  # Chỉ giữ lại các cột cần thiết để vẽ
  select(date, observed, year, VARNAME_2, lower, median, upper)


# ==========================================
# 2. TÍNH TOÁN SAI SỐ (Toàn thành phố HCM-1)
# ==========================================
metrics_by_fh <- hcm1 %>%
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

# --- BIỂU ĐỒ 2: HCM-3 13 PHƯỜNG/XÃ (XỬ LÝ TỰ ĐỘNG BẰNG LIST) ---
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
      title = paste("Khớp mô hình tại Phường/Xã:", ward_name, "(Tầm nhìn fh = 1)"), 
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

# Lấy danh sách tên 13 phường xã
wards_list <- unique(hcm3_fh1$VARNAME_2)
names(wards_list) <- wards_list

# Dùng purrr::map để tạo 1 cục list chứa 13 cái biểu đồ
p_hcm3_wards <- map(wards_list, function(w) {
  df_ward <- hcm3_fh1 %>% filter(VARNAME_2 == w)
  plot_ward(df_ward, w)
})


# ==========================================
# 4. ĐÁNH GIÁ SỰ BIẾN THIÊN SAI SỐ THEO TẦM NHÌN DỰ BÁO (fh 1-12) CỦA 13 PHƯỜNG
# ==========================================

# 4.1 Chuẩn bị dữ liệu giữ nguyên toàn bộ fh từ 1 đến 12
hcm3_all_fh <- hcm3 %>%
  # Chỗ này nếu chạy bị 0 dòng thì nhớ tắt / comment dòng dưới đi nhé, giống đợt trước
  # filter(interval == 0.95) %>% 
  rename(observed = truth, median = response) %>%
  select(date, observed, year, VARNAME_2, fh, median)

# ---------------------------------------------------------
# ĐỀ XUẤT 1: BIỂU ĐỒ XU HƯỚNG TĂNG SAI SỐ THEO TẦM NHÌN (FH)
# ---------------------------------------------------------
# Tính MAE gom theo Phường, Năm và Tầm nhìn fh
error_trend_by_fh <- hcm3_all_fh %>%
  group_by(VARNAME_2, year, fh) %>%
  summarise(MAE = mae(observed, median), .groups = "drop")

p_hcm3_error_horizon <- ggplot(error_trend_by_fh, aes(x = fh, y = MAE, color = as.factor(year))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ VARNAME_2, scales = "free_y", ncol = 3) + # Vẽ khung 13 phường
  scale_x_continuous(breaks = 1:12) + # Ép trục X hiện đủ số từ 1 đến 12
  scale_color_viridis_d(option = "plasma", end = 0.8) + # Màu chuẩn học thuật cho các năm
  theme_bw(base_size = 12) +
  labs(
    x = "Tầm nhìn dự báo (tuần)",
    y = "Sai số tuyệt đối (MAE)",
    color = "Năm"
  ) +
  theme(
    axis.text = element_text(color = "black", size = 14),
    legend.position = "bottom",
    strip.text = element_text(size = 13, face = "bold"),
    strip.background = element_rect(fill = "#e9ecef", color = "black"),
    panel.grid.minor = element_blank()
  )

# ---------------------------------------------------------
# ĐỀ XUẤT 2: BIỂU ĐỒ BIẾN ĐỘNG SAI SỐ TRÊN CHUỖI THỜI GIAN (LỌC MỐC ĐẠI DIỆN)
# ---------------------------------------------------------
# Lọc 3 mốc đại diện: ngắn hạn (1), trung hạn (4), và dài hạn (8)
error_time_fh <- hcm3_all_fh %>%
  filter(fh %in% c(1, 4, 8)) %>% 
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(VARNAME_2, year, month_date, fh) %>%
  summarise(MAE = mae(observed, median), .groups = "drop")

# Tạo hàm vẽ cho từng phường (tương tự như list hồi nãy) để gọi bên Quarto
plot_error_time_ward <- function(df, ward_name) {
  ggplot(df, aes(x = month_date, y = MAE, color = as.factor(fh))) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    facet_wrap(~ year, scales = "free_x", ncol = 1) +
    scale_x_date(date_labels = "%b", date_breaks = "2 months") +
    scale_color_manual(
      values = c("1" = "#2ca02c", "4" = "#ff7f0e", "8" = "#d62728"),
      labels = c("1 tuần (fh=1)", "4 tuần (fh=4)", "8 tuần (fh=8)")
    ) +
    theme_bw(base_size = 12) +
    labs(
      title = paste("Biến động sai số MAE theo thời gian - Phường/Xã:", ward_name),
      x = "Thời gian",
      y = "Mức độ sai số (MAE)",
      color = "Tầm nhìn dự báo:"
    ) +
    theme(
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      strip.text = element_text(size = 11, face = "bold"),
      strip.background = element_rect(fill = "lightblue", color = "black"),
      panel.grid.minor = element_blank()
    )
}

# Áp dụng map để tạo list 13 biểu đồ thời gian cho 13 phường
p_hcm3_error_time_wards <- map(wards_list, function(w) {
  df_ward <- error_time_fh %>% filter(VARNAME_2 == w)
  plot_error_time_ward(df_ward, w)
})