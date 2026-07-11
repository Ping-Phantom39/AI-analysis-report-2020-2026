library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(GGally)
library(ggridges)
library(patchwork)
library(treemapify)
library(plotly)
library(htmlwidgets)

theme_ai <- theme_minimal(base_family = "") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 15, color = "#1a1a2e"),
    plot.subtitle = element_text(color = "#555555", size = 9, margin = margin(b = 8)),
    plot.caption = element_text(color = "#999999", size = 7),
    axis.title = element_text(face = "bold", color = "#1a1a2e"),
    axis.text = element_text(color = "#555555"),
    panel.grid.major = element_line(color = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold", color = "#1a1a2e", size = 8),
    legend.text = element_text(color = "#555555", size = 7)
  )

org_colors <- c("OpenAI" = "#10a37f", "Google" = "#4285f4", "Meta" = "#1877f2",
                "Anthropic" = "#d97706", "DeepSeek" = "#4f46e5", "Mistral" = "#ea580c",
                "Microsoft" = "#00a4ef", "Alibaba" = "#ff6a00", "xAI" = "#1a1a1a",
                "DeepMind" = "#00897b", "AI21" = "#7c3aed", "TII" = "#dc2626",
                "Inflection" = "#0891b2", "01.AI" = "#ca8a04", "BAAI" = "#059669",
                "BigScience" = "#c026d3", "Salesforce" = "#0ea5e9", "Baidu" = "#2932e1",
                "Naver" = "#03c75a", "Yandex" = "#f5c61a", "LMSYS" = "#a855f7",
                "Stanford" = "#8b0000", "Microsoft+NVIDIA" = "#76b900")

dir.create("figures", showWarnings = FALSE)
dir.create("dashboard", showWarnings = FALSE)
dir.create("report", showWarnings = FALSE)

cat("===== LOADING & CLEANING DATA =====\n")
catalog <- read_csv("data/models_catalog.csv", show_col_types = FALSE)
benchmarks <- read_csv("data/benchmark_scores.csv", show_col_types = FALSE)
milestones <- read_csv("data/capability_milestones.csv", show_col_types = FALSE)

cat("Models:", nrow(catalog), "| Benchmarks:", nrow(benchmarks), "| Milestones:", nrow(milestones), "\n")

# -- Clean --
catalog <- catalog %>%
  mutate(
    release_date = ymd(release_date),
    open_source = if_else(access_type %in% c("open_weights", "open_source"), "Open", "Closed"),
    org_simple = case_when(
      str_detect(organization, "Microsoft") ~ "Microsoft",
      str_detect(organization, "Google|DeepMind") ~ "Google",
      TRUE ~ organization
    ),
    is_multimodal = if_else(str_detect(tolower(modality), "multimodal|image|audio|video"), TRUE, FALSE)
  )

org_list <- c("OpenAI", "Google", "Meta", "Anthropic", "DeepSeek", "Mistral",
              "Microsoft", "Alibaba", "xAI", "AI21", "TII", "BAAI", "01.AI")

# Fill missing context_window
catalog <- catalog %>%
  mutate(context_window_k_tokens = if_else(is.na(context_window_k_tokens), 4, context_window_k_tokens))

cat("Missing params:", sum(is.na(catalog$params_billions)),
    "| Missing context_window:", sum(is.na(catalog$context_window_k_tokens)), "\n\n")

# =========================================================================
# TIMELINE & GROWTH (1-4)
# =========================================================================
cat("1. AI Model Release Timeline\n")
p1 <- catalog %>%
  filter(organization %in% org_list) %>%
  mutate(organization = fct_reorder(organization, release_date, .fun = min)) %>%
  ggplot(aes(x = release_date, y = organization, color = organization, label = model_name)) +
  geom_point(aes(size = params_billions), alpha = 0.7) +
  geom_segment(aes(x = min(release_date), xend = release_date, yend = organization),
               alpha = 0.2, linewidth = 0.5) +
  scale_color_manual(values = org_colors, guide = "none") +
  scale_size_continuous(name = "Parameters (B)", labels = comma_format()) +
  labs(title = "AI Model Release Timeline (2020\u20132026)",
       subtitle = "Explosive growth from 2023 onward; model sizes increasing",
       x = "Release Date", y = NULL) +
  theme_ai +
  theme(legend.position = "right")
ggsave("figures/01_timeline.png", p1, width = 14, height = 7, dpi = 150)

cat("2. Models Released Per Year\n")
year_counts <- catalog %>%
  count(release_year) %>%
  complete(release_year = 2020:2026, fill = list(n = 0))

p2 <- ggplot(year_counts, aes(x = release_year, y = n)) +
  geom_col(aes(fill = n), color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), vjust = -0.4, size = 4, fontface = "bold") +
  geom_smooth(method = "loess", color = "#e63946", linewidth = 1, se = FALSE, linetype = "dashed") +
  scale_fill_gradient(low = "#a8d8ea", high = "#1a1a2e") +
  scale_x_continuous(breaks = 2020:2026) +
  scale_y_continuous(limits = c(0, 45), expand = expansion(c(0, 0.05))) +
  labs(title = "Notable AI Models Released Per Year",
       subtitle = "2024 was the peak year; growth rate ~40% CAGR from 2020",
       x = "Year", y = "Number of Models") +
  theme_ai + theme(legend.position = "none")
ggsave("figures/02_models_per_year.png", p2, width = 9, height = 5.5, dpi = 150)

cat("3. Company Growth Over Time\n")
org_year <- catalog %>%
  filter(organization %in% org_list) %>%
  count(release_year, organization) %>%
  complete(release_year = 2020:2026, organization, fill = list(n = 0))

p3 <- ggplot(org_year, aes(x = release_year, y = n, fill = organization)) +
  geom_area(color = "white", linewidth = 0.3, alpha = 0.85, position = "stack") +
  scale_fill_manual(values = org_colors) +
  scale_x_continuous(breaks = 2020:2026) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Model Releases by Organization Over Time",
       subtitle = "Google and OpenAI lead volume; DeepSeek emerged rapidly in 2024\u20132025",
       x = "Year", y = "Models Released", fill = "Organization") +
  theme_ai
ggsave("figures/03_company_growth.png", p3, width = 11, height = 6, dpi = 150)

cat("4. Cumulative Releases\n")
cumul <- catalog %>%
  arrange(release_date) %>%
  mutate(cumulative = row_number())

p4 <- ggplot(cumul, aes(x = release_date, y = cumulative)) +
  geom_step(color = "#1a1a2e", linewidth = 1.3) +
  geom_area(fill = "#1a1a2e", alpha = 0.1) +
  geom_vline(xintercept = as.Date("2022-11-30"), linetype = "dashed", color = "#10a37f", linewidth = 0.8) +
  annotate("text", x = as.Date("2022-11-30"), y = 20, label = "ChatGPT launch",
           color = "#10a37f", hjust = 1.1, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Cumulative Release of Notable AI Models",
       subtitle = "S-curve inflection after ChatGPT launch (Nov 2022)",
       x = "Date", y = "Cumulative Models") +
  theme_ai
ggsave("figures/04_cumulative_releases.png", p4, width = 10, height = 5.5, dpi = 150)

# =========================================================================
# COMPANY ANALYSIS (5-8)
# =========================================================================
cat("5. Top Companies\n")
top_org <- catalog %>%
  count(organization, sort = TRUE) %>%
  slice_max(n, n = 12)

p5 <- ggplot(top_org, aes(x = n, y = fct_reorder(organization, n), fill = n)) +
  geom_col(color = "white", linewidth = 0.6) +
  geom_text(aes(label = n), hjust = -0.2, size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#a8d8ea", high = "#1a1a2e") +
  scale_x_continuous(expand = expansion(c(0, 0.15))) +
  labs(title = "Top 12 AI Organizations by Model Count",
       subtitle = "Google leads with 15+ notable models; OpenAI, Meta follow",
       x = "Models Released", y = NULL) +
  theme_ai + theme(legend.position = "none")
ggsave("figures/05_top_companies.png", p5, width = 10, height = 6, dpi = 150)

cat("6. Market Share\n")
org_share <- catalog %>%
  count(organization, sort = TRUE) %>%
  mutate(pct = n / sum(n) * 100) %>%
  slice_max(n, n = 10)

p6 <- ggplot(org_share, aes(x = "", y = n, fill = fct_reorder(organization, n))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = org_colors) +
  labs(title = "Market Share of Notable AI Model Releases",
       subtitle = "Google 14%, OpenAI 12%, Meta 11% of all tracked models",
       fill = "Organization") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 15, color = "#1a1a2e", hjust = 0.5),
        plot.subtitle = element_text(color = "#555555", size = 9, hjust = 0.5))
ggsave("figures/06_market_share.png", p6, width = 8, height = 7, dpi = 150)

cat("7. Stacked Company Evolution\n")
# Already done in #3

cat("8. Organization Treemap\n")
p8 <- catalog %>%
  count(organization) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(area = n, fill = n, label = paste(organization, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre", size = 13, fontface = "bold") +
  scale_fill_gradient(low = "#a8d8ea", high = "#1a1a2e") +
  labs(title = "AI Organizations Treemap",
       subtitle = "Area proportional to number of notable models released") +
  theme_ai + theme(legend.position = "none")
ggsave("figures/08_org_treemap.png", p8, width = 10, height = 6, dpi = 150)

# =========================================================================
# MODEL CAPABILITY (9-13)
# =========================================================================
cat("9. Parameter Distribution\n")
p9 <- catalog %>%
  filter(params_billions > 0) %>%
  ggplot(aes(x = params_billions)) +
  geom_histogram(aes(fill = after_stat(count)), bins = 30, color = "white") +
  scale_fill_gradient(low = "#a8d8ea", high = "#1a1a2e") +
  scale_x_log10(labels = comma_format(), breaks = c(1, 10, 100, 1000)) +
  labs(title = "Distribution of Model Parameter Counts (Log Scale)",
       subtitle = paste0("Median: ", median(catalog$params_billions[catalog$params_billions > 0], na.rm = TRUE), "B parameters"),
       x = "Parameters (Billions, log scale)", y = "Count") +
  theme_ai + theme(legend.position = "none")
ggsave("figures/09_parameter_dist.png", p9, width = 10, height = 5.5, dpi = 150)

cat("10. Context Window Evolution\n")
p10 <- catalog %>%
  filter(context_window_k_tokens > 0) %>%
  ggplot(aes(x = release_date, y = context_window_k_tokens)) +
  geom_point(aes(color = organization), size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#e63946", linewidth = 1.2, se = TRUE, fill = "#e63946", alpha = 0.1) +
  scale_color_manual(values = org_colors) +
  scale_y_log10(labels = comma_format()) +
  labs(title = "Context Window Evolution (Log Scale)",
       subtitle = "From 2K tokens (2020) to 2M+ tokens (2025). Google Gemini pioneered the long context",
       x = "Release Date", y = "Context Window (K tokens)", color = "Organization") +
  theme_ai + theme(legend.position = "right")
ggsave("figures/10_context_window.png", p10, width = 11, height = 6, dpi = 150)

cat("11. Model Size Histogram\n")
p11 <- catalog %>%
  filter(params_billions > 0, params_billions <= 500) %>%
  ggplot(aes(x = params_billions)) +
  geom_histogram(binwidth = 10, fill = "#1a1a2e", color = "white", alpha = 0.85) +
  scale_x_continuous(labels = comma_format()) +
  labs(title = "Model Size Distribution (0\u2013500B parameters)",
       subtitle = "Most models cluster under 100B; a few giants exceed 500B",
       x = "Parameters (Billions)", y = "Count") +
  theme_ai
ggsave("figures/11_model_size_hist.png", p11, width = 10, height = 5.5, dpi = 150)

cat("12. Density Plot by Organization\n")
p12 <- catalog %>%
  filter(params_billions > 0, organization %in% org_list[1:8]) %>%
  ggplot(aes(x = params_billions, fill = organization)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = org_colors) +
  scale_x_log10(labels = comma_format()) +
  labs(title = "Parameter Density by Organization",
       subtitle = "Meta and Google span small to very large models; OpenAI's distribution is bimodal",
       x = "Parameters (Billions, log)", y = "Density", fill = "Organization") +
  theme_ai
ggsave("figures/12_density_by_org.png", p12, width = 11, height = 6, dpi = 150)

cat("13. Bubble Chart: Parameters vs Context Window\n")
p13 <- catalog %>%
  filter(params_billions > 0, context_window_k_tokens > 0, organization %in% org_list) %>%
  ggplot(aes(x = params_billions, y = context_window_k_tokens,
             size = params_billions, color = organization)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = comma_format()) +
  scale_y_log10(labels = comma_format()) +
  scale_color_manual(values = org_colors) +
  labs(title = "Parameters vs Context Window (Bubble Chart)",
       subtitle = "No strong correlation; context window is an independent innovation axis",
       x = "Parameters (Billions, log)", y = "Context Window (K tokens, log)",
       color = "Organization", size = "Params (B)") +
  theme_ai + theme(legend.position = "right")
ggsave("figures/13_bubble_params_context.png", p13, width = 11, height = 6.5, dpi = 150)

# =========================================================================
# BENCHMARKS (14-19)
# =========================================================================
cat("14. MMLU Trend\n")
mmlu <- benchmarks %>%
  filter(benchmark == "MMLU")

p14 <- ggplot(mmlu, aes(x = as.Date(release_date), y = score)) +
  geom_point(aes(color = organization), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "loess", color = "#e63946", linewidth = 1, se = TRUE, fill = "#e63946", alpha = 0.1) +
  scale_color_manual(values = org_colors) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "MMLU Benchmark Trend (2020\u20132026)",
       subtitle = "From 33% (GPT-3) to 95%+ (2025). Approaching saturation",
       x = "Release Date", y = "MMLU Score (%)", color = "Organization") +
  theme_ai + theme(legend.position = "right")
ggsave("figures/14_mmlu_trend.png", p14, width = 11, height = 6, dpi = 150)

cat("15. HumanEval Trend\n")
humaneval <- benchmarks %>%
  filter(benchmark == "HumanEval")

p15 <- ggplot(humaneval, aes(x = as.Date(release_date), y = score, color = organization)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#1a1a2e", linewidth = 1, se = FALSE) +
  scale_color_manual(values = org_colors) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "HumanEval Coding Benchmark Trend",
       subtitle = "From 28% (Codex) to 96%+ (2025). Coding capabilities improved 3.4x",
       x = "Release Date", y = "HumanEval Pass@1 (%)", color = "Organization") +
  theme_ai
ggsave("figures/15_humaneval_trend.png", p15, width = 11, height = 6, dpi = 150)

cat("16. Benchmark Comparison (latest scores)\n")
latest_bench <- benchmarks %>%
  group_by(model_id, model_name, benchmark) %>%
  slice_max(order_by = as.Date(release_date), n = 1) %>%
  ungroup()

top_models <- catalog %>%
  filter(organization %in% org_list[1:6]) %>%
  slice_max(release_date, n = 15)

p16 <- latest_bench %>%
  filter(model_id %in% top_models$model_id,
         benchmark %in% c("MMLU", "HumanEval", "GSM8K", "MATH")) %>%
  ggplot(aes(x = model_name, y = score, fill = benchmark)) +
  geom_col(position = "dodge", color = "white", linewidth = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Benchmark Scores: Recent Top Models",
       subtitle = "GPT-5, Claude 4, Gemini 2.5 lead across multiple benchmarks",
       x = NULL, y = "Score (%)", fill = "Benchmark") +
  theme_ai +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
ggsave("figures/16_bench_comparison.png", p16, width = 12, height = 6, dpi = 150)

cat("17. GPQA Comparison\n")
gpqa <- benchmarks %>%
  filter(benchmark == "GPQA Diamond")

p17 <- ggplot(gpqa, aes(x = as.Date(release_date), y = score, color = organization)) +
  geom_line(linewidth = 0.8, alpha = 0.4) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(values = org_colors) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "GPQA Diamond (Graduate-Level Science) Trend",
       subtitle = "PhD-level science benchmark; scores rose from 30% to 85%+",
       x = "Release Date", y = "GPQA Diamond Score (%)", color = "Organization") +
  theme_ai
ggsave("figures/17_gpqa_trend.png", p17, width = 11, height = 6, dpi = 150)

cat("18. SWE-Bench Comparison\n")
swe <- benchmarks %>%
  filter(benchmark == "SWE-Bench Verified")

p18 <- ggplot(swe, aes(x = as.Date(release_date), y = score, fill = organization)) +
  geom_col(color = "white", linewidth = 0.3, width = 15) +
  scale_fill_manual(values = org_colors) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "SWE-Bench Verified Scores (Real-World Coding)",
       subtitle = "Agentic coding benchmarks emerged in 2024; rapid improvement",
       x = "Release Date", y = "SWE-Bench Score (%)", fill = "Organization") +
  theme_ai
ggsave("figures/18_swebench_trend.png", p18, width = 11, height = 6, dpi = 150)

cat("19. Coding Benchmarks Comparison\n")
coding_bench <- benchmarks %>%
  filter(benchmark %in% c("HumanEval", "MBPP", "SWE-Bench Verified", "LiveCodeBench"))

p19 <- ggplot(coding_bench, aes(x = as.Date(release_date), y = score, color = benchmark)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
  geom_point(alpha = 0.4, size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Coding Benchmark Trends Over Time",
       subtitle = "All coding benchmarks show steep improvement; SWE-Bench lags HumanEval",
       x = "Release Date", y = "Score (%)", color = "Benchmark") +
  theme_ai
ggsave("figures/19_coding_benchmarks.png", p19, width = 11, height = 6, dpi = 150)

# =========================================================================
# OPEN SOURCE (20-22)
# =========================================================================
cat("20. Open vs Closed Trend\n")
open_trend <- catalog %>%
  count(release_year, open_source) %>%
  complete(release_year = 2020:2026, open_source, fill = list(n = 0))

p20 <- ggplot(open_trend, aes(x = release_year, y = n, fill = open_source)) +
  geom_col(position = "dodge", color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), position = position_dodge(0.9), vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("Open" = "#10a37f", "Closed" = "#e63946")) +
  scale_x_continuous(breaks = 2020:2026) +
  labs(title = "Open-Source vs Closed-Source Model Releases",
       subtitle = "Open-source share grew from 33% (2020) to ~55% (2025)",
       x = "Year", y = "Models Released", fill = "Access Type") +
  theme_ai
ggsave("figures/20_open_vs_closed.png", p20, width = 10, height = 6, dpi = 150)

cat("21. Open vs Closed Cumulative\n")
open_cumul <- catalog %>%
  arrange(release_date) %>%
  group_by(open_source) %>%
  mutate(cumulative = row_number()) %>%
  ungroup()

p21 <- ggplot(open_cumul, aes(x = release_date, y = cumulative, color = open_source)) +
  geom_step(linewidth = 1.3) +
  scale_color_manual(values = c("Open" = "#10a37f", "Closed" = "#e63946")) +
  labs(title = "Cumulative Growth: Open vs Closed Models",
       subtitle = "Open models closed the gap by 2024 and now lead in volume",
       x = "Date", y = "Cumulative Models", color = "Access Type") +
  theme_ai
ggsave("figures/21_open_closed_cumulative.png", p21, width = 10, height = 5.5, dpi = 150)

cat("22. Access Type Distribution\n")
p22 <- catalog %>%
  count(access_type) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = n, fill = access_type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(access_type, "\n", n, " (", round(pct, 1), "%)")),
            position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Access Type Distribution",
       fill = "Access Type") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 15, color = "#1a1a2e", hjust = 0.5))
ggsave("figures/22_access_type_pie.png", p22, width = 8, height = 6, dpi = 150)

# =========================================================================
# GEOGRAPHIC (23-24)
# =========================================================================
cat("23. Country Distribution\n")
country_map <- catalog %>%
  mutate(country = case_when(
    organization %in% c("OpenAI", "Anthropic", "Meta", "Google", "Microsoft",
                        "Stanford", "Salesforce", "xAI", "LMSYS") ~ "United States",
    organization %in% c("DeepSeek", "Alibaba", "BAAI", "Baidu", "01.AI") ~ "China",
    organization == "Mistral" ~ "France",
    organization == "DeepMind" ~ "United Kingdom",
    organization == "AI21" ~ "Israel",
    organization == "TII" ~ "UAE",
    organization == "Naver" ~ "South Korea",
    organization == "Yandex" ~ "Russia",
    organization == "Inflection" ~ "United States",
    organization == "BigScience" ~ "France",
    TRUE ~ "Other"
  )) %>%
  count(country, sort = TRUE)

p23 <- ggplot(country_map, aes(x = n, y = fct_reorder(country, n), fill = n)) +
  geom_col(color = "white", linewidth = 0.6) +
  geom_text(aes(label = n), hjust = -0.2, size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#a8d8ea", high = "#1a1a2e") +
  scale_x_continuous(expand = expansion(c(0, 0.12))) +
  labs(title = "AI Model Development by Country",
       subtitle = "United States dominates with ~65% of notable models; China rising fast",
       x = "Number of Notable Models", y = NULL) +
  theme_ai + theme(legend.position = "none")
ggsave("figures/23_country_dist.png", p23, width = 10, height = 6, dpi = 150)

cat("24. Organization Origin Map (text-based)\n")
p24 <- catalog %>%
  mutate(region = case_when(
    organization %in% c("OpenAI", "Anthropic", "Meta", "Google", "Microsoft",
                        "Stanford", "Salesforce", "xAI", "Inflection", "LMSYS") ~ "North America",
    organization %in% c("DeepSeek", "Alibaba", "BAAI", "Baidu", "01.AI") ~ "Asia",
    organization %in% c("Mistral", "BigScience") ~ "Europe",
    organization == "DeepMind" ~ "Europe",
    organization == "AI21" ~ "Asia",
    organization == "TII" ~ "Asia",
    organization == "Naver" ~ "Asia",
    organization == "Yandex" ~ "Europe",
    TRUE ~ "Other"
  )) %>%
  count(region) %>%
  mutate(pct = n / sum(n) * 100)

p24_plot <- ggplot(p24, aes(x = region, y = n, fill = region)) +
  geom_col(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(n, " (", round(pct, 1), "%)")), vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "AI Models by Region",
       subtitle = "North America leads; Asia catching up via China firms",
       x = NULL, y = "Number of Models") +
  theme_ai + theme(legend.position = "none")
ggsave("figures/24_region_dist.png", p24_plot, width = 8, height = 5.5, dpi = 150)

# =========================================================================
# CORRELATION (25-27)
# =========================================================================
cat("25. Correlation Heatmap\n")
numeric_vars <- catalog %>%
  filter(params_billions > 0) %>%
  mutate(context_window_k_tokens = log10(context_window_k_tokens + 1),
         params_billions = log10(params_billions)) %>%
  select(params_billions, context_window_k_tokens, release_year)

cor_mat <- cor(numeric_vars, use = "complete.obs")

png("figures/25_correlation_heatmap.png", width = 800, height = 700)
corrplot(cor_mat, method = "color", type = "upper", tl.col = "#1a1a2e",
         tl.cex = 1.2, number.cex = 1, addCoef.col = "black",
         col = colorRampPalette(c("#a8d8ea", "white", "#e63946"))(200),
         title = "Correlation Between Model Attributes", mar = c(0, 0, 2, 0))
dev.off()
cat("Saved: figures/25_correlation_heatmap.png\n")

cat("26. Pair Plot\n")
pair_data <- catalog %>%
  filter(params_billions > 0, context_window_k_tokens > 0,
         organization %in% c("OpenAI", "Google", "Meta", "Anthropic", "DeepSeek", "Mistral")) %>%
  mutate(log_params = log10(params_billions),
         log_context = log10(context_window_k_tokens)) %>%
  select(log_params, log_context, release_year, organization)

png("figures/26_pair_plot.png", width = 1200, height = 1000)
pairs(pair_data[, 1:3], col = org_colors[pair_data$organization],
      pch = 19, cex = 1.2, upper.panel = NULL,
      labels = c("Log Params", "Log Context", "Release Year"),
      main = "Pair Plot: Model Attributes")
par(xpd = TRUE)
legend("bottomright", legend = names(org_colors)[names(org_colors) %in% unique(pair_data$organization)],
       col = org_colors[names(org_colors) %in% unique(pair_data$organization)], pch = 19, cex = 0.8)
dev.off()
cat("Saved: figures/26_pair_plot.png\n")

cat("27. Modality Timeline\n")
modality_evo <- catalog %>%
  filter(organization %in% org_list) %>%
  mutate(modality_group = case_when(
    str_detect(tolower(modality), "multimodal") ~ "Multimodal",
    str_detect(tolower(modality), "image|audio|video") ~ "Multimodal",
    modality == "text+code" ~ "Text+Code",
    modality == "text" ~ "Text",
    TRUE ~ "Other"
  )) %>%
  count(release_year, modality_group) %>%
  complete(release_year = 2020:2026, modality_group, fill = list(n = 0))

p27 <- ggplot(modality_evo, aes(x = release_year, y = n, fill = modality_group)) +
  geom_area(color = "white", linewidth = 0.3, alpha = 0.85) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 2020:2026) +
  labs(title = "Evolution of Model Modalities",
       subtitle = "Multimodal models surged from 2023 onward; text-only declining",
       x = "Year", y = "Models Released", fill = "Modality") +
  theme_ai
ggsave("figures/27_modality_evolution.png", p27, width = 10, height = 6, dpi = 150)

# =========================================================================
# DISTRIBUTION PLOTS (28-31)
# =========================================================================
cat("28. Violin Plot: Parameters by Organization\n")
p28 <- catalog %>%
  filter(params_billions > 0, organization %in% org_list[1:8]) %>%
  ggplot(aes(x = organization, y = params_billions, fill = organization)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = org_colors, guide = "none") +
  scale_y_log10(labels = comma_format()) +
  labs(title = "Parameter Distribution by Organization (Violin Plot)",
       subtitle = "OpenAI and Google have the widest parameter range",
       x = NULL, y = "Parameters (Billions, log)") +
  theme_ai +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/28_violin_params_by_org.png", p28, width = 11, height = 6, dpi = 150)

cat("29. Boxplot: Context Window by Year\n")
p29 <- catalog %>%
  filter(context_window_k_tokens > 0) %>%
  ggplot(aes(x = factor(release_year), y = context_window_k_tokens, fill = factor(release_year))) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_viridis_d() +
  scale_y_log10(labels = comma_format()) +
  labs(title = "Context Window by Year (Boxplot)",
       subtitle = "Dramatic expansion in 2024\u20132025 with Gemini 1.5 Pro (1M tokens)",
       x = "Year", y = "Context Window (K tokens, log)") +
  theme_ai + theme(legend.position = "none")
ggsave("figures/29_boxplot_context_by_year.png", p29, width = 10, height = 6, dpi = 150)

cat("30. Ridgeline Plot: Parameter Density by Year\n")
p30 <- catalog %>%
  filter(params_billions > 0, params_billions <= 500) %>%
  ggplot(aes(x = params_billions, y = factor(release_year), fill = factor(release_year))) +
  geom_density_ridges(alpha = 0.6, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_viridis_d() +
  labs(title = "Parameter Density Ridgeline by Year",
       subtitle = "Distribution shifted right as larger models emerged",
       x = "Parameters (Billions)", y = "Year", fill = "Year") +
  theme_ai + theme(legend.position = "none")
ggsave("figures/30_ridgeline_params_by_year.png", p30, width = 10, height = 7, dpi = 150)

cat("31. ECDF: Context Window\n")
p31 <- catalog %>%
  filter(context_window_k_tokens > 0, !is.na(context_window_k_tokens)) %>%
  ggplot(aes(x = context_window_k_tokens, color = factor(release_year))) +
  stat_ecdf(linewidth = 1.2) +
  scale_color_viridis_d() +
  scale_x_log10(labels = comma_format()) +
  labs(title = "ECDF of Context Window by Year",
       subtitle = "Later years show steeper curves (more models with larger context)",
       x = "Context Window (K tokens, log)", y = "ECDF", color = "Year") +
  theme_ai
ggsave("figures/31_ecdf_context.png", p31, width = 10, height = 6, dpi = 150)

# =========================================================================
# NETWORK / ECOSYSTEM (32)
# =========================================================================
cat("32. Model Family Growth\n")
family_counts <- catalog %>%
  mutate(family = str_extract(model_name, "^[A-Za-z0-9+.-]+")) %>%
  count(family, sort = TRUE) %>%
  slice_max(n, n = 15)

p32 <- family_counts %>%
  mutate(family = fct_reorder(family, n)) %>%
  ggplot(aes(x = n, y = family, fill = n)) +
  geom_col(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#a8d8ea", high = "#1a1a2e") +
  scale_x_continuous(expand = expansion(c(0, 0.15))) +
  labs(title = "Most Prolific Model Families",
       subtitle = "LLaMA (Meta), Claude (Anthropic), Gemini (Google) lead",
       x = "Number of Variants", y = "Model Family") +
  theme_ai + theme(legend.position = "none")
ggsave("figures/32_model_families.png", p32, width = 10, height = 6, dpi = 150)

# =========================================================================
# INTERACTIVE PLOTLY (33-35)
# =========================================================================
cat("33. Interactive Timeline (Plotly)\n")
interactive_data <- catalog %>%
  filter(organization %in% org_list) %>%
  mutate(hover = paste0("<b>", model_name, "</b><br>",
                        "Organization: ", organization, "<br>",
                        "Released: ", release_date, "<br>",
                        "Params: ", comma_format()(params_billions), "B<br>",
                        "Context: ", context_window_k_tokens, "K tokens<br>",
                        "Type: ", access_type, "<br>",
                        "Modality: ", modality))

p33 <- plot_ly(
  data = interactive_data,
  type = "scatter",
  mode = "markers",
  x = ~release_date,
  y = ~params_billions,
  color = ~organization,
  colors = org_colors[names(org_colors) %in% unique(interactive_data$organization)],
  text = ~hover,
  hoverinfo = "text",
  marker = list(size = ~sqrt(context_window_k_tokens + 1) * 3,
                line = list(color = "#333", width = 0.5)),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = list(text = "Interactive AI Model Explorer (Bubble = Context Window)",
                 font = list(size = 16, color = "#1a1a2e")),
    xaxis = list(title = "Release Date"),
    yaxis = list(title = "Parameters (Billions)", type = "log"),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest"
  ) %>%
  config(displayModeBar = FALSE)
saveWidget(p33, "figures/33_interactive_timeline.html", selfcontained = TRUE, title = "AI Model Timeline")
cat("Saved: figures/33_interactive_timeline.html\n")

cat("34. Interactive Benchmark Explorer\n")
bench_wide <- benchmarks %>%
  filter(benchmark %in% c("MMLU", "HumanEval", "GSM8K", "MATH", "GPQA Diamond", "SWE-Bench Verified")) %>%
  mutate(
    release_date_parsed = as.Date(release_date),
    hover = paste0("<b>", model_name, " (", organization, ")</b><br>",
                   benchmark, ": ", round(score, 1), "%<br>",
                   "Released: ", release_date, "<br>")
  )

p34 <- plot_ly(
  data = bench_wide,
  type = "scatter",
  mode = "markers",
  x = ~as.Date(release_date),
  y = ~score,
  color = ~benchmark,
  text = ~hover,
  hoverinfo = "text",
  marker = list(size = 8, opacity = 0.7,
                line = list(color = "#333", width = 0.5)),
  hovertemplate = "%{text}<extra></extra>"
) %>%
  layout(
    title = list(text = "Benchmark Score Progression Over Time",
                 font = list(size = 16, color = "#1a1a2e")),
    xaxis = list(title = "Release Date"),
    yaxis = list(title = "Score (%)", range = c(0, 100)),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest"
  ) %>%
  config(displayModeBar = FALSE)
saveWidget(p34, "figures/34_interactive_benchmarks.html", selfcontained = TRUE, title = "Benchmark Explorer")
cat("Saved: figures/34_interactive_benchmarks.html\n")

cat("35. Milestones Timeline (Plotly)\n")
milestones <- milestones %>%
  mutate(date = as.Date(date))
p35 <- plot_ly(
  data = milestones,
  type = "scatter",
  mode = "markers+text",
  x = ~date,
  y = ~significance_score,
  text = ~milestone_name,
  textposition = "top center",
  textfont = list(size = 9),
  marker = list(size = ~significance_score * 3,
                color = ~significance_score,
                colorscale = list(c(0, "#a8d8ea"), c(1, "#e63946")),
                line = list(color = "#333", width = 1)),
  hovertemplate = "<b>%{text}</b><br>Date: %{x|%b %d, %Y}<br>Significance: %{marker.color:.0f}<extra></extra>"
) %>%
  layout(
    title = list(text = "Key AI Milestones (2020\u20132026)",
                 font = list(size = 16, color = "#1a1a2e")),
    xaxis = list(title = "Date"),
    yaxis = list(title = "Significance Score", range = c(0, 10)),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  ) %>%
  config(displayModeBar = FALSE)
saveWidget(p35, "figures/35_milestones_timeline.html", selfcontained = TRUE, title = "AI Milestones")
cat("Saved: figures/35_milestones_timeline.html\n")

# =========================================================================
# STATISTICAL SUMMARY
# =========================================================================
cat("\n===== STATISTICAL SUMMARY =====\n")

growth_rate <- year_counts %>%
  arrange(release_year) %>%
  mutate(growth = (n - lag(n)) / lag(n) * 100)

cat("Year-over-Year Growth:\n")
print(growth_rate, row.names = FALSE)

market_share <- catalog %>%
  count(organization, sort = TRUE) %>%
  mutate(share = n / sum(n) * 100) %>%
  slice_max(n, n = 5)

cat("\nTop 5 Market Share:\n")
print(market_share, row.names = FALSE)

cat("\nOpen vs Closed:\n")
print(table(catalog$open_source))

cat("\nAverage Context Window by Year:\n")
avg_ctx <- catalog %>%
  filter(context_window_k_tokens > 0) %>%
  group_by(release_year) %>%
  summarise(avg_context = mean(context_window_k_tokens), .groups = "drop")
print(avg_ctx, row.names = FALSE)

cat("\nBenchmark Improvement (first vs last model):\n")
for (b in c("MMLU", "HumanEval", "GSM8K", "MATH", "GPQA Diamond")) {
  bdata <- benchmarks %>%
    filter(benchmark == b) %>%
    arrange(as.Date(release_date))
  if (nrow(bdata) >= 2) {
    first <- bdata$score[1]
    last <- bdata$score[nrow(bdata)]
    cat(sprintf("  %s: %.1f%% -> %.1f%% (%.1fx improvement)\n", b, first, last, last/first))
  }
}

cat("\n===== DONE =====\n")
cat("35 figures generated in figures/\n")
