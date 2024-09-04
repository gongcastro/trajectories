library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

theme_set(theme_minimal())

phor <- read_xlsx(
  here::here(
    "data-raw", "PHOR_in_One_LDB.xlsx"
  ),
  na = "NA",
  .name_repair = janitor::make_clean_names
)

rename_func <- function(x) {
  y <- ifelse(
    grepl("wordform", x),
    paste0("wordform_", gsub("_wordform", "", x)),
    paste0("freq_", gsub("subtlex_|_log10_abs_", "", x))
  )
  y <- gsub("_zipf", "", y)
  y <- gsub("am_e", "us1", y)
  y <- gsub("uk_e", "uk1", y)
  y <- gsub("br_e", "uk1", y)
  y <- gsub("br_p", "pt1", y)
  y <- gsub("ep", "pt1", y)
  y <- gsub("espal1", "es1", y)
  y <- gsub("1", "", y)
  return(y)
}

vars <- c(
  "subtlex_us_log10_abs_1",
  "subtlex_uk_log10_abs_1",
  "subtlex_de_log10_abs_1",
  "subtlex_pt_log10_abs_1",
  "espal_log10_abs_1"
)

freqs <- phor |>
  filter(po_s_all == "N") |>
  select(
    matches("wordform"),
    any_of(vars),
    -ends_with("_2")
  ) |>
  rename_with(rename_func) |>
  mutate(id = row_number()) |>
  pivot_longer(
    -id,
    names_to = c(".value", "language"),
    names_pattern = "(.+)_(.+)",
    names_transform = toupper
  ) |>
  janitor::clean_names()


all_combs <- expand_grid(
  l1 = unique(freqs$language),
  l2 = unique(freqs$language)
) |>
  filter(l1 != l2) |>
  rowwise() |>
  mutate(all = list(sort(c(l1, l2)))) |>
  pull(all) |>
  unique()

comb_labels <- map_chr(all_combs, \(x) paste0(x, collapse = "-"))

freq_pairs <- map(all_combs, function(x) {
  freqs |>
    filter(language %in% x) |>
    pivot_wider(
      id_cols = id,
      names_from = language,
      values_from = c(freq, wordform),
    ) |>
    set_names(c("id", "freq1", "freq2", "wordform1", "wordform2")) |>
    drop_na()
}) |>
  set_names(comb_labels)

corrs <- freq_pairs |>
  map2_df(comb_labels, function(x, y) {
    corr <- cor.test(x$freq1, x$freq2)
    tibble(
      pair = y,
      estimate = corr$estimate,
      .lower = corr$conf.int[1],
      .upper = corr$conf.int[2]
    )
  })

ggplot(corrs, aes(reorder(pair, desc(estimate)), estimate,
  ymin = .lower, ymax = .upper
)) +
  geom_errorbar(width = 0.2, linewidth = 3 / 4) +
  geom_point(size = 2.5) +
  geom_text(
    aes(
      y = .upper,
      label = sprintf(
        "%.2f\n[%.2f, %.2f]",
        estimate, .lower, .upper
      )
    ),
    position = position_nudge(y = 0.025),
    vjust = 0,
    hjust = 0.5
  ) +
  labs(
    x = "Language pair",
    y = "Pearson correlation"
  )

corrs |>
  map("estimate")
ggplot()

freq_pairs |>
  map2(comb_labels, function(x, y) {
    corr <- cor.test(x$freq1, x$freq2)
    ggplot(x, aes(freq1, freq2)) +
      geom_point(alpha = 1 / 4, size = 1) +
      labs(
        x = "Log10 (abs+1)",
        y = "Log10 (abs+1)",
        title = y,
        subtitle = sprintf(
          "r(%.0f) = %.2f [%.2f, %.2f]",
          corr$parameter, corr$estimate, corr$conf.int[1], corr$conf.int[2]
        )
      ) +
      geom_smooth(method = "lm")
  }) |>
  reduce(`+`)



freqs |>
  ggplot(aes())
