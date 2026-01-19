####****************************************************************************
#### SCRIPT: code.R (Manuscript 1)
#### DESCRIPTION: Analytical pipeline for Manuscript 1: Systems of Vegetable Production.
####              Integrates data processing, heatmap generation, forest plots.
#### AUTHOR: Andy A. Acosta-Monterrosa (Refactored)
#### DATE: 2026-01-19
####****************************************************************************

####****************************************************************************
##### 0. LIBRARIES #####
####****************************************************************************

library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(tibble)
library(grid)
library(gridExtra)

####****************************************************************************
##### 1. PATHS & INPUT (DEFINICION DE RUTAS) #####
####****************************************************************************

# Paths (Relative to this script in income/manuscript_1/src)
# Data is in ../data/
path_s1_local <- "../data/step_1_income_food_production.xlsx"
path_s2_local <- "../data/step_2_income_food_production.xlsx"
path_s3_local <- "../data/step_3_income_food_production.xlsx"

# Output paths
path_s1_out <- path_s1_local
path_s3_out <- path_s3_local

cat("Paths defined for Manuscript 1.\n")


####****************************************************************************
##### 2. STEP 1: DATA PROCESSING & HEATMAPS #####
####****************************************************************************
cat("\n--- STARTING STEP 1 ---\n")

# 2.1 LOAD SCRIPT 1 DATA
#-------------------------------------------------------------------------------
if (exists("step1") && is.function(step1)) rm(step1)
step1 <- readxl::read_xlsx(path_s1_local)

# 2.2 PROCESS DATA (SCALING & DICTIONARY)
#-------------------------------------------------------------------------------
step1 <- step1 %>%
    mutate(
        # Estimate
        estimate_scaled_100 = case_when(
            effect_unit %in% c("Ratio", "OR") ~ estimate_interpretable^100,
            TRUE ~ NA_real_
        ),
        # Lower Limit
        ci_low_scaled_100 = case_when(
            effect_unit %in% c("Ratio", "OR") ~ ci_low_interpretable^100,
            TRUE ~ NA_real_
        ),
        # Upper Limit
        ci_high_scaled_100 = case_when(
            effect_unit %in% c("Ratio", "OR") ~ ci_high_interpretable^100,
            TRUE ~ NA_real_
        )
    )

# Indicator Dictionary definition (Manuscript 1 Only)
indicator_dict <- tribble(
    ~indicator, ~unit, ~category,
    # Staple crops
    "Cereal production, 1961 to 2023", "t", "Staple crops",
    "Corn production, 1961 to 2023", "t", "Staple crops",
    "Rice production, 2023", "t", "Staple crops",
    "Barley production, 2023", "t", "Staple crops",
    "Wheat production, 2023", "t", "Staple crops",
    "Bean production, 2023", "t", "Staple crops",
    "Soybean production, 2023", "t", "Staple crops",
    "Sesame seed production, 2023", "t", "Staple crops",
    # Horticulture
    "Apple production, 2023", "t", "Horticulture",
    "Avocado production, 2023", "t", "Horticulture",
    "Banana production, 2023", "t", "Horticulture",
    "Banana production by region, 1961 to 2023", "t", "Horticulture",
    "Cashew nut production, 2023", "t", "Horticulture",
    "Tomato production, 2023", "t", "Horticulture",
    "Potato production, 2023", "t", "Horticulture",
    "Yams production, 2023", "t", "Horticulture",
    # Commodity
    "Cocoa bean production, 2023", "t", "Commodity & industrial crops",
    "Cocoa bean production by region, 1961 to 2023", "t", "Commodity & industrial crops",
    "Coffee production by region, 1961 to 2023", "t", "Commodity & industrial crops",
    "Green coffee beans production, 2023", "t", "Commodity & industrial crops",
    "Oil palm production", "t", "Commodity & industrial crops",
    "Sugar cane production, 2023", "t", "Commodity & industrial crops",
    "Wine production, 2022", "t", "Commodity & industrial crops"
)

# Clean keys
step1 <- step1 %>%
    mutate(dv_key = str_squish(str_replace_all(dependent_var, "[\r\n]+", " ")))

indicator_dict2 <- indicator_dict %>%
    mutate(dv_key = str_squish(str_replace_all(indicator, "[\r\n]+", " "))) %>%
    select(dv_key, unit, category)

# Join dictionary
# Remove existing category/unit columns from step1 if they exist to avoid duplicates (.x, .y)
if ("category" %in% names(step1)) step1 <- dplyr::select(step1, -category)
if ("unit" %in% names(step1)) step1 <- dplyr::select(step1, -unit)

step1 <- step1 %>%
    left_join(indicator_dict2, by = "dv_key") %>%
    select(-dv_key)

# Filter for Manuscript 1 Categories only (Redundant if data is split, but safe)
step1 <- step1 %>% filter(!is.na(category))

# Ensure unit and category columns
if (!"unit" %in% names(step1)) step1$unit <- NA_character_
if (!"category" %in% names(step1)) step1$category <- NA_character_

# Relocate
step1 <- step1 %>% relocate(unit, category, .after = last_col())

# Save updated Excel
openxlsx::write.xlsx(step1, path_s1_out)
cat("Step 1 processed file saved to:", path_s1_out, "\n")

# 2.3 CLEAN DATA FOR PLOTTING
#-------------------------------------------------------------------------------
step1_clean <- step1 %>%
    dplyr::mutate(
        dependent_var   = stringr::str_squish(dependent_var),
        independent_var = stringr::str_squish(independent_var),
        model_type      = stringr::str_squish(model_type),
        income_group    = stringr::str_squish(income_group),
        effect_unit     = stringr::str_squish(effect_unit)
    )

# 2.4 DISPLAY SETTINGS
#-------------------------------------------------------------------------------
income_abbrev_map <- c(
    "High income"         = "HIC",
    "Upper middle income" = "UMIC",
    "Lower middle income" = "LMIC",
    "Low income"          = "LIC"
)
income_levels <- c("LIC", "LMIC", "UMIC", "HIC")

model_map <- tibble::tibble(
    model_type = c("Linear Gaussian", "Log-linear Gaussian (log y)", "Quasi-binomial (logit link)"),
    model_sup  = c("ᵃ", "ᵇ", "ᶜ")
)

# Superscripts logic
model_sup_df <- step1_clean %>%
    dplyr::count(dependent_var, model_type, sort = TRUE) %>%
    dplyr::group_by(dependent_var) %>%
    dplyr::slice_max(n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(model_map, by = "model_type") %>%
    dplyr::mutate(model_sup = dplyr::coalesce(model_sup, "")) %>%
    dplyr::select(dependent_var, model_sup)

# 2.5 CATEGORY MAPPINGS (Manuscript 1)
#-------------------------------------------------------------------------------
m1_cats <- list(
    "Staple crops" = c(
        "Barley production, 2023", "Bean production, 2023", "Cereal production, 1961 to 2023",
        "Corn production, 1961 to 2023", "Rice production, 2023", "Sesame seed production, 2023",
        "Soybean production, 2023", "Wheat production, 2023"
    ),
    "Horticulture" = c(
        "Apple production, 2023", "Avocado production, 2023", "Banana production, 2023",
        "Banana production by region, 1961 to 2023", "Cashew nut production, 2023",
        "Potato production, 2023", "Tomato production, 2023", "Yams production, 2023"
    ),
    "Commodity & industrial crops" = c(
        "Cocoa bean production, 2023", "Cocoa bean production by region, 1961 to 2023",
        "Coffee production by region, 1961 to 2023", "Green coffee beans production, 2023",
        "Oil palm production", "Sugar cane production, 2023", "Wine production, 2022"
    )
)

make_mapping <- function(cat_list, manuscript_label) {
    out <- list()
    i <- 1
    for (cat in names(cat_list)) {
        vec <- stringr::str_squish(cat_list[[cat]])
        out[[i]] <- tibble::tibble(
            manuscript    = manuscript_label,
            category      = cat,
            dependent_var = vec,
            order_within  = seq_along(vec)
        )
        i <- i + 1
    }
    dplyr::bind_rows(out)
}

map_m1 <- make_mapping(m1_cats, "Manuscript 1")
map_m1 <- map_m1 %>%
    dplyr::mutate(category = if_else(category == "Commodity & industrial crops", "Commodity &\nindustrial crops", category))

# 2.6 PLOT FUNCTION (HEATMAP)
#-------------------------------------------------------------------------------
make_heatmap <- function(df_clean, mapping_df) {
    mapping_df2 <- mapping_df %>%
        dplyr::mutate(dependent_var = stringr::str_squish(dependent_var)) %>%
        dplyr::rename(category_map = category)

    order_master <- mapping_df2 %>%
        dplyr::left_join(model_sup_df, by = "dependent_var") %>%
        dplyr::mutate(
            model_sup = dplyr::coalesce(model_sup, ""),
            final_label = paste0(dependent_var, model_sup)
        ) %>%
        dplyr::arrange(factor(category_map, levels = unique(mapping_df2$category_map)), order_within)

    ordered_labels <- unique(order_master$final_label)
    plot_levels <- rev(ordered_labels)
    cat_levels <- unique(mapping_df2$category_map)

    plot_data <- df_clean %>%
        dplyr::semi_join(mapping_df2, by = "dependent_var") %>%
        dplyr::left_join(mapping_df2 %>% dplyr::select(dependent_var, category_map), by = "dependent_var") %>%
        dplyr::left_join(model_sup_df, by = "dependent_var") %>%
        dplyr::mutate(
            model_sup = dplyr::coalesce(model_sup, ""),
            dependent_var_sup = paste0(dependent_var, model_sup),
            income_abbrev = unname(income_abbrev_map[income_group]),
            income_abbrev = factor(income_abbrev, levels = income_levels),
            category_facet = factor(category_map, levels = cat_levels),
            effect_size = suppressWarnings(as.numeric(estimate_interpretable))
        ) %>%
        dplyr::filter(stringr::str_detect(independent_var, "^total_public")) %>%
        dplyr::filter(!is.na(effect_size), !is.na(income_abbrev), !is.na(category_facet)) %>%
        dplyr::mutate(dependent_var_sup = factor(dependent_var_sup, levels = plot_levels)) %>%
        dplyr::group_by(dependent_var_sup) %>%
        dplyr::mutate(
            coef_rank = rank(effect_size, ties.method = "min"),
            rank_bin = dplyr::ntile(coef_rank, 4),
            rank_category = factor(rank_bin, levels = 1:4, labels = c("Lowest", "Low", "High", "Highest"))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            significance = dplyr::case_when(
                p_value < 0.001 ~ "***",
                p_value < 0.01 ~ "**",
                p_value < 0.05 ~ "*",
                TRUE ~ ""
            ),
            is_ratio_type = stringr::str_detect(effect_unit, "OR") | stringr::str_detect(effect_unit, "Ratio"),
            is_beta = stringr::str_detect(effect_unit, "β"),
            effect_direction = dplyr::case_when(
                is_ratio_type ~ ifelse(effect_size > 1, "Positive", "Negative"),
                is_beta ~ ifelse(effect_size >= 0, "Positive", "Negative"),
                TRUE ~ ifelse(effect_size >= 0, "Positive", "Negative")
            ),
            effect_direction = factor(effect_direction, levels = c("Positive", "Negative"))
        ) %>%
        dplyr::group_by(category_facet, dependent_var_sup) %>%
        tidyr::complete(income_abbrev = factor(income_levels, levels = income_levels)) %>%
        dplyr::ungroup()

    direction_key <- tibble::tibble(
        income_abbrev      = factor(income_levels[1], levels = income_levels),
        dependent_var_sup  = factor(plot_levels[1], levels = plot_levels),
        effect_direction   = factor(c("Positive", "Negative"), levels = c("Positive", "Negative"))
    )

    ggplot(plot_data, aes(x = income_abbrev, y = dependent_var_sup)) +
        geom_tile(
            data = subset(plot_data, !is.na(effect_direction) & effect_direction == "Positive"),
            aes(fill = rank_category), color = "white", linewidth = 0.5
        ) +
        geom_point(
            data = subset(plot_data, !is.na(effect_direction) & effect_direction == "Negative"),
            aes(fill = rank_category), color = "white", size = 6, shape = 21
        ) +
        geom_text(data = subset(plot_data, !is.na(effect_size)), aes(label = significance), size = 2.6) +
        geom_point(
            data = direction_key, aes(x = income_abbrev, y = dependent_var_sup, shape = effect_direction),
            inherit.aes = FALSE, color = "black", size = 4, alpha = 0, show.legend = TRUE
        ) +
        scale_fill_brewer(palette = "RdBu", direction = -1, drop = FALSE, na.value = "white") +
        scale_shape_manual(
            name   = "Direction",
            values = c("Positive" = 15, "Negative" = 16),
            breaks = c("Positive", "Negative")
        ) +
        facet_grid(category_facet ~ ., scales = "free_y", space = "free_y") +
        labs(x = "Country Income Classification", y = "", fill = "Coefficient\nMagnitude") +
        guides(
            fill  = guide_legend(order = 1),
            shape = guide_legend(order = 2, override.aes = list(alpha = 1, color = "black", size = 4))
        ) +
        theme_minimal(base_size = 12) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position  = "right",
            axis.text.y      = element_text(size = 8, hjust = 1),
            axis.text.x      = element_text(size = 9, face = "bold", angle = 45, hjust = 1),
            strip.text.y     = element_text(angle = 0, face = "bold", hjust = 0, size = 8),
            strip.background = element_rect(fill = "grey95", color = NA),
            legend.title     = element_text(size = 10, face = "bold"),
            legend.text      = element_text(size = 9)
        )
}

plot_m1 <- make_heatmap(step1_clean, map_m1)
print(plot_m1)

####****************************************************************************
##### 3. STEP 2: FOREST PLOTS #####
####****************************************************************************
cat("\n--- STARTING STEP 2 ---\n")

# 3.1 LOAD SCRIPT 2 DATA
#-------------------------------------------------------------------------------
step2 <- read_xlsx(path_s2_local)

# 3.2 DEFINE FOREST LISTS (Manuscript 1)
#-------------------------------------------------------------------------------
staple <- str_squish(c(
    "Cereal production, 1961 to 2023", "Corn production, 1961 to 2023", "Rice production, 2023",
    "Barley production, 2023", "Wheat production, 2023", "Bean production, 2023",
    "Soybean production, 2023", "Sesame seed production, 2023"
))
hort <- str_squish(c(
    "Apple production, 2023", "Avocado production, 2023", "Banana production, 2023",
    "Banana production by region, 1961 to 2023", "Cashew nut production, 2023",
    "Tomato production, 2023", "Potato production, 2023", "Yams production, 2023"
))
commodity <- str_squish(c(
    "Cocoa bean production, 2023", "Cocoa bean production by region, 1961 to 2023",
    "Coffee production by region, 1961 to 2023", "Green coffee beans production, 2023",
    "Oil palm production", "Sugar cane production, 2023", "Wine production, 2022"
))

cat_full <- c(
    "Staple crops (cereals & pulses/oilseeds)", "Horticulture (fruits, vegetables & tubers)",
    "Commodity & industrial crops"
)

cat_short_map <- c(
    "Staple crops (cereals & pulses/oilseeds)" = "Staple crops",
    "Horticulture (fruits, vegetables & tubers)" = "Horticulture",
    "Commodity & industrial crops" = "Commodity/industrial"
)

# 3.3 PROCESS STEP 2 DATA
#-------------------------------------------------------------------------------
infer_effect_unit <- function(effect_unit, effect_ci) {
    u <- str_squish(as.character(effect_unit))
    u[u == ""] <- NA_character_
    ec <- str_squish(as.character(effect_ci))
    pref <- str_match(ec, "^([A-Za-zβ]+)\\s*:")[, 2]
    pref <- str_squish(pref)
    pref[pref == ""] <- NA_character_
    out <- dplyr::coalesce(u, pref)
    case_when(
        tolower(out) %in% c("beta", "β") ~ "β",
        toupper(out) %in% c("OR", "RR", "IRR", "HR") ~ toupper(out),
        tolower(out) == "ratio" ~ "Ratio",
        TRUE ~ out
    )
}

step2 <- step2 %>%
    mutate(
        dependent_var = str_squish(as.character(dependent_var)),
        independent_var = str_squish(as.character(independent_var)),
        model_type = str_squish(as.character(model_type)),
        effect_ci_interpretable = str_squish(as.character(effect_ci_interpretable)),
        effect_unit2 = infer_effect_unit(effect_unit, effect_ci_interpretable),
        category_fig = case_when(
            dependent_var %in% staple ~ "Staple crops (cereals & pulses/oilseeds)",
            dependent_var %in% hort ~ "Horticulture (fruits, vegetables & tubers)",
            dependent_var %in% commodity ~ "Commodity & industrial crops",
            TRUE ~ NA_character_
        ),
        category_fig = factor(category_fig, levels = cat_full),
        category_short = unname(cat_short_map[as.character(category_fig)])
    ) %>%
    filter(!is.na(category_fig)) %>%
    filter(independent_var == "total_publications")

# 3.4 HELPERS (Forest Plot)
#-------------------------------------------------------------------------------
sig_bucket <- function(p) {
    case_when(
        p < 0.001 ~ "p < 0.001",
        p < 0.01 ~ "p < 0.01",
        p < 0.05 ~ "p < 0.05",
        TRUE ~ "Not significant"
    )
}
sig_colors <- c("p < 0.001" = "#00a6fb", "p < 0.01" = "#009E73", "p < 0.05" = "#D55E00", "Not significant" = "#CCCCCC")

format_num <- function(x, unit = "β") {
    if (is.na(x) || !is.finite(x)) {
        return(NA_character_)
    }
    if (unit %in% c("OR", "Ratio", "RR", "IRR", "HR")) {
        if (x >= 1000 | x < 0.001) {
            return(sprintf("%.2e", x))
        }
        return(sprintf("%.4f", x))
    }
    ax <- abs(x)
    if (ax < 0.001) {
        return(sprintf("%.2e", x))
    }
    if (ax < 0.01) {
        return(sprintf("%.5f", x))
    }
    if (ax < 1) {
        return(sprintf("%.4f", x))
    }
    if (ax < 10) {
        return(sprintf("%.3f", x))
    }
    if (ax < 1000) {
        return(sprintf("%.2f", x))
    }
    if (ax < 1e6) {
        return(sprintf("%.1f", x))
    }
    sprintf("%.2e", x)
}

symlog_trans <- function(threshold = 1) {
    trans <- function(x) sign(x) * log10(1 + abs(x) / threshold)
    inv <- function(x) sign(x) * (10^(abs(x)) - 1) * threshold
    scales::trans_new(name = paste0("symlog-", format(threshold)), transform = trans, inverse = inv, domain = c(-Inf, Inf))
}

nice_limit_125 <- function(x) {
    x <- abs(x)
    if (!is.finite(x) || x <= 0) {
        return(1)
    }
    k <- floor(log10(x))
    base <- 10^k
    m <- x / base
    mult <- if (m <= 1) 1 else if (m <= 2) 2 else if (m <= 5) 5 else 10
    mult * base
}

symlog_breaks_smart <- function(limit, max_ticks = 7) {
    limit <- abs(limit)
    if (!is.finite(limit) || limit <= 0) {
        return(c(-1, 0, 1))
    }
    n_side <- max(2, floor((max_ticks - 1) / 2))
    kmax <- ceiling(log10(limit))
    if (kmax + 1 <= n_side) {
        k_seq <- 0:kmax
    } else {
        step <- ceiling(kmax / (n_side - 1))
        k_seq <- seq(0, kmax, by = step)
        if (tail(k_seq, 1) != kmax) k_seq <- c(k_seq, kmax)
    }
    pos <- 10^k_seq
    pos <- sort(unique(c(pos, limit)))
    br <- sort(unique(c(-rev(pos), 0, pos)))
    br[abs(br) <= limit * 1.000001]
}

label_symlog_compact <- function(x) {
    vapply(x, function(z) {
        if (!is.finite(z)) {
            return("")
        }
        if (z == 0) {
            return("0")
        }
        az <- abs(z)
        if (az < 10000) {
            return(format(z, scientific = FALSE, trim = TRUE))
        }
        s <- formatC(z, format = "e", digits = 1)
        s <- gsub("e\\+0?", "e+", s)
        s <- gsub("e-0?", "e-", s)
        s
    }, character(1))
}

ratio_axis_spec <- function(lo, hi, max_ticks = 6, x_right_fixed = NULL) {
    rng <- range(c(lo, hi), finite = TRUE)
    lo0 <- rng[1]
    hi0 <- rng[2]
    narrow <- (hi0 / lo0) < 1.30
    if (narrow) {
        pad <- max((hi0 - lo0) * 0.10, 0.0002)
        x_left <- max(lo0 - pad, 1e-8)
        if (!is.null(x_right_fixed)) {
            x_right <- x_right_fixed
            if (x_left >= x_right) x_left <- x_right / 1.002
            xlim <- c(x_left, x_right)
        } else {
            xlim <- c(x_left, hi0 + pad)
        }
        br <- pretty(xlim, n = max_ticks)
        br <- sort(unique(c(br, 1)))
        br <- br[br > 0 & br >= xlim[1] & br <= xlim[2]]
        span <- diff(xlim)
        digits <- if (span < 0.01) 4 else if (span < 0.1) 3 else 2
        lab <- function(x) formatC(x, format = "f", digits = digits)
        list(xlim = xlim, breaks = br, labels = lab, angle = 0)
    } else {
        xlim <- rng * c(1 / 1.08, 1.08)
        br <- scales::log_breaks(n = max_ticks)(xlim)
        br <- br[br > 0]
        lab <- scales::label_number(accuracy = 0.01)
        list(xlim = xlim, breaks = br, labels = lab, angle = 45)
    }
}

align_and_combine <- function(p_left, p_right, widths = c(0.80, 0.20)) {
    g1 <- ggplotGrob(p_left)
    g2 <- ggplotGrob(p_right)
    max_h <- grid::unit.pmax(g1$heights, g2$heights)
    g1$heights <- max_h
    g2$heights <- max_h
    g <- gridExtra::arrangeGrob(g1, g2, ncol = 2, widths = widths)
    grid::grid.newpage()
    grid::grid.draw(g)
    invisible(g)
}

make_beta_group_figure <- function(cat_keep_full, fig_title = NULL, use_letters = FALSE,
                                   strip_text_size = 8, table_num_size = 2.8,
                                   left_plot_margin_right = 35, combine_widths = c(0.80, 0.20),
                                   threshold_symlog = 1, max_ticks = 7) {
    cat_keep_short <- unname(cat_short_map[cat_keep_full])
    dat0 <- step2 %>%
        filter(effect_unit2 == "β") %>%
        filter(as.character(category_fig) %in% cat_keep_full) %>%
        mutate(
            eff = suppressWarnings(as.numeric(estimate_interpretable)),
            lo  = suppressWarnings(as.numeric(ci_low_interpretable)),
            hi  = suppressWarnings(as.numeric(ci_high_interpretable)),
            sig = sig_bucket(p_adj_fdr_bh)
        ) %>%
        filter(is.finite(eff), is.finite(lo), is.finite(hi))

    if (nrow(dat0) == 0) {
        return(invisible(NULL))
    }

    present_cats <- dat0 %>%
        distinct(category_short) %>%
        mutate(category_short = factor(category_short, levels = cat_keep_short)) %>%
        arrange(category_short) %>%
        pull(category_short) %>%
        as.character()

    if (use_letters) {
        letter_map_fixed <- setNames(LETTERS[seq_along(cat_keep_short)], cat_keep_short)
        dat <- dat0 %>%
            mutate(
                strip = unname(letter_map_fixed[category_short]),
                strip = factor(strip, levels = LETTERS[seq_along(cat_keep_short)])
            )
    } else {
        dat <- dat0 %>% mutate(strip = factor(category_short, levels = present_cats))
    }

    dat <- dat %>%
        arrange(strip, desc(abs(eff))) %>%
        mutate(dependent_var = factor(dependent_var, levels = rev(unique(dependent_var)))) %>%
        rowwise() %>%
        mutate(display_text = paste0(format_num(eff, "β"), " (", format_num(lo, "β"), ", ", format_num(hi, "β"), ")")) %>%
        ungroup()

    x_min <- min(c(dat$eff, dat$lo, dat$hi), na.rm = TRUE)
    x_max <- max(c(dat$eff, dat$lo, dat$hi), na.rm = TRUE)
    max_abs <- max(abs(x_min), abs(x_max), na.rm = TRUE)
    lim <- nice_limit_125(max_abs)
    lim_pad <- lim * 1.05
    xlim_use <- c(-lim_pad, lim_pad)
    brks <- symlog_breaks_smart(limit = lim, max_ticks = max_ticks)

    p <- ggplot(dat, aes(y = dependent_var, x = eff, xmin = lo, xmax = hi)) +
        geom_vline(xintercept = 0, linewidth = 0.5) +
        geom_errorbarh(height = 0, linewidth = 0.3, color = "black") +
        geom_point(aes(fill = sig), shape = 21, color = "black", stroke = 0.4, size = 3) +
        geom_text(aes(label = model_type), hjust = -0.2, vjust = -0.4, size = 2.7, color = "gray50") +
        scale_fill_manual(values = sig_colors) +
        scale_x_continuous(
            trans = symlog_trans(threshold = threshold_symlog), breaks = brks, minor_breaks = NULL,
            labels = label_symlog_compact, expand = expansion(mult = c(0.01, 0.01))
        ) +
        facet_grid(strip ~ ., scales = "free_y", space = "free_y") +
        labs(title = fig_title, x = "Effect size (symmetric log scale)", y = NULL, fill = "Significance") +
        coord_cartesian(xlim = xlim_use, clip = "off") +
        guides(x = guide_axis(check.overlap = FALSE)) +
        theme_minimal(base_size = 11) +
        theme(
            plot.margin = margin(t = 5, r = left_plot_margin_right, b = 10, l = 5),
            strip.placement = "outside",
            strip.text.y.right = element_text(angle = -90, face = "bold", size = strip_text_size),
            strip.background.y = element_rect(fill = "#d6e8ff", color = NA),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 9),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom",
            panel.spacing = unit(1, "lines"),
            plot.title = element_text(face = "bold")
        )

    p_table <- ggplot(dat, aes(y = dependent_var, x = 1)) +
        geom_text(aes(label = display_text), hjust = 0, size = table_num_size) +
        facet_grid(strip ~ ., scales = "free_y", space = "free_y") +
        scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
        labs(title = "\u03B2 (95% CI)") +
        theme_void(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 9, hjust = 0),
            strip.text = element_blank(),
            strip.background = element_blank(),
            panel.spacing = unit(1, "lines"),
            plot.margin = margin(t = 0, r = 10, b = 10, l = 0)
        )

    align_and_combine(p, p_table, widths = combine_widths)
}

make_ratio_or_figure <- function(fig_title = NULL, strip_text_size = 8, table_num_size = 2.8,
                                 cat_keep_full = NULL, use_letters = FALSE, left_plot_margin_right = 35,
                                 combine_widths = c(0.80, 0.20), max_ticks = 6, x_right_fixed = NULL) {
    dat0 <- step2 %>%
        filter(effect_unit2 %in% c("OR", "Ratio", "RR", "IRR", "HR")) %>%
        {
            if (!is.null(cat_keep_full)) filter(., as.character(category_fig) %in% cat_keep_full) else .
        } %>%
        mutate(
            eff = suppressWarnings(as.numeric(estimate_interpretable)),
            lo  = suppressWarnings(as.numeric(ci_low_interpretable)),
            hi  = suppressWarnings(as.numeric(ci_high_interpretable)),
            sig = sig_bucket(p_adj_fdr_bh)
        ) %>%
        filter(is.finite(eff), is.finite(lo), is.finite(hi), eff > 0, lo > 0, hi > 0)

    if (nrow(dat0) == 0) {
        return(invisible(NULL))
    }

    strip_levels_base <- if (!is.null(cat_keep_full)) unname(cat_short_map[cat_keep_full]) else unname(cat_short_map[cat_full])
    present_cats <- dat0 %>%
        distinct(category_short) %>%
        mutate(category_short = factor(category_short, levels = strip_levels_base)) %>%
        arrange(category_short) %>%
        pull(category_short) %>%
        as.character()

    if (use_letters) {
        fixed_cats <- if (!is.null(cat_keep_full)) unname(cat_short_map[cat_keep_full]) else present_cats
        letter_map_fixed <- setNames(LETTERS[seq_along(fixed_cats)], fixed_cats)
        dat <- dat0 %>%
            mutate(
                strip = unname(letter_map_fixed[category_short]),
                strip = factor(strip, levels = LETTERS[seq_along(fixed_cats)])
            )
    } else {
        dat <- dat0 %>% mutate(strip = factor(category_short, levels = present_cats))
    }

    dat <- dat %>%
        arrange(strip, desc(abs(log(eff)))) %>%
        mutate(dependent_var = factor(dependent_var, levels = rev(unique(dependent_var)))) %>%
        rowwise() %>%
        mutate(display_text = paste0(format_num(eff, effect_unit2), " (", format_num(lo, effect_unit2), ", ", format_num(hi, effect_unit2), ")")) %>%
        ungroup()

    ax <- ratio_axis_spec(lo = dat$lo, hi = dat$hi, max_ticks = max_ticks, x_right_fixed = x_right_fixed)

    p <- ggplot(dat, aes(y = dependent_var, x = eff, xmin = lo, xmax = hi)) +
        geom_vline(xintercept = 1, linewidth = 0.5) +
        geom_errorbarh(height = 0, linewidth = 0.3, color = "black") +
        geom_point(aes(fill = sig), shape = 21, color = "black", stroke = 0.4, size = 3) +
        geom_text(aes(label = model_type), hjust = -0.2, vjust = -0.4, size = 2.7, color = "gray50") +
        scale_fill_manual(values = sig_colors) +
        scale_x_continuous(trans = "log10", breaks = ax$breaks, labels = ax$labels, expand = expansion(mult = c(0.02, 0.06))) +
        facet_grid(strip ~ ., scales = "free_y", space = "free_y") +
        labs(title = fig_title, x = "Effect size (log scale)", y = NULL, fill = "Significance") +
        coord_cartesian(xlim = ax$xlim, clip = "off") +
        guides(x = guide_axis(check.overlap = FALSE)) +
        theme_minimal(base_size = 11) +
        theme(
            plot.margin = margin(t = 5, r = left_plot_margin_right, b = 10, l = 5),
            strip.placement = "outside",
            strip.text.y.right = element_text(angle = -90, face = "bold", size = strip_text_size),
            strip.background.y = element_rect(fill = "#d6e8ff", color = NA),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 9),
            axis.text.x = element_text(angle = ax$angle, hjust = ifelse(ax$angle == 0, 0.5, 1), size = 8),
            legend.position = "bottom",
            panel.spacing = unit(1, "lines"),
            plot.title = element_text(face = "bold")
        )

    p_table <- ggplot(dat, aes(y = dependent_var, x = 1)) +
        geom_text(aes(label = display_text), hjust = 0, size = table_num_size) +
        facet_grid(strip ~ ., scales = "free_y", space = "free_y") +
        scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
        labs(title = "Ratio / OR (95% CI)") +
        theme_void(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 9, hjust = 0),
            strip.text = element_blank(),
            strip.background = element_blank(),
            panel.spacing = unit(1, "lines"),
            plot.margin = margin(t = 0, r = 10, b = 10, l = 0)
        )

    align_and_combine(p, p_table, widths = combine_widths)
}

# 3.5 GENERATE FIGURES (Manuscript 1)
#-------------------------------------------------------------------------------
cat("Generating Forest Plots (Manuscript 1)...\n")

cats_crops <- c(
    "Staple crops (cereals & pulses/oilseeds)",
    "Horticulture (fruits, vegetables & tubers)",
    "Commodity & industrial crops"
)

# 3) FIGURA BETA (Crops)
make_beta_group_figure(
    cat_keep_full = cats_crops, use_letters = TRUE, strip_text_size = 7,
    table_num_size = 2.8, threshold_symlog = 1, max_ticks = 7, fig_title = "Crops: Beta"
)

# 4) FIGURA RATIO (Crops)
make_ratio_or_figure(
    strip_text_size = 7, table_num_size = 2.8, cat_keep_full = cats_crops,
    use_letters = TRUE, max_ticks = 6, fig_title = "Crops: Ratio/OR"
)


####****************************************************************************
##### 4. STEP 3: INTERACTIONS #####
####****************************************************************************
cat("\n--- STARTING STEP 3 ---\n")

# 4.1 LOAD SCRIPT 3 DATA
#-------------------------------------------------------------------------------
step3 <- readxl::read_xlsx(path_s3_local)

# 4.2 CALCULATE INTERACTIONS
#-------------------------------------------------------------------------------
X_SCALE <- 100

step3 <- step3 %>%
    mutate(
        is_log_model = !is.na(effect_unit) & (effect_unit == "OR" | effect_unit == "Ratio"),

        # Interaction
        interaction_exp_per100 = if_else(
            is_log_model & is.finite(estimate_raw),
            exp(X_SCALE * estimate_raw),
            NA_real_
        ),
        interaction_exp_per100_ci_low = if_else(
            is_log_model & is.finite(ci_low_raw),
            exp(X_SCALE * ci_low_raw),
            NA_real_
        ),
        interaction_exp_per100_ci_high = if_else(
            is_log_model & is.finite(ci_high_raw),
            exp(X_SCALE * ci_high_raw),
            NA_real_
        ),

        # Simple Slopes
        exp_X_per100_at_Zp25 = if_else(
            is_log_model & is.finite(x_slope_p25_raw),
            exp(X_SCALE * x_slope_p25_raw),
            NA_real_
        ),
        exp_X_per100_at_Zp25_ci_low = if_else(
            is_log_model & is.finite(x_slope_p25_ci_low_raw),
            exp(X_SCALE * x_slope_p25_ci_low_raw),
            NA_real_
        ),
        exp_X_per100_at_Zp25_ci_high = if_else(
            is_log_model & is.finite(x_slope_p25_ci_high_raw),
            exp(X_SCALE * x_slope_p25_ci_high_raw),
            NA_real_
        ),
        exp_X_per100_at_Zp75 = if_else(
            is_log_model & is.finite(x_slope_p75_raw),
            exp(X_SCALE * x_slope_p75_raw),
            NA_real_
        ),
        exp_X_per100_at_Zp75_ci_low = if_else(
            is_log_model & is.finite(x_slope_p75_ci_low_raw),
            exp(X_SCALE * x_slope_p75_ci_low_raw),
            NA_real_
        ),
        exp_X_per100_at_Zp75_ci_high = if_else(
            is_log_model & is.finite(x_slope_p75_ci_high_raw),
            exp(X_SCALE * x_slope_p75_ci_high_raw),
            NA_real_
        ),

        # Ratio of Effects
        slope_diff_raw_p75_minus_p25 = if_else(
            is_log_model & is.finite(x_slope_p75_raw) & is.finite(x_slope_p25_raw),
            x_slope_p75_raw - x_slope_p25_raw,
            NA_real_
        ),
        ratio_X_per100_high_vs_lowZ = if_else(
            is_log_model & is.finite(slope_diff_raw_p75_minus_p25),
            exp(X_SCALE * slope_diff_raw_p75_minus_p25),
            NA_real_
        ),

        # Formatting
        exp_X_per100_at_Zp25_text = if_else(
            is_log_model & is.finite(exp_X_per100_at_Zp25),
            sprintf(
                "Exp(+%d X) @ Zp25: %.4f [%.4f–%.4f]",
                X_SCALE, exp_X_per100_at_Zp25, exp_X_per100_at_Zp25_ci_low, exp_X_per100_at_Zp25_ci_high
            ),
            NA_character_
        ),
        exp_X_per100_at_Zp75_text = if_else(
            is_log_model & is.finite(exp_X_per100_at_Zp75),
            sprintf(
                "Exp(+%d X) @ Zp75: %.4f [%.4f–%.4f]",
                X_SCALE, exp_X_per100_at_Zp75, exp_X_per100_at_Zp75_ci_low, exp_X_per100_at_Zp75_ci_high
            ),
            NA_character_
        ),
        ratio_X_per100_text = if_else(
            is_log_model & is.finite(ratio_X_per100_high_vs_lowZ),
            sprintf(
                "Ratio of Exps (+%d X): %.4f (Zp75 vs Zp25)",
                X_SCALE, ratio_X_per100_high_vs_lowZ
            ),
            NA_character_
        )
    )

# 4.3 SAVE STEP 3 RESULTS (Manuscript 1)
#-------------------------------------------------------------------------------
writexl::write_xlsx(step3, path_s3_out)
cat("Step 3 interaction updated file saved to:", path_s3_out, "\n")
cat("\n--- SCRIPT COMPLETE (MANUSCRIPT 1) ---\n")
