# List of functions used in analysis of  -----------------------------------------------------------------
# various Maritz surveys  -----------------------------------------------------------------


# . -----------------------------------------------------------------------


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Helper functions -----------------------------------------------------------------

#Function to wrap a string based on the provided length
wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

#Function to extract a legend from a ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Plotting ----------------------------------------------------------------


#Function to plot and save Pairwise Analyses, with a horizontal orientation
plot_calc_pairs <- function( df = rs,
                             x,
                             y3,
                             ex_label = name_token,
                             caption = survey_info
) {

  #do some organizing
  x <- enquo(x)
  y3 <- enquo(y3)
  yn <- quo_name(y3)


  #calculate the proportions
  df2 <- df %>%
    filter(!is.na(!!y3)) %>%
    mutate(tot_n = nrow(.)) %>%
    count(!!x, !!y3, tot_n) %>%
    group_by( !!x ) %>%
    mutate(prop = n / sum(n),
           x_temp = paste0(!!x, "\n (n = ", sum(n), ")")
    ) %>%
    ungroup() %>%
    mutate(x2 = factor(x_temp, levels = unique(x_temp))) %>%
    select(-n, -x_temp)

  tot_n <- df2$tot_n[[1]]

  groups_n <- df2 %>% count(!!y3 ) %>% nrow(.)
  groups_n_not_used_begin <- df2 %>%
    filter(!!y3 == "Not used in past 6 months") %>%
    count(!!y3 ) %>%
    nrow(.)
  groups_n_not_used_end <- df2 %>%
    filter(!!y3 == "Do Not Have Product") %>%
    count(!!y3 ) %>%
    nrow(.)
  groups_n_not_used_end <- groups_n_not_used_end +
    df2 %>%
    filter(!!y3 == "NA - Question not asked") %>%
    count(!!y3 ) %>%
    nrow(.)

  n_blacks_begin <- if (length(groups_n_not_used_begin) == 0) {
    0
  } else if (groups_n_not_used_begin > 0 ) {groups_n_not_used_begin} else {0}

  n_blacks_end <- if (length(groups_n_not_used_end) == 0) {
    0
  } else if (groups_n_not_used_end > 0 ) {groups_n_not_used_end} else {0}

  n_colors = groups_n - n_blacks_begin - n_blacks_end

  my_cols <- c(rep("darkgray", n_blacks_begin),
               colorRampPalette(brewer.pal(11,"RdYlGn"))(n_colors),
               rep("darkgray", n_blacks_end)
  )


  #plots
  df_plot <- df2 %>%
    ggplot(mapping = aes( x2 ), environment = environment() ) +
    geom_bar(stat = "identity", aes_string(y = "prop", fill = yn), position = position_fill(reverse = TRUE)) +
    coord_flip() +
    labs(x = quo_name(x),
         y = "Proportion of responses",
         title = paste0(ex_label, " - Proportion of ", quo_name(y3),
                        " by ", quo_name(x), " (n = ", tot_n, ")"
         ),
         fill = quo_name(y3),
         caption = caption

    ) +
    #scale_fill_brewer(palette = "RdYlGn", direction = 1) +
    scale_fill_manual(values = my_cols) +
    guides(fill = guide_legend(quo_name(y3), reverse = TRUE))

  ggsave(paste0(output_folder, "/",
                name_token, "RS_graph_",
                ex_label, "_Proportion_of_",
                quo_name(y3), "_by_", quo_name(x), ".png"),
         width = 9,
         height = 2.75)

  print(df_plot)
}

#Function to plot and save Pairwise Analyses, with a vertical orientation
plot_calc_pairs_vert <- function( df = rs, x, y3, ex_label = name_token, caption = survey_info) {

  #do some organizing
  x <- enquo(x)
  y3 <- enquo(y3)
  yn <- quo_name(y3)


  #calculate the proportions
  df2 <- df %>%
    filter(!is.na(!!y3)) %>%
    mutate(tot_n = nrow(.)) %>%
    count(!!x, !!y3, tot_n) %>%
    group_by( !!x ) %>%
    mutate(prop = n / sum(n),
           x_temp = paste0(!!x, "\n (n = ", sum(n), ")")
    ) %>%
    ungroup() %>%
    mutate(x2 = factor(x_temp, levels = unique(x_temp))) %>%
    select(-n, -x_temp)


  tot_n <- df2$tot_n[[1]]

  groups_n <- df2 %>% count(!!y3 ) %>% nrow(.)
  groups_n_not_used_begin <- df2 %>% filter(!!y3 == "Not used in past 6 months") %>% count(!!y3 ) %>% nrow(.)
  groups_n_not_used_end <- df2 %>% filter(!!y3 == "Do Not Have Product") %>% count(!!y3 ) %>% nrow(.)
  groups_n_not_used_end <- groups_n_not_used_end +
    df2 %>% filter(!!y3 == "NA - Question not asked") %>% count(!!y3 ) %>% nrow(.)

  n_blacks_begin <- if (length(groups_n_not_used_begin) == 0) {
    0
  } else if (groups_n_not_used_begin > 0 ) {groups_n_not_used_begin} else {0}

  n_blacks_end <- if (length(groups_n_not_used_end) == 0) {
    0
  } else if (groups_n_not_used_end > 0 ) {groups_n_not_used_end} else {0}

  n_colors = groups_n - n_blacks_begin - n_blacks_end

  my_cols <- c(rep("darkgray", n_blacks_begin),
               colorRampPalette(brewer.pal(11,"RdYlGn"))(n_colors),
               rep("darkgray", n_blacks_end)
  )


  #plots
  df_plot <- df2 %>%
    ggplot(mapping = aes( x2 ), environment = environment() ) +
    geom_bar(stat = "identity", aes_string(y = "prop", fill = yn), position = position_fill(reverse = TRUE)) +
    #coord_flip() +
    labs(x = quo_name(x),
         y = "Proportion of responses",
         title = paste0(ex_label, " - Proportion of ", quo_name(y3),
                        " by ", quo_name(x), " (n = ", tot_n, ")"
         ),
         fill = quo_name(y3),
         caption = caption

    ) +
    #scale_fill_brewer(palette = "RdYlGn", direction = 1) +
    scale_fill_manual(values = my_cols) +
    guides(fill = guide_legend(quo_name(y3), reverse = TRUE))

  ggsave(paste0(output_folder, "/",
                name_token, "RS_graph_",
                ex_label, "_Proportion_of_",
                quo_name(y3), "_by_", quo_name(x), ".png"),
         width = 3.5,
         height = 5)

  print(df_plot)
}

#Function to perform an nps.test
nps_model_f <- function(dfx, dfy = NULL, x_col_name = NA, y_col_name = NA) {
  x_col <- if ("Likely_to_Recommend" %in% names(dfx)) {dfx$Likely_to_Recommend} else {dfx}
  y_col <- if ("Likely_to_Recommend" %in% names(dfy)) {dfy$Likely_to_Recommend} else {dfy}
  x_col_name <- if (is.na(x_col_name)) { deparse(substitute(dfx)) } else {x_col_name}
  y_col_name <- if (is.na(y_col_name)) { deparse(substitute(dfy)) } else {y_col_name}

  test1 <- nps.test(x_col, y_col)

  test1["x"] <- x_col_name
  test1["y"] <- y_col_name
  test1["int1"] <- test1[[4]][1]
  test1["int2"] <- test1[[4]][2]
  test1["x_lab"] <- paste0(x_col_name, "\n (n = ", test1["n.x"], ")")
  test1["y_lab"] <- paste0(y_col_name, "\n (n = ", test1["n.y"], ")")
  test2 <- test1[-4]

  as_tibble(test2) %>%
    select(x, y, everything())
}

#Function to plot and save the results of one sample nps.tests
nps_plot_one_sample <- function(df, x_name = NA) {

  x_name <- if (is.na(x_name)) { deparse(substitute(df)) } else {x_name}

  df$int1[df$int1 > 1 & !is.na(df$int1)] <- 1
  df$int2[df$int2 < -1 & !is.na(df$int2)] <- -1

  nps_plot <- df %>%
    filter(type == "One sample") %>%
    gather(nps.x, nps.y, int1, int2, key = "test", value = "NPS") %>%
    filter(test != "nps.y") %>%
    ggplot() +
    geom_point(aes(x = x_lab, y = NPS, group = test, color = test), size = 4) +
    geom_line(aes(x = x_lab, y = NPS, group = test, color = test), size = 2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    ylim(-1, 1) +
    labs(x = x_name,
         y = "NPS") +
    scale_colour_manual(values = c("grey65", "grey65", "skyblue3"),
                        breaks = c("int1", "nps.x", "int2"),
                        labels = c("Upper 95% \nConfidence \nInterval",
                                   "NPS",
                                   "Lower 95% \nConfidence \nInterval"))


  ggsave(paste0(output_folder, "/",
                name_token, "NPS_graph_One_sample_",
                x_name, "_by_NPS.png"))

  print(nps_plot)

}



# . -----------------------------------------------------------------------



# Assign Factors ----------------------------------------------------------
apply_factors <- function(df, lvls) {
  for (i in seq_along(lvls)) {
    for (j in 1:ncol(df)) {
      df[[j]] <- if ( class(lvls[[i]]) == class(df[[j]]) &&
                      all(df[[j]] %in% lvls[[i]])) {
        factor(df[[j]], levels = lvls[[i]])
      } else {
        df[[j]]
      }
    }
  }
  return(df)
}


fb_assign_factors <- function(df) {

#create df_names_class
df_names_class <- tibble( "names" = names(df), "class_orig" = map(df, class))

#set levels for each factor
levels_all <- list()
levels_all[["levels_tf"]] <- c(TRUE,
                               FALSE,
                               NA)
levels_all[["levels_sat"]] <- c("Not used in past 6 months",
                                "Completely Dissatisfied",
                                "Somewhat Dissatisfied",
                                "Neutral",
                                "Somewhat Satisfied",
                                "Completely Satisfied",
                                "Do Not Have Product",
                                "NA - Question not asked",
                                NA )
levels_all[["levels_agree"]] <- c("Completely Disagree",
                                  "Somewhat Disagree",
                                  "Neither Disagree Nor Agree",
                                  "Neutral",
                                  "Somewhat Agree",
                                  "Completely Agree",
                                  "NA - Question not asked",
                                  NA)
levels_all[["levels_likely"]] <- c("Highly Unlikely",
                                   "Somewhat Unlikely",
                                   "Neither Unlikely Nor Likely",
                                   "Somewhat Likely",
                                   "Highly Likely",
                                   NA)
levels_all[["levels_ease"]] <- c("Extremely Difficult",
                                 "Somewhat Difficult",
                                 "Neither Difficult Nor Easy",
                                 "Somewhat Easy",
                                 "Extremely Easy",
                                 NA)
levels_all[["levels_real_pbo_indicator"]] <- c("PBO",
                                               "CRM & Other RM",
                                               "'Other' RM",
                                               "No RM",
                                               NA)
levels_all[["levels_acct_status"]] <- c("Active",
                                        "Inactive/Dormant",
                                        "None",
                                        NA)
levels_all[["levels_yes_no"]] <- c("YES",
                                   "Yes",
                                   "No",
                                   "NO",
                                   "None",
                                   "NA - Question not asked",
                                   NA)
levels_all[["levels_real_rm_indicator"]] <- c("Yes RM",
                                              "No RM",
                                              NA)
levels_all[["levels_NPS_category"]] <- c("Detractors",
                                         "Passives",
                                         "Promoters",
                                         NA)
levels_all[["levels_acct_type"]] <- c("Retail",
                                      "Both",
                                      "Commercial",
                                      NA)
levels_all[["levels_age_bucket"]] <- c("18 to 24",
                                       "25 to 34",
                                       "35 to 44",
                                       "45 to 54",
                                       "55 to 64",
                                       "65 or more",
                                       NA)
levels_all[["levels_tenure_bucket"]] <- c("Less than 2",
                                          "2 to 5",
                                          "6 to 10",
                                          "11 to 20",
                                          "20 or more",
                                          "No Idea",
                                          NA)
levels_all[["levels_access_time"]] <- c("0-7 days",
                                        "8-30 days",
                                        "31-90 days",
                                        ">90 days",
                                        "None",
                                        NA)
levels_all[["levels_marital_status"]] <- c("Married including registered domestic partner",
                                           "Unmarried including single, divorce, widowed",
                                           "Prefer not to disclose",
                                           "NA - Question not asked",
                                           NA)
levels_all[["levels_hh_income"]] <- c("Less than $50, 000",
                                      "$50, 000 to $99, 999",
                                      "$100, 000 to $149, 999",
                                      "$150, 000 to $249, 999",
                                      "$250, 000 to $499, 999",
                                      "$500, 000 or more",
                                      "Prefer not to disclose",
                                      "NA - Question not asked",
                                      NA)
levels_all[["levels_bus_gross_rev"]] <- c("Less than $50, 000",
                                          "$50, 000 to under $100, 000",
                                          "$100, 000 to under $250, 000",
                                          "$250, 000 to under $500, 000",
                                          "$500, 000 to under $1 million",
                                          "$1 million to under $2.5 million",
                                          "$2.5 million to under $5 million",
                                          "$5 million to under $10 million",
                                          "$10 million or over",
                                          "Prefer not to disclose",
                                          "NA - Question not asked",
                                          NA)
levels_all[["levels_bus_employees"]] <- c("1 (just myself)",
                                          "2-10",
                                          "11-50",
                                          "51-100",
                                          "101-999",
                                          "1000 or more",
                                          "Prefer not to disclose",
                                          "NA - Question not asked",
                                          NA)
levels_all[["levels_diamond_rating"]] <- c("Diamond 1",
                                           "Diamond 2",
                                           "Diamond 3",
                                           "Diamond 4",
                                           "Diamond 5",
                                           "Diamond 6",
                                           "No Diamond Rating",
                                           NA )
levels_all[["levels_ltr"]] <- c(0:10, NA)
levels_all[["levels_survey_phase"]] <- c("Phase 1",
                                         "Phase 2",
                                         "Phase 3",
                                         "Phase 4+",
                                         NA )





#apply factor levels to all columns that fit within each levels values
for (i in seq_along(levels_all)) {
  for (j in 1:ncol(df)) {
    df[[j]] <- if ( all(df[[j]] %in% levels_all[[i]])) {
      factor(df[[j]], levels = levels_all[[i]])
    } else {
      df[[j]]
    }
  }
}



#update names variable to verify if all appropriate fields were assigned a factor
df_names_class[["class_updated"]] <- map(df, class)

return(df)

}
