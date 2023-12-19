library(tidyverse)
library(haven)
library(modelsummary)
library(panelView)
library(fixest)
library(insight)
library(broom)
library(kableExtra)
library(gtsummary)
library(rdrobust)
library(ggpubr)
library(glue)
library(stargazer)

options(modelsummary_format_numeric_latex = "plain")

# load the dataset ----
data <- read_dta("data/subway_analysis_use.dta")

################################################################################
# Summary Statistics: replication of table 1 of Lei and Zhou (table 1) ----
################################################################################

# rename and select only variables that will go into the table
summary_table <- data |> 
  select(
    Mayor_promotion3y, Mayor_connection_work, Mayor_age, Per_pop, gdp, rev, 
    GRP_growth, Mayor_plan, inv1_per, GRP_per, land_per, rev_per
  ) |>
  rename(
    `Mayor promoted within three years` = Mayor_promotion3y,
    `Mayor connection` = Mayor_connection_work,
    `Mayor age` = Mayor_age,
    `City population` = Per_pop,
    `City GDP (billion ¥)` = gdp,
    `City fiscal revenue (billion ¥)` = rev,
    `City GDP growth rate (%)` = GRP_growth,
    `Mayor obtaining subway approval` = Mayor_plan,
    `City investment in infrastructure per capita (¥)` = inv1_per,
    `City GDP per capita (¥)` = GRP_per,
    `City land sales revenue per capita (¥)` = land_per,
    `City fiscal revenue per capita (¥)` = rev_per
  )

# create table with summary stats using datasummary and save it in .tex file
datasummary(
  All(summary_table) ~ N + mean * Arguments(na.rm = T) + sd * Arguments(na.rm = T) + min * Arguments(na.rm = T) + 
    max * Arguments(na.rm = T),
  data = table1,
  col.names = c("Variable", "N", "Mean", "St. Dev.", "Min", "Max"),
  output = "tables/summary_stats_replication.tex"
)

################################################################################
# Table A1 ----
################################################################################
tmp <- data |> 
  filter(fsj2 == 0)

datasummary(factor(Year) ~ N * factor(Mayor_plan),
            col.names = c("Year", "Num. Control", "Num. Treated"),
            output = "tables/num_treated_by_year.tex",
            data = tmp)

################################################################################
# Figure A1 ----
################################################################################

panelview(Mayor_promotion3y ~ Mayor_plan, 
          data = tmp,
          index = c("City_Code", "Year"),
          xlab = "",
          ylab = "",
          axis.lab.gap = c(0, 5),
          main = "Subway Approval Treatment"
) 

ggsave("figures/struct_treat_assignmen.png", height = 10, width = 10)

rm(list=setdiff(ls(), "data"))

################################################################################
# Generalized Difference-in-Difference (Table 2) -----
################################################################################

# Replicate authors' main findings: table 2 on page 461

# model 1 
m1 <- feols(Mayor_promotion3y ~ Mayor_plan |
              Year + City_Code,
            ~ City_Code,
            data = filter(data, fsj2 == 0)
            )

# model 2
m2 <- feols(
  Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work |
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
  )

# model 3
m3 <- feols(
  Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
  )

# model 4
m4 <- feols(
  Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + Year:pro_code + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
  )

modelsummary(list(m1, m2, m3, m4),
             coef_map = c("Mayor_plan" = "Subway approval"),
             gof_map = "nobs",
             add_rows = data.frame(
               Specification = c(
                 "City FE",
                 "Year FE",
                 "Mayor controls",
                 "City controls",
                 "Province-year FE"
               ),
               Model1 = c("$\\checkmark$", "$\\checkmark$", "", "", ""),
               Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", ""),
               Model3 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model4 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
             ),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
             output = "latex"
             ) |> 
  add_header_above(c(" " = 1, "Mayor Promoted Within Three Years" = 4)) |> 
  save_kable(file = "tables/generalized_DID_table_2.tex")
  
rm(list=setdiff(ls(), "data"))

################################################################################
# Mayor Fixed Effects ----
################################################################################

rm(list=setdiff(ls(), "data"))

dependent_variables <- c("Mayor_promotion1y", "Mayor_promotion2y", "Mayor_promotion3y", 
                         "Mayor_promotion4y", "Mayor_promotion5y")

results <- list()

for (i in seq_along(dependent_variables)) {
  dep_var <- dependent_variables[i]
  
  # model with only Mayor_plan
  formula_mayor_only <- paste(dep_var, "~ Mayor_plan | Year + City_Code + Mayor_leaderindex")
  model_mayor_only <- feols(as.formula(formula_mayor_only), 
                            ~ City_Code, 
                            data = filter(data, fsj2 == 0))
  
  # model with Mayor_plan, time-variant controls for mayors, and city-level covariates
  formula_with_covariates <- paste(dep_var, "~ Mayor_plan + Mayor_age + Mayor_connection_work + lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 | 
                                   Year + City_Code + Mayor_leaderindex")
  model_with_covariates <- feols(as.formula(formula_with_covariates), 
                                 ~ City_Code, 
                                 data = filter(data, fsj2 == 0))
  
  results[[i * 2 - 1]] <- model_mayor_only
  results[[i * 2]] <- model_with_covariates
}

# do not change standard errors
modelsummary(results,
             coef_map = c("Mayor_plan" = "Subway approval"),
             gof_map = "nobs",
             add_rows = data.frame(
               Specification = c(
                 "City FE",
                 "Year FE",
                 "Mayor FE",
                 "City and mayor controls"
               ),
               Model1 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model3 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model4 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model5 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model6 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model7 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model8 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model9 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model10 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
             ),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
             output = "latex"
) |> 
  add_header_above(c(" " = 1, "Promotion in 1 Yr" = 2,
                     "Promotion in 2 Yrs" = 2, "Promotion in 3 Yrs" = 2,
                     "Promotion in 4 Yrs" = 2, "Promotion in 5 Yrs" = 2)) |> 
  save_kable(file = "tables/DID_with_mayor_fe.tex")

# show the results when computing standard errors like STATA
modelsummary(results,
             vcov = c("stata"),
             coef_map = c("Mayor_plan" = "Subway approval"),
             gof_map = "nobs",
             add_rows = data.frame(
               Specification = c(
                 "City FE",
                 "Year FE",
                 "Mayor FE",
                 "City and mayor controls"
               ),
               Model1 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model3 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model4 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model5 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model6 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model7 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model8 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               Model9 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model10 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
             ),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
             output = "latex"
) |> 
  add_header_above(c(" " = 1, "Promotion in 1 Yr" = 2,
                     "Promotion in 2 Yrs" = 2, "Promotion in 3 Yrs" = 2,
                     "Promotion in 4 Yrs" = 2, "Promotion in 5 Yrs" = 2)) |> 
  save_kable(file = "tables/DID_with_mayor_fe_STATA_SE.tex")

rm(list=setdiff(ls(), "data"))


################################################################################
# Check whether results of DiD are driven by 1 city (Figure 1 and A2) ----
################################################################################

# run model 4 by dropping 1 city at the time

tmp <- data |> 
  filter(fsj2 == 0)

results <- list()

for (i in unique(tmp$City_Code)) {
  results[[as.character(i)]] <- feols(
    Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
      Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
      Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
      Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
      lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
      Year + Year:pro_code + City_Code,
    ~ City_Code,
    data = filter(tmp, City_Code != i)
  )
}

# Tidy up the results
tidy_results <- lapply(results, tidy)

tidy_table <- bind_rows(tidy_results, .id = "city_excluded") |> 
  filter(term == "Mayor_plan") |> 
  mutate(
    CI_lower = estimate - 1.96 * std.error,
    CI_upper = estimate + 1.96 * std.error
  )

# plot density of point estimates
ggplot(tidy_table, aes(x = estimate)) + 
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.0003,
                 colour = 1, fill = "lightgrey", alpha = 0.1) +
  geom_vline(xintercept = 0.213446, color = "red", linetype = "dashed", linewidth = 1) +
  labs(y = "Density",
       x = "Point Estimates"
  ) +
  theme_classic()

ggsave("figures/point_estimates_dropping_1_city_at_time.png", height = 6, width = 6)

# plot density of p-values
ggplot(tidy_table, aes(x = p.value)) + 
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.0002,
                 colour = 1, fill = "lightgrey", alpha = 0.1) +
  geom_vline(xintercept = 0.028315, color = "red", linetype = "dashed", linewidth = 1) +
  labs(y = "Density",
       x = "p-values"
  ) +
  theme_classic()

ggsave("figures/p_values_dropping_1_city_at_time.png", height = 6, width = 6)


ggplot(tidy_table, aes(x = estimate, y = p.value)) + 
  geom_point(size = 1, alpha = 0.8) +
  geom_point(aes(x = 0.213446, y = 0.028315), colour="red") +
  labs(y = "p-value",
       x = "Point Estimate"
  ) +
  theme_classic()

ggsave("figures/scatter_dropping_1_city_at_time.png", height = 4, width = 4)


# Figure A2 in the appendix 
tidy_table |> 
  slice(1:45) |>
  ggplot(aes(x = forcats::fct_rev(city_excluded), y = estimate)) +
  geom_point() +
  labs(
    y = "Estimate",          
    x = "City Excluded from Regression"
  ) +
  geom_errorbar( aes(ymin = estimate - 2 * std.error, 
                     ymax = estimate + 2 * std.error), 
                 width = 0.2, linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "indianred") +
  theme_bw() +
  coord_flip()

ggsave("figures/exclude_city_from_reg_1.png", width = 7, height = 7)

tidy_table |> 
  slice(46:90) |>
  ggplot(aes(x = forcats::fct_rev(city_excluded), y = estimate)) +
  geom_point() +
  labs(
    y = "Estimate",          
    x = "City Excluded from Regression"
  ) +
  geom_errorbar( aes(ymin = estimate - 2 * std.error, 
                     ymax = estimate + 2 * std.error), 
                 width = 0.2, linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "indianred") +
  theme_bw() +
  coord_flip() 

ggsave("figures/exclude_city_from_reg_2.png", width = 7, height = 7)

tidy_table |> 
  slice(91:135) |>
  ggplot(aes(x = forcats::fct_rev(city_excluded), y = estimate)) +
  geom_point() +
  labs(
    y = "Estimate",          
    x = "City Excluded from Regression"
  ) +
  geom_errorbar( aes(ymin = estimate - 2 * std.error, 
                     ymax = estimate + 2 * std.error), 
                 width = 0.2, linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "indianred") +
  theme_bw() +
  coord_flip() 

ggsave("figures/exclude_city_from_reg_3.png", width = 7, height = 7)

tidy_table |> 
  slice(136:180) |>
  ggplot(aes(x = forcats::fct_rev(city_excluded), y = estimate)) +
  geom_point() +
  labs(
    y = "Estimate",          
    x = "City Excluded from Regression"
  ) +
  geom_errorbar( aes(ymin = estimate - 2 * std.error, 
                     ymax = estimate + 2 * std.error), 
                 width = 0.2, linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "indianred") +
  theme_bw() +
  coord_flip() 

ggsave("figures/exclude_city_from_reg_4.png", width = 7, height = 7)

tidy_table |> 
  slice(181:225) |>
  ggplot(aes(x = forcats::fct_rev(city_excluded), y = estimate)) +
  geom_point() +
  labs(
    y = "Estimate",          
    x = "City Excluded from Regression"
  ) +
  geom_errorbar( aes(ymin = estimate - 2 * std.error, 
                     ymax = estimate + 2 * std.error), 
                 width = 0.2, linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "indianred") +
  theme_bw() +
  coord_flip()

ggsave("figures/exclude_city_from_reg_5.png", width = 7, height = 7)

tidy_table |> 
  slice(226:265) |>
  ggplot(aes(x = forcats::fct_rev(city_excluded), y = estimate)) +
  geom_point() +
  labs(
    y = "Estimate",          
    x = "City Excluded from Regression"
  ) +
  geom_errorbar( aes(ymin = estimate - 2 * std.error, 
                     ymax = estimate + 2 * std.error), 
                 width = 0.2, linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "indianred") +
  theme_bw() +
  coord_flip()

ggsave("figures/exclude_city_from_reg_6.png", width = 7, height = 7)


rm(list=setdiff(ls(), "data"))

################################################################################
# Finite sample robustness: influence value (tables 3 and A3) ----
################################################################################

# keep only observations included in model 3, it will be useful later
m3 <- feols(
  Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)

Year <- as.numeric(attr(m3$fixef_id$Year,"fixef_names")[m3$fixef_id$Year])
City_Code <- attr(m3$fixef_id$City_Code,"fixef_names")[m3$fixef_id$City_Code]

restricted_sample <- data.frame(Year, City_Code)

# regress outcome on control variables excluding the treatment
Y_resid <- feols(
  Mayor_promotion3y ~ gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)$residuals

# regress the treatment against the control variables
D_resid <- feols(
  Mayor_plan ~ gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)$residuals

# compute influence value
ifvals <- Y_resid*D_resid/mean(D_resid^2)

# check which observation has the highest influence value
city <- paste(restricted_sample$City_Code, restricted_sample$Year, sep = " ")
city[which.max(ifvals)]

# smallest number of observations would need to drop for the effect to no longer be statistically significant?
temp <- data.frame(city, ifvals)
temp <- temp[order(-ifvals),]

significant <- TRUE
drop_me <- 0
while(significant){
  drop_me <- drop_me + 1
  coef <- mean(as.numeric(temp$ifvals)[drop_me:3092])
  se <- sd(as.numeric(temp$ifvals[drop_me:3092])/sqrt(length(drop_me:3092)))
  significant <- coef/se > 2
}

temp$city[1:drop_me]

# save it in a dataframe
m3_dropped_obs <- data.frame(city_id = temp$city[1:drop_me])

m3_dropped_obs <- m3_dropped_obs |> 
  mutate(City_Code = as.numeric(substr(city_id, 1, 4)),
         Year = as.numeric(substr(city_id, 5, nchar(city_id)))) |> 
  mutate(drop_m3 = 1)

# compute the regression estimate without these 16 observations
tmp <- data |>
  filter(fsj2 == 0) |> 
  filter(!(City_Code == 1401 & Year == 2012)) |> 
  filter(!(City_Code == 3204 & Year == 2012)) |>
  filter(!(City_Code == 1501 & Year == 2016)) |>
  filter(!(City_Code == 3401 & Year == 2010)) |>
  filter(!(City_Code == 1501 & Year == 2015)) |>
  filter(!(City_Code == 2101 & Year == 2013)) |>
  filter(!(City_Code == 6201 & Year == 2014)) |>
  filter(!(City_Code == 6201 & Year == 2016)) |>
  filter(!(City_Code == 3401 & Year == 2011)) |>
  filter(!(City_Code == 3205 & Year == 2005)) |>
  filter(!(City_Code == 6501 & Year == 2013)) |>
  filter(!(City_Code == 3601 & Year == 2016)) |>
  filter(!(City_Code == 2101 & Year == 2012)) |>
  filter(!(City_Code == 3205 & Year == 2006)) |>
  filter(!(City_Code == 6201 & Year == 2015)) |>
  filter(!(City_Code == 1301 & Year == 2012))

m3_drop <- feols(
  Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + City_Code,
  ~ City_Code,
  data = tmp
)


# keep only observations included in model 4, it will be useful later
m4 <- feols(
  Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + Year:pro_code + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)

Year <- as.numeric(attr(m4$fixef_id$Year,"fixef_names")[m4$fixef_id$Year])
City_Code <- attr(m4$fixef_id$City_Code,"fixef_names")[m4$fixef_id$City_Code]

restricted_sample <- data.frame(Year, City_Code)

# regress outcome on control variables excluding the treatment
Y_resid <- feols(
  Mayor_promotion3y ~ gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + Year:pro_code + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)$residuals

# regress the treatment against the control variables
D_resid <- feols(
  Mayor_plan ~ gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + Year:pro_code + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)$residuals

# compute influence value
ifvals <- Y_resid*D_resid/mean(D_resid^2)

# check which observation has the highest influence value
city <- paste(restricted_sample$City_Code, restricted_sample$Year, sep = " ")
city[which.max(ifvals)]

# smallest number of observations would need to drop for the effect to no longer be statistically significant?
temp <- data.frame(city, ifvals)
temp <- temp[order(-ifvals),]

significant <- TRUE
drop_me <- 0
while(significant){
  drop_me <- drop_me + 1
  coef <- mean(as.numeric(temp$ifvals)[drop_me:3092])
  se <- sd(as.numeric(temp$ifvals[drop_me:3092])/sqrt(length(drop_me:3092)))
  significant <- coef/se > 2
}

temp$city[1:drop_me]

# save it in a dataframe
m4_dropped_obs <- data.frame(city_id = temp$city[1:drop_me])

m4_dropped_obs <- m4_dropped_obs |> 
  mutate(City_Code = as.numeric(substr(city_id, 1, 4)),
         Year = as.numeric(substr(city_id, 5, nchar(city_id)))) |> 
  mutate(drop_m4 = 1)

# shows the regression estimate without these 10 observations
tmp <- data |>
  filter(fsj2 == 0) |> 
  filter(!(City_Code == 1401 & Year == 2012)) |> 
  filter(!(City_Code == 3204 & Year == 2012)) |>
  filter(!(City_Code == 3401 & Year == 2010)) |>
  filter(!(City_Code == 2101 & Year == 2013)) |>
  filter(!(City_Code == 2101 & Year == 2014)) |>
  filter(!(City_Code == 3401 & Year == 2011)) |>
  filter(!(City_Code == 6201 & Year == 2014)) |>
  filter(!(City_Code == 3601 & Year == 2015)) |>
  filter(!(City_Code == 4419 & Year == 2013)) |>
  filter(!(City_Code == 6201 & Year == 2015))

m4_drop <- feols(
  Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 |
    Year + Year:pro_code + City_Code,
  ~ City_Code,
  data = tmp
)

modelsummary(list(m3_drop, m4_drop),
             coef_map = c("Mayor_plan" = "Subway approval"),
             gof_map = "nobs",
             add_rows = data.frame(
               Specification = c(
                 "City FE",
                 "Year FE",
                 "Mayor controls",
                 "City controls",
                 "Province-year FE"
               ),
               Model1 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
             ),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
             output = "latex"
) |> 
  add_header_above(c(" " = 1, "Mayor Promoted Within Three Years" = 2)) |> 
  save_kable(file = "tables/reg_ifvals.tex")

# show which city-year are dropped. Show also the province
tbl_dropped <- left_join(data, m3_dropped_obs, by = c("City_Code", "Year")) 
tbl_dropped <- left_join(tbl_dropped, m4_dropped_obs, by = c("City_Code", "Year"))

tbl_dropped <- tbl_dropped |> 
  select(Year, city_id.x, city_id.y, City_Code, pro_code, drop_m3, drop_m4, Mayor_plan, Mayor_promotion3y) |> 
  filter(!(is.na(city_id.x) & is.na(city_id.y))) |>
  select(-c(city_id.x, city_id.y)) |> 
  mutate(drop_m3 = case_when(drop_m3 == 1 ~ "Yes",
                             TRUE ~ "No"),
         drop_m4 = case_when(drop_m4 == 1 ~ "Yes",
                             TRUE ~ "No"),
         )

stargazer(tbl_dropped,
          summary = F,
          covariate.labels = c("", "Year", "City ID", "Province ID", 
                               "Dropped from model 3", "Dropped from model 4",
                               "Subway approval", "Mayor promoted in 3 years"),
          out = "tables/dropped_obs.tex"
          )

rm(list=setdiff(ls(), "data"))


################################################################################
# Parallel trend assumption (figure 2) ----
################################################################################

# create leads and lags using authors' method
data <- data |> 
  arrange(City_Code, Year) |>
  group_by(City_Code) |> 
  mutate(
    mpprior1 = ifelse((Mayor_plan == 0 & lead(Mayor_plan) == 1), 1, 0),
    mpprior2 = ifelse((Mayor_plan == 0 & lead(Mayor_plan) == 0 & lead(Mayor_plan, 2) == 1), 1, 0),
    mpprior3 = ifelse((Mayor_plan == 0 & lead(Mayor_plan) == 0 & lead(Mayor_plan, 2) == 0 & lead(Mayor_plan, 3) == 1), 1, 0),
    mpprior4 = ifelse((Mayor_plan == 0 & lead(Mayor_plan) == 0 & lead(Mayor_plan, 2) == 0 & lead(Mayor_plan, 3) == 0 & lead(Mayor_plan, 4) == 1), 1, 0),
    mpprior5 = ifelse((Mayor_plan == 0 & lead(Mayor_plan) == 0 & lead(Mayor_plan, 2) == 0 & lead(Mayor_plan, 3) == 0 & lead(Mayor_plan, 4) == 0 & lead(Mayor_plan, 5) == 0), 1, 0),
    
    mpconn1 = ifelse((Mayor_plan == 1 & lag(Mayor_plan) == 0), 1, 0),
    mpconn2 = ifelse((Mayor_plan == 1 & lag(Mayor_plan) == 1 & lag(Mayor_plan, 2) == 0), 1, 0),
    mpconn3 = ifelse((Mayor_plan == 1 & lag(Mayor_plan) == 1 & lag(Mayor_plan, 2) == 1 & lag(Mayor_plan, 3) == 0), 1, 0),
    mpconn4 = ifelse((Mayor_plan == 1 & lag(Mayor_plan) == 1 & lag(Mayor_plan, 2) == 1 & lag(Mayor_plan, 3) == 1 & lag(Mayor_plan, 4) == 0), 1, 0),
    mpconn5 = ifelse((Mayor_plan == 1 & mpconn1 == 0 & mpconn2 == 0 & mpconn3 == 0 & mpconn4 == 0), 1, 0)
  ) |> 
  ungroup() |> 
  relocate(Year, City_Code, Mayor_leaderindex, Mayor_plan, starts_with("mpprior"), starts_with("mpconn"))

# create alternative leads and lags
data <- data |> 
  arrange(City_Code, Year) |> 
  group_by(City_Code) |> 
  mutate(
    lead_5 = lead(Mayor_plan, 5),
    lead_4 = lead(Mayor_plan, 4),
    lead_3 = lead(Mayor_plan, 3),
    lead_2 = lead(Mayor_plan, 2),
    lead_1 = lead(Mayor_plan, 1),
    lag_1 = lag(Mayor_plan, 1),
    lag_2 = lag(Mayor_plan, 2),
    lag_3 = lag(Mayor_plan, 3),
    lag_4 = lag(Mayor_plan, 4)
  ) |> 
  ungroup() |> 
  relocate(Year, Mayor_leaderindex, City_Code, Mayor_promotion3y, Mayor_plan, mpconn1, mpprior1, 
           lead_1, mpprior2, lead_2, mpprior3, lead_3, mpprior4, lead_4, mpprior5, 
           lead_5, mpconn2, lag_1, mpconn3, lag_2, mpconn4, lag_3, mpconn5, lag_4,
           Mayor_promotion1y, Mayor_promotion2y)

# use the data I have constructed using authors' method with 1yr prior treatment
authors_method_par_trend <- feols(
  Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 +
    mpprior2 + mpprior1 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mpconn5  +
    gender2 + race6 + Mayor_age + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
    Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
    Mayor_connection_work + lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1|
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
  )

authors_method_par_trend |>
  tidy() |>
  filter(grepl("mpprior|mpconn", term)) |> 
  mutate(
    term = recode_factor(
      term,
      mpprior5 = "<=-5",
      mpprior4 = "-4",
      mpprior3 = "-3",
      mpprior2 = "-2",
      mpprior1 = "-1",
      mpconn1 = "0",
      mpconn2 = "1",
      mpconn3 = "2",
      mpconn4 = "3",
      mpconn5 = ">=4"
    )
  ) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 *
                      std.error),
                width = 0.2,  linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 6, linetype = "dashed") +
  labs(
    y = "",
    x = ""
  ) +
  theme_bw()

ggsave("figures/authors_method_par_trend.png", height = 4, width = 4)

# use the data I have constructed using simply leads and lags
lead_lags <- feols(
  Mayor_promotion3y ~ lead_5 + lead_4 + lead_3 + lead_2 + lead_1 +
    Mayor_plan + lag_1 + lag_2 + lag_3 + lag_4 +
    gender2 + race6 + Mayor_age + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
    Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
    Mayor_connection_work + lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1|
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
  )

lead_lags |>
  tidy() |>
  filter(grepl("lead|lag|Mayor_plan", term)) |> 
  mutate(
    term = recode_factor(
      term,
      lead_5 = "-5",
      lead_4 = "-4",
      lead_3 = "-3",
      lead_2 = "-2",
      lead_1 = "-1",
      Mayor_plan = "0",
      lag_1 = "1",
      lag_2 = "2",
      lag_3 = "3",
      lag_4 = ">=4"
    )
  ) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 *
                      std.error),
                width = 0.2,  linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 6, linetype = "dashed") +
  labs(
    y = "",
    x = ""
  ) +
  theme_bw()

ggsave("figures/leads_lags_par_trend.png", height = 4, width = 4)

################################################################################
# Parallel trend assumption (Table A2 and Table A3) ----
################################################################################

# authors' method
m1 <- feols(
  Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 +
    mpprior2 + mpprior1 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mpconn5 +
    gender2 + race6 + Mayor_age + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
    Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
    Mayor_connection_work + lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1|
    City_Code + Year,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
  )

m2 <- feols(
  Mayor_promotion3y ~ mpprior5 + mpprior4 + mpprior3 +
    mpprior2 + mpprior1 + mpconn1 + mpconn2 + mpconn3 + mpconn4 + mpconn5 +
    gender2 + race6 + Mayor_age + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
    Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
    Mayor_connection_work + lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1|
    Year:pro_code + City_Code + Year,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)

modelsummary(list(m1, m2),
             coef_map = c("mpprior5" = "To be approved in 5 or more years",
                          "mpprior4" = "To be approved in 4 years",
                          "mpprior3" = "To be approved in 3 years",
                          "mpprior2" = "To be approved in 2 years",
                          "mpprior1" = "To be approved in 1 year",
                          "mpconn1"  = "Approval year",
                          "mpconn2"  = "Approved for 1 year",
                          "mpconn3"  = "Approved for 2 years",
                          "mpconn4"  = "Approved for 3 years",
                          "mpconn5"  = "Approved for 4 or more years"
             ),
             gof_map = "nobs",
             add_rows = data.frame(
               Specification = c(
                 "City FE",
                 "Year FE",
                 "Baseline controls",
                 "Province-year FE"
               ),
               Model1 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
               ),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
             output = "latex"
             ) |> 
  add_header_above(c(" " = 1, "Mayor Promoted Within Three Years" = 2)) |> 
  save_kable(file = "tables/parallel_trend_author_coding.tex")

# alternative coding
m1 <- feols(
  Mayor_promotion3y ~ lead_5 + lead_4 + lead_3 + lead_2 + lead_1 +
    Mayor_plan + lag_1 + lag_2 + lag_3 + lag_4 +
    gender2 + race6 + Mayor_age + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
    Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
    Mayor_connection_work + lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1|
    Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
)

m2 <- feols(
  Mayor_promotion3y ~ lead_5 + lead_4 + lead_3 + lead_2 + lead_1 +
    Mayor_plan + lag_1 + lag_2 + lag_3 + lag_4 +
    gender2 + race6 + Mayor_age + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
    Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
    Mayor_connection_work + lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1|
    Year:pro_code + Year + City_Code,
  ~ City_Code,
  data = filter(data, fsj2 == 0)
  )

modelsummary(list(m1, m2),
             coef_map = c("lead_5" = "5 years lead",
                          "lead_4" = "4 years lead",
                          "lead_3" = "3 years lead",
                          "lead_2" = "2 years lead",
                          "lead_1" = "1 year lead",
                          "Mayor_plan"  = "Treatment",
                          "lag_1"  = "1 year lag",
                          "lag_2"  = "2 years lag",
                          "lag_3"  = "3 years lag",
                          "lag_4"  = "4 years lag"
             ),
             gof_map = "nobs",
             add_rows = data.frame(
               Specification = c(
                 "City FE",
                 "Year FE",
                 "Baseline controls",
                 "Province-year FE"
               ),
               Model1 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
               Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
             ),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
             output = "latex"
             ) |> 
  add_header_above(c(" " = 1, "Mayor Promoted Within Three Years" = 2)) |> 
  save_kable(file = "tables/parallel_trend_alternative_coding.tex")


rm(list=setdiff(ls(), "data"))

################################################################################
# Add city-specific time trend (Table 3) ----
################################################################################

# Add city-specific time trends of different polynomials

# create time trend to different powers
data <- data |>  
  mutate(time = as.integer(factor(Year, levels = unique(data$Year))),
         time2 = time^2,
         time3 = time^3
  ) 

# create dummy variables for each city
for (city_code in unique(data$City_Code)) {
  dummy_variable_name <- paste0("citydum_", city_code)
  data[[dummy_variable_name]] <- as.numeric(data$City_Code == city_code)
}

# interact city dummies with time variables of different polynomials
for (var_name in str_subset(colnames(data), "citydum_")) {
  data[paste0("linear_", var_name)] <- data$time * data[[var_name]]
  data[paste0("quadratic_", var_name)] <- data$time^2 * data[[var_name]]
  data[paste0("cubed_", var_name)] <- data$time^3 * data[[var_name]]
}

# check whether everything is correct 
data <- data |> 
  relocate(City_Code, Year, starts_with("time"), starts_with("linear_"), 
           starts_with("quadratic_"), starts_with("cubed_")
           )

# models with linear city time-trend 
linear <- str_subset(colnames(data), "linear_")

# linear with covariates
linear_model_covs <- glue(
  "Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 +
    {str_c(linear, collapse = ' + ')} | City_Code + Year"
)

m1 <- feols(as.formula(linear_model_covs), 
            ~ City_Code,
            data = filter(data, fsj2 == 0)
)


# models with quadratic city time-trend 
quadratic <- str_subset(colnames(data), "quadratic_")

# quadratic model with covariates
quadratic_model_covs <- glue(
  "Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 +
    {str_c(linear, collapse = ' + ')} +
    {str_c(quadratic, collapse = ' + ')}| City_Code + Year"
)

m2 <- feols(as.formula(quadratic_model_covs), 
            ~ City_Code,
            data = filter(data, fsj2 == 0)
)


# models with cubed city time-trend 
cubed <- str_subset(colnames(data), "cubed_")

# cubic model with covariates
cubed_model_covs <- glue(
  "Mayor_promotion3y ~ Mayor_plan + gender2 + race6 +
    Mayor_age + Mayor_c_edu + Mayor_c_central_exp +
    Mayor_c_prov_exp + Mayor_c_county_exp + Mayor_c_soe_exp +
    Mayor_c_univ_exp + Mayor_c_league + Mayor_connection_work +
    lpop_1 + lgdp_1 + lrev_1 + GRP_growth_1 +
    {str_c(linear, collapse = ' + ')} +
    {str_c(quadratic, collapse = ' + ')} +
    {str_c(cubed, collapse = ' + ')} | City_Code + Year"
)

m3 <- feols(as.formula(cubed_model_covs), 
            ~ City_Code,
            data = filter(data, fsj2 == 0)
)


modelsummary(list(m1, m2, m3),
             coef_map = c("Mayor_plan" = "Subway approval"),
             gof_map = "nobs",
             add_rows = data.frame(
               Specification = c(
                 "City FE",
                 "Year FE",
                 "Mayor controls",
                 "City controls",
                 "City-specific time trend (polynomial)"
               ),
               Model1 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "1st"),
               Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "2nd"),
               Model3 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "3d")
             ),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
             output = "latex"
) |> 
  save_kable(file = "tables/reg_with_time_trends.tex")

rm(list=setdiff(ls(), "data"))


################################################################################
# Fuzzy RDD - Replicate Table 3----
################################################################################

rm(list=setdiff(ls(), "data"))

# create running variable
fuzzy_data <- data |>
  mutate(Per_pop_2 = (Per_pop_2 - 300)/100) |>
  mutate(iv1 = ifelse(Per_pop_2 >= 0, 1, 0),
         iv1_int = iv1*Per_pop_2)

# verify whether the bandwidth is correct
rdbwselect(fuzzy_data$Mayor_plan, fuzzy_data$Per_pop_2, c(0)) |> 
  summary()

# subset the data
fuzzy_data <- fuzzy_data |> 
  filter(Budget_income_2 > 1000000 & GRP_2 > 10000000)

# model 1
m1 <- feols(Mayor_promotion3y ~ Per_pop_2 + iv1_int
            | Year:pro_code + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_2) <= 1.058  & fsj2 == 0)
)

m1_first <- summary(m1, stage = 1)
m1_second <- summary(m1, stage = 2)

# model 2
m2 <- feols(Mayor_promotion3y ~ Per_pop_2 + iv1_int + 
              Mayor_age + gender2 + + race6 + Mayor_c_edu + 
              Mayor_c_central_exp + Mayor_c_prov_exp + Mayor_c_county_exp + 
              Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
              Mayor_connection_work
            | Year:pro_code + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_2) <= 1.058  & fsj2 == 0)
)

m2_first <- summary(m2, stage = 1)
m2_second <- summary(m2, stage = 2)

# model 3
m3 <- feols(Mayor_promotion3y ~ Per_pop_2 + iv1_int + Mayor_age + gender2 + 
              race6 + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
              Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp +
              Mayor_c_league + Mayor_connection_work + lgdp_per_1 + lrev_per_1 +
              GRP_growth_1 | 
              Year:pro_code + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_2) <= 1.058  & fsj2 == 0)
)

m3_first <- summary(m3, stage = 1)
m3_second <- summary(m3, stage = 2)


panels <- list(
  "Second Stage" = list(m1_second, m2_second, m3_second),
  "First Stage" = list(m1_first, m2_first, m3_first)
)

modelsummary(
  panels,
  coef_map = c("fit_Mayor_plan" = "Subway Approval",
               "Per_pop_2" = "Population",
               "lead_3" = "3 years lead",
               "iv1_int" = "Population x IV",
               "iv1" = "IV (population $>$3 million)"
  ),
  shape = "rcollapse",
  gof_map = "nobs",
  add_rows = data.frame(
    Specification = c(
      "City FE",
      "Province-year FE",
      "Mayor characteristics",
      "City characteristics"
    ),
    Model1 = c("$\\checkmark$", "$\\checkmark$", "", ""),
    Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
    Model3 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  ),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
  output = "latex"
) |> 
  add_header_above(c(" " = 1, "Mayor Promoted in 3 Years" = 3)) |> 
  save_kable(file = "tables/fuzzy_rdd_replication.tex")


################################################################################
# Estimate the same model with year fixed effects instead of province X year ----
################################################################################

# model 1
m1 <- feols(Mayor_promotion3y ~ Per_pop_2 + iv1_int
            | Year + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_2) <= 1.058  & fsj2 == 0)
            )

m1_first <- summary(m1, stage = 1)
m1_second <- summary(m1, stage = 2)

# model 2
m2 <- feols(Mayor_promotion3y ~ Per_pop_2 + iv1_int + 
              Mayor_age + gender2 + + race6 + Mayor_c_edu + 
              Mayor_c_central_exp + Mayor_c_prov_exp + Mayor_c_county_exp + 
              Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
              Mayor_connection_work
            | Year + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_2) <= 1.058  & fsj2 == 0)
            )

m2_first <- summary(m2, stage = 1)
m2_second <- summary(m2, stage = 2)

# model 3
m3 <- feols(Mayor_promotion3y ~ Per_pop_2 + iv1_int + Mayor_age + gender2 + 
              race6 + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
              Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp +
              Mayor_c_league + Mayor_connection_work + lgdp_per_1 + lrev_per_1 +
              GRP_growth_1 | 
              Year + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_2) <= 1.058  & fsj2 == 0)
            )

m3_first <- summary(m3, stage = 1)
m3_second <- summary(m3, stage = 2)


panels <- list(
  "Second Stage" = list(m1_second, m2_second, m3_second),
  "First Stage" = list(m1_first, m2_first, m3_first)
)

modelsummary(
  panels,
  coef_map = c("fit_Mayor_plan" = "Subway Approval",
               "Per_pop_2" = "Population",
               "lead_3" = "3 years lead",
               "iv1_int" = "Population x IV",
               "iv1" = "IV (population $>$3 million)"
  ),
  shape = "rcollapse",
  gof_map = "nobs",
  add_rows = data.frame(
    Specification = c(
      "City FE",
      "Year FE",
      "Mayor characteristics",
      "City characteristics"
    ),
    Model1 = c("$\\checkmark$", "$\\checkmark$", "", ""),
    Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
    Model3 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  ),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
  output = "latex"
) |> 
  add_header_above(c(" " = 1, "Mayor Promoted in 3 Years" = 3)) |> 
  save_kable(file = "tables/fuzzy_rdd_year_fe.tex")

rm(list=setdiff(ls(), "data"))

################################################################################
# Use population 1 year before ----
################################################################################

# create running variable
fuzzy_data <- data |>
  mutate(Per_pop_1 = (Per_pop_1 - 300)/100) |>
  mutate(iv1 = ifelse(Per_pop_1 >= 0, 1, 0),
         iv1_int = iv1*Per_pop_1) 

# compute bandwidth
rdbwselect(fuzzy_data$Mayor_plan, fuzzy_data$Per_pop_1, c(0)) |> 
  summary()

fuzzy_data <- fuzzy_data |> 
  filter(Budget_income_1 > 1000000 & GRP_1 > 10000000)

# model 1
m1 <- feols(Mayor_promotion3y ~ Per_pop_1 + iv1_int
            | Year:pro_code + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_1) <= 1.616  & fsj2 == 0)
)

m1_first <- summary(m1, stage = 1)
m1_second <- summary(m1, stage = 2)

# model 2
m2 <- feols(Mayor_promotion3y ~ Per_pop_1 + iv1_int + 
              Mayor_age + gender2 + + race6 + Mayor_c_edu + 
              Mayor_c_central_exp + Mayor_c_prov_exp + Mayor_c_county_exp + 
              Mayor_c_soe_exp + Mayor_c_univ_exp + Mayor_c_league + 
              Mayor_connection_work
            | Year:pro_code + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_1) <= 1.616  & fsj2 == 0)
)

m2_first <- summary(m2, stage = 1)
m2_second <- summary(m2, stage = 2)

# model 3
m3 <- feols(Mayor_promotion3y ~ Per_pop_1 + iv1_int + Mayor_age + gender2 + 
              race6 + Mayor_c_edu + Mayor_c_central_exp + Mayor_c_prov_exp + 
              Mayor_c_county_exp + Mayor_c_soe_exp + Mayor_c_univ_exp +
              Mayor_c_league + Mayor_connection_work + lgdp_per_1 + lrev_per_1 +
              GRP_growth_1 | 
              Year:pro_code + City_Code|
              Mayor_plan ~ iv1, 
            ~ City_Code,
            ssc = ssc(adj = FALSE, cluster.adj = FALSE),
            data = filter(fuzzy_data, abs(Per_pop_1) <= 1.616  & fsj2 == 0)
)

m3_first <- summary(m3, stage = 1)
m3_second <- summary(m3, stage = 2)


panels <- list(
  "Second Stage" = list(m1_second, m2_second, m3_second),
  "First Stage" = list(m1_first, m2_first, m3_first)
)


modelsummary(
  panels,
  coef_map = c("fit_Mayor_plan" = "Subway Approval",
               "Per_pop_1" = "Population",
               "iv1_int" = "Population x IV",
               "iv1" = "IV (population $>$3 million)"
  ),
  shape = "rcollapse",
  gof_map = "nobs",
  add_rows = data.frame(
    Specification = c(
      "City FE",
      "Province FE",
      "Mayor characteristics",
      "City characteristics"
    ),
    Model1 = c("$\\checkmark$", "$\\checkmark$", "", ""),
    Model2 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", ""),
    Model3 = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  ),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01 ),
  output = "latex"
) |> 
  add_header_above(c(" " = 1, "Mayor Promoted in 3 Years" = 3)) |> 
  save_kable(file = "tables/fuzzy_rdd_pop_1_year_before.tex")
