# Load packages
library(Lahman)
library(tidyverse)
library(MASS)
current_year <- 2023

# Player names
names_df <- People %>%
  dplyr::select(playerID, nameFirst, nameLast)

# Ineligible players (played >=2017)
ineligible_players <- Batting %>%
  filter(yearID >= 2017) %>%
  distinct(playerID)

batting_age <- Batting %>%
  left_join(People %>% dplyr::select(playerID, birthYear, birthMonth), by = "playerID") %>%
  mutate(
    age = yearID - birthYear - ifelse(birthMonth > 6, 1, 0),
    across(c(G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP), ~replace_na(.x, 0))
  ) %>%
  filter(!is.na(age)) %>%
  group_by(playerID, yearID, age) %>%
  mutate(
    across(c(G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    X1B = H - X2B - X3B - HR,
    OBP = ifelse((AB + BB + IBB + HBP + SF) > 0,
                 (H + BB + IBB + HBP) / (AB + BB + IBB + HBP + SF), 0),
    SLG = ifelse(AB > 0,
                 (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB, 0),
    OPS = round(OBP + SLG, 6)
  ) %>%
  group_by(playerID) %>%
  mutate(debutYear = min(yearID)) %>%
  mutate(Era = (debutYear + current_year)/2) %>%
  ungroup() %>%
  dplyr::select(-c(teamID, .groups, debutYear))

league_stats <- batting_age %>%
  filter(AB > 0) %>%
  group_by(yearID) %>%
  summarise(
    lg_OBP = mean(OBP, na.rm = TRUE),
    lg_SLG = mean(SLG, na.rm = TRUE),
    lg_OPS = mean(OPS, na.rm = TRUE),
    sd_OBP = sd(OBP, na.rm = TRUE),
    sd_SLG = sd(SLG, na.rm = TRUE),
    sd_OPS = sd(OPS, na.rm = TRUE),
    total_R = sum(R, na.rm = TRUE), 
    total_G = sum(G, na.rm = TRUE)
  )

batting_age_adj <- batting_age %>%
  left_join(league_stats, by = "yearID") %>%
  mutate(
    OBP_plus = round((OBP / lg_OBP) * 100, 1),
    SLG_plus = round((SLG / lg_SLG) * 100, 1),
    OPS_plus = round((OPS / lg_OPS) * 100, 1),
    
    OBP_z = (OBP - lg_OBP) / sd_OBP,
    SLG_z = (SLG - lg_SLG) / sd_SLG,
    OPS_z = (OPS - lg_OPS) / sd_OPS,
    
    # Approx neutralization: scale based on runs per game
    lg_RPG = total_R / total_G,
    neutralized_HR = HR * (4.5 / lg_RPG),  # 4.5 = average modern RPG baseline
    neutralized_H = H * (4.5 / lg_RPG)
  )

batting_by_age_adj <- batting_age_adj %>%
  arrange(playerID, age) %>%
  group_by(playerID) %>%
  mutate(
    cum_PA = cumsum(AB + BB + IBB + HBP + SF)
  ) %>%
  group_by(playerID, age) %>%
  summarise(
    OBP_plus = weighted.mean(OBP_plus, w = AB, na.rm = TRUE),
    SLG_plus = weighted.mean(SLG_plus, w = AB, na.rm = TRUE),
    OPS_plus = weighted.mean(OPS_plus, w = AB, na.rm = TRUE),
    OBP_z = weighted.mean(OBP_z, w = AB, na.rm = TRUE),
    SLG_z = weighted.mean(SLG_z, w = AB, na.rm = TRUE),
    OPS_z = weighted.mean(OPS_z, w = AB, na.rm = TRUE),
    neutralized_HR = sum(neutralized_HR, na.rm = TRUE),
    neutralized_H = sum(neutralized_H, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))


# Function: cumulative stats through a given age for each player
batting_by_age <- batting_age %>%
  dplyr::select(-c(lgID, stint)) %>%
  distinct(.keep_all = TRUE) %>%
  arrange(playerID, age) %>%
  group_by(playerID) %>%
  mutate(
    Years = cumsum(!duplicated(yearID)),
    G_cum = cumsum(G), AB_cum = cumsum(AB), R_cum = cumsum(R), H_cum = cumsum(H),
    X1B_cum = cumsum(X1B), X2B_cum = cumsum(X2B), X3B_cum = cumsum(X3B), HR_cum = cumsum(HR),
    RBI_cum = cumsum(RBI), SB_cum = cumsum(SB), CS_cum = cumsum(CS),
    BB_cum = cumsum(BB), SO_cum = cumsum(SO), IBB_cum = cumsum(IBB), HBP_cum = cumsum(HBP),
    SH_cum = cumsum(SH), SF_cum = cumsum(SF), GIDP_cum = cumsum(GIDP)
  ) %>%
  mutate(
    AVG = H_cum / AB_cum,
    SLG = (X1B_cum + 2 * X2B_cum + 3 * X3B_cum + 4 * HR_cum) / AB_cum,
    OBP = (H_cum + BB_cum + IBB_cum + HBP_cum) / (AB_cum + BB_cum + IBB_cum + HBP_cum + SF_cum),
    OPS = SLG + OBP
  ) %>%
  ungroup() %>%
  rename(age_through = age) %>%
  dplyr::select(playerID, age_through, Years,
         G = G_cum, AB = AB_cum, R = R_cum, H = H_cum,
         X1B = X1B_cum, X2B = X2B_cum, X3B = X3B_cum, HR = HR_cum,
         RBI = RBI_cum, SB = SB_cum, CS = CS_cum,
         BB = BB_cum, SO = SO_cum, IBB = IBB_cum, HBP = HBP_cum,
         SH = SH_cum, SF = SF_cum, GIDP = GIDP_cum,
         AVG, SLG, OBP, OPS, Era)

# Function to compute peak OPS window of up to 7 seasons for each player-age group
get_dynamic_peak <- function(df) {
  df <- df %>% arrange(age)
  n <- nrow(df)
  peak_data <- vector("list", n)
  
  for (i in seq_len(n)) {
    max_window <- min(i, 7)
    best_ops <- -Inf
    best_window <- NULL
    
    for (k in 1:max_window) {
      window_df <- df[(i - k + 1):i, ]
      ab_total <- sum(window_df$AB, na.rm = TRUE)
      pa_total <- sum(window_df$AB + window_df$BB + window_df$IBB + window_df$HBP + window_df$SF, na.rm = TRUE)
      
      if (ab_total == 0 || pa_total == 0) next
      
      obp <- sum(window_df$H + window_df$BB + window_df$IBB + window_df$HBP, na.rm = TRUE) / pa_total
      slg <- sum(window_df$X1B + 2 * window_df$X2B + 3 * window_df$X3B + 4 * window_df$HR, na.rm = TRUE) / ab_total
      ops <- obp + slg
      
      if (ops > best_ops) {
        best_ops <- ops
        best_window <- window_df
      }
    }
    
    # 🛡️ Skip if no valid window was found
    if (is.null(best_window)) {
      next
    }
    
    peak_row <- best_window %>%
      summarise(
        peak_H = sum(H), peak_AB = sum(AB), peak_X1B = sum(X1B),
        peak_X2B = sum(X2B), peak_X3B = sum(X3B), peak_HR = sum(HR),
        peak_RBI = sum(RBI), peak_BB = sum(BB), peak_SO = sum(SO),
        peak_HBP = sum(HBP), peak_IBB = sum(IBB), peak_SF = sum(SF),
        peak_OPS = round(best_ops, 6),
        minyear_peak = min(age),
        maxyear_peak = max(age)
      )
    
    peak_data[[i]] <- cbind(tibble(playerID = df$playerID[i], age = df$age[i]), peak_row)
  }
  
  bind_rows(peak_data)
}

# Apply peak calculation across all player-ages
batting_peak_by_age <- batting_age %>%
#  filter(playerID == 'judgeaa01') %>%
  group_by(playerID) %>%
  arrange(age) %>%
  group_split() %>%
  map_df(get_dynamic_peak)

batting_combined <- batting_by_age %>%
  inner_join(batting_peak_by_age, by = c("playerID", "age_through" = "age")) %>%
  inner_join(batting_by_age_adj, by = c('playerID', 'age_through' = 'age'))

# View example
batting_combined %>%
  filter(playerID == "troutmi01") %>%
  arrange(age_through)

fielding_full <- Fielding %>%
  left_join(People %>% dplyr::select(playerID, birthYear, birthMonth), by = "playerID") %>%
  mutate(
    age = yearID - birthYear - ifelse(birthMonth > 6, 1, 0)) %>%
  group_by(playerID, age) %>%
  summarise(across(c(InnOuts, PO, A, E, DP, ZR), ~sum(.x, na.rm = TRUE)))

# Main position per player
player_mainpos <- Fielding %>%
  group_by(playerID, POS) %>%
  summarise(pos_games = sum(G, na.rm = TRUE)) %>%
  group_by(playerID) %>%
  filter(pos_games == max(pos_games)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(playerID, POS)

# Major Awards
# NOTE: found anomoly where Ryan Braun in 2012 was shown as braunry01 instead of braunry02, so corrected manually
award_start_years <- data.frame(
  awardID = c('GG', 'SS', 'MVP','RoY'),
  first_year = c(1957, 1980, 1911, 1947)
)
award_years <- expand.grid(
  yearID = min(Batting$yearID):max(Batting$yearID),
  awardID = award_start_years$awardID
) %>%
  left_join(award_start_years, by = 'awardID') %>%
  filter(yearID >= first_year) %>%
  dplyr::select(-first_year)
player_awards <- AwardsPlayers %>%
  mutate(playerID = ifelse(playerID == 'braunry01', 'braunry02', playerID)) %>%
  filter(tolower(awardID) %in% c('gold glove', 'silver slugger', 'most valuable player', 'rookie of the year')) %>%
  mutate(award_short = case_when(
    tolower(awardID) == 'gold glove' ~ 'GG',
    tolower(awardID) == 'silver slugger' ~ 'SS',
    tolower(awardID) == 'most valuable player' ~ 'MVP',
    tolower(awardID) == 'rookie of the year' ~ 'RoY',
    TRUE ~ awardID
  )) %>%
  count(playerID, yearID, award_short)
allstar_counts <- AllstarFull %>%
  distinct(playerID, yearID) %>%
  count(playerID, yearID, name = 'AllStar')
award_counts <- player_awards %>%
  pivot_wider(names_from = award_short, values_from = n, values_fill = 0) %>%
  full_join(allstar_counts, by = c('playerID', 'yearID')) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(awards_total = GG+ SS + MVP + RoY + AllStar)
award_opportunities <- Batting %>%
  distinct(playerID, yearID) %>%
  group_by(playerID, yearID) %>%
  mutate(
    GG_years = sum(yearID %in% 1957:2023),
    SS_years = sum(yearID %in% 1980:2023),
    MVP_years = sum(yearID %in% 1911:2023),
    RoY_years = sum(yearID %in% 1947:2023),
    AllStar_years = sum(yearID %in% 1933:2023)
  ) %>%
  ungroup() %>%
  mutate(total_award_opportunities = GG_years + SS_years + MVP_years + RoY_years + AllStar_years)
award_summary <- award_counts %>%
  full_join(award_opportunities, by = c("playerID", 'yearID')) %>%
  group_by(playerID, yearID) %>%
  summarise(
    awards_won = sum(awards_total, na.rm = TRUE),
    awards_available = sum(total_award_opportunities, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(cum_awards_won = cumsum(awards_won),
         cum_awards_available = cumsum(awards_available),
         award_share = cum_awards_won/cum_awards_available) %>%
  dplyr::select(playerID, yearID, awards_won, award_share) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  left_join(People %>% dplyr::select(playerID, birthYear, birthMonth), by = "playerID") %>%
  mutate(
    age = yearID - birthYear - ifelse(birthMonth > 6, 1, 0)) %>%
  dplyr::select(-yearID, birthYear, birthMonth)

# Hall of Fame voting
hof <- HallOfFame %>%
  group_by(playerID) %>%
  filter(yearID == max(yearID)) %>%
  mutate(inducted = as.character(inducted)) %>%
  dplyr::select(playerID, inducted) %>%
  group_by(playerID) %>%
  mutate(Count = n()) %>%
  mutate(inducted = ifelse(Count > 1, "Y", inducted)) %>%
  dplyr::select(-Count) %>%
  ungroup() %>%
  mutate(inducted = as.factor(inducted)) %>%
  distinct(.keep_all = TRUE)

# Combine all data
data_full <- names_df %>%
  inner_join(batting_combined, by = "playerID") %>%
#  inner_join(batting_peak, by = "playerID") %>%
  inner_join(fielding_full, by = c('playerID', 'age_through' = 'age')) %>%
  inner_join(award_summary, by= c('playerID', 'age_through' = 'age')) %>%
  left_join(player_mainpos, by = "playerID") %>%
  left_join(hof, by = "playerID", relationship = 'many-to-one') %>%
  filter(!is.na(peak_AB), POS != "P") %>%
  mutate(inducted = replace_na(inducted, "N")) %>%
  distinct(.keep_all = TRUE)

featureset <- colnames(data_full[, -c(1:5, 60:63)])

# Normalize numeric variables
df_normal <- data_full %>%
  group_by(age_through) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  mutate(across(all_of(featureset), ~(. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)))) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  ungroup() %>%
  mutate(age = age_through) %>%
  dplyr::select(-age_through)

# Save Processed Data:
df_eligible <- df_normal %>% filter(!(playerID %in% ineligible_players$playerID))
df_current <- df_normal %>% filter(playerID %in% ineligible_players$playerID)
saveRDS(df_eligible, "Data/data_clean.rds")
saveRDS(df_current, "Data/data_current.rds")