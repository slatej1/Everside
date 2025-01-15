irr_data_file <- "Everside Fund III IRR Model 3Q24"
fund_filter <- "Fund III"
date_filter <- "2024-09-30" # NEED TO FIX BELOW REFERENCES ONCE WE HAVE UPDATED NAVS AVAILABLE

create_irr_front_page_view <- function(irr_data_file = "Everside Fund III IRR Model 3Q24", fund_filter = "Fund III", date_filter = "2024-09-30") {

  irr_existing_data <- read.xlsx(path.expand(paste0("~/R Data/", irr_data_file, ".xlsx")), sheet = "Data", startRow = 4) %>% 
    clean_names() %>% 
    mutate(
      vintage_year = as.numeric(vintage_year),
      transaction_date = as.Date(transaction_date, origin = "1899-12-30"),
      commitment = ifelse(commitment == "NA" | is.na(commitment), 0, commitment),
      unfunded_commitment = ifelse(unfunded_commitment == "NA" | is.na(unfunded_commitment), 0, as.numeric(unfunded_commitment)),
      gap = "",
      investment_amount = ifelse(investment_amount == "NA" | is.na(investment_amount), 0, investment_amount),
      investment_expenses = ifelse(investment_expenses == "NA" | is.na(investment_expenses), 0, as.numeric(investment_expenses)),
      proceeds_return_of_capital = ifelse(proceeds_return_of_capital == "NA" | is.na(proceeds_return_of_capital), 0, as.numeric(proceeds_return_of_capital)),
      proceeds_interest_income = ifelse(proceeds_interest_income == "NA" | is.na(proceeds_interest_income), 0, proceeds_interest_income),
      proceeds_realized_gain = ifelse(proceeds_realized_gain == "NA" | is.na(proceeds_realized_gain), 0, proceeds_realized_gain),
      proceeds_mfee_rebate = ifelse(proceeds_mfee_rebate == "NA" | is.na(proceeds_mfee_rebate), 0, proceeds_mfee_rebate),
      proceeds_closing_fee =ifelse(proceeds_closing_fee == "NA" | is.na(proceeds_closing_fee), 0, proceeds_closing_fee),
      proceeds_due_to_manager = ifelse(proceeds_due_to_manager == "NA" | is.na(proceeds_due_to_manager), 0, as.numeric(proceeds_due_to_manager)),
      realized_proceeds = ifelse(realized_proceeds == "NA" | is.na(realized_proceeds), 0, realized_proceeds),
      fair_market_value = ifelse(fair_market_value == "NA" | is.na(fair_market_value), 0, as.numeric(fair_market_value)),
      percentage = ifelse(percentage == "NA" | is.na(percentage), 0, as.numeric(percentage)),
      value = ifelse(value == "NA" | is.na(value), 0, value)
    ) %>% 
    filter(
      vintage_year >= 2017,
      position_name != "Place Holder"
    ) %>% 
    select(
      vintage_year,
      transaction_date,
      position_name,
      investment_type,
      commitment,
      unfunded_commitment,
      gap,
      investment_amount,
      investment_expenses,
      proceeds_return_of_capital,
      proceeds_interest_income,
      proceeds_realized_gain,
      proceeds_mfee_rebate,
      proceeds_closing_fee,
      proceeds_due_to_manager,
      realized_proceeds,
      fund_name,
      fair_market_value
    )
  
  source("~/R Data/IRR_Data_Tab_Creation.R")
  
  new_irr_data <- create_irr_data_tab(fund_filter, as.Date(date_filter)+days(1), "LTD Investment Transactions - 12.31.24") %>% 
    mutate(
      fair_market_value = NA_real_
    )
  
  irr_full_data <- rbind(
    irr_existing_data,
    new_irr_data
  ) %>% 
    arrange(
      position_name,
      investment_type,
      transaction_date
    )
  
  aggregated_irr_data <- irr_full_data %>% 
    group_by(
      position_name,
      investment_type
    ) %>% 
    arrange(
      position_name,
      investment_type,
      transaction_date
    ) %>% 
    mutate(
      total_invested_called = cumsum(investment_amount)*-1 - cumsum(investment_expenses),
      total_realized_proceeds = cumsum(realized_proceeds),
      total_unrealized_value = cumsum(fair_market_value),
      total_value = total_realized_proceeds + total_unrealized_value,
      gross_moic = total_value / total_invested_called,
      position_row_number = row_number(),
      position_last_row = max(position_row_number)
    ) 
    
  
  total_fund_called <- aggregated_irr_data %>% 
    ungroup() %>% 
    filter(position_row_number == position_last_row) %>% 
    summarize(sum(total_invested_called, na.rm = TRUE)) %>% 
    pull()
  
  total_fund_realized <- aggregated_irr_data %>% 
    ungroup() %>% 
    filter(position_row_number == position_last_row) %>% 
    summarize(sum(total_realized_proceeds, na.rm = TRUE)) %>% 
    pull()
  
  total_fund_unrealized <- aggregated_irr_data %>% 
    ungroup() %>% 
    filter(position_row_number == position_last_row) %>% 
    summarize(sum(total_unrealized_value, na.rm = TRUE)) %>% 
    pull()
  
  total_fund_value <- total_fund_realized + total_fund_unrealized
  
  fund_gross_moic <- total_fund_value / total_fund_called
  
  capital_called_to_date <- read.xlsx("~/R Data/Aggregated Capital Calls and Distributions.xlsx") %>% 
    clean_names() %>% 
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    ) %>% 
    filter(
      fund == fund_filter,
      call_dist == "Call",
      date <= date_filter
    ) %>% 
    summarize(capital_called = sum(amount, na.rm = TRUE)*-1) %>% 
    pull()
  
  actual_cash_distributions <- read.xlsx("~/R Data/Aggregated Capital Calls and Distributions.xlsx") %>% 
    clean_names() %>% 
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    ) %>% 
    filter(
      fund == fund_filter,
      call_dist == "Distribution",
      date <= date_filter
    ) %>% 
    summarize(capital_distribution = sum(amount, na.rm = TRUE)) %>% 
    pull()
  
  net_asset_value <- read.xlsx("~/R Data/Aggregated Capital Calls and Distributions.xlsx") %>% 
    clean_names() %>% 
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    ) %>% 
    filter(
      fund == fund_filter,
      call_dist == "NAV",
      date == date_filter
    ) %>% 
    summarize(capital_nav = sum(amount, na.rm = TRUE)) %>% 
    pull()
  
  total_value_paid_in <- net_asset_value + actual_cash_distributions
  
  tvpi <- total_value_paid_in / capital_called_to_date
  
  gross_moic_multiplier <- (fund_gross_moic - tvpi) * capital_called_to_date
  
  net_moic_calculation <- aggregated_irr_data %>% 
    mutate(
      net_moic = ifelse(
        position_row_number == position_last_row, 
        (total_value - (total_value / (total_fund_value * gross_moic_multiplier))) / total_invested_called,
        NA_real_
      )
    )
  
  fund_alias_lookup_file <- read.xlsx("~/R Data/Fund_Alias_Lookup.xlsx", sheet = fund_filter) %>% 
    clean_names()
  
  front_page_mimic <- merge(
      net_moic_calculation,
      fund_alias_lookup_file,
      by.x = "position_name",
      by.y = "name",
      all.x = TRUE
    ) %>% 
    filter(
      position_row_number == position_last_row
    ) %>% 
    select(
      alias,
      position_name,
      investment_type,
      total_invested_called,
      total_realized_proceeds,
      total_unrealized_value,
      total_value,
      gross_moic,
      net_moic,
      order
    ) %>% 
    arrange(order) %>% 
    select(-order)
  
  return(front_page_mimic)
  
}

