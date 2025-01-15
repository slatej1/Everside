
create_irr_data_tab <- function(fund = "Fund IV", start_date = "2024-07-01", gl_file_input = "LTD Investment Transactions - 11.30.24") {

  gl_file <- read.xlsx(path.expand(paste0("~/R Data/", gl_file_input, ".xlsx"))) %>% 
    clean_names() %>% 
    mutate(
      gl_date = as.Date(gl_date, origin = "1899-12-30")
    )
  
  if (fund == "Fund I Founders") {
    
    entities <- c(
      "Everside Founders Fund, LP"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else if (fund == "Fund II") {
    
    entities <- c(
      "Everside Fund II F1, LP",
      "Everside Fund II, LP",
      "Everside International Fund II, LP",
      "Everside F1 Blocker, LP",
      "Everside Offshore Fund II Blocker, LP"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else if (fund == "Fund III") {
    
    entities <- c(
      "Everside Fund III PF Debt, LP",
      "Everside Fund III PF, LP",
      "Everside Fund III, LP",
      "Everside International Fund III, LP",
      "Everside Offshore Fund III Blocker LP",
      "Everside PF Blocker, LP",
      "Everside Treeline Blocker III, LP"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else if (fund == "Fund IV") {
  
    entities <- c(
      "Everside Fund IV PF Debt, LP",
      "Everside Fund IV PF, LP",
      "Everside Fund IV, LP",
      "Everside International Fund IV, LP",
      "Everside Offshore Fund IV Blocker, LP",
      "Everside PF IV Blocker, LP",
      "Everside Treeline Blocker IV, LP"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else if (fund == "Direct I") {
    
    entities <- c(
      "Everside Overflow Direct Fund, LP",
      "Everside Offshore Overflow Direct Fund, LP",
      "Everside Direct I Blocker, LLC",
      "Everside Offshore Overflow Deal Blockers"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else if (fund == "Direct II") {
    
    entities <- c(
      "Everside SBIC I LP",
      "Everside SBIC I Offshore"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else {
    
    print("ERROR: INCORRECT FUND PROVIDED")
    
  }
  
  fund_iv_data_all <- gl_file %>% 
    filter(
      legal_entity %in% entities,
      gl_date >= start_date,
      !grepl("RSM Payment|Transfer", comments_batch),
      !grepl("Transfer", comments_transaction)
    )
  
  fund_iv_batch_ids <- fund_iv_data_all %>% 
    filter(trans_type %in% c("Cash Out - Investment", "Cash Out - Main Account", "Cash Out - Consolidation")) %>% 
    select(
      batch_id
    ) %>% 
    pull()
  
  fund_iv_data <- fund_iv_data_all %>% 
    filter(batch_id %in% fund_iv_batch_ids,
           !trans_type %in% c("Cash Out - Investment", "Cash Out - Main Account", "Cash Out - Consolidation"))
  
  
  debit_check <- fund_iv_data_all %>% 
    filter(batch_id %in% fund_iv_batch_ids,
           trans_type %in% c("Cash Out - Investment", "Cash Out - Main Account", "Cash Out - Consolidation")) %>% 
    summarize(sum(dr_cr_amount, na.rm = TRUE)) %>% 
    pull()
  
  credit_check <- fund_iv_data_all %>% 
    filter(batch_id %in% fund_iv_batch_ids,
           !trans_type %in% c("Cash Out - Investment", "Cash Out - Main Account", "Cash Out - Consolidation")) %>% 
    summarize(sum(dr_cr_amount, na.rm = TRUE)) %>% 
    pull()
  
  check <- debit_check + credit_check
  print(paste0("Difference between debits and credits: ", debit_check + credit_check))
  
  fund_iv_data_accounts <- fund_iv_data %>% 
    distinct(position)
  
  aggregated_cash_out_investments <- data.table()
  
  i <- 1
  
  for (i in 1:nrow(fund_iv_data_accounts)) {
    
    account_name <- fund_iv_data %>% 
      filter(position == fund_iv_data_accounts[i,1]) %>% 
      select(position) %>% 
      first() %>% 
      pull()
    
    investment_amount <- fund_iv_data %>% 
      filter(position == account_name,
             trans_type == "Investment - Cost") %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = dr_cr_amount*-1,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_closing_fee <- fund_iv_data %>% 
      filter(position == account_name,
             trans_type == "Other Income - Investment") %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = dr_cr_amount*-1,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_interest_income <- fund_iv_data %>% 
      filter(position == account_name,
             trans_type == "Interest Income - Investment") %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = dr_cr_amount*-1,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    aggregated_cash_out_investments_current_position <- rbind(
      investment_amount,
      proceeds_closing_fee,
      proceeds_interest_income
    )
      
    aggregated_cash_out_investments <- rbind(
      aggregated_cash_out_investments,
      aggregated_cash_out_investments_current_position
    )
    
    i <- i + 1
    
  }
  
  ### END OF CASH OUT ###
 
  cash_in_batch_ids <- fund_iv_data_all %>% 
    filter(trans_type %in% c("Cash In - Investment", "Cash In - Main Account", "Cash In - Consolidation")) %>% 
    select(
      batch_id
    ) %>% 
    pull()
  
  fund_iv_cash_in_accounts <- fund_iv_data_all %>% 
    filter(batch_id %in% cash_in_batch_ids,
           !trans_type %in% c("Cash In - Investment", "Cash In - Main Account", "Cash In - Consolidation")) %>% 
    distinct(position)
   
  fund_iv_data_cash_in <- fund_iv_data_all %>% 
    filter(batch_id %in% cash_in_batch_ids,
           !trans_type %in% c("Cash In - Investment", "Cash In - Main Account", "Cash In - Consolidation"))
  
  aggregated_cash_in_investments <- data.table()
  
  i <- 1

  for (i in 1:nrow(fund_iv_cash_in_accounts)) {
    
    account_name <- fund_iv_data_cash_in %>% 
      filter(position == fund_iv_cash_in_accounts[i,1]) %>% 
      select(position) %>% 
      first() %>% 
      pull()
    
    interest_proceeds <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             str_detect(comments_batch, "Interest Payment|Interest Income|Interest & Dividend Income"),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = dr_cr_amount*-1,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_dividend_income <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             str_detect(comments_batch, "Dividend Payment|Dividend payment|Dividend Income|Dividend income|Dividend -"),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = dr_cr_amount*-1,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_return_of_capital <- fund_iv_data_cash_in %>% 
      filter(position == account_name,,
             trans_type %in% c("Investment - Return of Capital", "Investment - Return of Capital - ROC"),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = dr_cr_amount*-1,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_realized_gain <- fund_iv_data_cash_in %>% 
      filter(position == account_name,,
             trans_type %in% c("Realized Gain"),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = dr_cr_amount*-1,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_mfee_rebate <- fund_iv_data_cash_in %>% 
      filter(position == account_name,,
             trans_type %in% c("Other Receivable"),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = dr_cr_amount*-1
      )
    
    proceeds_interest_income <- fund_iv_data_cash_in %>% 
      filter(position == account_name,,
             trans_type %in% c("Interest Receivable"),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = dr_cr_amount*-1,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    aggregated_cash_in_investments_current_position <- rbind(
      interest_proceeds,
      proceeds_dividend_income,
      proceeds_return_of_capital,
      proceeds_realized_gain,
      proceeds_mfee_rebate,
      proceeds_interest_income
    )
    
    aggregated_cash_in_investments <- rbind(
      aggregated_cash_in_investments,
      aggregated_cash_in_investments_current_position
    )
    
    i <- i + 1
    
  }
  
  full_data_tab_output <- rbind(
    aggregated_cash_out_investments,
    aggregated_cash_in_investments
  ) %>% 
    transmute(
      vintage_year = year(gl_date),
      transaction_date = gl_date,
      position_name = deal_name,
      investment_type = case_when(
        grepl("(Primary)|(Secondary)", position) ~ "Partnership",
        grepl("Promissory Note|Term Note|Subordinated Debt|Secured Note|Senior|Subordinated|Debt", position) ~ "Debt",
        grepl("Preferred|Common|Units|Stock|Equity", position) ~ "Equity",
        TRUE ~ NA
      ),
      commitment = NA_real_,
      unfunded_commitment = NA_real_,
      gap = NA_real_,
      investment_amount,
      investment_expenses = NA_real_, # FOLLOW UP WITH HAO
      proceeds_return_of_capital,
      proceeds_interest_income,
      proceeds_realized_gain,
      proceeds_mfee_rebate,
      proceeds_closing_fee,
      proceeds_due_to_manager = NA_real_, # FOLLOW UP WITH HAO
      realized_proceeds = proceeds_return_of_capital + proceeds_interest_income + proceeds_realized_gain + proceeds_mfee_rebate + proceeds_closing_fee + proceeds_due_to_manager,
      fund_name = legal_entity
    ) %>% 
    arrange(position_name,
            desc(transaction_date),
            investment_type)
  
  write.csv(full_data_tab_output, path.expand(paste0("~/R Output/IRR Data Output/", fund, "-", start_date, "-IRR_data_tab_output", ".csv")), row.names = FALSE)
   
  return(full_data_tab_output)
  
}
