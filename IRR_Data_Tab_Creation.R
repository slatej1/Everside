
install_and_library_packages <- function(install_flag = FALSE) {
  
  if (install_flag != FALSE) {
    
    install.packages("dplyr")
    install.packages("openxlsx")
    install.packages("janitor")
    install.packages("data.table")
    install.packages("stats")
    install.packages("stringr")
    install.packages("readxl")
    install.packages("ggplot2")
    install.packages("jrvFinance")
    
  }
  
  library(dplyr)
  library(tibble)
  library(openxlsx)
  library(janitor)
  library(data.table)
  library(stats)
  library(jrvFinance)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(readxl)
  library(purrr)
  library(data.table)
  library(tidyr)
  library(stringr)
  options(scipen = 999)
  
}


create_irr_data_tab <- function(fund = "Fund IV", start_date = "2025-04-01", gl_file_input = "LTD Investment Transactions - 6.30.25 - Team Air update", excel_flag = TRUE) {

  gl_file <- read.xlsx(path.expand(paste0("~/R Data/", gl_file_input, ".xlsx"))) %>% 
    clean_names() %>% 
    mutate(
      gl_date = as.Date(gl_date, origin = "1899-12-30")
    ) %>% 
    filter(
      !grepl("Class B", comments_batch)
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
      "Everside Fund IV PF, LP",
      "Everside Fund IV, LP",
      "Everside International Fund IV, LP",
      "Everside Offshore Fund IV Blocker, LP",
      "Everside PF IV Blocker, LP",
      "Everside Fund IV Blocker, LP"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else if (fund == "Direct I") {
    
    entities <- c(
      "Everside Overflow Direct Fund, LP",
      "Everside Overflow Direct Fund, L.P.",
      "Everside Offshore Overflow Direct Fund, LP",
      "Everside Direct I Blocker, LLC"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else if (fund == "Direct II") {
    
    entities <- c(
      "Everside SBIC I, LP",
      "Everside SBIC I, Offshore"
    )
    
    print(paste0("Filtering for the following entities: ", entities))
    
  } else {
    
    print("ERROR: INCORRECT FUND PROVIDED")
    
  }
  # DIRECTS #
  
  fund_iv_data_all <- gl_file %>% 
    filter(
      legal_entity %in% entities,
      gl_date >= start_date,
      !grepl("RSM Payment|Transfer", comments_batch),
      !grepl("Transfer", comments_transaction),
      (position == "RF Ramsoft Investment, LP (Primary)" | !grepl("Primary|Secondary", position)),
      !grepl("Everside ", position)
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
        investment_expense = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    investment_expense <- fund_iv_data %>% 
      filter(position == account_name,
             trans_type == "Interest Expense") %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = dr_cr_amount*-1,
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
             trans_type == "Deferred Income") %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
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
        investment_expense = NA_real_,
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
      investment_expense,
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
             (grepl("Interest Receivable", trans_type) | 
                (str_detect(comments_transaction, "Interest Payment|Interest Income|Interest & Dividend Income") &
                   !grepl("Dividends Receivable", trans_type) &
                   !trans_type %in% c("Investment - Return of Capital", "Investment - Return of Capital - ROC") &
                   !trans_type %in% c("Realized Gain") &
                   !trans_type %in% c("Other Receivable", "Other Receivables") &
                   !grepl("ROC", comments_transaction)
                 )),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = dr_cr_amount*-1,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_closing_fee <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             trans_type == "Deferred Income") %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
        proceeds_closing_fee = dr_cr_amount*-1,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_dividend_income <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             (grepl("Dividends Receivable", trans_type) | (str_detect(comments_transaction, "Dividend Payment|Dividend payment|Dividend Income|Dividend income|Dividend -") & !str_detect(comments_transaction, "Interest"))),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = dr_cr_amount*-1,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_return_of_capital <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             trans_type %in% c("Investment - Return of Capital", "Investment - Return of Capital - ROC") | grepl("ROC", comments_transaction),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = dr_cr_amount*-1,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_realized_gain <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             trans_type %in% c("Realized Gain"),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = dr_cr_amount*-1,
        proceeds_mfee_rebate = NA_real_
      )
    
    proceeds_mfee_rebate <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             trans_type %in% c("Other Receivable", "Other Receivables"),
             grepl("Management Fee|Management fee|management fee|mfee", comments_transaction),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
        proceeds_closing_fee = NA_real_,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = dr_cr_amount*-1
      )
    
    proceeds_closing_fee_in <- fund_iv_data_cash_in %>% 
      filter(position == account_name,
             trans_type %in% c("Other Receivable", "Other Receivables"),
             !grepl("Management Fee|Management fee|management fee|mfee", comments_transaction),
             !grepl("ROC", comments_transaction),
             !(str_detect(comments_transaction, "Dividend Payment|Dividend payment|Dividend Income|Dividend income|Dividend -") & !str_detect(comments_transaction, "Interest")),
             batch_id %in% cash_in_batch_ids) %>% 
      transmute(
        legal_entity,
        deal_name,
        position,
        gl_date,
        investment_amount = NA_real_,
        investment_expense = NA_real_,
        proceeds_closing_fee = dr_cr_amount*-1,
        proceeds_interest_income = NA_real_,
        interest_proceeds = NA_real_,
        proceeds_dividend_income = NA_real_,
        proceeds_return_of_capital = NA_real_,
        proceeds_realized_gain = NA_real_,
        proceeds_mfee_rebate = NA_real_
      )
    
    aggregated_cash_in_investments_current_position <- rbind(
      proceeds_closing_fee,
      interest_proceeds,
      proceeds_dividend_income,
      proceeds_return_of_capital,
      proceeds_realized_gain,
      proceeds_mfee_rebate,
      proceeds_closing_fee_in
    )
    
    aggregated_cash_in_investments <- rbind(
      aggregated_cash_in_investments,
      aggregated_cash_in_investments_current_position
    )
    
    i <- i + 1
    
  }
  
  full_data_tab_output_directs <- rbind(
    aggregated_cash_out_investments,
    aggregated_cash_in_investments
  ) %>% 
    transmute(
      vintage_year = year(gl_date),
      transaction_date = gl_date,
      position_name = position,
      investment_type = case_when(
        grepl("Promissory Note|Term Note|Subordinated Debt|Secured Note|Senior|Subordinated|Debt|Note|Loan", position) ~ "Debt",
        grepl("Preferred|Common|Units|Stock|Equity|LLC Interest", position) ~ "Equity",
        grepl("(Primary)|(Secondary)", position) ~ "Partnership",
        TRUE ~ NA
      ),
      commitment = NA_real_,
      unfunded_commitment = NA_real_,
      gap = NA_real_,
      investment_amount,
      investment_expense = investment_expense,
      proceeds_return_of_capital,
      proceeds_interest_income = interest_proceeds,
      proceeds_dividend_income,
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
  
  # END DIRECTS #
  
  # FUNDS #
  
  fund_iv_data_all <- gl_file %>% 
    filter(
      legal_entity %in% entities,
      gl_date >= start_date,
      !grepl("RSM Payment|Transfer", comments_batch),
      !grepl("Transfer", comments_transaction),
      (position != "RF Ramsoft Investment, LP (Primary)" & grepl("Primary|Secondary", position)),
      !grepl("Everside ", position)
    )
  
  if (nrow(fund_iv_data_all) == 0) {
    
    print("NO PRIMARY/SECONDARY ENTRIES. LOGIC SKIPPED")
    
    full_data_tab_output <- full_data_tab_output_directs
    
    
  } else {
  
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
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_interest_income <- fund_iv_data %>% 
        filter(position == account_name,
               grepl("Sub Closing Interest", comments_transaction)) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = dr_cr_amount*-1,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_closing_fee <- fund_iv_data %>% 
        filter(position == account_name,
               grepl("Closing Fee", comments_transaction)) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = dr_cr_amount*-1,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_dividend_income <- fund_iv_data %>% 
        filter(position == account_name,
               trans_type == "Dividend Income - Investment" |
                 trans_type == "Dividends Receivable") %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = dr_cr_amount*-1,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_realized_gain <- fund_iv_data %>% 
        filter(position == account_name,
               grepl("Realized Gain", comments_transaction)) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = dr_cr_amount*-1,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_mfee_rebate <- fund_iv_data %>% 
        filter(position == account_name,
               grepl("Mgmt Fee Rebate", comments_transaction)) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = dr_cr_amount*-1
        )
      
      aggregated_cash_out_investments_current_position <- rbind(
        investment_amount,
        proceeds_interest_income,
        proceeds_closing_fee,
        proceeds_dividend_income,
        proceeds_realized_gain,
        proceeds_mfee_rebate
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
      
      investment_amount <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               grepl("Investment - Cost", trans_type),
               batch_id %in% cash_in_batch_ids) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = dr_cr_amount*-1,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_interest_income <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               (grepl("Interest Receivable", trans_type) | 
                  grepl("Interest Income - Investment", trans_type) |
                    grepl("Sub Closing Interest|subsequent close interest|Subsequent Close Interest", comments_transaction)),
               batch_id %in% cash_in_batch_ids) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = dr_cr_amount*-1,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_closing_fee <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               grepl("Closing Fee", comments_transaction)) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = dr_cr_amount*-1,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_dividend_income <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               (grepl("Dividends Receivable", trans_type) | grepl("Dividend Income - Investment", trans_type)),
               batch_id %in% cash_in_batch_ids) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = dr_cr_amount*-1,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_return_of_capital <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               grepl("ROC", comments_transaction),
               batch_id %in% cash_in_batch_ids) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = dr_cr_amount*-1,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_realized_gain <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               grepl("Realized Gain", comments_transaction),
               batch_id %in% cash_in_batch_ids) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = dr_cr_amount*-1,
          proceeds_mfee_rebate = NA_real_
        )
      
      proceeds_mfee_rebate <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               grepl("Mgmt Fee Rebate", comments_transaction),
               batch_id %in% cash_in_batch_ids) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = NA_real_,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = dr_cr_amount*-1
        )
      
      proceeds_closing_fee_in <- fund_iv_data_cash_in %>% 
        filter(position == account_name,
               grepl("Closing Fee", comments_transaction),
               batch_id %in% cash_in_batch_ids) %>% 
        transmute(
          legal_entity,
          deal_name,
          position,
          gl_date,
          investment_amount = NA_real_,
          investment_expense = NA_real_,
          proceeds_closing_fee = dr_cr_amount*-1,
          proceeds_interest_income = NA_real_,
          interest_proceeds = NA_real_,
          proceeds_dividend_income = NA_real_,
          proceeds_return_of_capital = NA_real_,
          proceeds_realized_gain = NA_real_,
          proceeds_mfee_rebate = NA_real_
        )
      
      aggregated_cash_in_investments_current_position <- rbind(
        investment_amount,
        proceeds_closing_fee,
        proceeds_interest_income,
        proceeds_dividend_income,
        proceeds_return_of_capital,
        proceeds_realized_gain,
        proceeds_mfee_rebate,
        proceeds_closing_fee_in
      )
      
      aggregated_cash_in_investments <- rbind(
        aggregated_cash_in_investments,
        aggregated_cash_in_investments_current_position
      )
      
      i <- i + 1
      
    }
    
    full_data_tab_output_funds <- rbind(
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
        investment_expense = investment_expense,
        proceeds_return_of_capital,
        proceeds_interest_income,
        proceeds_dividend_income,
        proceeds_realized_gain,
        proceeds_mfee_rebate,
        proceeds_closing_fee,
        proceeds_due_to_manager = NA_real_, # FOLLOW UP WITH HAO
        realized_proceeds = proceeds_return_of_capital + proceeds_interest_income + proceeds_dividend_income + proceeds_realized_gain + proceeds_mfee_rebate + proceeds_closing_fee + proceeds_due_to_manager,
        fund_name = legal_entity
      ) %>% 
      arrange(position_name,
              desc(transaction_date),
              investment_type)
    
    # END FUNDS #
    
    full_data_tab_output <- rbind(
      full_data_tab_output_funds,
      full_data_tab_output_directs
    ) %>% mutate(
      realized_proceeds = ifelse(is.na(proceeds_return_of_capital), 0, proceeds_return_of_capital) +
        ifelse(is.na(proceeds_interest_income), 0, proceeds_interest_income) +
        ifelse(is.na(proceeds_dividend_income), 0, proceeds_dividend_income) +
        ifelse(is.na(proceeds_realized_gain), 0, proceeds_realized_gain) +
        ifelse(is.na(proceeds_mfee_rebate), 0, proceeds_mfee_rebate) +
        ifelse(is.na(proceeds_closing_fee), 0, proceeds_closing_fee) +
        ifelse(is.na(proceeds_due_to_manager), 0, proceeds_due_to_manager)
    )
    
  }
  
  if (excel_flag == TRUE) {
  
    full_data_tab_output_excel <- full_data_tab_output %>% mutate(
      realized_proceeds = ifelse(is.na(proceeds_return_of_capital), 0, proceeds_return_of_capital) +
        ifelse(is.na(proceeds_interest_income), 0, proceeds_interest_income) +
        ifelse(is.na(proceeds_dividend_income), 0, proceeds_dividend_income) +
        ifelse(is.na(proceeds_realized_gain), 0, proceeds_realized_gain) +
        ifelse(is.na(proceeds_mfee_rebate), 0, proceeds_mfee_rebate) +
        ifelse(is.na(proceeds_closing_fee), 0, proceeds_closing_fee) +
        ifelse(is.na(proceeds_due_to_manager), 0, proceeds_due_to_manager)
    )
    
    full_data_tab_output_excel[is.na(full_data_tab_output_excel)] <- ""
    
    write.csv(full_data_tab_output_excel, path.expand(paste0("~/R Output/IRR Data Output/", fund, "-", start_date, "-IRR_data_tab_output", ".csv")), row.names = FALSE)
   
  }
  
  return(full_data_tab_output)
  
}
