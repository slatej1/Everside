
everside_capital_plot <- function(input_fund = "All", x_axis = "years") {
  
  ingested_capital_data <- read.xlsx("~/R Data/Aggregated Capital Calls and Distributions.xlsx") %>% 
    clean_names() %>% 
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    )
    
  everside_capital_data <- ingested_capital_data %>% 
    arrange(fund,
            date) %>% 
    mutate(
      facility_max = case_when(
        fund == "Direct I" ~ 86080000,
        fund == "Direct II" ~ 135000000,
        fund == "Fund I Founders" ~ 28900000,
        fund == "Fund I Suncap" ~ 44000000,
        fund == "Fund II" ~ 240000000,
        fund == "Fund III" ~ 510000000,
        fund == "Fund IV" ~ 601000000,
        TRUE ~ NA_real_
      ),
      cumulative_called_percent = (-1 * cumulative_called)/facility_max,
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
    ) %>% 
    tibble()
  
  all_everside_capital_data_called <- ingested_capital_data %>% 
    filter(call_dist == "Call") %>% 
    arrange(date) %>% 
    mutate(
      cumulative_called = cumsum(amount)
    )
  
  all_everside_capital_data_distribution <- ingested_capital_data %>% 
    filter(call_dist == "Distribution") %>% 
    arrange(date) %>% 
    mutate(
      cumulative_distribution = cumsum(amount)
    )
  
  all_everside_capital <- merge(
    merge(
      ingested_capital_data,
      all_everside_capital_data_called,
      by = c(
        "fund",
        "entity",
        "call_dist",
        "date",
        "amount",
        "days",
        "years",
        "fund_max_age",
        "fund_max_days",
        "cumulative_distribution"
      ),
      all.x = TRUE,
      suffixes = c("", "_agg")
    ),
    all_everside_capital_data_distribution,
    by = c(
      "fund",
      "entity",
      "call_dist",
      "date",
      "amount",
      "days",
      "years",
      "fund_max_age",
      "fund_max_days",
      "cumulative_called"
    ),
    all.x = TRUE,
    suffixes = c("", "_agg")
  )
  
  all_everside_capital_filled <- all_everside_capital %>% 
    arrange(date) %>% 
    fill(cumulative_called_agg) %>% 
    fill(cumulative_distribution_agg) %>% 
    mutate(
      cumulative_called = cumulative_called_agg,
      cumulative_distribution = cumulative_distribution_agg,
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
    ) %>% 
    select(
      -cumulative_called_agg,
      -cumulative_distribution_agg
    )
  
  if (x_axis == "years") {
    
    direct_i_distribution <- everside_capital_data %>% 
      filter(fund == "Direct I") %>% 
      select(
        years,
        direct_i_distribution = cumulative_distribution_percent
      )
    
    fund_i_founders_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Founders") %>% 
      select(
        years,
        founders_distribution = cumulative_distribution_percent
      )
    
    fund_i_suncap_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Suncap") %>% 
      select(
        years,
        suncap_distribution = cumulative_distribution_percent
      )
    
    fund_ii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund II") %>% 
      select(
        years,
        fund_ii_distribution = cumulative_distribution_percent
      )
    
    fund_iii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund III") %>% 
      select(
        years,
        fund_iii_distribution = cumulative_distribution_percent
      )
    
    fund_iv_distribution <- everside_capital_data %>% 
      filter(fund == "Fund IV") %>% 
      select(
        years,
        fund_iv_distribution = cumulative_distribution_percent
      )
    
    combined_data <- merge(
      merge(
        merge(
          merge(
            merge(
              direct_i_distribution,
              fund_i_founders_distribution,
              by = "years",
              all = TRUE
            ),
            fund_i_suncap_distribution,
            by = "years",
            all = TRUE
          ),
          fund_ii_distribution,
          by = "years",
          all = TRUE
        ),
        fund_iii_distribution,
        by = "years",
        all = TRUE
      ),
      fund_iv_distribution,
      by = "years",
      all = TRUE
    )
    
    combined_data_filled <- combined_data %>% 
      fill(direct_i_distribution) %>% 
      fill(founders_distribution) %>% 
      fill(suncap_distribution) %>% 
      fill(fund_ii_distribution) %>% 
      fill(fund_iii_distribution) %>% 
      fill(fund_iv_distribution)
    
    if (tolower(input_fund) %in%  c("founders", "fund i founders", "fund_i_founders")) {
      
      gfg_plot_founders <- ggplot(combined_data_filled, aes(x = years)) +
        geom_line(aes(y = founders_distribution), color = "red")
      
      gfg_plot_founders
      
    } else if (tolower(input_fund) %in%  c("suncap", "fund i suncap", "fund_i_suncap")) {
      
      gfg_plot_suncap <- ggplot(combined_data_filled, aes(x = years)) +
        geom_line(aes(y = suncap_distribution), color = "blue")
      
      gfg_plot_suncap
      
    } else if (tolower(input_fund) %in%  c("fund ii", "fund_ii")) {
      
      gfg_plot_fund_ii <- ggplot(combined_data_filled, aes(x = years)) +
        geom_line(aes(y = fund_ii_distribution), color = "green")
      
      gfg_plot_fund_ii
      
    } else if (tolower(input_fund) %in%  c("fund iii", "fund_iii")) {
      
      gfg_plot_fund_iii <- ggplot(combined_data_filled, aes(x = years)) +
        geom_line(aes(y = fund_iii_distribution), color = "purple")
      
      gfg_plot_fund_iii
      
    } else if (tolower(input_fund) %in%  c("fund iv", "fund_iv")) {
      
      gfg_plot_fund_iv <- ggplot(combined_data_filled, aes(x = years)) +
        geom_line(aes(y = fund_iv_distribution), color = "black")
      
      gfg_plot_fund_iv
      
    } else if (tolower(input_fund) %in%  c("direct i", "direct_i")) {
      
      gfg_plot_direct_i <- ggplot(combined_data_filled, aes(x = years)) +
        geom_line(aes(y = direct_i_distribution), color = "pink")
      
      gfg_plot_direct_i
      
    } else if (tolower(input_fund) %in% c("as_one", "as one", "all_everside", "all everside", "everside", "aggregate")) {
      
      one_everside_agg_distribution <- all_everside_capital_filled %>% 
        select(
          years,
          one_everside_agg_distribution = cumulative_distribution_percent
        )
      
      gfg_plot_one_everside_agg <- ggplot(one_everside_agg_distribution, aes(x = years)) +
        geom_line(aes(y = one_everside_agg_distribution), color = "red")
      
      gfg_plot_one_everside_agg
      
    } else {
      
      gfg_plot_all <- ggplot(combined_data_filled, aes(x = years)) +
        geom_line(aes(y = direct_i_distribution), color = "pink") +
        geom_line(aes(y = founders_distribution), color = "red") +
        geom_line(aes(y = suncap_distribution), color = "blue") +
        geom_line(aes(y = fund_ii_distribution), color = "green") +
        geom_line(aes(y = fund_iii_distribution), color = "purple") +
        geom_line(aes(y = fund_iv_distribution), color = "black") +
        labs(x = "Fund Age - Years", y = "Distribution % of Capital Called") +
        ggtitle("Distribution Rate - All Funds")
      
      gfg_plot_all
      
    }
    
  } else {
    
    direct_i_distribution <- everside_capital_data %>% 
      filter(fund == "Direct I") %>% 
      select(
        date,
        direct_i_distribution = cumulative_distribution_percent
      )
    
    fund_i_founders_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Founders") %>% 
      select(
        date,
        founders_distribution = cumulative_distribution_percent
      )
    
    fund_i_suncap_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Suncap") %>% 
      select(
        date,
        suncap_distribution = cumulative_distribution_percent
      )
    
    fund_ii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund II") %>% 
      select(
        date,
        fund_ii_distribution = cumulative_distribution_percent
      )
    
    fund_iii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund III") %>% 
      select(
        date,
        fund_iii_distribution = cumulative_distribution_percent
      )
    
    fund_iv_distribution <- everside_capital_data %>% 
      filter(fund == "Fund IV") %>% 
      select(
        date,
        fund_iv_distribution = cumulative_distribution_percent
      )
    
    combined_data <- merge(
      merge(
        merge(
          merge(
            merge(
              direct_i_distribution,
              fund_i_founders_distribution,
              by = "date",
              all = TRUE
            ),
            fund_i_suncap_distribution,
            by = "date",
            all = TRUE
          ),
          fund_ii_distribution,
          by = "date",
          all = TRUE
        ),
        fund_iii_distribution,
        by = "date",
        all = TRUE
      ),
      fund_iv_distribution,
      by = "date",
      all = TRUE
    )
    
    combined_data_filled <- combined_data %>% 
      fill(direct_i_distribution) %>% 
      fill(founders_distribution) %>% 
      fill(suncap_distribution) %>% 
      fill(fund_ii_distribution) %>% 
      fill(fund_iii_distribution) %>% 
      fill(fund_iv_distribution)
    
    if (tolower(input_fund) %in%  c("founders", "fund i founders", "fund_i_founders")) {
      
      gfg_plot_founders <- ggplot(combined_data_filled, aes(x = date)) +
        geom_line(aes(y = founders_distribution), color = "red")
      
      gfg_plot_founders
      
    } else if (tolower(input_fund) %in%  c("suncap", "fund i suncap", "fund_i_suncap")) {
      
      gfg_plot_suncap <- ggplot(combined_data_filled, aes(x = date)) +
        geom_line(aes(y = suncap_distribution), color = "blue")
      
      gfg_plot_suncap
      
    } else if (tolower(input_fund) %in%  c("fund ii", "fund_ii")) {
      
      gfg_plot_fund_ii <- ggplot(combined_data_filled, aes(x = date)) +
        geom_line(aes(y = fund_ii_distribution), color = "green")
      
      gfg_plot_fund_ii
      
    } else if (tolower(input_fund) %in%  c("fund iii", "fund_iii")) {
      
      gfg_plot_fund_iii <- ggplot(combined_data_filled, aes(x = date)) +
        geom_line(aes(y = fund_iii_distribution), color = "purple")
      
      gfg_plot_fund_iii
      
    } else if (tolower(input_fund) %in%  c("fund iv", "fund_iv")) {
      
      gfg_plot_fund_iv <- ggplot(combined_data_filled, aes(x = date)) +
        geom_line(aes(y = fund_iv_distribution), color = "black")
      
      gfg_plot_fund_iv
      
    } else if (tolower(input_fund) %in%  c("direct i", "direct_i")) {
      
      gfg_plot_direct_i <- ggplot(combined_data_filled, aes(x = date)) +
        geom_line(aes(y = direct_i_distribution), color = "pink")
      
      gfg_plot_direct_i
      
    } else if (tolower(input_fund) %in% c("as_one", "as one", "all_everside", "all everside", "everside", "aggregate")) {
      
      one_everside_agg_distribution <- all_everside_capital_filled %>% 
        select(
          date,
          one_everside_agg_distribution = cumulative_distribution_percent
        )
      
      gfg_plot_one_everside_agg <- ggplot(one_everside_agg_distribution, aes(x = date)) +
        geom_line(aes(y = one_everside_agg_distribution), color = "red") +
        labs(x = "Date", y = "Distribution % of Capital Called") +
        ggtitle("Distribution Rate - All Everside")
      
      gfg_plot_one_everside_agg
      
    } else {
      
      gfg_plot_all <- ggplot(combined_data_filled, aes(x = date)) +
        geom_line(aes(y = direct_i_distribution), color = "pink") +
        geom_line(aes(y = founders_distribution), color = "red") +
        geom_line(aes(y = suncap_distribution), color = "blue") +
        geom_line(aes(y = fund_ii_distribution), color = "green") +
        geom_line(aes(y = fund_iii_distribution), color = "purple") +
        geom_line(aes(y = fund_iv_distribution), color = "black") +
        labs(x = "Date", y = "Distribution % of Capital Called") +
        ggtitle("Distribution Rate - All Funds")
      
      gfg_plot_all
      
    }
    
  }
  
}

everside_capital_plot_selected_funds <- function(input_fund = c("All"), x_axis = "years") {
  
  ingested_capital_data <- read.xlsx("~/R Data/Aggregated Capital Calls and Distributions.xlsx") %>% 
    clean_names() %>% 
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    ) %>% 
    filter(
      fund %in% input_fund
    )
  
  everside_capital_data <- ingested_capital_data %>% 
    arrange(fund,
            date) %>% 
    mutate(
      facility_max = case_when(
        fund == "Direct I" ~ 86080000,
        fund == "Direct II" ~ 135000000,
        fund == "Fund I Founders" ~ 28900000,
        fund == "Fund I Suncap" ~ 44000000,
        fund == "Fund II" ~ 240000000,
        fund == "Fund III" ~ 510000000,
        fund == "Fund IV" ~ 601000000,
        TRUE ~ NA_real_
      ),
      cumulative_called_percent = (-1 * cumulative_called)/facility_max,
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
    ) %>% 
    tibble()
  
  all_everside_capital_data_called <- ingested_capital_data %>% 
    filter(call_dist == "Call") %>% 
    arrange(date) %>% 
    mutate(
      cumulative_called = cumsum(amount)
    )
  
  all_everside_capital_data_distribution <- ingested_capital_data %>% 
    filter(call_dist == "Distribution") %>% 
    arrange(date) %>% 
    mutate(
      cumulative_distribution = cumsum(amount)
    )
  
  all_everside_capital <- merge(
    merge(
      ingested_capital_data,
      all_everside_capital_data_called,
      by = c(
        "fund",
        "entity",
        "call_dist",
        "date",
        "amount",
        "days",
        "years",
        "fund_max_age",
        "fund_max_days",
        "cumulative_distribution"
      ),
      all.x = TRUE,
      suffixes = c("", "_agg")
    ),
    all_everside_capital_data_distribution,
    by = c(
      "fund",
      "entity",
      "call_dist",
      "date",
      "amount",
      "days",
      "years",
      "fund_max_age",
      "fund_max_days",
      "cumulative_called"
    ),
    all.x = TRUE,
    suffixes = c("", "_agg")
  )
  
  all_everside_capital_filled <- all_everside_capital %>% 
    arrange(date) %>% 
    fill(cumulative_called_agg) %>% 
    fill(cumulative_distribution_agg) %>% 
    mutate(
      cumulative_called = cumulative_called_agg,
      cumulative_distribution = cumulative_distribution_agg,
      cumulative_distribution_percent = cumulative_distribution/(-1 * cumulative_called)
    ) %>% 
    select(
      -cumulative_called_agg,
      -cumulative_distribution_agg
    )
  
  if (x_axis == "years") {
    
    direct_i_distribution <- everside_capital_data %>% 
      filter(fund == "Direct I") %>% 
      select(
        years,
        direct_i_distribution = cumulative_distribution_percent
      )
    
    fund_i_founders_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Founders") %>% 
      select(
        years,
        founders_distribution = cumulative_distribution_percent
      )
    
    fund_i_suncap_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Suncap") %>% 
      select(
        years,
        suncap_distribution = cumulative_distribution_percent
      )
    
    fund_ii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund II") %>% 
      select(
        years,
        fund_ii_distribution = cumulative_distribution_percent
      )
    
    fund_iii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund III") %>% 
      select(
        years,
        fund_iii_distribution = cumulative_distribution_percent
      )
    
    fund_iv_distribution <- everside_capital_data %>% 
      filter(fund == "Fund IV") %>% 
      select(
        years,
        fund_iv_distribution = cumulative_distribution_percent
      )
    
    combined_data <- merge(
      merge(
        merge(
          merge(
            merge(
              direct_i_distribution,
              fund_i_founders_distribution,
              by = "years",
              all = TRUE
            ),
            fund_i_suncap_distribution,
            by = "years",
            all = TRUE
          ),
          fund_ii_distribution,
          by = "years",
          all = TRUE
        ),
        fund_iii_distribution,
        by = "years",
        all = TRUE
      ),
      fund_iv_distribution,
      by = "years",
      all = TRUE
    )
    
    combined_data_filled <- combined_data %>% 
      fill(direct_i_distribution) %>% 
      fill(founders_distribution) %>% 
      fill(suncap_distribution) %>% 
      fill(fund_ii_distribution) %>% 
      fill(fund_iii_distribution) %>% 
      fill(fund_iv_distribution)
      
      one_everside_agg_distribution <- all_everside_capital_filled %>% 
        select(
          years,
          one_everside_agg_distribution = cumulative_distribution_percent
        )
      
      gfg_plot_one_everside_agg <- ggplot(one_everside_agg_distribution, aes(x = years)) +
        geom_line(aes(y = one_everside_agg_distribution), color = "red")
      
      gfg_plot_one_everside_agg
    
  } else {
    
    direct_i_distribution <- everside_capital_data %>% 
      filter(fund == "Direct I") %>% 
      select(
        date,
        direct_i_called = cumulative_called,
        direct_i_distribution_total = cumulative_distribution,
        direct_i_distribution = cumulative_distribution_percent,
        facility_max
      )
    
    fund_i_founders_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Founders") %>% 
      select(
        date,
        founders_called = cumulative_called,
        founders_distribution_total = cumulative_distribution,
        founders_distribution = cumulative_distribution_percent,
        facility_max
      )
    
    fund_i_suncap_distribution <- everside_capital_data %>% 
      filter(fund == "Fund I Suncap") %>% 
      select(
        date,
        suncap_called = cumulative_called,
        suncap_distribution_total = cumulative_distribution,
        suncap_distribution = cumulative_distribution_percent,
        facility_max
      )
    
    fund_ii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund II") %>% 
      select(
        date,
        fund_ii_called = cumulative_called,
        fund_ii_distribution_total = cumulative_distribution,
        fund_ii_distribution = cumulative_distribution_percent,
        facility_max
      )
    
    fund_iii_distribution <- everside_capital_data %>% 
      filter(fund == "Fund III") %>% 
      select(
        date,
        fund_iii_called = cumulative_called,
        fund_iii_distribution_total = cumulative_distribution,
        fund_iii_distribution = cumulative_distribution_percent,
        facility_max
      )
    
    fund_iv_distribution <- everside_capital_data %>% 
      filter(fund == "Fund IV") %>% 
      select(
        date,
        fund_iv_called = cumulative_called,
        fund_iv_distribution_total = cumulative_distribution,
        fund_iv_distribution = cumulative_distribution_percent,
        facility_max
      )
    
    combined_data <- merge(
      merge(
        merge(
          merge(
            merge(
              direct_i_distribution,
              fund_i_founders_distribution,
              by = "date",
              all = TRUE,
              suffixes = c("_direct_i", "_founders")
            ),
            fund_i_suncap_distribution,
            by = "date",
            all = TRUE,
            suffixes = c("", "_suncap")
          ),
          fund_ii_distribution,
          by = "date",
          all = TRUE,
          suffixes = c("", "_fund_ii")
        ),
        fund_iii_distribution,
        by = "date",
        all = TRUE,
        suffixes = c("", "_fund_iii")
      ),
      fund_iv_distribution,
      by = "date",
      all = TRUE,
      suffixes = c("", "_fund_iv")
    )
    
    combined_data_filled <- combined_data %>% 
      fill(direct_i_distribution) %>% 
      fill(founders_distribution) %>% 
      fill(suncap_distribution) %>% 
      fill(fund_ii_distribution) %>% 
      fill(fund_iii_distribution) %>% 
      fill(fund_iv_distribution) %>% 
      fill(facility_max_direct_i) %>% 
      fill(facility_max_founders) %>% 
      fill(facility_max) %>% 
      fill(facility_max_fund_ii) %>% 
      fill(facility_max_fund_iii) %>% 
      fill(facility_max_fund_iv) %>% 
      ungroup() %>% 
      mutate(
        total_called = (
          ifelse(is.na(direct_i_called),0,direct_i_called) +
          ifelse(is.na(founders_called),0,founders_called) +
          ifelse(is.na(suncap_called),0,suncap_called) +
          ifelse(is.na(fund_ii_called),0,fund_ii_called) +
          ifelse(is.na(fund_iii_called),0,fund_iii_called) +
          ifelse(is.na(fund_iv_called),0,fund_iv_called)
        ),
        total_distribution = (
          ifelse(is.na(direct_i_distribution_total),0,direct_i_distribution_total) +
            ifelse(is.na(founders_distribution_total),0,founders_distribution_total) +
            ifelse(is.na(suncap_distribution_total),0,suncap_distribution_total) +
            ifelse(is.na(fund_ii_distribution_total),0,fund_ii_distribution_total) +
            ifelse(is.na(fund_iii_distribution_total),0,fund_iii_distribution_total) +
            ifelse(is.na(fund_iv_distribution_total),0,fund_iv_distribution_total)
        ),
        total_facility_max = (
          ifelse(is.na(facility_max_direct_i),0,facility_max_direct_i) +
            ifelse(is.na(facility_max_founders),0,facility_max_founders) +
            ifelse(is.na(facility_max),0,facility_max) +
            ifelse(is.na(facility_max_fund_ii),0,facility_max_fund_ii) +
            ifelse(is.na(facility_max_fund_iii),0,facility_max_fund_iii) +
            ifelse(is.na(facility_max_fund_iv),0,facility_max_fund_iv)
        )
      ) %>% 
      mutate(
        total_called_cum = cumsum(total_called),
        total_distribution_cum = cumsum(total_distribution)
      ) %>% 
      arrange(date) %>% 
      fill(total_distribution) %>% 
      mutate(
        cumulative_called_percentage = (-1*total_called_cum)/total_facility_max,
        cumulative_distribution_percentage = total_distribution_cum/(-1*total_called_cum)
      )
      
      one_everside_agg_distribution <- combined_data_filled %>% 
        transmute(
          date,
          one_everside_agg_distribution = cumulative_distribution_percentage
        )
      
      gfg_plot_one_everside_agg <- ggplot(one_everside_agg_distribution, aes(x = date)) +
        geom_line(aes(y = one_everside_agg_distribution), color = "red") +
        labs(x = "Date", y = "Distribution % of Capital Called") +
        ggtitle("Distribution Rate - All Everside")
      
      gfg_plot_one_everside_agg
    
  }
  
}



# cumulative_called <- everside_capital_data %>% 
#   filter(call_dist == "Call") %>% 
#   group_by(fund) %>% 
#   mutate(cumulative_called = cumsum(amount)*-1) %>% 
#   ungroup()
# 
# cumulative_distribution <- everside_capital_data %>% 
#   filter(call_dist == "Distribution") %>% 
#   group_by(fund) %>% 
#   mutate(cumulative_distribution = cumsum(amount)) %>% 
#   ungroup()
# 
# full_data <- merge(
#   merge(
#     everside_capital_data,
#     cumulative_called,
#     by = c(
#       "fund",
#       "entity",
#       "call_dist",
#       "date",
#       "amount",
#       "days",
#       "years",
#       "fund_max_age",
#       "fund_max_days"
#     ),
#     all.x = TRUE
#   ),
#   cumulative_distribution,
#   by = c(
#     "fund",
#     "entity",
#     "call_dist",
#     "date",
#     "amount",
#     "days",
#     "years",
#     "fund_max_age",
#     "fund_max_days"
#   ),
#   all.x = TRUE
# ) %>% 
#   arrange(fund, 
#           date)
#   
# filled_data_1 <- full_data %>% 
#   arrange(fund,
#           date,
#           cumulative_called) %>% 
#   group_by(fund) %>% 
#   fill(cumulative_called) %>% 
#   ungroup()
# 
# filled_data <- filled_data_1 %>% 
#   arrange(fund,
#           date,
#           cumulative_distribution) %>% 
#   group_by(fund) %>% 
#   fill(cumulative_distribution) %>% 
#   ungroup() %>% 
#   mutate(
#     cumulative_distribution = ifelse(
#       is.na(cumulative_distribution),
#       0,
#       cumulative_distribution
#     )
#   )
