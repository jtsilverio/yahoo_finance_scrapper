# Script que baixa e plota os dados de fechamento
# das companhias listadas na váriavel companies
companies = c("AAPL", "AMZN", "INTC", "GOOG", "CSCO")

# Busca dados da wikipedia e pega URL da página de cada empresa ----------------
wiki_urls = read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>% 
    html_element("tbody")

df_companies = wiki_urls %>% 
    html_table() %>%
    clean_names() %>% 
    select(c(1,2,4,5)) %>% 
    mutate(link = paste0("http://en.wikipedia.org",
                         html_nodes(wiki_urls, "td:nth-child(2) a") %>% 
                             html_attr("href"))
    ) %>% 
    filter(symbol %in% companies)

# Busca dados da wikipedia de cada empresa -------------------------------------
get_company_info = function(url){
    wiki_info = read_html(url) %>% 
        html_element(".infobox tbody") %>% 
        html_table() %>% 
        slice(3:n()) %>% 
        pivot_wider(names_from = X1, values_from = X2) %>% 
        clean_names() %>% 
        select(headquarters, key_people, number_of_employees) %>% 
        mutate(link = url)
    
    return(wiki_info)
}

wiki_info = purrr::map_df(df_companies$link, get_company_info)
df_companies = left_join(df_companies, wiki_info, "link") %>% 
    select(-link) %>% 
    rename(name = security)

# Busca dados de fechamentos ---------------------------------------------------
epoch = ymd_hms('1970-01-01 00:00:00', tz = "UTC")
today = as_datetime(Sys.Date(), tz = "UTC")

begin = as.numeric(as.duration(as_datetime(bizdays::offset(today, -200, "Brazil/ANBIMA")) - epoch))
end = as.numeric(as.duration(today - epoch))

urls = data.frame(
    symbol = companies,
    url = paste0("https://query1.finance.yahoo.com/v7/finance/download/", companies) %>%
        paste0("?period1=", begin,"&period2=", end, "&interval=1d&events=history")
) 

daily_prices = map_df(1:nrow(urls), function(i){
        read.csv(url(urls[i,]$url)) %>% 
            mutate(symbol = urls[i,]$symbol)
    }
    ) %>% 
    janitor::clean_names() %>% 
    select(symbol, date, close, volume) %>% 
    mutate(date = as_date(date))

# Salva dados em um único csv em um formato tidy -------------------------------
all_data = left_join(daily_prices, df_companies, by = "symbol")
write.csv(all_data, "Desafio de Programação/dados_fechamento.csv")

# Plota fechamento do ultimo dia de cada mês -----------------------------------
monthly_prices = daily_prices %>% 
    group_by(symbol, month = month(daily_prices$date)) %>% 
    slice_tail(n = 1) %>% 
    ungroup() %>% 
    select(-month)

ggplot(monthly_prices,
       aes(date,
           close,
           color = symbol)) +
    geom_line() +
    geom_point(size = 0.5) +
    scale_x_date(breaks = "1 month", label = label_date("%b"))+
    labs(color = "Empresa") +
    ylab("Preço de fechamento") +
    xlab("Data") 

