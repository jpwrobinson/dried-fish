
## pull fatty acid sheet
sheets<-10

df<-read_excel(path, sheet = sheets) %>% clean_names() %>% 
    select(customer_marking, x20_5n_3_epa_mg_g_ww, x22_6n_3_dha_mg_g_ww, sum_epa_dha_mg_g_ww)
colnames(df)<-c('sample_id', 'epa', 'dha', 'epa_dha')

## convert epa / dha from mg per g (equivalent to g per kg) to g per 100g (divide by 10)
df$epa<-df$epa / 10
df$dha<-df$dha / 10
df$epa_dha<-df$epa_dha / 10
df$unit<-'g_100g'
