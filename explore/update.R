source('lib/functions.R')
source('lib/get_data.R')

library(readxl)


my_dbReadTable('continuing.item') %>%
  mutate( drug = str_replace( generic_name, ' .*','')) %>% 
  { . } -> df_item 

read_excel( 'data/OME_input.xls') %>% 
{ . } ->    df_ome 


################################################################################
# Ashton
################################################################################

read_csv( 'data/ashton_input.csv') %>% 
  janitor::clean_names() %>%
  { . } ->    df_ashton_input

df_item %>% 
  filter( type_code == 10 ) %>%
  arrange(drug) %>%
  { . } -> df_item_ashton 

# error check, makes sure we get everything
df_item_ashton %>%
  anti_join( df_ashton_input)

df_item_ashton %>%
  inner_join( df_ashton_input) %>% 
  mutate(ome_mg_factor = NA) %>%
  { . } -> df_item_ashton

df_item_ashton %>%
  distinct( drug, ashton_mg_factor, ddd_mg_factor) %>%
  arrange( drug) 



################################################################################
# OME
################################################################################

df_oral=tibble( pbs_route=c('TABLET','ORAL LIQUID or SACH', 'LOZENGE','CAPSULE','SACHET'))

df_item %>% 
  filter( type_code != 10 ) %>%
  arrange(drug) %>%
  { . } -> df_item_ome


df_ome %>%
  filter( pbs_route=="ORAL") %>%
  select( -pbs_route ) %>%
  crossing( df_oral ) %>%
  bind_rows( df_ome  %>% filter( pbs_route!="ORAL") ) %>% 
  { . } -> df_ome_enhanced

df_item_ome %>% 
  left_join( df_ome_enhanced %>% select( type_code, pbs_route, ome_mg_factor), by=c('type_code', 'pbs_route')) %>% 
  mutate(ashton_mg_factor = NA) %>%
  { . } -> df_item_ome_final

bind_rows( df_item_ome_final, df_item_ashton) %>% 
{ . } -> df_final

df_final %>%
  filter( is.na( ome_mg_factor) &
  is.na( ashton_mg_factor) )



df_final %>%
  filter( is.na( ome_mg_factor)  ) %>%
  group_by(drug) %>%
  summarise( unit_wt = max(unit_wt), ddd_mg_factor = max(ddd_mg_factor), ashton_mg_factor = max(ashton_mg_factor)) %>%
  mutate( d=unit_wt / ddd_mg_factor,
         a=unit_wt / ashton_mg_factor, 
         ad=unit_wt * ashton_mg_factor, 
         r=d/a,
         r1 = d/ad
  ) %>% summarise( mean(r, na.rm=TRUE), mean(r1, na.rm=TRUE))

  df_final %>%
    filter( is.na( ashton_mg_factor)  ) %>%
    group_by(drug) %>%
    summarise( unit_wt = max(unit_wt), ddd_mg_factor = max(ddd_mg_factor), ome_mg_factor = max(ome_mg_factor)) %>%
    mutate( d=unit_wt / ddd_mg_factor,
           a=unit_wt / ome_mg_factor / 50, 
           ad=unit_wt * ome_mg_factor / 50, 
           r=d/a,
           r1 = d/ad
           ) %>% summarise( mean(r, na.rm=TRUE), mean(r1, na.rm=TRUE))

    df_final %>%
      filter(  drug == 'Morphine') %>%

  my_dbWriteTable(df_final, c('continuing','item_ome'))

df_final %>% 
  filter( drug=='Morphine' |
          startsWith(drug, 'Diaze')) %>%
dplyr::select( drug, ends_with('mg_factor'), pbs_route) %>%
distinct()





if(FALSE) {
# we were internally consistent with our oral ddd
df_item_ome %>%
  inner_join( df_oral ) %>%
  distinct(type_code, pbs_route,  ddd_mg_factor) %>% 
  count( type_code, pbs_route) %>%
  filter( n>1) 

df_item_ome %>% 
  distinct( pbs_route) %>%
  anti_join( df_ome %>% distinct(pbs_route))


df_item_ome %>%
  anti_join( df_ome_enhanced, by=c('type_code', 'pbs_route'))

df_ome_enhanced %>% 
  anti_join( df_oral) %>%
  anti_join( df_item_ome , by=c('type_code', 'pbs_route'))

df_item_ome%>% 
  anti_join( df_ome_enhanced  )


# note, the ddd and OME factors match approximately
df_item_ome %>% 
  left_join( df_ome_enhanced %>% select( type_code, pbs_route, ome_mg_factor), by=c('type_code', 'pbs_route')) %>%
  filter( !is.na(ome_mg_factor )) %>%
  mutate( pbs_route = ifelse( pbs_route %in% df_oral$pbs_route, 'ORAL', pbs_route)) %>%
  group_by( type_code, pbs_route, ddd_mg_factor, ome_mg_factor ) %>%
  summarise( min(generic_name) ) %>%   
  mutate( 1/ome_mg_factor * 100) 

}

get_continuing_df() -> a

a %>%
  mutate(factor = n_dose_ome / n_dose ) %>% 
  distinct( type_name, factor) %>%
  ggplot( aes(type_name, factor)) + geom_col()

  distinct( generic_name )


