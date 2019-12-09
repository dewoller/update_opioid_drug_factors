
```{r  }

df_dd_overlap_all %>%
  mutate( supply_year = as.character( supply_year.opioid ) ) %>%
  left_join( df_patient ) %>%
  filter( lga != '.' ) %>%
  distinct( age, sex, pin, lga, supply_year) %>%
  count( supply_year, lga, age, sex ) %>%
  simple_standardise_value( group_by_vars=qc(lga, supply_year) ,  count_var='n' ) %>%
  mutate( state = factor( get_state_code_from_lga( lga) )) %>%
  group_by( state, supply_year ) %>%
  summarise( rate = mean( rate * 10 )) %>% 
  { . } -> two_step_standardise

two_step_standardise  %>% write.csv( '/tmp/two_step_standardise.csv')


df_dd_overlap_all %>%
  mutate( supply_year = as.character( supply_year.opioid ) ) %>%
  left_join( df_patient ) %>%
  filter( lga != '.' ) %>%
  mutate( state = factor( get_state_code_from_lga( lga) )) %>%
  distinct( age, sex, pin, state, supply_year) %>%
  count( supply_year, state, age, sex ) %>%
  simple_standardise_value( group_by_vars=qc(state,supply_year) ,  count_var='n' ) %>%
  mutate( rate =  rate * 10 ) %>% 
  { . } -> one_step_standardise

one_step_standardise %>% write.csv( '/tmp/one_step_standardise.csv')
```


