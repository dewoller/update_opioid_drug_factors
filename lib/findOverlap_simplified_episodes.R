# devtools::install_github("r-lib/progress")
 
########################################################################################
update_balance = function( ndose, supply_date, balance ) {
  if ( is.na( balance$date_of_balance )) 
  {
    balance$balance_dose = ndose
  } 
  else 
  { 
    # account for the case where there may be dose left from last supply
    balance$balance_dose = 
      ndose + pmax( 0, balance$balance_dose - (as.numeric(supply_date) - as.numeric(balance$date_of_balance )))
  }
  balance$date_of_balance = supply_date # update date
  return( balance )
}
########################################################################################
########################################################################################
find_episode_overlap = function( df  ) {

  df_n = dim(df)[1]
#  library(progress)
#  pb <- progress_bar$new( format = " [:bar] :percent eta: :eta elapsed: :elapsedfull",
#                        total = df_n, clear = FALSE, width= 60)
  df$ndays_overlap = 0
  df$benzo_balance = NA
  df$opioid_balance = NA
  opioid_balance = data.frame( date_of_balance = NA, balance_dose=0)
  benzo_balance = data.frame( date_of_balance = NA, balance_dose=0)
  for (i in 1:df_n ) {
    row = df[i,]

    if (row$drug_type == 'opioid') {
      opioid_balance  = update_balance( row$n_dose, row$supply_date, opioid_balance ) 
      benzo_balance  = update_balance( 0, row$supply_date, benzo_balance ) 
    } else {
      benzo_balance  = update_balance( row$n_dose, row$supply_date, benzo_balance ) 
      opioid_balance  = update_balance( 0, row$supply_date, opioid_balance ) 
    }
    df[i,]$benzo_balance = benzo_balance$balance_dose 
    df[i,]$opioid_balance = opioid_balance$balance_dose 
    df[i,]$ndays_overlap = ceiling( pmin( opioid_balance$balance_dose, benzo_balance$balance_dose ))
  }
  df
}
  ########################################################################################



cl <- create_cluster(11)
set_default_cluster(cl)
cluster_copy(cl, update_balance )    
cluster_copy(cl, find_episode_overlap  )


df %>%
  group_by( pin, supply_date, drug_type ) %>%
  summarise( n_dose = sum( n_dose )) %>%
  ungroup() %>%
  #
  #  group_by( pin, drug_type ) %>%
  #  filter( n() > 2 ) %>%
  #  ungroup() %>%
  #
  #  mutate( ndays_overlap = 0 ) %>%
  arrange(pin, supply_date) %>%
  partition( pin ) %>% 
  group_by( pin ) %>%
  do( find_episode_overlap(.) ) %>%
  collect() %>%
  filter( ndays_overlap  > 0 ) 
  mutate( supply_year = as.character( year( supply_date ))) %>%
  mutate( ndays_overlap_noduplication = ifelse ( is.na( lead( supply_date )), ndays_overlap,
                               ifelse( supply_date == lead( supply_date ), 0, 
                                       pmin( ndays_overlap, lead( supply_date ) - supply_date)
          ))) -> df_match

#df_match  %>% 
#  ungroup() %>%
#  write.csv( row.names=F, file= 'data/overlaps.csv')


df_match %>%
  inner_join(  df_patient_usage, by='pin'  ) %>%
  inner_join( select( df_patient, pin, benzo_total_doses:doc_benzo_doses ), by='pin') %>% 
  foreign::write.dta( '/tmp/match_multiyear.v2.dta')

df_match %>%
  ungroup() %>%
  group_by( pin, sex, age,lga,lga_name,state_code,state_name,area_albers_sqkm,irsd_score_raw,seifa,class_type,class_name,urbanization,state ) %>%
  summarise( ndays = sum( ndays )) %>%
  ungroup() %>%
  inner_join( select( df_patient, pin, benzo_total_doses:doc_benzo_doses ), by='pin') %>% 
  left_join(  df_patient_usage, by='pin'  ) %>%
  foreign::write.dta( '/tmp/match_single_period.v1.dta')

df_patient


