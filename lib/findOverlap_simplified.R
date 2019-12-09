tic( 'join')
df_opioid %>%
  inner_join( df_benzo, by=qc( pin, supply_date )) %>% 
  rename_at( vars( ends_with( '.x')), funs(str_replace( .,'.x$', '.opioid'))) %>%
  rename_at( vars( ends_with( '.y')), funs(str_replace( .,'.y$', '.benzo'))) %>% 
  mutate( supply_date.benzo = supply_date) %>%
  rename( supply_date.opioid = supply_date) %>%
  { . } -> df_dd_0_new
toc()

tic( 'join+1')
df_opioid %>%
  mutate( target_supply_date = supply_date + 1 ) %>%
  inner_join( df_benzo, by=c( 'pin'='pin', 'target_supply_date' = 'supply_date' )) %>% 
  rename_at( vars( ends_with( '.x')), funs(str_replace( .,'.x$', '.opioid'))) %>%
  rename_at( vars( ends_with( '.y')), funs(str_replace( .,'.y$', '.benzo'))) %>% 
  rename( supply_date.opioid = supply_date, supply_date.benzo = target_supply_date) %>%
  { . } -> df_dd_1a_new
toc()

tic( 'join-1')
df_opioid %>%
  mutate( target_supply_date = supply_date - 1 ) %>%
  inner_join( df_benzo, by=c( 'pin'='pin', 'target_supply_date' = 'supply_date' )) %>% 
  rename_at( vars( ends_with( '.x')), funs(str_replace( .,'.x$', '.opioid'))) %>%
  rename_at( vars( ends_with( '.y')), funs(str_replace( .,'.y$', '.benzo'))) %>% 
  rename( supply_date.opioid = supply_date, supply_date.benzo = target_supply_date) %>%
  { . } -> df_dd_1b_new
toc()

tic( 'join-any_overlap - part 1')
df %>%
  group_by( pin ) %>%
  arrange( supply_date, item_code )  %>%
  mutate( is_dd = ifelse( is_benzo( type_code ) != is_benzo( lead ( type_code )) &
                              n_dose >= lead( supply_date ) - supply_date ,
                        1, 
                        0 
                      )
        )  %>%
  mutate( 
         is_dd = ifelse( is.na( lag( is_dd ) ), 
                        is_dd,
                        ifelse( lag( is_dd ) == 1, 2, is_dd )
                        ),
          other_row = ifelse( is_dd == 2, lag(row), NA )
         ) %>% 
  ungroup() %>%
  { . } -> df_dd_overlap
toc()

tic( 'join-any_overlap - part 2 - select out benzo and opioid, combine them')
df_dd_overlap %>%
  filter( is_dd == 1 & is_benzo( type_code ) ) %>%
  inner_join( df_dd_overlap, by=c('row'='other_row')) %>% 
  rename_at( vars( ends_with( '.x')), funs(str_replace( .,'.x$', '.benzo'))) %>%
  rename_at( vars( ends_with( '.y')), funs(str_replace( .,'.y$', '.opioid'))) %>% 
  rename( row.benzo = row, row.opioid = other_row ) %>%
  { . } -> df_dd_overlap_benzo
#
df_dd_overlap %>%
  filter( is_dd == 1 & !is_benzo( type_code ) ) %>%
  inner_join( df_dd_overlap, by=c('row'='other_row')) %>% 
  rename_at( vars( ends_with( '.x')), funs(str_replace( .,'.x$', '.opioid'))) %>%
  rename_at( vars( ends_with( '.y')), funs(str_replace( .,'.y$', '.benzo'))) %>% 
  rename( row.benzo = other_row, row.opioid = row ) %>%
  { . } -> df_dd_overlap_opioid
#
df_dd_overlap_opioid %>%
  bind_rows( df_dd_overlap_benzo ) %>% 
  rename( pin = pin.opioid ) %>%
  select( -pin.benzo, -is_dd.opioid, -is_dd.benzo ) %>%
  { . } -> df_dd_overlap_all
toc()

df_dd_overlap_all %>%
  bind_rows( df_dd_0_new ) %>%
  mutate( supply_year = year( supply_date.opioid )) %>%
  { . } -> df_dd_overlap_combined

################# ################# ################# ################# 
