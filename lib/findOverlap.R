

#Methods  - find overlaps 

#################################################################################
#################################################################################

find_overlap <- function ( df_opioid, df_benzo, dataset, start_day_difference = NA, min_overlap = NA ) {

  save_filename = paste0( 'data/cache/intersect', dataset, '.rdata')
  tic('about to get base intersect data')
  get_intersect( df_opioid, df_benzo, save_filename ) -> rv
  toc()
  tic('about to set start_day_difference filter')
  if ( !is.na( start_day_difference ) ) {
    rv %>% 
      filter( abs( supply_date.opioid - supply_date.benzo ) <= 
             start_day_difference ) %>% 
      { . } -> rv
  }
  toc()
  tic('about to set min_overlap filter')
  if ( !is.na( min_overlap ) ) {
    rv %>% 
      filter( diff  >= min_overlap ) %>% 
      { . } -> rv
  }
  toc()
  rv

}





#################################################################################
#################################################################################

get_intersect <- function( df_benzo, df_opioid, save_filename ) 
{
  tic( 'get intersect total' )
  if (file.exists( save_filename) ) {
    load( save_filename, verbose=TRUE )
  } else {
    #

    tic( 'create nested datasets opioids' )

    df_benzo %>%
      mutate( end_date = supply_date + pmax( 0, round(n_dose)-1)) %>%
      select( pin, supply_date, end_date, row) %>% 
      group_by(pin) %>%
      nest( supply_date, end_date, row, .key=benzo ) %>%
      { . } -> df_benzo_nested

    #
    toc()
    tic( 'create nested datasets benzo' )

    df_opioid %>%
      mutate( end_date = supply_date + pmax( 0, round(n_dose)-1)) %>%
      select( pin, supply_date, end_date, row) %>% 
      group_by(pin) %>%
      nest( supply_date, end_date, row, .key=opioid ) %>%
      { . } -> df_opioid_nested

    #
    toc()
    tic( 'find intersection' )
    library('IRanges')

    no_cores <- detectCores() - 1
    cluster <- create_cluster(no_cores)
    #
    df_benzo_nested %>%
      inner_join( df_opioid_nested ) %>% 
      partition(pin, cluster=cluster )  %>%
      { . } -> df_both
    #
    cluster_library(df_both, c("tidyverse", "IRanges", "fuzzyjoin"))
    #


    df_both %>%
      do( joined = interval_inner_join( data.frame(.$opioids), 
                                      data.frame(.$benzo), 
                                      by=c('supply_date','end_date'),
                                      minoverlap=1))  %>%
    collect() %>%
    ungroup() %>%
    unnest() %>% 
    mutate( supply_year = year( pmax( supply_date.x, supply_date.y )), 
          diff= pmin( end_date.x, end_date.y ) - 
            pmax( supply_date.x, supply_date.y )+1) %>%
    ungroup() %>%
    { . } -> df_intersect
    toc()

    names(df_intersect) %>%
      sub( '\\.x', '.opioid', . ) %>%
      sub( '\\.y', '.benzo', . ) %>% 
      { . } -> names( df_intersect)
    tic( 'save intersection dataset')
    save(df_intersect, file=save_filename)
    toc()
  }
  toc()
  df_intersect
}

  #cl <- makeCluster("thealfred.duckdns.org", user="dewoller", master="131.172.55.229", homogeneous=FALSE)

