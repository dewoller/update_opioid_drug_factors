safe_load("RPostgreSQL")
safe_load("keyring")

# -------------------------------------------------
get_continuing_df <- function( 
                              base_table="continuing_rr", 
                              benzo=FALSE
                              ) {

  type_code_limit = ifelse( benzo, 10, 9 )
  query  <-  paste0( "
                    SELECT pin, gender, age, state, lga, item_code, type_code, generic_name  ,
                    type_name, supply_date, quantity, unit_wt, ddd_mg_factor 
                    FROM continuing." , base_table , " r 
                    JOIN continuing.item i USING (item_code) 
                    JOIN public.generictype USING (type_code)
                    WHERE (EXTRACT( YEAR FROM supply_date ) != '2017') ",
                    ifelse( benzo, '', " AND (type_code <> 10)") 
                    )

  my_db_get_query( query ) %>%
    as.tibble() %>%
    mutate( n_dose = (unit_wt * quantity / ddd_mg_factor ),
           agen=ifelse( age=='100+', 101, suppressWarnings( as.numeric( age ))),
           age = cut( agen, 
                     c(0,19,44,64,9999), 
                     labels=qw("0-19 20-44 45-64 65+")
                     )
           ) %>%
    select( -unit_wt, -quantity, -ddd_mg_factor, -agen, -item_code) %>% 
    rename(sex=gender) 
}


# -------------------------------------------------
get_drugs <- function( ) {

  query  <-  paste0( "
    SELECT type_code, generic_name, type_name, ddd_mg_factor 
    FROM 
    continuing.item i 
    JOIN public.generictype USING (type_code)"
      )

  my_db_get_query( query ) %>%
    as.tibble() 
}

# -------------------------------------------------
is_benzo = function( type_code ) {
  type_code == 10
}

# -------------------------------------------------
get_usage_df <- function( ) {

  query  <-  paste( "
    SELECT * from continuing.usage 
          "
          , sep = ""
      )
  my_db_get_query( query ) %>%
    # TODO mutate  - map ". " LGA to appropriate state 99 LGA
    as.tibble() %>%
  return( . )
}


# -------------------------------------------------
my_dbWriteTable <- function ( df, table_name ) {
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "mofi",
          host = "localhost", port = 5432,
          user = "dewoller", password = Sys.getenv('PASSWD'))
  on.exit(dbDisconnect(con))
  dbWriteTable( con, table_name, df )
}
# -------------------------------------------------

# -------------------------------------------------
my_db_get_query <- function ( query ) {

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "mofi",
          host = "localhost", port = 5432,
          user = "dewoller", password = Sys.getenv("PASSWD") )
  on.exit(dbDisconnect(con))
  dbGetQuery( con, query )

}
# -------------------------------------------------
# get size dataframe -------------------------------------------------
get_lga_size_df<- function ( state_id ) {
  query = paste(
          "
          select lga, sum(area_albers_sqkm) as size
          from lga_full_mb_2011
          WHERE "
          , get_lga_restriction( state_id )
          , " group by 1"
          , sep = ''
        )
  df_size <- my_db_get_query( query ) %>%
    mutate_at( c(  "lga" ), funs( factor(.) ) ) %>%
    ungroup() %>%
    as.tibble()
  return( df_size )
}



# get Population dataframe -------------------------------------------------
get_population_df<- function ( state_id = 0 ) {
  df_seifa = get_seifa_df( state_id)
  query = paste(
                "WITH base as (
                SELECT supply_year, lga, age, sex, sum(population) as population
                FROM abs_population_asgs_2011 p
                WHERE 
                "
            , get_lga_restriction( state_id )
            ,   " 
                group by 1,2,3,4  
                UNION
                SELECT supply_year, lga, age, sex, population
                FROM abs_population_asgs_2011_projection_2016
                WHERE 
                "
            , get_lga_restriction( state_id ),
                " 
                UNION
                SELECT supply_year, lga, age, sex, sum(population) as population
                FROM abs_population_asgs_2011_projection_2016_57560
                WHERE 
                "
            , get_lga_restriction( state_id )
            ,   " group by 1,2,3,4  
                 UNION
                SELECT supply_year, lga, age, sex, sum(population) as population
                FROM abs_population_asgs_2011_projection_2016_56020 
                WHERE 
                "
            , get_lga_restriction( state_id )
            ,   " group by 1,2,3,4  "
            ,   ") SELECT * 
                FROM base LEFT JOIN lga_full_2011 USING (lga)"
          , sep = ''
        )
  #cat( query )

  df_population <- my_db_get_query( query ) %>%
    left_join( df_seifa, by = "lga") %>%
    mutate( state = get_state_code_from_lga( lga ) ) %>%
    ungroup() %>%
    as.tibble()
  return( df_population )
}

#  -------------------------------------------------
is_valid_supply_year <- function(supply_year) {
  as.character(supply_year) %in% qw("2013 2014 2015 2016")
}

# get lga_restriction -------------------------------------------------
get_lga_restriction <- function ( state_id ) {
  if( 0 %in% state_id ) {
     "TRUE"
  } else {
    if (1 %in% state_id ) {  # add in canberra if NSW
      state_id = c( state_id, 8)
    }
   paste0( '(',  
    paste0( "lga like '", state_id ,"%'", collapse = ' OR ' ),
    ')'
    )
  }
}

# -------------------------------------------------
# -------------------------------------------------

which_urbanisation_single <- function( lga, class_type ) {
  if ( substring( class_type,1,1 ) == "R" ) {
    rv = "Rural"
  } else if ( substring( class_type,1,1 ) == "U" ) {
    rv = "Urban"
  } else if ( substring( lga,1,1 ) == "8" ) {
    rv = "Urban"
  } else if ( substring( class_type,1,1 ) == "N" ) {
    rv = "Rural"
  } else {
    rv = paste("error", lga, class_type )
  }
  rv
}

# -------------------------------------------------
which_urbanisation <- function( lga, class_type ) {
  mapply( which_urbanisation_single, lga, class_type )
}

# get Seifa dataframe -------------------------------------------------
get_seifa_df <- function( state_id ) {
  # missing the #99 values, so need to left join in

  seifa_query   <-   paste( "
                           SELECT l.lga, s.value as score, l.class_type, ct.class_name
                           FROM lga_class_2014  l
                           LEFT JOIN lga_class_type ct USING (class_type)
                           LEFT JOIN seifa_2011 s using (lga)
                           WHERE s.measure_code = 'SCORE'
                           and s.index_code = 'IRSD'
                           and "
                           , get_lga_restriction( state_id )
                           , sep = ""
                           )

  df_seifa <- my_db_get_query( seifa_query ) %>%
    as.tibble() %>%
    rename(irsd_score_raw = score ) %>%
    mutate(
           seifa  = 
             ntile(  irsd_score_raw , 4)  %>%
             ordered(  levels = 1:4, labels = c("Least", "Moderate", "High", "Very High"))
           ) %>%
    mutate( urbanization = which_urbanisation( lga, class_type ))  %>%
    mutate_at( c( "class_type", "class_name", "urbanization"), funs( factor(.) ) ) %>%
    select( lga, irsd_score_raw, seifa, everything())

  #
  return( df_seifa )
}


# -------------------------------------------------

get_state_capital_xlim <- function( state_id ) { 
  get_state_geo_df ( state_id ) %$%
    c(capital_tl_lon, capital_br_lon) %>% sort()
}
# -------------------------------------------------

get_state_capital_ylim <- function( state_id ) { 
  get_state_geo_df ( state_id ) %$%
    c(capital_tl_lat, capital_br_lat) %>% sort()
}

# -------------------------------------------------

get_state_capital_bb <- function( state_id ) { 
  get_state_geo_df ( state_id ) %$%
    matrix(c(capital_tl_lat,capital_tl_lon, capital_br_lat,capital_br_lon), nrow=2, byrow=TRUE)
}

# -------------------------------------------------

get_state_geo_df <- function( state_id ) {

  state_geo_query   <-   paste( "
                               SELECT * FROM state_geo
                               WHERE  state_id = "
                               ,  state_id
                               , sep = ""
                               )

  my_db_get_query( state_geo_query ) %>%
    summarise(
              state_id = first( state_id )
              , capital = first( capital )
              , capital_tl_lat = first( capital_tl_lat )
              , capital_tl_lon = first( capital_tl_lon )
              , capital_br_lat = first( capital_br_lat )
              , capital_br_lon = first( capital_br_lon )
              , state_center_lat = mean( state_center_lat )
              , state_center_lon = mean( state_center_lon )
              , state_tl_lat = max( state_tl_lat )
              , state_tl_lon = max( state_tl_lon )
              , state_br_lat = min( state_br_lat )
              , state_br_lon = min( state_br_lon )
              ) %>%
  mutate_at( c( "state_id", "capital"), funs( factor(.) ) ) %>%
  as.tibble() %>%
  return( . )
}


