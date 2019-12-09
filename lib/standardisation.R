

# default australian population breakdown for standardisation
standardisation_default_population = structure(list(age = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L,
                                                                      4L), .Label = c("0-19", "20-44", "45-64", "65+"), class = c("ordered",
                                                                      "factor")), sex = c("F", "M", "F", "M", "F", "M", "F", "M"),
                                                    population = c(2945332, 3103585, 4268595, 4278986, 3028266,
                                                                   2912534, 1956770, 1716741)), class = c("tbl_df", "tbl", "data.frame"
                                                    ), row.names = c(NA, -8L), .Names = c("age", "sex", "population"
                                                    ))


if (FALSE ) {   # find out which lga has the closest population spread to entire Australia
standardisation_default_population %>%
  mutate( prop = population/sum(population)) %>%
  select( -population ) %>%
  inner_join ( df_population %>% 
              filter( supply_year==2013) %>% 
              group_by( lga, age, sex ) %>% 
              summarise( population = sum(population)  ) %>%
              group_by( lga ) %>%
              mutate( lprop = population / sum(population )) %>%
              select( -population ) 
            , by=qc( age, sex)) %>%
  group_by( lga ) %>%
  summarise( totdiff = sum( ( prop - lprop )^2)) %>%
  arrange( totdiff ) 
 # 33430
}


############################################################################################
# standardisation_test 
############################################################################################

standardisation_test = function() {

  read.csv('data/standardisation.csv') %>%
  as.tibble() %>%
  mutate( pct = rate/pop) -> df_test

  #df_test %>%
  #  group_by( Age, cat) %>%
  #  summarise( population = sum(pop)) -> df_population_test


  df_test %>%
    inner_join( df_population_test,  by = c("Age", "cat")) %>%
    mutate( pop_rate = pct * population ) %>%
    ungroup() %>%
    group_by( bp ) %>%
    summarise( tot_rate = sum( pop_rate ), 
              gtot_pop = sum( population), 
              gpct = tot_rate / gtot_pop )

#df_test %>% filter( row_number() < 5 ) %>% complete( Age, bp, cat)

  df_test %>%
    group_by( Age, cat, bp ) %>%
    summarise( population = sum(pop)) -> df_population_test

}


############################################################################################
# get_population_grouped 
############################################################################################
get_population_grouped = function ( pop, rollup_level = NA) {
  if (typeof(rollup_level ) == "logical" && is.na( rollup_level )) {
    pop %>% 
      ungroup() %>%
      dplyr::summarize( population=sum(population)  )
  } else {
    pop %>% 
      ungroup() %>%
      group_by_( .dots=rollup_level ) %>%
      dplyr::summarize( population=sum(population)  )
  }
}

############################################################################################
# get_population_grouped_test
############################################################################################
get_pop_grouped_test = function() {
  get_population_grouped(df_population_test ) 
  get_population_grouped(df_population_test, 'cat')
  get_population_grouped(df_population_test, 'Age')
  get_population_grouped(df_population_test, qw('Age cat'))
  get_population_grouped(df_population_test, qw('bp Age cat'))

  get_population_grouped(df_population, qw('lga seifa'))
  get_population_grouped(df_population, qw('lga supply_year'))
}

############################################################################################
# f_join_population_test_dataset 
############################################################################################
f_join_population_test_dataset = function( dataset, 
                             rollup_level = NA,  
                             join_key = rollup_level, 
                             df_population. = df_population_test )  {
  f_join_population( dataset, rollup_level, join_key, df_population. )
}

############################################################################################
# f_join_population 
############################################################################################

f_join_population = function( dataset, 
                    rollup_level = NA,  
                    join_key = rollup_level, 
                    df_population. = df_population )  {

  pop_names = names( df_population. )
  # can only join and group on what we have available
  available_rollup_vars = intersect( pop_names, rollup_level)
  join_key = intersect(pop_names, join_key )

  if ( length( available_rollup_vars ) == 0 )  {

    # fallback to total database population if no variables to join with
    dataset %>% 
      mutate(temp_var=1) %>% 
      inner_join( get_population_grouped( df_population. ) %>% mutate(temp_var=1)
                  ,by="temp") %>%
      dplyr::select(-temp_var)  %>% 
      { . } -> rv

  } else {

    # every item in dataset should endup with a population
    dataset %>%
      left_join( get_population_grouped(  df_population., available_rollup_vars ),  by=join_key) %>% 
      { . } -> rv

  }
  assertthat::assert_that( sum( is.na( rv$population)) == 0, 
                          msg=paste( "All items in the input dataset must have a population, missing = ", 
                                    filter( rv, is.na(population)) %>% 
                                      distinct(lga) %$% 
                                      lga , 
                                    collapse=',' 
                                    )
                          )
  rv
}


############################################################################################
# f_join_population_test
############################################################################################

f_join_population_test = function( ) {

  df_patient %>%
    filter( lga=='10050' ) %>%
    f_join_population( )

  df_patient %>%
    filter( lga=='10050' ) %>%
    f_join_population( rollup_level='lga' )

  df_patient %>%
    distinct( lga ) %>%
    f_join_population( rollup_level=qw('lga seifa'), join_key='lga' )

  df_patient %>%
    distinct( lga ) %>%
    f_join_population( rollup_level=qw(' c("lga", "usage_category", "supply_year", "age", "sex") ') )

}



############################################################################################
# standardise_test 
############################################################################################

standardise_test = function() {

df_test %>%
  select( Age, bp, cat, rate) %>%
  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat', 'bp') ) %>%
  mutate( ratio = rate / population ) %>%
  select( Age, bp, cat, rate, ratio) %>%
  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat') ) %>%
  mutate( tot_ratio = ratio * population ) %>%
  group_by_(.dots= c('bp' ) )  %>%
  summarise( tot_ratio = sum(tot_ratio)) %>%
  f_join_population_test_dataset( ) %>%
  mutate( ratio = tot_ratio / population ) %>%
  select( bp, ratio )



df_test %>% group_by( bp ) %>% summarise( sum( pop))

  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat', 'bp') ) %>%
  mutate( ratio = rate / population ) %>%
  select( Age, bp, cat, rate, ratio) %>%
  f_join_population_test_dataset( rollup_level=c( 'Age', 'cat') ) %>%
  mutate( tot_ratio = ratio * population ) %>%
  group_by_(.dots= c('bp' ) )  %>%
  summarise( tot_ratio = sum(tot_ratio)) %>%
  f_join_population_test_dataset( ) %>%
  mutate( ratio = tot_ratio / population ) %>%
  select( bp, ratio )

}


#debug(simple_standardise_value )
############################################################################################
# simple_standardise_value 
############################################################################################
simple_standardise_value = function( df_standardise, 
                                    group_by_vars = 'lga', 
                                    count_var = 'n',
                                    df_population.=df_population, 
                                    standardisation_default = standardisation_default_population 
                                    ) {

  # 1) sum count_var in df based on c( standardise_using + standardise_across)
  # 2) join summed population on the SAME variables, to get df_test rate for each count
  # 3) join summed population on standardise_using, to multiply rate by population total
  # arriving at population_overall_rate
  # 4) total the population_overall_rate by standardise_across to get grand_total_rate
  # 4) join total population, divide grand_total by total_population

    standardise_using <- qw("age sex")  %>%   
      setdiff( group_by_vars ) 

    standardisation_default %>%
      group_by_( .dots=standardise_using ) %>%
      summarise( population = sum( population )) %>% 
      { . } -> this_standardisation_default 



    #debug_pipe() %>%

    df_standardise %>%  # group by everything we want to group by to get base level sum of count variable
      ungroup() %>%
      group_by_( .dots=c( group_by_vars, standardise_using)) %>%
      mutate_( n=count_var ) %>% # standardise variable name
      dplyr::summarise( n = sum( n ) ) %>%
      ungroup() %>%
      #
      # calculate rate for this count
      # join up with population on the maximal set of the same variables
      f_join_population ( rollup_level = c( group_by_vars, standardise_using), df_population. = df_population. ) %>%
      group_by_( .dots=c( group_by_vars, standardise_using )) %>%
      #
      # calculate rate / 1000 pp at this level
      dplyr::mutate( rate = n / population * 1000 ) %>%
      #
      # now that we have the total count and population for each subgroup,
      # now we standardise that count based on the total population
      # first throw away extraneous variables brought in by previous join, 
      # all we want is n,and the key variables
      select_( .dots = c(group_by_vars, standardise_using, "rate") ) %>%
      #
      # get the population grouped by levels we want to standardise on, that is, 
      # the standardisation variables (possibly age and/or sex, as long as they are not
      inner_join( this_standardisation_default, by = c(standardise_using ) ) %>%
      mutate( rate_standardized = rate * population ) %>%  
      #
      # now, sum up our standardized_proportion, 
      # grouped on age and/or sex as long as we ae not using it elsewhere
      group_by_( .dots=c( group_by_vars )) %>%  # no standardize_vars here
      dplyr::summarise( rate_standardized = sum(rate_standardized )) %>%
      select_( .dots = c( group_by_vars, "rate_standardized") ) %>% # and cleanup
      #
      # divide by the total population, and standardisation finished
      mutate( rate = rate_standardized /  sum(this_standardisation_default$population) ) %>%  
      #
      # finished standardisation, cleanup by selecting only variables of interest
      select_( .dots = c(group_by_vars, "rate") ) %>%
      ungroup()
}


############################################################################################
# simple_standardise_value_test_1 
############################################################################################
simple_standardise_value_test_1 = function() {

  df_standardise=
  structure(list(supply_year = c("2013", "2013", "2013", "2013",
  "2013", "2013"), lga = c("10050", "10050", "10050", "10050",
  "10050", "10050"), age = structure(c(2L, 2L, 3L, 3L, 4L, 4L), .Label = c("0-19",
  "20-44", "45-64", "65+"), class = "factor"), sex = structure(c(1L,
  2L, 1L, 2L, 1L, 2L), .Label = c("F", "M"), class = "factor"),
  n = c(2L, 2L, 12L, 6L, 15L, 4L)), class = c("tbl_df", "tbl",
  "data.frame"), row.names = c(NA, -6L), .Names = c("supply_year",
  "lga", "age", "sex", "n"))


  df_population = 
  structure(list(lga = c("10050", "10050", "10050", "10050", "10050",
  "10050", "10050", "10050"), sex = c("F", "F", "M", "F", "M",
  "F", "M", "M"), age = c("20-44", "0-19", "0-19", "45-64", "65+",
  "65+", "45-64", "20-44"), population = c(8498, 6525, 6555, 6324,
  3567, 4483, 6036, 8232), supply_year = c("2013", "2013", "2013",
  "2013", "2013", "2013", "2013", "2013")), row.names = c(NA, -8L
  ), class = c("tbl_df", "tbl", "data.frame"), .Names = c("lga",
  "sex", "age", "population", "supply_year"))

  expected_results=
  structure(list(lga = "10050", supply_year = "2013", rate = 0.791301362438), class = c("tbl_df",
  "tbl", "data.frame"), row.names = c(NA, -1L), .Names = c("lga",
  "supply_year", "rate"))


  simple_standardise_value( df_standardise, group_by_vars=qc(lga, supply_year) ,  count_var='n' )  %>% 
  { . } -> rv

  assertthat::assert_that( 
              rv$supply_year == expected_results$supply_year,
              rv$lga == expected_results$lga,
              round( rv$rate, 5) == round( expected_results$rate,5)
              )



}



############################################################################################
# simple_standardise_value_test
############################################################################################
simple_standardise_value_test = function() {

  df_test %>% simple_standardise_value( standardise_using=qw('Age cat'), 
                                      standardise_across=c('bp'), 
                                      count_var=.$rate, 
                                      f_join_population_test )

  df_patient_dose %>%
    inner_join( df_patient_usage ) %>%
    inner_join( df_patient ) %>%
    ungroup() %>%
    select( lga, usage_category, supply_year, age, sex, n_dose) %>% 
    simple_standardise_value(standardise_using=qw('age sex'), 
                            standardise_across=qw("supply_year lga usage_category"), 
                            .$n_dose,
                            f_join_population)  
}

############################################################################################
# framework
############################################################################################
function () {

  #undebug( f_join_population )

  standardise_over=qw('sex')

  join_with=qw('supply_year')

}


############################################################################################
# select_and_standardise_ddd 
############################################################################################
#  -------------------------------------------------
select_and_standardise_ddd <- function( df, 
                                       standardise_over, 
                                       join_with = c() , 
                                       df_population=df_population, 
                                       standardisation_default = standardisation_default_population 
                                       ) {
  # select out standardise_over and join_with from df,
  # standardising value based on standardise_using, generally sex and age

  # expects a df containing standardise_over, join_with variables
  # need to standardise the standardise_over
  # returns df containing proportion, standardised based on population figures from 
  # join_population_grouped, along with standardise_over and join_with
  # eg select_and_standardise_variable( standardise_over=qw( "seifa" ), join_with="supply_year", value=.$n_user )
  # join_with are other variables that join with population, eg supply_year, sex, age, 
  # what 



  standardise_using <- qw("age sex")  %>%   
    setdiff( standardise_over ) %>% 
      setdiff( join_with )

  standardisation_default %>%
    group_by_( .dots=standardise_using ) %>%
    summarise( population = sum( population )) %>% 
    { . } -> this_standardisation_default 

    df %>%  # group by everything we want to group by to get base level number of doses
      ungroup() %>%
        group_by_( .dots=c( standardise_over, join_with, standardise_using, 'supply_year' )) %>%
        dplyr::summarise( n_dose = sum( n_dose ) ) %>%
        ungroup() %>%
        # join up with population on the maximal set of the same variables
        f_join_population ( c( standardise_over, join_with, standardise_using, 'supply_year' )) %>%
        group_by_( .dots=c( standardise_over, join_with, standardise_using )) %>%
        # calculate proportion at this level
        dplyr::summarise( proportion = sum((n_dose * 1000 * 10)) / sum(population * my_year_length( supply_year ) )) %>%  

        # now that we have the proportion for each subgroup,
        # now we standardise that proportion based on the total population
        # first throw away extraneous variables brought in by previous join, 
        # eg population and person_days
        # all we want is the proportion,and the key variables

        select_( .dots = c(standardise_over, join_with, standardise_using, "proportion") ) %>%

        # get the population grouped by levels we want to standardise on, that is, 
        # the standardisation variables (possibly age and/or sex, as long as they are not
        # included in the non-standard_vars), and join_with, eg. supply_year
        inner_join( this_standardisation_default, by = c(standardise_using ) ) %>%
        mutate( proportion_standardized = proportion * population ) %>%  

        # now, sum up our standardized_proportion, 
        # grouped on age and/or sex as long as we ae not using it elsewhere
        group_by_( .dots=c( standardise_over, join_with )) %>%  # no standardize_vars here
        dplyr::summarise( proportion_standardized = sum(proportion_standardized )) %>%
        select_( .dots = c( standardise_over, join_with, "proportion_standardized") ) %>% # and cleanup
        # divide by the total population, and standardisation finished
        mutate( ddd = proportion_standardized /  sum(this_standardisation_default$population) ) %>%  
        # finished standardisation, cleanup by selecting only variables of interest
        select_( .dots = c(standardise_over, join_with, "ddd") ) %>%
        ungroup()
}

############################################################################################
# select_and_standardise_ddd_test 
############################################################################################
select_and_standardise_ddd_test = function() {

  df_patient_dose %>%
    inner_join( df_patient_usage ) %>%
    inner_join( df_patient ) %>%
    ungroup() %>%
    select_and_standardise_ddd(standardise_over=qw('lga usage_category'), 
                               join_with= c('supply_year'), 
                               f_join_population
                               )  



  df_patient_dose %>%
    inner_join( df_patient_usage ) %>%
    inner_join( df_patient ) %>%
    ungroup() %>%
    select( lga, usage_category, supply_year, age, sex, n_dose) %>% 
    f_join_population( rollup_level=c( 'age', 'sex', 'lga', 'supply_year') , df_population.=df_population) %>%
    mutate( ratio = (n_dose * 1000 * 10) / (population * my_year_length( supply_year) )) %>%
    select( -population ) %>%
    f_join_population( rollup_level=c( 'age', 'sex', 'supply_year') , df_population.=df_population) %>%
    mutate( tot_ratio = ratio * population ) %>%
    group_by_(.dots= qw('lga usage_category supply_year' ) )  %>%
    summarise( tot_ratio = sum(tot_ratio)) %>%
    f_join_population( rollup_level=c( 'supply_year') , df_population.=df_population) %>%
    mutate( ratio = tot_ratio / population ) %>%
    select( -population, -tot_ratio )

}

