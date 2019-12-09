safe_load("tmap", 'https://cloud.r-project.org')
safe_load("tmaptools", 'https://cloud.r-project.org')
safe_load("grid")
#safe_load("ggtern")
#safe_load("tricolore") 
safe_load("mapview") 


library(sp)
library(spbabel)




# display a map for each lga in lga_categories, displaying only a colors corresponding to the display_factors
# show the category_name

#display_factors=c("Moderate", "High", "Very High")
#display_colors=c("green", "orange", "red")
#category_column = "lga_colorisation"
#df_lga_category=lga_colors

get_australia_state_map <- function( df_lga_category , 
    state_id = 1 , 
    category_column = "lga_colorisation" , 
    display_factors = levels( df_lga_category[, category_column ]), # default to labelling everything
    category_name  = category_column , 
    display_colors= brewer.pal( length( display_factors ), "RdBu")[1:length(display_factors)],
    label_factors = display_factors, # which factors to default to labelling everything that we display 
    label_lga = df_lga_category$lga  # which LGAS to display,  default to labelling everything that we display
    )
{

  df_lga_category %<>% 
    mutate( lga_colorisation = get(category_column ) ) 
    df_lga_category %>% 
    filter(!lga_colorisation %in% display_factors) %>% 
    select(category_column) %>% 
    unique() %>% 
    count()  %>%
    {.} -> nwhite
  actual_colors=c( rep( "white", nwhite ), display_colors )
    df_lga_category %>% 
    filter(lga_colorisation %in% label_factors) %>%
    filter( lga %in% label_lga )  %>%
    select(lga) %>%
    {.} -> lga_to_label 

  area_extents <- get_state_geo_df(state_id)
    area <- readShapePoly("~/mydoc/research/mofi/shapefiles/LGA11aAust.shp") 
    tidy( area, region="LGA_CODE11") %>% 
    as.tibble() %>%
    rename( lga = id ) %>%
    mutate( lga=factor(lga)) %>%
    left_join( area@data,  by=c("lga"="LGA_CODE11")) %>%
    inner_join( df_lga_category, by="lga") %>%
    mutate( lga=factor(lga)) %>%
    {.} -> area.t

  area.t %>% 
    rename( lga_name = LGA_NAME11) %>%
    select(lga, lga_name, lga_colorisation ) %>%
    unique() %>%
    mutate( lga_name = 
        lga_name %>% 
        as.character() %>%
        substr(1, str_length(lga_name)-4) %>%
        tools::toTitleCase( ) %>%
        gsub("  *","\n", .)
        )  %>%
    mutate(lga=as.character(lga)) %>%
    {.} -> lga_names

  area.t %>% 
    group_by(lga ) %>%
    summarize ( lat=mean(range(lat)), lon=mean(range(long))) %>%
    mutate(lga=as.character(lga)) %>%
    select(lga, lat, lon) %>%
    {.} -> lga_center

  lga_names %>%
    inner_join( lga_center, by="lga") %>%
    inner_join( lga_to_label, by="lga" ) %>%
    filter( is_geographic_LGA( lga )) %>%
    {.} ->  lga_names_to_display 
#	


  ggplot() +
    geom_polygon(aes_string(x = "long",
          y = "lat",
          group = "group",
          fill=category_column,
          color=category_column
          ),
        color="black",
        data = area.t
        ) +
## Configure the colors, transparency and panel
#scale_alpha(range = c(.25, .55)) +
    coord_quickmap( 
        xlim=c( area_extents$state_tl_lon + .1, area_extents$state_br_lon -.1 ) 
        , ylim=c( area_extents$state_tl_lat + .1, area_extents$state_br_lat - .1 ) 
        ) + 
    theme_map() + 
    scale_fill_manual(breaks=display_factors, values= actual_colors , name = category_name )  %>%
    {.} -> map
  list( map=map
      , repel_names = geom_label_repel(data=lga_names_to_display , aes(lon, lat, label = lga_name, fill=lga_colorisation), color="black", size=3, fontface='bold')
      , regular_names = geom_label(data=lga_names_to_display , aes(lon, lat, label = lga_name, fill=lga_colorisation), color="black", size=3, fontface='bold')
      )
}


# ------------------------------------------------------------------
get_australia_base_map = function(states=c(1:8)) {
  if (states==0) states = c(1:8)
  
  read_shape("~/mydoc/research/mofi/shapefiles/LGA11aAust.shp")  %>%
    subset( STATE_CODE %in% states )  %>% 
    simplify_shape(0.05)
}



# ------------------------------------------------------------------
get_australia_states_map = function(states=c(1:8)) {
  if (states==0) states = c(1:8)
  read_shape("~/mydoc/research/mofi/shapefiles/aust_cd66states.shp", current.projection='WGS84')  %>% 
    subset( STE %in% states ) 
  }


# ------------------------------------------------------------------
#unused?
getBoundary <- function( state_id ) {

  area.t %>% 
    filter(substr(lga,1,1)== state_id) %>%
    summarize( 
        center_long = min(long) + (max(long) - min(long))/2
        ,center_lat = min(lat) + (max(lat) - min(lat))/2
        ,max_lat = max(lat) 
        ,min_lat = min(lat) 
        ,max_long = max(long) 
        ,min_long = min(long) 
        ) %>%
    {.} -> area_extents
  return(area_extents)
}

# ------------------------------------------------------------------
box_around_capital <- function(state_id, lat_offset = .1, lon_offset) {
#
  get_state_geo_df( state_id ) %>%
    select( starts_with("capital_"))  %$%
    data.frame( 
        lat=c(capital_tl_lat + lat_offset,capital_tl_lat + lat_offset,capital_br_lat - lat_offset,capital_br_lat - lat_offset,capital_tl_lat + lat_offset)
        , lon=c(capital_tl_lon - lon_offset,capital_br_lon + lon_offset,capital_br_lon + lon_offset,capital_tl_lon - lon_offset, capital_tl_lon - lon_offset)) %>%
    geom_path(data=.,  aes(x = lon, y = lat), size = 0.7, color="red") 
}
# ------------------------------------------------------------------
box_around_annotation = function( xmax_r_lon ,xmin_l_lon ,ymax_t_lat  ,ymin_b_lat, lat_offset, lon_offset )  {
#
  data.frame( 
      lat=c(ymax_t_lat + lat_offset,ymax_t_lat + lat_offset,ymin_b_lat - lat_offset,ymin_b_lat - lat_offset,ymax_t_lat + lat_offset)
      , lon=c(xmin_l_lon - lon_offset,xmax_r_lon + lon_offset,xmax_r_lon + lon_offset,xmin_l_lon - lon_offset, xmin_l_lon - lon_offset)) %>%
    geom_path(data=.,  aes(x = lon, y = lat), size = 0.7, color="red") 
}

# ------------------------------------------------------------------
get_inset_capital_map <- function( map_list, state_id, inset_location="br" ) {

  ratio = 3 # inset map should be 1:ratio the size of big map

    geo =get_state_geo_df (state_id) 
    box_size_lon = abs(geo$state_br_lon - geo$state_tl_lon)/ ratio
    box_size_lat = abs(geo$state_br_lat - geo$state_tl_lat)/ ratio

# multiply latitude by this ratio to get even longitude
    lat2lon_ratio = (geo$capital_tl_lat - geo$capital_br_lat) / (geo$capital_br_lon-geo$capital_tl_lon)
    lat_offset= .0001
    lon_offset = lat_offset / lat2lon_ratio

    if (inset_location == "br") {
      xmin_l_lon = geo$state_br_lon-box_size_lon + 1
        xmax_r_lon = geo$state_br_lon + 1
        ymin_b_lat = geo$state_br_lat
        ymax_t_lat = geo$state_br_lat+box_size_lat
        arrow=data.frame(
            arrow_from_lon = xmin_l_lon
            , arrow_from_lat = ymax_t_lat 
            , arrow_to_lon = geo$capital_br_lon
            , arrow_to_lat = geo$capital_br_lat
            )
    } else if (inset_location == "tc") {  # broken
      cl_lon = mean(geo$state_tl_lon, geo$state_br_lon) + box_size_lon/2
        xmin_l_lon = cl_lon 
        xmax_r_lon = cl_lon + box_size_lon
        ymin_b_lat = geo$state_tl_lat 
        ymax_t_lat = geo$state_tl_lat + box_size_lat
        arrow=data.frame(
            arrow_from_lon = mean(xmin_l_lon, xmax_r_lon) 
            , arrow_from_lat = ymin_b_lat 
            , arrow_to_lon = mean( geo$capital_br_lon, geo$capital_tl_lon)
            , arrow_to_lat = geo$capital_tl_lat
            )
    } else if (inset_location == "bc") {  # broken
      cl_lon = mean(geo$state_tl_lon, geo$state_br_lon) + box_size_lon/2
        xmin_l_lon = cl_lon 
        xmax_r_lon = cl_lon + box_size_lon
        ymin_b_lat = geo$state_br_lat - box_size_lat
        ymax_t_lat = geo$state_br_lat 
        arrow=data.frame(
            arrow_from_lon = mean(xmin_l_lon, xmax_r_lon) 
            , arrow_from_lat = ymax_t_lat 
            , arrow_to_lon = mean( geo$capital_br_lon, geo$capital_tl_lon)
            , arrow_to_lat = geo$capital_br_lat
            )
    } else if (inset_location == "tr") {  
      xmin_l_lon = geo$state_br_lon - box_size_lon
        xmax_r_lon = geo$state_br_lon
        ymin_b_lat = geo$state_tl_lat - box_size_lat
        ymax_t_lat = geo$state_tl_lat
        arrow=data.frame(
            arrow_from_lon = xmin_l_lon 
            , arrow_from_lat = ymin_b_lat
            , arrow_to_lon = geo$capital_br_lon
            , arrow_to_lat = geo$capital_tl_lat
            )
    }


  map_list [[ 'map' ]] +
    map_list [[ 'repel_names' ]] +
    annotation_custom(grob = get_australia_capital_map( map_list [[ 'map' ]] + map_list [[ 'regular_names' ]] 
          , state_id) %>% ggplotGrob()
        , xmax = xmax_r_lon
        , xmin = xmin_l_lon
        , ymax = ymax_t_lat  
        , ymin = ymin_b_lat
        )  +
    expand_limits( x=xmax_r_lon ) +
    expand_limits( x=xmin_l_lon ) +
    expand_limits( y=ymin_b_lat ) +
    expand_limits( y=ymax_t_lat ) +
    box_around_annotation( xmax_r_lon ,xmin_l_lon ,ymax_t_lat  ,ymin_b_lat, lat_offset/ratio, lon_offset/ratio ) +
    box_around_capital( state_id , lat_offset, lon_offset ) +
    geom_curve(data=arrow, 
        aes(x=arrow_from_lon,y=arrow_from_lat,xend=arrow_to_lon,yend=arrow_to_lat),
        curvature = 0.1, 
        arrow = arrow(type="closed",length = unit(0.25,"cm")), color="red") 
}

# get Australia Capital Map ------------------------------------------------------------------

get_australia_capital_map <- function( map, state_id ) {
  capital <- get_state_geo_df( state_id ) %>%
    select( starts_with("capital_"))
    getZoomedMap( map, 
        edge_lat <- capital %>% 
        select( ends_with("_lat")) %>%
        unlist(),
        edge_lon <- capital %>% 
        select( ends_with("_lon")) %>%
        unlist()
        ) 
}
# get Zoomed ------------------------------------------------------------------


getZoomedMap <- function( map, edge_lat, edge_lon ) {
  map + 
    coord_quickmap( xlim=edge_lon, ylim=edge_lat) +
#theme(panel.background = element_rect(fill = NULL)) +
    theme(legend.position = "none") +
    ggtitle("")
}

# theme_map------------------------------------------------------------------


theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        plot.background = element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0, 0))
}

# ------------------------------------------------------------------
testGetSeifaColors <- function (state_id=1) {
# get me the LGA Seifa's that are in the sampledata
  dbGetQuery(con, paste(
        "
        SELECT DISTINCT lga, lga_name, value AS score
        FROM lga_full_2011 l
        JOIN seifa_2011 a USING (lga)
        WHERE a.measure_code='SCORE'
        AND a.index_code='IRSAD' 
        AND " , get_lga_restriction( state_id ) 
        )
      ) %>% 
    mutate( lga_colorisation = ntile( score, 4)  %>%
        ordered(  levels=1:4, labels=c("Least", "Moderate", "High", "Very High")) ) %>%
    select(lga, lga_colorisation) 
}

# ------------------------------------------------------------------
# testGetMap ------------------------------------------------------------------


# ------------------------------------------------------------------
testGetMap  <- function( state_id=1 ) {
  testGetSeifaColors( state_id) %>%
    get_australia_state_map(  )
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

#### unused?


# display a map for each lga in lga_categories, displaying only a colors corresponding to the display_factors
# show the category_name

#display_factors=c("Moderate", "High", "Very High")
#display_colors=c("green", "orange", "red")
#category_column = "lga_colorisation"
#df_lga_category=lga_colors

get_australia_state_map <- function( df_lga_category , 
                                    state_id = 1 , 
                                    category_column = "lga_colorisation" , 
                                    display_factors = levels( df_lga_category[, category_column ]), # default to labelling everything
                                    category_name  = category_column , 
                                    display_colors= brewer.pal( length( display_factors ), "RdBu")[1:length(display_factors)],
                                    label_factors = display_factors, # which factors to default to labelling everything that we display 
                                    label_lga = df_lga_category$lga  # which LGAS to display,  default to labelling everything that we display
                                    )
{

  df_lga_category %<>% 
    mutate( lga_colorisation = get(category_column ) ) 
  df_lga_category %>% 
    filter(!lga_colorisation %in% display_factors) %>% 
    select(category_column) %>% 
    unique() %>% 
    count()  %>%
    {.} -> nwhite
  actual_colors=c( rep( "white", nwhite ), display_colors )
  df_lga_category %>% 
    filter(lga_colorisation %in% label_factors) %>%
    filter( lga %in% label_lga )  %>%
    select(lga) %>%
    {.} -> lga_to_label 

  area_extents <- get_state_geo_df(state_id)
  area <- readShapePoly("~/mydoc/research/mofi/shapefiles/LGA11aAust.shp") 
  tidy( area, region="LGA_CODE11") %>% 
    as.tibble() %>%
    rename( lga = id ) %>%
    mutate( lga=factor(lga)) %>%
    left_join( area@data,  by=c("lga"="LGA_CODE11")) %>%
    inner_join( df_lga_category, by="lga") %>%
    mutate( lga=factor(lga)) %>%
    {.} -> area.t

  area.t %>% 
    rename( lga_name = LGA_NAME11) %>%
    select(lga, lga_name, lga_colorisation ) %>%
    unique() %>%
    mutate( lga_name = 
           lga_name %>% 
           as.character() %>%
           substr(1, str_length(lga_name)-4) %>%
           tools::toTitleCase( ) %>%
           gsub("  *","\n", .)
         )  %>%
    mutate(lga=as.character(lga)) %>%
    {.} -> lga_names

  area.t %>% 
    group_by(lga ) %>%
    summarize ( lat=mean(range(lat)), lon=mean(range(long))) %>%
    mutate(lga=as.character(lga)) %>%
    select(lga, lat, lon) %>%
    {.} -> lga_center

  lga_names %>%
    inner_join( lga_center, by="lga") %>%
    inner_join( lga_to_label, by="lga" ) %>%
    filter( is_geographic_LGA( lga )) %>%
    {.} ->  lga_names_to_display 
  #	


  ggplot() +
    geom_polygon(aes_string(x = "long",
                            y = "lat",
                            group = "group",
                            fill=category_column,
                            color=category_column
                            ),
                 color="black",
                 data = area.t
                 ) +
## Configure the colors, transparency and panel
#scale_alpha(range = c(.25, .55)) +
coord_quickmap( 
               xlim=c( area_extents$state_tl_lon + .1, area_extents$state_br_lon -.1 ) 
               , ylim=c( area_extents$state_tl_lat + .1, area_extents$state_br_lat - .1 ) 
               ) + 
theme_map() + 
scale_fill_manual(breaks=display_factors, values= actual_colors , name = category_name )  %>%
{.} -> map
    list( map=map
         , repel_names = geom_label_repel(data=lga_names_to_display , aes(lon, lat, label = lga_name, fill=lga_colorisation), color="black", size=3, fontface='bold')
         , regular_names = geom_label(data=lga_names_to_display , aes(lon, lat, label = lga_name, fill=lga_colorisation), color="black", size=3, fontface='bold')
         )
}



# ------------------------------------------------------------------
#unused?
getBoundary <- function( state_id ) {

  area.t %>% 
    filter(substr(lga,1,1)== state_id) %>%
    summarize( 
              center_long = min(long) + (max(long) - min(long))/2
              ,center_lat = min(lat) + (max(lat) - min(lat))/2
              ,max_lat = max(lat) 
              ,min_lat = min(lat) 
              ,max_long = max(long) 
              ,min_long = min(long) 
              ) %>%
    {.} -> area_extents
  return(area_extents)
}

# ------------------------------------------------------------------
box_around_capital <- function(state_id, lat_offset = .1, lon_offset) {
  #
  get_state_geo_df( state_id ) %>%
    select( starts_with("capital_"))  %$%
    data.frame( 
               lat=c(capital_tl_lat + lat_offset,capital_tl_lat + lat_offset,capital_br_lat - lat_offset,capital_br_lat - lat_offset,capital_tl_lat + lat_offset)
               , lon=c(capital_tl_lon - lon_offset,capital_br_lon + lon_offset,capital_br_lon + lon_offset,capital_tl_lon - lon_offset, capital_tl_lon - lon_offset)) %>%
    geom_path(data=.,  aes(x = lon, y = lat), size = 0.7, color="red") 
}
# ------------------------------------------------------------------
box_around_annotation = function( xmax_r_lon ,xmin_l_lon ,ymax_t_lat  ,ymin_b_lat, lat_offset, lon_offset )  {
  #
  data.frame( 
             lat=c(ymax_t_lat + lat_offset,ymax_t_lat + lat_offset,ymin_b_lat - lat_offset,ymin_b_lat - lat_offset,ymax_t_lat + lat_offset)
             , lon=c(xmin_l_lon - lon_offset,xmax_r_lon + lon_offset,xmax_r_lon + lon_offset,xmin_l_lon - lon_offset, xmin_l_lon - lon_offset)) %>%
  geom_path(data=.,  aes(x = lon, y = lat), size = 0.7, color="red") 
}

# ------------------------------------------------------------------
get_inset_capital_map <- function( map_list, state_id, inset_location="br" ) {

  ratio = 3 # inset map should be 1:ratio the size of big map

  geo =get_state_geo_df (state_id) 
  box_size_lon = abs(geo$state_br_lon - geo$state_tl_lon)/ ratio
  box_size_lat = abs(geo$state_br_lat - geo$state_tl_lat)/ ratio

  # multiply latitude by this ratio to get even longitude
  lat2lon_ratio = (geo$capital_tl_lat - geo$capital_br_lat) / (geo$capital_br_lon-geo$capital_tl_lon)
  lat_offset= .0001
  lon_offset = lat_offset / lat2lon_ratio

  if (inset_location == "br") {
    xmin_l_lon = geo$state_br_lon-box_size_lon + 1
    xmax_r_lon = geo$state_br_lon + 1
    ymin_b_lat = geo$state_br_lat
    ymax_t_lat = geo$state_br_lat+box_size_lat
    arrow=data.frame(
                     arrow_from_lon = xmin_l_lon
                     , arrow_from_lat = ymax_t_lat 
                     , arrow_to_lon = geo$capital_br_lon
                     , arrow_to_lat = geo$capital_br_lat
                     )
  } else if (inset_location == "tc") {  # broken
    cl_lon = mean(geo$state_tl_lon, geo$state_br_lon) + box_size_lon/2
    xmin_l_lon = cl_lon 
    xmax_r_lon = cl_lon + box_size_lon
    ymin_b_lat = geo$state_tl_lat 
    ymax_t_lat = geo$state_tl_lat + box_size_lat
    arrow=data.frame(
                     arrow_from_lon = mean(xmin_l_lon, xmax_r_lon) 
                     , arrow_from_lat = ymin_b_lat 
                     , arrow_to_lon = mean( geo$capital_br_lon, geo$capital_tl_lon)
                     , arrow_to_lat = geo$capital_tl_lat
                     )
  } else if (inset_location == "bc") {  # broken
    cl_lon = mean(geo$state_tl_lon, geo$state_br_lon) + box_size_lon/2
    xmin_l_lon = cl_lon 
    xmax_r_lon = cl_lon + box_size_lon
    ymin_b_lat = geo$state_br_lat - box_size_lat
    ymax_t_lat = geo$state_br_lat 
    arrow=data.frame(
                     arrow_from_lon = mean(xmin_l_lon, xmax_r_lon) 
                     , arrow_from_lat = ymax_t_lat 
                     , arrow_to_lon = mean( geo$capital_br_lon, geo$capital_tl_lon)
                     , arrow_to_lat = geo$capital_br_lat
                     )
  } else if (inset_location == "tr") {  
    xmin_l_lon = geo$state_br_lon - box_size_lon
    xmax_r_lon = geo$state_br_lon
    ymin_b_lat = geo$state_tl_lat - box_size_lat
    ymax_t_lat = geo$state_tl_lat
    arrow=data.frame(
                     arrow_from_lon = xmin_l_lon 
                     , arrow_from_lat = ymin_b_lat
                     , arrow_to_lon = geo$capital_br_lon
                     , arrow_to_lat = geo$capital_tl_lat
                     )
  }


  map_list [[ 'map' ]] +
    map_list [[ 'repel_names' ]] +
    annotation_custom(grob = get_australia_capital_map( map_list [[ 'map' ]] + map_list [[ 'regular_names' ]] 
                                                       , state_id) %>% ggplotGrob()
    , xmax = xmax_r_lon
    , xmin = xmin_l_lon
    , ymax = ymax_t_lat  
    , ymin = ymin_b_lat
    )  +
expand_limits( x=xmax_r_lon ) +
expand_limits( x=xmin_l_lon ) +
expand_limits( y=ymin_b_lat ) +
expand_limits( y=ymax_t_lat ) +
box_around_annotation( xmax_r_lon ,xmin_l_lon ,ymax_t_lat  ,ymin_b_lat, lat_offset/ratio, lon_offset/ratio ) +
box_around_capital( state_id , lat_offset, lon_offset ) +
geom_curve(data=arrow, 
           aes(x=arrow_from_lon,y=arrow_from_lat,xend=arrow_to_lon,yend=arrow_to_lat),
           curvature = 0.1, 
           arrow = arrow(type="closed",length = unit(0.25,"cm")), color="red") 
}

# get Australia Capital Map ------------------------------------------------------------------

get_australia_capital_map <- function( map, state_id ) {
  capital <- get_state_geo_df( state_id ) %>%
    select( starts_with("capital_"))
  getZoomedMap( map, 
               edge_lat <- capital %>% 
                 select( ends_with("_lat")) %>%
                 unlist(),
               edge_lon <- capital %>% 
                 select( ends_with("_lon")) %>%
                 unlist()
               ) 
}
# get Zoomed ------------------------------------------------------------------


getZoomedMap <- function( map, edge_lat, edge_lon ) {
  map + 
    coord_quickmap( xlim=edge_lon, ylim=edge_lat) +
    #theme(panel.background = element_rect(fill = NULL)) +
    theme(legend.position = "none") +
    ggtitle("")
}

# theme_map------------------------------------------------------------------


theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0))
}

# ------------------------------------------------------------------
testGetSeifaColors <- function (state_id=1) {
  # get me the LGA Seifa's that are in the sampledata
  dbGetQuery(con, paste(
                        "
                        SELECT DISTINCT lga, lga_name, value AS score
                        FROM lga_full_2011 l
                        JOIN seifa_2011 a USING (lga)
                        WHERE a.measure_code='SCORE'
                        AND a.index_code='IRSAD' 
                        AND " , get_lga_restriction( state_id ) 
                        )
  ) %>% 
  mutate( lga_colorisation = ntile( score, 4)  %>%
         ordered(  levels=1:4, labels=c("Least", "Moderate", "High", "Very High")) ) %>%
  select(lga, lga_colorisation) 
}

# ------------------------------------------------------------------
# testGetMap ------------------------------------------------------------------


# ------------------------------------------------------------------
testGetMap  <- function( state_id=1 ) {
  testGetSeifaColors( state_id) %>%
    get_australia_state_map(  )
}


