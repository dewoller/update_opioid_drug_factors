##################################################################
##################################################################
bbox2poly <- function( bbox ) {

  e <- as(raster::extent( bbox[1], bbox[3], bbox[2], bbox[4]) , "SpatialPolygons")
  proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  e

}


##################################################################
##################################################################
get_direction_offset <- function () {

  rv <- data.frame( 
             direction=qc( N,S,E,W, O ),
             x_offset_direction = c( 0,0,1,-1,0 ), 
             y_offset_direction = c( 1,-1, 0, 0,0 ),
             stringsAsFactors=FALSE   )

  expand.grid ( qc( N,S ), qc( E,W), stringsAsFactors=F) %>%
    inner_join( rv, by=c('Var1'='direction')) %>%
    inner_join( rv, by=c('Var2'='direction')) %>%
    mutate( direction=paste0( Var1, Var2 ), 
           x_offset_direction = x_offset_direction.x + x_offset_direction.y,
           y_offset_direction = y_offset_direction.y + y_offset_direction.x ) %>%
    select( names(rv )) %>% 
    bind_rows( rv ) 

}

##################################################################
##################################################################
get_state_vp_coordinates <- function ( bb,  inner_margins ) {

  direction_offset = get_direction_offset()
    
  state_viewport_position = rbind( 
  data.frame(name='Sydney',state_id=1, direction='E', offset_x= 0.0, offset_y= -0.02, stringsAsFactors=FALSE),
  data.frame(name='Melbourne',state_id=2, direction='SW', offset_x= 0.0, offset_y= 0.0, stringsAsFactors=FALSE),
  data.frame(name='Perth',state_id=4, direction='W', offset_x= 0.0, offset_y= 0.0, stringsAsFactors=FALSE),
  data.frame(name='Adelaide',state_id=5, direction='SW', offset_x= -0.0280, offset_y= 0.012, stringsAsFactors=FALSE),
  data.frame(name='Brisbane',state_id=3, direction='E', offset_x= 0.0, offset_y= 0.02, stringsAsFactors=FALSE),
  data.frame(name='Darwin',state_id=7, direction='N', offset_x= 0.0, offset_y= 0.0, stringsAsFactors=FALSE),
  data.frame(name='Hobart',state_id=6, direction='E', offset_x= 0.0, offset_y= 0.0, stringsAsFactors=FALSE)
  )

  abs_offset = .09 # how far to offset the box from the center of the bb for the city
  remains_x = 1 - ( inner_margins[ 2 ] + inner_margins[ 4 ] )
  remains_y = 1 - ( inner_margins[ 1 ] + inner_margins[ 3 ] )
  bbx = as.numeric( bb$xmax - bb$xmin )
  bby = as.numeric( bb$ymax - bb$ymin )

  state_viewport_position %>%
    rowwise() %>%
    # what is coordinates of state capital in lat lon
    # what is coordinates of state capital in 0:1
    mutate( 
          cap_lon_x = mean(get_state_capital_xlim( state_id )),
          cap_lat_y = mean(get_state_capital_ylim( state_id )),
          cap_lon_offset_x = cap_lon_x  - as.numeric( bb$xmin ) ,
          cap_lat_offset_y = cap_lat_y  - as.numeric( bb$ymin ) ,
          cap_lon_x_scaled = cap_lon_offset_x/ bbx * remains_x + inner_margins[ 2 ],
          cap_lat_y_scaled = cap_lat_offset_y/ bby * remains_y + inner_margins[ 1 ]
          ) %>% 
    inner_join( direction_offset, by='direction') %>%
    mutate( vp_x = cap_lon_x_scaled + abs_offset * x_offset_direction+offset_x ) %>%
    mutate( vp_y = cap_lat_y_scaled + abs_offset * y_offset_direction+offset_y ) %>%
    mutate( vp_lon_x = cap_lon_x + (abs_offset * bbx * remains_x *  x_offset_direction )) %>%
    mutate( vp_lat_y = cap_lat_y + (abs_offset * bby * remains_y *  y_offset_direction ))


}


inset_states=1
##################################################################


print_map= function( 
                    df_map, 
                    title='', 
                    filename=NA, 
                    inset_states=vector() ,
                    states=0, 
                    states_outline_map = get_australia_states_map( states )
                    )  
{

  map_list = generate_map ( 
                           df_map = df_map, 
                           title=title ,
                           filename=filename, 
                           inset_states=inset_states ,
                           states_outline_map = states_outline_map
                           )
                           
  print_map_list( map_list )
  map_list
}

##################################################################

print_map_list= function( map_list )  {
  print( map_list[['map']] + 
        map_list[['map_city_inset_box_overlay']])

  #
  # print insets
  for(i in 1:length(map_list[['insets_tm']]) ) 
    print( map_list[['insets_tm']][[i]], vp=map_list[['insets_vp']] [[i]])
  #

}


##################################################################
generate_map = function( df_map, title, filename, inset_states, states_outline_map)  {

  line_color = 'green'
#
  #map_color_set_1= RColorBrewer::brewer.pal(6, "Oranges")[2:6]
  #map_color_set_2= RColorBrewer::brewer.pal(6, "Blues")[2:6] 
  #
  #map_color_set_2= c(RColorBrewer::brewer.pal(5, "PuBuGn") )
  map_color_set_1= c(RColorBrewer::brewer.pal(5, "YlOrRd") )
#
#

  # bottom, left, top, and right inner  margin
  inner_margins = c( .1, .1, 0, .1 )

   #zoom in on actual mainland oz
  states_outline_map %>%
    bb( xlim=c(112.92, 154)) %>% 
    { . } -> oz_bb

#
  get_state_vp_coordinates ( oz_bb, inner_margins ) %>%
    filter( state_id %in% inset_states ) %>% 
    { . } -> state_vp_coordinates

  #make picture proportional to the map 
  pixel_multiplier = 60
  pic_width= floor((oz_bb$xmax - oz_bb$xmin) * pixel_multiplier )
  # make lat and long proportional to picture size
  pic_height= floor((oz_bb$ymax - oz_bb$ymin) * pixel_multiplier / 47.43 * 54.58 )

  df_map %>%
    select( lga, value ) %>%
    append_data( base_map, 
                ., 
                key.shp="LGA_CODE11", 
                key.data="lga" 
                )  %>%
  {.} -> df_geom_map

  df_geom_map %>%
    tm_shape( bbox=oz_bb ) + 
    tm_polygons( "value", 
                title = title,
                palette = map_color_set_1 ,
                showNA=FALSE,
                colorNA='#FFFFFF'
                ) +
    tm_shape( states_outline_map) + 
    tm_borders(  alpha=1, col="#000000"  ) %>%
    {.} -> base_map

  base_map +  
    tm_layout(frame=FALSE,
              inner.margins = inner_margins,  # how far in from the sides (for insets)
              legend.show=FALSE) %>%  # we want to place our legend ourselves 
    { . } -> map_alone
 
  g( inset_legend_tm, inset_legend_vp ) %=% generate_legend_vp( base_map )
  g( inset_capitals_tm, inset_capitals_vp, map_city_inset_box_overlay) %=% 
    generate_capital_map_helpers( state_vp_coordinates, df_geom_map, map_color_set_1, line_color ) 

  if (length( inset_states ) == 0 ) { 
    insets_vp = inset_legend_vp
    insets_tm = inset_legend_tm
  } else {
    insets_tm=c( inset_capitals_tm, inset_legend_tm ) 
    insets_vp=c( inset_capitals_vp, inset_legend_vp)
  }

  if( !is.na( filename )) {
    tmap_save( map_alone + map_city_inset_box_overlay , 
              insets_vp = insets_vp,
              insets_tm = insets_tm,
              filename=filename, 
              width=pic_width, height=pic_height, asp=0
              ) 
  }

  return (list( map=map_alone, 
    map_city_inset_box_overlay = map_city_inset_box_overlay, 
    insets_vp = insets_vp,
    insets_tm = insets_tm
    ))
}

##################################################################

generate_capital_map_helpers  = function( state_vp_coordinates, df_geom_map, map_color_set_1, line_color ) {

  map_city_inset_box_overlay = 0
  cc_box_width=0.12

  # create capital view ports 
  state_vp_coordinates  %>%
    rowwise() %>%
    mutate( vp = list(viewport( x= vp_x , y= vp_y , 
                                width= cc_box_width, height= cc_box_width) )) %>%
    mutate( inset_tm = list( create_capital_inset( df_geom_map, 
                                                  state_id, 
                                                  map_color_set_1, 
                                                  unlist(vp ), 
                                                  line_color ))
    ) %>%
    mutate( line = list( latlon2line( vp_lon_x, vp_lat_y, cap_lon_x, cap_lat_y    ))) %>%
    { . } -> df_states 

  # add in capital city boxes for later overlay
  for(i in 1:dim(df_states)[1] ) 
  {
    map_city_inset_box_overlay <- 
      tm_shape( capital_city_box(df_states[i,]$state_id )) + 
      tm_borders(col= line_color ) +
      tm_shape( df_states[i, ]$line[[1]] ) +
      tm_lines( col= line_color) +
      map_city_inset_box_overlay 
  }
  return( list(  
                df_states$inset_tm,  
                df_states$vp,
                map_city_inset_box_overlay 
                )
  ) 
}
##################################################################

generate_legend_vp = function(  base_map, legend_direction='NW' ) {
  # generate legend
  legend_dim = .2
  get_direction_offset() %>%
    filter( direction==legend_direction) %>%
    mutate_if(is_numeric, function(x) { ((x+1)/2) + (-x * legend_dim/2)  }) %$%
    viewport(x= x_offset_direction, 
             y= y_offset_direction, 
             width= legend_dim, 
             height= legend_dim) %>% 
    { . } -> vp_legend

  # create legend viewport from base map
  base_map + tm_layout( bg.color="#FFFFFF",
                       legend.only=TRUE,
                       legend.title.size = 1.1, 
                       legend.text.size = 0.9,
                       legend.frame = TRUE
                       ) %>%
  { . } -> m_legend

return( list(list( m_legend ), list( vp_legend )))

}

##################################################################

latlon2line = function( lon1, lat1, lon2, lat2 ) {
  coords = matrix( c(lon1, lat1, lon2, lat2 ), ncol=2, byrow=TRUE)
  #cat (coords, '\n')
  e=coords2Lines( coords = coords, ID='A' )
 proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
 e

}
###################################################################

capital_city_box = function( state_id ) {
  coords = c( get_state_capital_xlim( state_id ), get_state_capital_ylim( state_id ))
  #cat(coords, '\n' )
  e <- as(raster::extent(coords), "SpatialPolygons")
  proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  e
}

##################################################################
##################################################################

# debug( create_capital_inset)

create_capital_inset = function( df_map, state_id, map_color_set, vp, line_color  ) {
  # slice out a chunk of df_map, put it in a red box, and print it into a viewport.  
  # Return the map for later use 
  df_map %>%
    tm_shape( xlim=get_state_capital_xlim(state_id),
              ylim=get_state_capital_ylim(state_id) ) +
  tm_polygons( "value", 
            palette = map_color_set ,
            showNA=FALSE,
            colorNA='#FFFFFF',
            legend.show=FALSE) +
  tm_layout(frame= line_color , bg.color="#FFFFFF")  %>%
  {.} -> inset_map
  #print(inset_map, vp=vp)
  inset_map
}


