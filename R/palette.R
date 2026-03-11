#' Display the palettes in SCP
#'
#' @param palette_names A character vector of palette names in SCP package
#' @param ncolor Number of colors to display
#' @param ncol Number of columns for displaying colors
#' @param h2w The ratio of color bar height to width
#' @param name.size Text size for color names
#'
#' @returns
#' A ggplot2 object.
#'
#' @export
wb.palette_show_scp <- function (palette_names = NULL, ncolor = 20, ncol = 5, h2w = 0.6,
                                 name.size = 10){
  library(data.table)
  library(ggplot2)
  library(SCP)
  library(ggpubr)

  #
  if (is.null(palette_names)) {
    palette_names = names(SCP::palette_list)
  }
  palette_names <- sort(palette_names , decreasing = F )

  #
  split_groups <- list(palette_names)
  if (length(palette_names) > ncol) {
    if (ncol != 1) {
      split_groups <- split(palette_names,
                            cut(seq_along(palette_names),ncol)
      )
    }
  }

  #
  pdata <- wb.smc(1:length(split_groups), function(x) {
    g = paste0("group", x)
    v = lapply(split_groups[[x]], function(y) {
      detect <- tryCatch({
        colors <- SCP::palette_scp(palette = y, n = 100)
      }, error = function(e) {
        NULL
      })
      if ( ! "NULL" %in% class( detect ) ) {
        #all colors
        colors <- as.character(detect)
        final_colors_show  <- colors

        #show colors
        if( ncolor < 100  ){
          idx <- round(quantile(seq_len(100), probs = seq(0, 1, length.out = ncolor)))
          final_colors_show <- colors[ idx  ]
        }
        #
        return(data.table(name = y, color = final_colors_show, height = 1:length( final_colors_show )   ))
      }
      else {
        return(NULL)
      }
    })
    v = rbindlist(v[!unlist(lapply(v, is.null))])
    v$group = g
    return(v)
  } ,  pb = F, time = F )

  #
  pdata <- data.frame(rbindlist(pdata))
  ps <- wb.smc(unique(pdata$group), function(x) {
    sdata <- pdata[pdata$group == x, ]
    p <- ggplot(sdata, aes(x = height, y = name, fill = color)) +
      geom_tile(  ) + scale_fill_identity() +
      scale_y_discrete(limits = rev) + labs(x = NULL,
                                            y = NULL) +
      coord_fixed( ratio =  ( ncolor / 5  ) * h2w ) +
      theme_void() +
      theme(axis.text.y = element_text(color = "black",
                                       hjust = 1, size = name.size))
    return(p)
  }, pb = F, time = F )

  #
  if (length(palette_names) > ncol) {
    final_p <- ggpubr::ggarrange(plotlist = ps, ncol = ncol,
                                 align = "hv")
  }
  else {
    final_p <- ggpubr::ggarrange(plotlist = ps, ncol = 1)
  }

  #
  print(final_p)
  return(final_p)
}


#
#' Get colors from a palette
#'
#' @param palette_names A palette name in SCP package
#' @param ncolor Number of colors returned
#' @param colors A character vector of color names for previewing. When provided, the palette_names argument will be ignored
#' @param cat Whether to print colors
#'
#' @returns
#' A color vector.
#'
#' @export
wb.palette_get <- function( palette_names = NULL , ncolor = 5 , colors = NULL , cat = F   ){
  #
  library( crayon  )
  #
  if( !is.null( palette_names ) ){
    scp_colors <- SCP::palette_scp( palette = palette_names , n = 100 )
    #
    idx <- round(quantile(seq_len(100), probs = seq(0, 1, length.out = ncolor)))
    final_colors <- scp_colors[ idx  ]
    #
    color = as.character( final_colors  )
  }
  if( ! is.null( colors  )  ){ color = colors   }
  #cat
  if ( cat ){
    for (col in color) {
      #
      col_func <- crayon::make_style( col )

      rgbs <- as.numeric(col2rgb( col ) / 255)

      myfg <- make_style(rgb(rgbs[1], rgbs[2], rgbs[3]), bg = T)
      cat( myfg(col_func( paste0(  col , paste(rep( ' ', 60 ) ,collapse = '' )  ) ))   )

      mybg <- make_style(rgb(rgbs[1], rgbs[2], rgbs[3]), bg = F)
      cat( mybg(col_func( paste0(' ', col , '\n'  ) ))  )
    }
    cat('Colors:\n')
    #
  }
  return( as.character(color)  )
  #
}
