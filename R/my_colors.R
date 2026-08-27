

#' Obtain multiple sets of colors
#' @description
#' Up to 50 groups
#'
#' @param number Number of colors to return
#' @param continuous Whether to return a continuous color scale

#' @export
ww.group_colors <- function( number , continuous = F ){
  #
  group_color <- c(
    "#FF3B30", "#007AFF", "#CC00CC", "#00CCFF", "#FDB9A0",
    "#748FFC", "#32E0C4", "#FFEC8B", "#9772FB", "#33C6AD",
    "#FF00FF", "#90BE6D", "#FDCB82", "#FF6B35", "#D67AB1",
    "#FFBC42", "#56CFE1", "#C6D57E", "#FF6F91", "#F7CAC9",
    "#B388EB", "#00B4D8", "#48ACF0", "#F4A261", "#B5E48C",
    "#FF5C5C", "#EF798A", "#6A994E", "#C9CBA3", "#FF9F1C",
    "#00C49A", "#FFE066", "#F06595", "#6BCB77", "#B5EAEA",
    "#F5A9B8", "#9BDEAC", "#57CC99", "#FFB5A7", "#CAF7E3",
    "#F8B195", "#F67280", "#FF6B6B", "#9B5DE5", "#3A86FF",
    "#D0F4DE", "#A9DEF9", "#E4C1F9", "#F694C1", "#FFDE7D"
  )
  #
  group_color_continuous <-  c(
    # warm colors
    "#FFEA00", "#FF7F50", "#FFAB40", "#FF4500", "#FFC8DD", "#FAD02E",
    "#FBCEB1", "#FF9AA2", "#FFD166", "#FFCC99", "#FFB347", "#FF6961",
    "#FF9A8B", "#FF6B6B", "#FFA07A", "#FFD700", "#FFEC8B", "#FFF44F",

    # cool colors
    "#A0E6C3", "#6EC5E9", "#88D8C0", "#B0E0E6", "#48D1CC", "#E2F0CB",
    "#87CEEB", "#D4FF59", "#B4F0F0", "#C1E1C1", "#B5EAD7", "#77DD77",
    "#AEC6CF", "#B0E57C", "#89CFF0", "#99FFFF", "#AAF0D1", "#C6E2FF",

    # neutral colors
    "#DDA0DD", "#FFB7C5", "#C8A2C8", "#C3B1E1", "#FF9E9D", "#F49AC2",
    "#FFB6C1", "#FFC1E1", "#F4C2C2", "#E0BBE4", "#D291BC", "#FADADD"
  )
  #
  if( continuous ){
    return( group_color[1:number] )

  }else{
    return( group_color_continuous[1:number] )
  }
}


#########################################################################################
my.ggplot.op <- function( plot, pre = NULL, sur = '.png' ,
                     file = NULL , res = 600 , h = 5 , w = 5 , model = 'M1'  ){

  #
  if(  is.character( plot  )  ){
    filename = plot
    model = 'M2'
  }else{
    #
    if ( is.null(file)  ){
      filename = paste0( pre , sur  )
    }else{
      filename = file
    }
    #
    suppressMessages(
      if (  stringr::str_ends( filename , 'svg' )  | stringr::str_ends( filename , 'pdf' )   ){
        ggpubr::ggexport( plot , filename = filename,
                          height = h,
                          width = w , verbose = F
        )
      }else{
        ggpubr::ggexport( plot , filename = filename,
                          res = res ,
                          height = h * res ,
                          width = w * res , verbose = F
        )
      }
    )
    #
    message( ww.log_time_title(), "Saved to local: ",
             ww.log_text_coloured( text =  filename , color = 'red' ), '.'  )
  }
  #
  if ( base::interactive() ){
    #
    if(  model  %in% c( 'M1' , 'M2' )  ){
      message( ww.log_time_title(), "Previewing local file: ",
               ww.log_text_coloured( text =  filename , color = 'red' ), '.'  )
    }

    #
    if ( model == 'M1'  ){
      ww.package_install( "ggview" ,method = "devtools::install_github('idmn/ggview')"   )

      p.view <- plot + ggview::canvas(   height = h , width =  w , dpi = res , bg = "white"  )
      print(p.view)
    }
    if ( model == 'M2'  ){

      ww.package_install( "magick" ,method = "I"   )
      ww.package_install( "EBImage" ,method = "B" )

      raw.image <-tryCatch({
        png <- EBImage::readImage( filename )
      },error = function(e){
        NULL
      }
      )

      #
      if(  is.null( raw.image ) ){
        #
        if(  grepl("\\.pdf$", filename , ignore.case = TRUE)   ){
          ww.package_install( "pdftools" ,method = "I" )
          filename <- pdftools::pdf_render_page(  filename , dpi = 600 ,  page = 1 )
        }
        raw.image <- magick::image_read(  filename )
        raw.image <- magick::as_EBImage(  raw.image  )
        #
      }

      print(EBImage::display( raw.image ))
      #
    }
    #
  }else{
    warning( "Image preview is only available in the interactive GUI." )
  }
  #
  return( invisible( TRUE ) )
}


###############################################################
#' Preview and save images
#'
#' @description
#' This function wraps and extends `ggpubr::ggexport` to facilitate saving ggplot objects to local files. In addition, it provides a preview window for immediate visualization of exported images.
#'
#' It can also preview an existing local image by specifying its file path in an interactive GUI.
#'
#'
#' @param plot A ggplot2 object or a file path. If a ggplot2 object is provided, it is saved to a local file and then previewed. If a file path is provided, the image is previewed directly.
#' @param pre File name prefix.
#' @param sur File name suffix, default is '.png'. Supported image formats are identical to those of [ggpubr::ggexport].
#' @param file Full file name. If this parameter is provided, pre and sur will be ignored.
#' @param res Dots per inch (DPI) resolution.
#' @param h Height of the image (inch). For raster plots, the final image height is (h × res) pixels. For vector graphics, the final height is h inches.
#' @param w Width of the image (inch). For raster plots, the final image width is (w × res) pixels. For vector graphics, the final width is w inches.
#' @param mode There are two preview modes:
#'
#' (1) 'M1'. M1 shows a preview generated according to the specified parameters. F1 may sometimes differ slightly from the actual saved image, but it can be viewed in a separate graphics window in R.
#'
#' (2) 'M2'. M2 reloads the exported image from the local file and previews it.
#'
#' (3) FALSE. If local image preview is not required, set this parameter to FALSE.
#'
#' @examples
#'
#' ###
#' library(ggpubr)
#'
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' ### ggplot object
#' bxp <- ggboxplot(df, x = "dose", y = "len", color = "dose", palette = "jco")
#'
#' #
#' ww.ggp( plot = bxp , pre = 'bxp'  )
#' ww.ggp( plot = bxp , pre = 'bxp' , sur = '.pdf'  )
#' ww.ggp( plot = bxp , file = 'bxp.tiff' )
#'
#' ###local file
#' ww.ggp(  plot = 'bxp.tiff'  )
#' #ww.ggp(  plot = 'bxp.pdf'  )
#'
#'
#' @export
#'
ww.ggp <- function( plot, pre = NULL, sur = '.png' ,
                     file = NULL , res = 600 , h = 5 , w = 5 , mode = 'M2'  ){
  #
  if( is.null(pre) & is.null(file) ){
    pre = base::basename( base::tempfile( pattern = 'ggp.tempfile.'  )  )
  }

  #
  my.ggplot.op( plot = plot, pre = pre, sur = sur ,
               file = file , res = res , h = h , w = w , model = mode
               )
  #
}




















