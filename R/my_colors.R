

#' 获取多组的颜色
#' @description
#' 最多50组
#'
#' @param number 颜色数目
#' @param continuous 是否需要连续的色阶

#' @export
group_colors <- function( number , continuous = F ){
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
    # 暖色系
    "#FFEA00", "#FF7F50", "#FFAB40", "#FF4500", "#FFC8DD", "#FAD02E",
    "#FBCEB1", "#FF9AA2", "#FFD166", "#FFCC99", "#FFB347", "#FF6961",
    "#FF9A8B", "#FF6B6B", "#FFA07A", "#FFD700", "#FFEC8B", "#FFF44F",

    # 冷色系
    "#A0E6C3", "#6EC5E9", "#88D8C0", "#B0E0E6", "#48D1CC", "#E2F0CB",
    "#87CEEB", "#D4FF59", "#B4F0F0", "#C1E1C1", "#B5EAD7", "#77DD77",
    "#AEC6CF", "#B0E57C", "#89CFF0", "#99FFFF", "#AAF0D1", "#C6E2FF",

    # 中间色系
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

#' @export
heatmap_color_3 <- function(){
  heatmap_color_3 <- c("#007AFF", "#FFFFFF", "#FF3B30")
  return( heatmap_color_3 )
}

#' @export
heatmap_color_5 <- function(){
  heatmap_color_5 <- c("#0066CC", "#00CCFF", "#FFFFFF", "#FF00FF", "#CC00CC")
  return( heatmap_color_5 )
}

#' @export
heatmap_color_blue <- function(){
  heatmap_color_blue <- c("#FFFFFF", "#007AFF" )
  return( heatmap_color_blue )
}

#' @export
heatmap_color_orange <- function(){
  heatmap_color_orange <- c("#FFFFFF", "#FF3B30" )
  return( heatmap_color_orange )
}

#' @export
volcano_color <- function(){
  volcano_color <- c("#007AFF", "#FFFFFF", "#FF3B30")
  return( volcano_color )
}

#' ggplot2对象的预览和保存
#'
#' @param plot ggplot2对象
#' @param pre 文件名前缀
#' @param sur 文件名后缀。默认是'.jpeg'
#' @param file 完整的文件名。调用该参数时，会忽略pre和sur
#' @param res DPI
#' @param height 图片的高度
#' @param width 图片的宽度
#'
#' @export
gg.op <- function( plot, pre = NULL, sur = '.jpg' ,
                   file = NULL , res = 600 , height = 5 , width = 5  ){
  #
  if ( is.null(file)  ){
    filename = paste0( pre , sur  )
  }else{
    filename = file
  }
  #
  p.view <- plot + ggview::canvas(   height = height , width =  width, dpi = res  )
  print(p.view)
  #
  suppressMessages(
    ggpubr::ggexport( plot , filename = filename,
                      res = res ,
                      height = height * res ,
                      width = width * res
    )
  )
  #
  message( paste0( filename ,' | ',Sys.time() , '\n'  )  )
  #
}




















