rotate_visium_v2 <- function(
    object,
    image = "slice1",
    direction = c("90", "-90")
) {
  direction <- match.arg(direction)

  img_obj <- object@images[[image]]

  if (!inherits(img_obj, "VisiumV2")) {
    stop("The image must be a VisiumV2 object")
  }

  # 1. Original image
  ori_img <- img_obj@image

  H <- dim(ori_img)[1]
  W <- dim(ori_img)[2]

  # 2. Rotate image
  new_img <- aperm(ori_img, c(2, 1, 3))

  if (direction == "-90") {
    # CCW 90 degrees
    new_img <- new_img[
      dim(new_img)[1]:1, , ,
      drop = FALSE
    ]
  } else {
    # CW 90 degrees
    new_img <- new_img[
      , dim(new_img)[2]:1, ,
      drop = FALSE
    ]
  }

  img_obj@image <- new_img

  # 3. Convert image dimensions to full-resolution coordinates
  sf <- img_obj@scale.factors$lowres

  H_full <- H / sf
  W_full <- W / sf

  # 4. Get spot coordinates
  centroids <- img_obj@boundaries$centroids
  coords <- centroids@coords

  old_x <- coords[, "x"]
  old_y <- coords[, "y"]

  # 5. Rotate coordinates
  if (direction == "-90") {
    coords[, "x"] <- old_y
    coords[, "y"] <- W_full - old_x
  } else {
    coords[, "x"] <- H_full - old_y
    coords[, "y"] <- old_x
  }

  centroids@coords <- coords

  # 6. Update boundaries
  boundaries <- img_obj@boundaries
  boundaries$centroids <- centroids
  img_obj@boundaries <- boundaries

  # 7. Update Seurat object
  object@images[[image]] <- img_obj

  return(object)
}



#' Rotate Seurat image
#'
#' @description
#' Spatial transcriptomics images sometimes need to be rotated, but Seurat does not provide an official function for this purpose. This function enables image rotation within Seurat objects.
#'
#'
#' @param object Seurat object.
#' @param version The image class in a Seurat object can be identified using `class(object@images[[1]])`. Options: VisiumV1 or VisiumV2.
#' @param image Name of the image to use.
#' @param direction Rotation angle. Character object! Options: 90, -90, P2L or L2P.
#'
#' P2L: Automatically detects whether an image is in landscape (L) or portrait (P) orientation and converts portrait images to landscape orientation. L2P performs the opposite operation, converting landscape images to portrait orientation.
#'
#' @returns
#' Seurat object.
#'
#' @examples
#'
#' # Seurat object
#' obj
#' obj_rotated <- ww.sc.rotate(obj, image  = "slice1.008um", direction = 'P2L' )
#'
#' # Verification 1: Inspect the cell image after rotation
#' p1 <- SpatialFeaturePlot( obj, features = "nCount_Spatial.008um" ) + ggtitle("Before rotation")
#' p2 <- SpatialFeaturePlot( obj_rotated, features = "nCount_Spatial.008um" ) + ggtitle("After 90° rotation")
#' p1 | p2
#'
#' # Verification 2: Randomly select a cell and verify its spatial location
#' cell <- colnames(obj)[ 200 ]
#' p3 <-Seurat::SpatialDimPlot(
#'   obj, cells.highlight = cell,
#'   cols = c("grey90" ), cols.highlight = c("blue" ,'grey90' ),
#'   pt.size.factor = 2, image.alpha = 0.1,
#'   label = T, label.size = 2, label.color = 'yellow'
#' )
#' p4 <-Seurat::SpatialDimPlot(
#'   obj_rotated, cells.highlight = cell,
#'   cols = c("grey90" ), cols.highlight = c("blue" ,'grey90' ),
#'   pt.size.factor = 2, image.alpha = 0.1,
#'   label = T, label.size = 2, label.color = 'yellow'
#' )
#' p3 | p4
#'
#' # Verification 3: Inspect the spatial coordinates of the cells
#' coords1 <- Seurat::GetTissueCoordinates( obj )
#' coords2 <- Seurat::GetTissueCoordinates( obj_rotated )
#'
#' @export
#'
ww.sc.rotate <- function( object  , version = 'VisiumV2' , image = "slice1", direction = "P2L" ){
  #
  ww.package_library( 'Seurat' )

  #
  coords_i <- as.data.frame(Seurat::GetTissueCoordinates( object = object , image = image ))
  x_range <- max( coords_i$x ) - min( coords_i$x )
  y_range <- max( coords_i$y ) - min( coords_i$y )
  image_direction <- ifelse( x_range > y_range , 'L' , 'P'  )

  #
  if( version == 'VisiumV2' ){
    #
    if( direction %in% c( "P2L", "L2P" )  ){
      #
      if(  direction == "P2L" & image_direction == 'P' ){
        direction = '90'
      }else if(  direction == "L2P" & image_direction == 'L' ){
        direction = '90'
      }else{
        return( object )
      }
      #
    }
    #
    object <- rotate_visium_v2( object = object, image = image, direction = direction  )
  }

  #
  return( object )
}

