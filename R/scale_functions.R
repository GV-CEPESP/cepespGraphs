
### Definindo tipos de cores
cores <- c(
# Cores do Cepesp/FGV
`cor_cepesp1`= rgb(28, 47, 103, maxColorValue = 255), # From the brandbook, azul escuro
`cor_cepesp2`= rgb(0, 150, 214, maxColorValue = 255), # azul piscina
`cor_cepesp3`= rgb(178, 178, 178, maxColorValue = 255), # cinza
`cor_pt`="firebrick1",
`cor_psdb`="dodgerblue3"
### Aqui podemos criar outras cores, tanto com nome de cor, hexidecimal, ou usando o "Red, Green and Blue" (como o pessoal do brandbook mandou)
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
)

# Definindo a função Cepesp_cores para escolher as cores depois
Cepesp_cores <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (cores)
  cores[cols]
}

### Definindo tipos de paletas
paletas<-list(
`principal`= Cepesp_cores("cor_cepesp1", "cor_cepesp2", "cor_cepesp3"),
`pt_psdb`= Cepesp_cores("cor_pt", "cor_psdb")
)
### Aqui podemos criar outras paletas com qualquer cor que definimos ali em cima

# Definindo a função Cepesp_paletas para definir as funções depois
Cepesp_paletas<-function(palette="principal", reverse=F){
	pal<-paletas[[palette]]
	if(reverse) pal<-rev(pal)
	colorRampPalette(pal)
}

#' CEPESP colour scales
#'
#' \code{scale_*_cepesp} functions are
#'
#' @example
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 4) +
#'   scale_color_cepesp("pt_psdb")
#'
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'   scale_fill_cepesp()
#'
#' @export

scale_color_cepesp <- function(palette = "principal", discrete = TRUE, reverse = FALSE, ...) {
  pal <- Cepesp_paletas(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("Cepesp_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_color_cepesp
#' @export
scale_fill_cepesp <- function(palette = "principal", discrete = TRUE, reverse = FALSE, ...) {
  pal <- Cepesp_paletas(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("Cepesp_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
