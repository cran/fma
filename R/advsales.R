#' Sales volume and advertising expenditure
#'
#' Sales volume and advertising expenditure for a dietary weight control
#' product.
#'
#'
#' @format Time series data
#' @references Blattberg and Jeuland (1981).
#' @source Makridakis, Wheelwright and Hyndman (1998) \emph{Forecasting:
#' methods and applications}, John Wiley & Sons: New York. Chapter 8.
#' @keywords datasets
#' @examples
#' plot(advsales)
#' @export

advsales <- stats::ts(t(matrix(data=c(
  12.0 , 15,
  20.5 , 16,
  21.0 , 18,
  15.5 , 27,
  15.3 , 21,
  23.5 , 49,
  24.5 , 21,
  21.3 , 22,
  23.5 , 28,
  28.0 , 36,
  24.0 , 40,
  15.5 ,  3,
  17.3 , 21,
  25.3 , 29,
  25.0 , 62,
  36.5 , 65,
  36.5 , 46,
  29.6 , 44,
  30.5 , 33,
  28.0 , 62,
  26.0 , 22,
  21.5 , 12,
  19.7 , 24,
  19.0 ,  3,
  16.0 ,  5,
  20.7 , 14,
  26.5 , 36,
  30.6 , 40,
  32.3 , 49,
  29.5 ,  7,
  28.3 , 52,
  31.3 , 65,
  32.2 , 17,
  26.4 ,  5,
  23.4 , 17,
  16.4 ,  1),nrow=2)),s=1,f=1)
colnames(advsales) = c("sales","advert")
