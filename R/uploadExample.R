#' uploadExample
#'
#' stat
#' @param
#' @keywords
#' @export
#' @examples
#' uploadExample()
#'
#'

uploadExample = function(path){
  # path = "C:\\Users\\Sili Fan\\Desktop\\WORK\\WCMC\\projects\\Zijuan Lai\\ZijuanVisu\\data\\test.xlsx"
  d<-openxlsx::read.xlsx(path,sheet=1,colNames=T)

  return(list(data=d,color=rainbow(6)))

}
