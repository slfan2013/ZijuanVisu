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
  dE<-openxlsx::read.xlsx(path,sheet=1,colNames=T)

  return(list(data=dE,color=substring(rainbow(6),1,nchar(rainbow(6))-2)))

}
