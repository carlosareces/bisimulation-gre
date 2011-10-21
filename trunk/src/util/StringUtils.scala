package util;

object StringUtils {

  
  def join(list:Iterable[String], sep:String) : String = join(list.toList, sep)
  
  def join(list:List[String], sep:String) : String = {
    if( list.isEmpty ) {
      ""
    } else {
      list.tail.foldLeft(list.head)((x,y) => x + sep + y)
    }
  }
}
