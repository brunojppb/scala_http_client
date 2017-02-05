import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.message._

def parseArgs(args: Array[String]): Map[String, List[String]] = {

  def nameValuePair(paramName: String) = {
    def values(commaSeparatedValues: String) = {
      commaSeparatedValues.split(",").toList
    }
    val index = args.indexOf(paramName)
    (paramName, if(index == -1) Nil else values(args(index+1)))
  }
  Map(nameValuePair("-d"), nameValuePair("-h"))
}

def splitByEqual(nameValue: String): Array[String] = nameValue.split('=')

def headers = for(nameValue <- params("-h")) yield {
  def tokens = splitByEqual(nameValue)
  new BasicHeader(tokens(0), tokens(1))
}

def formEntity = {
  def toJavaList(scalaList: List[BasicNameValuePair]) = {
    java.util.Arrays.asList(scalaList.toArray:_*)
  }

  def formParams = for(nameValue <- params("-d")) yield {
    def tokens = splitByEqual(nameValue)
    new BasicNameValuePair(tokens(0), tokens(1))
  }

  def formEntity = new UrlEncodedFormEntity(toJavaList(formParams), "UTF-8")
  formEntity
}

def handlePostRequest = {
  val httpPost = new HttpPost(url)
  headers.foreach { httpPost.addHeader(_) }
  httpPost.setEntity(formEntity)
  val responseBody = new DefaultHttpClient().execute(httpPost, new BasicResponseHandler())
  println(responseBody)
}

def handleGetRequest = {
  val query = params("-d").mkString("&")
  val httpGet = new HttpGet(s"${url}?${query}")
  headers.foreach { httpGet.addHeader(_) }
  val responseBody = new DefaultHttpClient().execute(httpGet, new BasicResponseHandler())
  println(responseBody)
}

require(args.size >= 2,
  "you should specify action(post, get, options or delete) and url")

val command = args.head
val params = parseArgs(args)
val url = args.last

command match {
  case "post"     => handlePostRequest
  case "get"      => handleGetRequest
}
