/**
 * Created by sebastian on 15.06.14.
 */

package playDebugToolbar;

import play.api.http.HeaderNames
import play.api.libs.iteratee.{Enumeratee, Enumerator, Iteratee}
import play.api.mvc._
import play.api.templates.Html
import play.Logger
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait ToolbarUpdater {
  def run: String
}
class RequestPath(params: RequestBag) extends ToolbarUpdater{
  def run = {
    s"${params.request.method} ${params.request.uri}"
  }
}

class ResponseTime(params: RequestBag) extends ToolbarUpdater{
  val endTime = System.currentTimeMillis
  val requestTime = endTime - params.startTime
  def run = {s"ttttook: $requestTime ms"}
}

class RequestBag(req: RequestHeader) {
  val request = req
  val startTime = System.currentTimeMillis
}



object AccessLog extends Filter {



  def apply(next: (RequestHeader) => Future[SimpleResult])(request: RequestHeader): Future[SimpleResult] = {

    val requestBag = new RequestBag(request)

    def   manipulateSimpleResult(sr : SimpleResult)(toolbarHtml: Html): SimpleResult = {

      val header = sr.header.headers.get(HeaderNames.CONTENT_TYPE)
      val newResult = header match {
        case item: Some[String] => {

          header match {
            case item: Some[String] => {
              if (header.getOrElse("").contains("text/html")) {

                val action = Iteratee.fold[Array[Byte], String]("") {
                  (length, bytes) => {
                    val content = new String(bytes)
                    content
                  }
                }

                val originalContent = sr.body.run(action)

                val newContent = originalContent.map{html =>
                  val replaced = html.replace("</body>", s"$toolbarHtml</body>")
                  val str:Enumerator[Array[Byte]] = Enumerator(replaced.getBytes())
                  SimpleResult(
                    header = ResponseHeader(200, sr.header.headers),
                    body = str
                  )
                }.recover {
                  case _ => {
                    val str:Enumerator[Array[Byte]] = Enumerator(s"error 5 test".getBytes())

                    SimpleResult(
                      header = ResponseHeader(200),
                      body = str
                    )
                  }
                }
                // :(
                Await.result(newContent, 2 seconds)

              }
              else{
                sr
              }
            }
            case (None | _) =>sr
          }

        }
        case (None | _) => sr
      }

      newResult

    }

    def appendDebugToolbar(result: SimpleResult) = {
      val toolbarHtml = buildToolbarHtml(requestBag)

      manipulateSimpleResult(result)(toolbarHtml)
    }

    next(request).map(appendDebugToolbar) recover {
      case item: Any => errorResponse("not able to append Debug toolbar")
    }


  }

  def errorResponse(msg: String) = {
    SimpleResult(
      header = ResponseHeader(200),
      body = Enumerator(msg.getBytes)
    )
  }

  def buildToolbarHtml(requestBag: RequestBag) = {
    val updater: List[ToolbarUpdater] = List(new RequestPath(requestBag), new ResponseTime(requestBag))
    val html = updater.foldLeft("")((start, updater) => start+updater.run)
    views.html.toolbar.render(html)
  }
}
