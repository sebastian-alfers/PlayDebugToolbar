package controllers

import play.api._
import play.api.mvc._
import play.api.templates.Html
import play.api.libs.iteratee.Enumerator
import scala.concurrent.Await
import scala.concurrent.duration._


object Application extends Controller {

  def index = Action {

    //val deadline = 3.seconds.fromNow
    //Thread.sleep(deadline.timeLeft.toMillis)
    Ok(views.html.index("Your new application is ready."))
  }

  def comet = Action {
    val events = Enumerator(
      """<script>conso  le.log('kiki')</script>""",
      """<script>console.log('foo')</script>""",
      """<script>console.log('bar')</script>"""
    )
    Ok.stream(events >>> Enumerator.eof).as(HTML)
  }

}