package example

import org.scalajs.dom
import org.scalajs.dom.raw.{MouseEvent, HTMLElement}
import scala.scalajs.js.Date
import scalajs.js
import rx._
import scalatags.JsDom.all._

trait Reactive {
  implicit def rxFrag[T <% Frag](r: Rx[T]): Frag = {
    def rSafe: dom.Node = span(r()).render
    var last = rSafe
    Obs(r, skipInitial = true){
      val newLast = rSafe
      js.Dynamic.global.last = last
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }
    last
  }

  def everySecond(): Var[Double] = {
    val timeVar = Var(0d)
    def refreshTime() = { timeVar() = new Date().getTime()}
    dom.setInterval(refreshTime _, 1000)
    timeVar
  }

  def everyClick(item: HTMLElement): Var[MouseEvent] = {
    val mouseVar = Var[MouseEvent](null)
    item.onclick = (event: MouseEvent) => {mouseVar() = event}
    mouseVar
  }
}
