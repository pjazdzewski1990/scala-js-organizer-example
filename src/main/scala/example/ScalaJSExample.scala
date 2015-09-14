package example

import scala.scalajs.js.Date

import org.scalajs.dom
import dom.html
import org.scalajs.dom.html.{Div, Button, Input}
import org.scalajs.dom.raw.MouseEvent
import scala.util.Try
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

@JSExport
object ScalaJSExample extends {

  var thingsToDo = List(Task("Task1", 30), Task("Task2", 45))

  val addDesc = input("New Task").render
  val addTime = input("0").render
  val addButton = button("Add a new task", `type`:="button", `class`:="btn btn-primary", marginTop:=10, marginBottom:=10).render
  val timeSummary = div().render

  @JSExport
  def main(target: html.Div): Unit = {
    println(s"main")

    addButton.onclick = (x: MouseEvent) => {
      val desc = addDesc.value
      val time = Try{ addTime.value.toInt }.toOption.getOrElse(0)
      println(s"Adding $desc/$time")
      thingsToDo = Task(desc, time) :: thingsToDo

      refreshScreen(target)
    }

    refreshScreen(target)
    dom.setInterval(refreshTimeSummary(target) _, 60 * 1000 / 100)
  }

  def refreshScreen(target: Div): Unit = {
    target.innerHTML = ""
    target.appendChild(
      rebuildUI(target, addDesc, addTime, addButton)
    )
    refreshTimeSummary(target)
  }

  def rebuildUI(target: html.Div, addDesc: Input, addTime: Input, addButton: Button): Div =
    div(
      h1("Scala.js organizer"),
      ul(
        for (it <- thingsToDo) yield
          li(
            div(
              s"${it.desc} takes ${it.time} minutes",
              createDeleteButton(target, it)
            ),
            `class` := "list-group-item"
          ),
        `class` := "list-group"
      ),
      addForm,
      timeSummary,
      `class`:="col-sm-4"
    ).render

  def createDeleteButton(target: Div, it: Task) = {
    val b = button("X", `class`:="btn btn-sm btn-danger", float.right, marginTop:= -5).render
    b.onclick = (_: MouseEvent) => {
      thingsToDo = thingsToDo.filterNot(_ == it)
      refreshScreen(target)
    }
    b
  }

  val addForm = Array(
    div(addDesc, addTime),
    div(addButton)
  )

  def format(d: Date) = s"${d.getHours()}:${d.getMinutes()}"
  def refreshTimeSummary(target: html.Div)() = {
    println(s"Refresh " + new Date)
    val timeNeeded: Int = thingsToDo.foldLeft(0)((x, y) => x + y.time)
    val now: Date = new Date
    val endDate = new Date(now.getTime() + timeNeeded*60000)

    timeSummary.innerHTML = s"${format(now)} + ${timeNeeded} minutes on tasks = ${format(endDate)}"
    if(thingsToDo.length > 5) timeSummary.style.backgroundColor = "red"
  }
}

case class Task(desc: String, time: Int)