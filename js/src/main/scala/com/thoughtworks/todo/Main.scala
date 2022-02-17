package com.thoughtworks.todo

import com.thoughtworks.binding
import com.thoughtworks.binding.Binding.BindingSeq.Snapshot
import com.thoughtworks.binding.Binding.*
import com.thoughtworks.binding.*
import com.thoughtworks.binding.bindable
import com.thoughtworks.binding.bindable.Bindable
import com.thoughtworks.binding.html.*
import com.thoughtworks.binding.keywords.Bind
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.domains.scalaz.given
import com.thoughtworks.dsl.keywords.*
import com.thoughtworks.dsl.macros.Reset.Default.*
import org.scalajs.dom.*
import scalaz.*

import scala.collection.View
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.timers.SetTimeoutHandle

// This does not work
// given ExecutionContext = ExecutionContext.parasitic

// This does not work
// import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits.given
import ExecutionContext.Implicits.given

enum EditEvent:
  case Start, Stop

@JSExportTopLevel("main")
def main(container: Element) =

  @inline given Equal[Option[Todo]] = Equal.equalA[Option[Todo]]
  final class Todo(initialTitle: String, isInitiallyCompleted: Boolean):
    val (readToggleClickEvent, writeToggleClickEvent) =
      Binding.jsPipe[MouseEvent]
    val (readDblclickEvent, writeDblclickEvent) = Binding.jsPipe[MouseEvent]
    val (readBlurEvent, writeBlurEvent) = Binding.jsPipe[Event]
    val (readKeyDownEvent, writeKeyDownEvent) = Binding.jsPipe[KeyboardEvent]
    val submit = readKeyDownEvent
      .filter(_.keyCode == KeyCode.Enter)
      .mergeWith(readBlurEvent)
    val title: Binding[String] =
      initialTitle :: submit.map(_ => editInput.value.value)
    val editEvent =
      readDblclickEvent
        .map { _ =>
          println("readDblclickEvent")
          EditEvent.Start
        }
        .mergeWith(submit.map(_ => EditEvent.Stop))
    val isCompleted: Binding[Boolean] = isInitiallyCompleted :: Binding {
      !Bind(readToggleClickEvent)
      toggleInput.value.checked
    }
    private val onDestroyPromise: Promise[Event] = Promise()
    def onDestroy: Future[Event] = onDestroyPromise.future
    lazy val toggleInput: NodeBinding[HTMLInputElement] =
      html"""<input class="toggle" type="checkbox" onclick=$writeToggleClickEvent>"""
    lazy val editInput: NodeBinding[HTMLInputElement] =
      html"""<input class="edit" onblur=$writeBlurEvent onkeydown=$writeKeyDownEvent value=$title>"""
    val html = html"""
      <li
        class=
         ${
            raw"""
             ${
                if !Bind(editingTodo) == Some(this) then
                  js.timers.setTimeout(Duration.Zero)(editInput.value.focus())
                  "editing"
                else ""
              }
             ${if !Bind(isCompleted) then "completed" else ""}
            """
          }
      >
        <div class="view">
          $toggleInput
          <label ondblclick=$writeDblclickEvent>$title</label>
          <button class="destroy" onclick=${onDestroyPromise.success}></button>
        </div>
        $editInput
      </li>
    """

  lazy val editingTodo: Binding[Option[Todo]] = Binding.dropHistory(
    (None #:: allTodos.mergeMap { todo =>
      Binding {
        (!Bind(todo.editEvent)) match
          case EditEvent.Start =>
            Some(todo)
          case EditEvent.Stop =>
            None
      }
    }).distinctUntilChanged
  )

  object Header:
    private val (readKeyDownEvent, writeKeyDownEvent) =
      Binding.jsPipe[KeyboardEvent]
    val newTodo: Binding[String] = Binding.events {
      val event = !Bind(readKeyDownEvent)
      (event.currentTarget, event.keyCode) match {
        case (input: HTMLInputElement, KeyCode.Enter) =>
          input.value.trim match {
            case "" =>
              !Bind(Binding.empty[String])
            case title =>
              title
          }
        case _ =>
          !Bind(Binding.empty[String])
      }
    }
    val html = html"""
      <header class="header">
        <h1>todos</h1>
        <input
          class="new-todo"
          autofocus
          placeholder="What needs to be done?"
          value=${ locally(!Bind(newTodo)); "" }
          onkeydown=${writeKeyDownEvent}/>
      </header>
    """
  lazy val allTodos: BindingSeq[Todo] =
    BindingSeq
      .fromMutationStream[Todo](
        ((Binding
          .events[Snapshot[Todo] => BindingSeq.Patch[Todo]] {
            val title = !Bind(Header.newTodo)
            (snapshot: Snapshot[Todo]) =>
              BindingSeq.Patch.Splice(
                snapshot.measure.getOrElse(0),
                0,
                View.Single(Todo(title, isInitiallyCompleted = false))
              )
          })
          .mergeWith(
            allTodos.zipWithIndex
              .mergeMap { (todo, indexBinding) =>
                Binding.events[Snapshot[Todo] => BindingSeq.Patch[Todo]] {
                  val index = !Bind(indexBinding)
                  !Await(todo.onDestroy)
                  (snapshot: Snapshot[Todo]) =>
                    BindingSeq.Patch.Splice[Todo](
                      index,
                      1,
                      View.Empty
                    )
                }
              }
          ))
      )
  enum TodoList(hash: String, items: BindingSeq[Todo]):
    case All extends TodoList("#/", allTodos)
    case Active
        extends TodoList(
          "#/active",
          // TODO: Drop history of BindingSeq
          allTodos.flatMap { todo =>
            BindingSeq.fromIterableCovariantStreamT(
              Binding {
                if (!Bind(todo.isCompleted)) {
                  View.Empty
                } else {
                  View.Single(todo)
                }
              }
            )
          }
        )
    case Completed
        extends TodoList(
          "#/completed",
          // TODO: Drop history of BindingSeq
          allTodos.flatMap { todo =>
            BindingSeq.fromIterableCovariantStreamT(
              Binding {
                if (!Bind(todo.isCompleted)) {
                  View.Single(todo)
                } else {
                  View.Empty
                }
              }
            )
          }
        )

  // TODO: The data structure might be very different from Binding.scala, with the help of LatestEvent we have
  // final case class Todo(val title: String, val completed: Boolean)
  // object Todo {
  //   given ReadWriter[Todo] = ReadWriter.derived[Todo]
  // }

  // final case class TodoList(text: String, hash: String, items: BindingSeq[Todo])

  // object Models {
  //   val LocalStorageName = "todos-binding.scala"
  // def load() = LocalStorage(LocalStorageName).toSeq.flatMap(read[Seq[Todo]](_))
  //   def save(todos: collection.Seq[Todo]) = LocalStorage(LocalStorageName) = write(todos)

  // val allTodos = Vars[Todo](load(): _*)

  //   val autoSave: Binding[Unit] = allTodos.all.map(save)
  //   autoSave.watch()

  //   val editingTodo = Var[Option[Todo]](None)

  //   val all = TodoList("All", "#/", allTodos)
  //   val active = TodoList("Active", "#/active", for (todo <- allTodos if !todo.completed) yield todo)
  //   val completed = TodoList("Completed", "#/completed", for (todo <- allTodos if todo.completed) yield todo)
  //   val todoLists = Vector(all, active, completed)
  //   val route = Route.Hash(all)(new Route.Format[TodoList] {
  //     override def unapply(hashText: String) = todoLists.find(_.hash == window.location.hash)
  //     override def apply(state: TodoList): String = state.hash
  //   })
  //   route.watch()
// }
// import Models._

// @html def todoListItem(todo: Todo): Binding[Node] = {
//   // onblur is not only triggered by user interaction, but also triggered by programmatic DOM changes.
//   // In order to suppress this behavior, we have to replace the onblur event listener to a dummy handler before programmatic DOM changes.
//   val suppressOnBlur = Var(false)
//   def submit = { event: Event =>
//     suppressOnBlur.value = true
//     editingTodo.value = None
//     event.currentTarget.asInstanceOf[HTMLInputElement].value.trim match {
//       case "" =>
//         allTodos.value.remove(allTodos.value.indexOf(todo))
//       case trimmedTitle =>
//         allTodos.value(allTodos.value.indexOf(todo)) = Todo(trimmedTitle, todo.completed)
//     }
//   }
//   def keyDownHandler = { event: KeyboardEvent =>
//     event.keyCode match {
//       case KeyCode.Escape =>
//         suppressOnBlur.value = true
//         editingTodo.value = None
//       case KeyCode.Enter =>
//         submit(event)
//       case _ =>
//     }
//   }
//   def blurHandler = Binding[Event => Any] { if (suppressOnBlur.bind) Function.const(()) else submit }
//   def toggleHandler = { event: Event =>
//     allTodos.value(allTodos.value.indexOf(todo)) = Todo(todo.title, event.currentTarget.asInstanceOf[HTMLInputElement].checked)
//   }
//   val editInput = <input id="editInput" class="edit" value={ todo.title } onblur={ blurHandler.bind } onkeydown={ keyDownHandler } />;
//   <li class={s"${if (todo.completed) "completed" else ""} ${if (editingTodo.bind.contains(todo)) "editing" else ""}"}>
//     <div class="view">
//       <input class="toggle" type="checkbox" checked={todo.completed} onclick={toggleHandler}/>
//       <label ondblclick={ _: Event => editingTodo.value = Some(todo); editInput.value.focus() }>{ todo.title }</label>
//       <button class="destroy" onclick={ _: Event => allTodos.value.remove(allTodos.value.indexOf(todo)) }></button>
//     </div>
//     {editInput}
//   </li>
// }

// @html def mainSection: Binding[Node] = {
//   def toggleAllClickHandler = { event: Event =>
//     for ((todo, i) <- allTodos.value.zipWithIndex) {
//       if (todo.completed != event.currentTarget.asInstanceOf[HTMLInputElement].checked) {
//         allTodos.value(i) = Todo(todo.title, event.currentTarget.asInstanceOf[HTMLInputElement].checked)
//       }
//     }
//   }
//   <section class="main" style={ if (allTodos.length.bind == 0) "display:none" else "" }>
//     <input type="checkbox" id="toggle-all" class="toggle-all" checked={active.items.length.bind == 0} onclick={toggleAllClickHandler}/>
//     <label for="toggle-all">Mark all as complete</label>
//     <ul class="todo-list">{ for (todo <- route.state.bind.items) yield todoListItem(todo).bind }</ul>
//   </section>
// }
  lazy val mainSection =
    html"""<ul class="todo-list">${allTodos.flatMap(_.html)}</ul>"""

  def footer: BindingSeq[Node] =
    // def clearCompletedClickHandler = { (_: Event) =>
    //   allTodos.value --= (for (todo <- allTodos.value if todo.completed) yield todo)
    // }
    html"<br><br>"
  // html"""
  //   <footer class="footer" style={ if (allTodos.length.bind == 0) "display:none" else "" }>
  //     <span class="todo-count">
  //       <strong>{ active.items.length.bind.toString }</strong> { if (active.items.length.bind == 1) "item" else "items"} left
  //     </span>
  //     <ul class="filters">{
  //       for (todoList <- todoLists) yield {
  //         <li>
  //           <a href={ todoList.hash } class={ if (todoList == route.state.bind) "selected" else "" }>{ todoList.text }</a>
  //         </li>
  //       }
  //     }</ul>
  //     <button class="clear-completed" onclick={clearCompletedClickHandler}
  //             style={if (completed.items.length.bind == 0) "visibility:hidden" else "visibility:visible"}>
  //       Clear completed
  //     </button>
  //   </footer>
  // """

  render(
    container,
    html"""
      <section class="todoapp">${Header.html}$mainSection$footer</section>
      <footer class="info">
        <p>Double-click to edit a todo</p>
        <p>Written by <a href="https://github.com/atry">Yang Bo</a></p>
        <p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
      </footer>
    """
  )

// def spinner(initialValue: Int): (Binding[Int], NodeBinding[HTMLDivElement]) =
//   val (readMinusEvent, writeMinusEvent) = Binding.jsPipe[MouseEvent]
//   val (readPlusEvent, writePlusEvent) = Binding.jsPipe[MouseEvent]
//   import com.thoughtworks.binding.Binding.scanLeft
//   val i: Binding[Int] = readMinusEvent
//     .map(Function.const(-1))
//     .mergeWith(readPlusEvent.map(Function.const(1)))
//     .scanLeft(initialValue)(_ + _)
//   i -> html"""<div>
//     <button onclick=$writeMinusEvent>-</button>
//     ${(!Bind(i)).toString}
//     <button onclick=$writePlusEvent>+</button>
//   </div>"""

// def spinnerApp =
//   val (i, spinnerNode) = spinner(0)
//   html"""
//     <div>
//       $spinnerNode
//       <hr>
//       控件当前值是：${(!Bind(i)).toString}
//     </div>
//   """
// render(container, spinnerApp)
