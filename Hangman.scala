package grape.demo

import scala.io.StdIn

object Hangman extends App {

  class Word(val actualWord: String) {
    val letterList: List[Letter] = actualWord.map(x => Letter(x, visible = false)).toList

    def enhance(letterList: List[Letter]): String = {
      letterList.map(letter => if (letter.visible) letter.value else '-')
        .map(_.toString)
        .reduce(_ + _)
    }

    def guessLetter(letter: Char): String = {
      if (isSolved) {
        return "Already solved!"
      }
      var modified = false
      letterList.foreach {
        case x if x.value == letter =>
          x.visible = true
          if (isSolved) return "Congratulations! Successfully solved!"
          modified = true
        case _ => ""
      }
      if (modified) enhance(letterList) else "Letter wasn't found"
    }

    def showPublicLetter(): String = {
      enhance(letterList)
    }

    def isSolved: Boolean = {
      letterList.forall(_.visible)
    }

    case class Letter(value: Char, var visible: Boolean)

  }

  val word = new Word("heeeello")
  println(word.showPublicLetter())
  while (!word.isSolved) {
    println("Enter guessing char: ")
    println(word.guessLetter(StdIn.readChar()))
  }
}
