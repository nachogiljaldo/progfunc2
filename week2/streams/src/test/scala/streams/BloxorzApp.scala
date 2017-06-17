package streams

object BloxorzApp extends App {
  abstract class Level extends Solver with StringParserTerrain
/*
  object Level0 extends Level {
    val level: String =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println(Level0.solution.take(1).toList)
*/
  object Level1 extends Level {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  println(Level1.solution)
}
