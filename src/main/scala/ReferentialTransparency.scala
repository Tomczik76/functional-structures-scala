object ReferentialTransparency {


  val program1 = {
    val x = 0
    def addRT(a: Int, b: Int):Int = a + b
    addRT(1, 2) + x 
  }  // 3

  val program2 = {
    val x = 0
    3 + x
  }  // 3

  assert(program1 == program2)

  val program3 = {
    var x = 0
    def addNotRT(a: Int, b: Int):Int = {
      x = x + 1
      a + b
    }
    addNotRT(1, 2) + x
  }  // 4

  val program4 = {
    var x = 0
    3 + x
  }  // 3

  assert(program3 != program4)

}