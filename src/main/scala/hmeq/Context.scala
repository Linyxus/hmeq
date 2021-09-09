package hmeq

class Context:
  private var tvarCount: Int = 0

  def freshTypeVarName: String =
    val result = "T$" + tvarCount.toString
    tvarCount += 1
    result

