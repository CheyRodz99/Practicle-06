object Q2 {
  import scala.io.StdIn.readLine

  def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
    if (name.trim.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (!marks.forall(_.isDigit) || !totalMarks.forall(_.isDigit)) {
      (false, Some("Marks and total marks should be positive integers."))
    } else if (marks.toInt > totalMarks.toInt || marks.toInt < 0 || totalMarks.toInt <= 0) {
      (false, Some("Marks should be positive and not exceed total possible marks. Total possible marks should be positive."))
    } else {
      (true, None)
    }
  }

  def getStudentInfo: (String, Int, Int, Double, Char) = {
    val name = readLine("Enter student's name: ")
    val marks = readLine("Enter student's marks: ")
    val totalMarks = readLine("Enter total possible marks: ")

    val validation = validateInput(name, marks, totalMarks)

    if (!validation._1) {
      println(validation._2.getOrElse("Invalid input. Please try again."))
      getStudentInfoWithRetry
    } else {
      val marksInt = marks.toInt
      val totalMarksInt = totalMarks.toInt
      val percentage = (marksInt.toDouble / totalMarksInt) * 100
      val grade = percentage match {
        case p if p >= 90 => 'A'
        case p if p >= 75 => 'B'
        case p if p >= 50 => 'C'
        case _ => 'D'
      }
      (name, marksInt, totalMarksInt, percentage, grade)
    }
  }

  def getStudentInfoWithRetry: (String, Int, Int, Double, Char) = {
    var validData = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

    while (!validData) {
      val name = readLine("Enter student's name: ")
      val marks = readLine("Enter student's marks: ")
      val totalMarks = readLine("Enter total possible marks: ")

      val validation = validateInput(name, marks, totalMarks)
      if (validation._1) {
        validData = true
        val marksInt = marks.toInt
        val totalMarksInt = totalMarks.toInt
        val percentage = (marksInt.toDouble / totalMarksInt) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        studentInfo = (name, marksInt, totalMarksInt, percentage, grade)
      } else {
        println(validation._2.getOrElse("Invalid input. Please try again."))
      }
    }

    studentInfo
  }

  def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = student
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  def main(args: Array[String]): Unit = {
    val studentInfo = getStudentInfoWithRetry
    printStudentRecord(studentInfo)
  }
}
