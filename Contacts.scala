import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.io._

object Contacts {
  def main(args: Array[String]) {
    val input = "contacts.vcf"
    val output = "addressbook"
    var writer = new PrintWriter(new FileWriter(output));
    var count = 0
    var emails = new ListBuffer[String]()
    var homenumbers = new ListBuffer[String]()
    var worknumbers = new ListBuffer[String]()
    var mobilenumbers = new ListBuffer[String]()
    var name = ""

    try {
      writer.write("\n[format]\nprogram=abook\n")
      for (line <- Source.fromFile(input).getLines()) {
        if (line.startsWith("FN")) {
          val names = line.split(":")
          if (names.length > 1) {
            name = names(1)
            writer.write("\n[%" + count + "]\n")
            writer.write("name=" + name + "\n")
          }
        }
        if (line.startsWith("EMAIL")) {
          val emailaddresses = line.split(":")
          if (emailaddresses.length > 1) {
            val email = emailaddresses(1)
            emails += email
          }
        }
        if (line.startsWith("TEL")) {
          val phone = line.split(":")(1)
          val parts = line.split(";")
          if (parts.length > 2) {
            // Fax
            val faxes = parts(2).split(":")
            if (faxes.length > 1) {
              val fax = faxes(1)
              writer.write("fax=" + fax + "\n")
            }
          } else if (parts.length > 1) {
            val tel = parts(1)
            val fulltype = tel.split("=")
            val phonetype = fulltype(1).split(":")(0)
            phonetype match {
              case "HOME" => homenumbers += phone
              case "CELL" => mobilenumbers += phone
              case "WORK" => worknumbers += phone
              case _ => println("Invalid phone type")
            }
          }
        }
        if (line.startsWith("NOTE")) {
          val notes = line.split(":")
          if (notes.length > 2) {
            val note = notes(2)
            writer.write("nick=" + note + "\n")
          } else if (notes.length > 1) {
            val note = notes(1)
            writer.write("nick=" + note + "\n")
          }
        }
        if (line.startsWith("ORG")) {
          val orgs = line.split(":")
          if (orgs.length > 1) {
            val company = orgs(1)
            if (company != "") {
              if (name == "") {
                writer.write("name=" + company + "\n")
              } else {
                writer.write("custom1=" + company + "\n")
              }
            }
          }
        }
        if (line.startsWith("ADR")) {
          val adr_parts = line.split(";").zipWithIndex
          for ((part, index) <- adr_parts) {
            index match {
              case 2 => writer.write("address=" + part + "\n")
              case 3 => writer.write("address2=" + part + "\n")
              case 4 => writer.write("city=" + part + "\n")
              case 5 => writer.write("state=" + part + "\n")
              case 6 => writer.write("zip=" + part + "\n")
              case 7 => writer.write("country=" + part + "\n")
              case _ => 
            }
          }
        }
        if (line.startsWith("BDAY")) {
          val bday_parts = line.split(":")
          if (bday_parts.length > 1) {
            writer.write("anniversary=" + bday_parts(1) + "\n")
          }
        }
        if (line.startsWith("END")) {
          if (emails.toList.length > 0) {
            writer.write("email=" + emails.toList.mkString(",") + "\n")
          }
          if (homenumbers.toList.length > 0) {
            writer.write("phone=" + homenumbers.toList.mkString(",") + "\n")
          }
          if (mobilenumbers.toList.length > 0) {
            writer.write("mobile=" + mobilenumbers.toList.mkString(",") + "\n")
          }
          if (worknumbers.toList.length > 0) {
            writer.write("work=" + worknumbers.toList.mkString(",") + "\n")
          }
          count += 1
          emails = new ListBuffer[String]()
          homenumbers = new ListBuffer[String]()
          worknumbers = new ListBuffer[String]()
          mobilenumbers = new ListBuffer[String]()
        }
      }
      println("Processed " + count + " contacts")
    } catch {
      case ex: FileNotFoundException => println("Could not find file: " + input)
      case ex: IOException => println("Unable to open input file: " + input)
    } finally {
      writer.close()
    }
  }
}
