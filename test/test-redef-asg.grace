print "STARTING"
object {

  var x := 5
  method x:=(new) { print "assignment to x blocked" }
  x := 3
  print "x = {x}"
  print "DONE"

}
