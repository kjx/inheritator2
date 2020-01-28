//Error handling for ng (or whatever we call it. Murray. GUGS. ng"

def errors = self

class exports {

  method warning (str) {  //signal warning in target program
                          //shouldn't really use this one
      print "WARNING: {str}"
  }

  method error (str) {  //signal error in target program
      interpreterError.raise(str)
  }

  method crash (str) {  //signal error in interpreter
      interpreterError.raise(str)
  }
  //these are currently done by mostly by calling error, should move to crash

  method assert (block) { assert(block) because "" }

  method assert (value) equals (desired) {
      if ((value == desired).not) then {crash "{value} shouldBe {desired}"}
  }
  method assert (block) because (str) {
      if (!block.apply) then {crash "Assertion failed: {str} {block}"}
  }

  def interpreterError = errors.interpreterError
}

def interpreterError is public  = Exception.refine "interpreterError"

