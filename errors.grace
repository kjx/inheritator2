//Error handling for ng (or whatever we call it. Murray. GUGS. ng"

def errors = self

class exports {

  method error (str) {  //signal error in target program
      interpreterError.raise(str)
  }

  method crash (str) {  //signal error in 
      interpreterError.raise(str)
  }

  method assert (block) { assert(block) because "" }

  method assert (value) equals (desired) {
      if ((value == desired).not) then {error "{value} shouldBe {desired}"}
  }
  method assert (block) because (str) {
      if (!block.apply) then {error "Assertion failed: {str} {block}"}
  }

  def interpreterError = errors.interpreterError
}

def interpreterError is public  = Exception.refine "interpreterError"

