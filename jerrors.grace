//Error handling for ng (or whatever we call it. Murray. GUGS. ng"

def errors = self

class exports {

  method error (str) { 
      interpreterError.raise(str)
  }

  method assert (block) { assert(block) because "" }

  method assert (block) because (str) {
      if (!block.apply) then {error "Assertion failed:{str} {block}"}
  }

  def interpreterError = errors.interpreterError
}

def interpreterError is public  = Exception.refine "interpreterError"

