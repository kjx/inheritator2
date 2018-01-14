//Error handling for ng (or whatever we call it. Murray. GUGS. ng"

def errors = self

class exports {

  method error (string) { 
      interpreterError.raise(string)
  }

  method assert (block) {
      if (!block.apply) then {error "Assertion failed: {block}"}
  }

  def interpreterError = errors.interpreterError
}

def interpreterError is public  = Exception.refine "interpreterError"

