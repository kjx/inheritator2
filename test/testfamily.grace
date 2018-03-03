class jast {
  class node {
    print "making node"
    //method JAST_NODE
  }
  class inh {
    inherit node
    //method INH { }

    print "making inh" 
  }
}



class jeval {
  inherit jast
    alias jnode = node
    alias jinh = inh

  //method JEVAL { } 

  class node {
    inherit jnode
    //method JEVAL_NODE { } 
    print "making jnode"
    def xxxxx = 3 
  }
  class inh {
    inherit jinh
    print "making jinh" 
    print(xxxxx)
  }

}


//jast.inh


jeval.inh
