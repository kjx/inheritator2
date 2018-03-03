class top {
  method x { print "X" }
  method y { print "Y" }
}

class bot {
  inherit top
     alias y = x 
     alias x = y 
}

bot.x
bot.y 

