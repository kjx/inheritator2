class top {
  method x { print "X" }
  method y { print "Y" }
}

class bot {
  inherit top
     exclude y
     alias y = x
}

bot.x
bot.y 

