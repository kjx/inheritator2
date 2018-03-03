class firstFamily {
   class top {
     print "firstTop"
   }
   class bot {
     inherit top
     print "firstbot"
   }
}

class secondFamily {
   inherit firstFamily
      alias firstBot = bot
   class top {
     print "secondtop"
   }
}

secondFamily.bot
