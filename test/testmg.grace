class sup {
   method x {"top-x"}
}

class mid {
   inherit sup
   method test {x}
}


class bot {
   inherit mid
   method x {"bot-x"}
}

print (bot.test)
