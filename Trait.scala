trait FatCat {
    def getFat() 
    {
        println("I consume memory and do nothing.")
    }
}

class Frog extends FatCat
{
    override def toString = "Green"
}

val frog = new Frog
frog.getFat()
