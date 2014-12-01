import Element.elem

abstract class Element {
    def contents: Array[String]
    
    def height = contents.length
    def width = if (height == 0) 0 else contents(0).length

    def above(that: Element) =
    {
        val this1 = this widen that.width
        val that1 = that widen this.width
        elem(this1.contents ++ that1.contents)
    }

    def beside(that: Element) =
    {
        val this1 = this heighten that.height
        val that1 = that heighten this.height
        elem(
            for ((line1, line2) <- this.contents zip that.contents)
                yield line1 + line2
        )
    }

    def widen(w: Int) : Element =
    {
        if (w <= width) this
        else
        {
            val left = elem(' ', (w - width) / 2, height)
            val right = elem(' ', w - width - left.width, height)
            left beside this beside right
        }
    }

    def heighten(h: Int) : Element =
    {
        if (h <= height) this
        else
        {
            val top = elem(' ', width, (h - height) / 2)
            val bottom = elem(' ', width, h - height - top.height)
            top above this above bottom
        }
    }

    override def toString = contents mkString "\n"
}

// companion object Element
object Element
{
    def elem(contents:Array[String]):Element = new ArrayElement(contents)

    def elem(ch:Char, width:Int, height:Int):Element = new UniformElement(ch, width, height)

    def elem(line:String):Element = new LineElement(line)
}

class ArrayElement(conts: Array[String]) extends Element {
    def contents: Array[String] = conts
}

class UniformElement(ch:Char,
                     override val width:Int,
                     override val height:Int) extends Element
{
    private val line = ch.toString * width
    def contents = Array.fill(height)(line)
}

class LineElement(s:String) extends Element {
    override val contents = Array(s)
    override def width = s.length
    override def height = 1
}

println(elem(Array("abc", "def")))
