package edu.colorado.csci3155.project2
import swing._
import java.awt.image.BufferedImage

/* A class to maintain a canvas */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D, Color}

sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(x: Double, y: Double): Figure
    def rotate(mat: (Double, Double, Double, Double)): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
    def reflectX: Figure 
    def reflectY: Figure 
    def scale(s: Double): Figure 
}

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    override def getBoundingBox: (Double, Double, Double, Double) = {
       /* TODO: implement getBoundingBox */
       ???
    }
    override def translate(x: Double, y: Double): Polygon = {
        val newList = cList.map {case (xc,yc) => (xc+x, yc+y)}
        new Polygon(newList)

    }
    override def rotate(mat: (Double, Double, Double, Double)): Polygon = {
        val nList = cList.map { case (x, y ) => (mat._1 * x + mat._2 * y, mat._3 * x + mat._4 * y)}
        new Polygon(nList)
    }

    override def reflectX: Polygon = {
       /* Todo: implement reflectX */
       ???
    }

    override def reflectY: Polygon = {
        /* TODO */
        ???
    }


    override def scale(s: Double): Figure = {
        /* TODO */
        ???
    }

    /* WARNING: DO NOT EDIT */
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }

}

case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    override def getBoundingBox: (Double, Double, Double, Double) = {
       /* TODO: implement getBoundingBox */
       ???
    }

    override def translate(x: Double, y: Double): MyCircle = {
        val ncenter = (c._1 + x, c._2 + y)
        new MyCircle(ncenter, r)
    }

    override def rotate(mat: (Double, Double, Double, Double)): MyCircle = {
        val newcenter = (c._1 * mat._1 + c._2 * mat._2, c._1 * mat._3 + c._2 * mat._4)
        new MyCircle(newcenter, r)
    }

    override def reflectX: MyCircle = {
        /* TODO */
        ???
    }

    override def reflectY: Figure = {
        /* TODO */
        ???
    }

    override def scale(s: Double): Figure = {
        /* TODO */
        ???
    }

    /* WARNING: DO NOT EDIT */
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

class MyCanvas (val listOfObjects: List[Figure]) {
    def getBoundingBox: (Double, Double, Double, Double) = {
        /* TODO */
        ??? 

    }

    def translate(shiftX: Double, shiftY: Double) = {
        val newList = listOfObjects.map {(f)=> f.translate(shiftX, shiftY)}
        new MyCanvas(newList)
    }

    def placeRight(myc2: MyCanvas):MyCanvas = {
        /* TODO */
        ???
    }

    def placeTop(myc2: MyCanvas): MyCanvas = {
        /* TODO */
        ???
    }

    // 2D Rotation about a point (x,y)
    // x' = xcos(theta) - ysin(theta), y' = ycos(theta) + xsin(theta)
    def rotate(angRad: Double): MyCanvas = {
        val mat = (math.cos(angRad), -math.sin(angRad), math.sin(angRad), math.cos(angRad))
        val newList = listOfObjects.map {(f) => f.rotate(mat) }
        new MyCanvas(newList)
    }

    def reflectX: MyCanvas = {
        /* TODO */
        ???
    }

    def reflectY: MyCanvas = {
        /* TODO */
        ???
    }


    /* WARNING: DO NOT EDIT */
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
         //g.setColor(Color.WHITE)
         //g.fillRect(0, 0, g.getWidth, g.getHeight)
        // enable anti-aliased rendering (prettier lines and circles)
        // Comment it out to see what this does!
        g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
		   java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
        g.setColor(Color.BLACK)
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1+1
        val shifty = -uy1-1
        val scaleX = xMax/(ux1 - lx1 + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        //println(scaleX, scaleY)
        val scale = math.min(scaleX, scaleY)
        listOfObjects.foreach(f => f.render(g,scale, -scale, shiftx, shifty))
    }

    def scale(s: Double): MyCanvas = {
        /* TODO */
        ???
    }
    
    def overlap(c2: MyCanvas): MyCanvas = {
        /* TODO */
        ???
    }

    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }

    def getListOfObjects: List[Figure] = listOfObjects

    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }

    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }

    def renderImage(filename: String) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
       
        val sx = (ux1 - lx1 + 1.0).toInt
        val sy = (uy1 - ly1 + 1.0).toInt
        val t = math.min(1200/sx, 1200/sy).toInt
        val size = (t *  sx, t * sy)
        // create an image
        val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
        // get Graphics2D for the image
        val g = canvas.createGraphics()
        // clear background
        g.setColor(Color.WHITE)
        g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
        // enable anti-aliased rendering (prettier lines and circles)
        // Comment it out to see what this does!
        g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
		   java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
        g.setColor(Color.BLACK)
        this.render(g, size._1, size._2)
       
        // done with drawing
        g.dispose()

        // write image to a file
        javax.imageio.ImageIO.write(canvas, "png", new java.io.File(filename))
    }
}
