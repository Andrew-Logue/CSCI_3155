package edu.colorado.csci3155.project1
import scala.language.implicitConversions // required to define an implicit conversion

sealed trait Expr {
    /*-- some operator overloading to support writing test cases easily --*/
    def + (e2: Expr) = Plus(this, e2)
    def - (e2: Expr) = Minus(this, e2)
    def * (e2: Expr) = Mult(this, e2)
    def / (e2: Expr) = Div(this, e2)
    def >= (e2: Expr) = Geq(this, e2)
}

case class Const(f: Double) extends  Expr
case class BoolConst(b: Boolean) extends Expr 
case class Ident(id: String) extends Expr
case class Plus(e1: Expr, e2: Expr) extends  Expr
case class Minus(e1: Expr, e2: Expr) extends  Expr
case class Mult(e1: Expr, e2: Expr) extends  Expr
case class Div(e1: Expr, e2: Expr) extends  Expr
case class Exp(e:Expr) extends Expr
case class Log(e: Expr) extends Expr
case class Sine(e: Expr) extends Expr
case class Cosine(e:Expr) extends Expr
case class Geq(e1: Expr, e2: Expr) extends Expr 
case class Eq(e1: Expr, e2: Expr) extends Expr 
case class And(e1:Expr, e2: Expr) extends Expr 
case class Or (e1: Expr, e2: Expr) extends Expr 
case class Not(e: Expr) extends Expr 
case class IfThenElse(cond: Expr, tExpr: Expr, elseExpr: Expr) extends Expr 
case class Let(ident: String, e1: Expr, e2: Expr) extends Expr

/* -- ignore code below -- it is meant for the simple test case writing DSL --*/
object Conversions { 
    implicit def to_expr(s: String) = Ident(s)
    implicit def to_const (f: Double) = Const(f)
}

case class If (cond: Expr) {
    def Then (e: Expr) = IfThen(cond, e)
}

case class IfThen(cond: Expr, e: Expr) {
    def Else (e2: Expr) = IfThenElse(cond, e, e2)
}

case class Llet (x: String) {
    def ~(e: Expr) = { LetBindPart(x, e)}
}

case class LetBindPart(x: String, e: Expr) {
    def in (e2: Expr): Expr = Let(x, e, e2)
}