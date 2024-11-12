package edu.colorado.csci3155.project1

import scala.annotation.tailrec



sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {
            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            case LoadEnv(s) => 
                val v = env.find(_._1 == s).get._2
                (v :: stack, env)
            case StoreEnv(s) => (stack.tail, (s, stack.head) :: env)
            case PopEnv => (stack, env.tail)
            case PushNumI(f) => (FloatValue(f) :: stack, env)
            case PushBoolI(b) => (BoolValue(b) :: stack, env)
            case AddI => stack match {
                case FloatValue(b) :: FloatValue(a) :: tail => (FloatValue(a + b) :: tail, env)
                case _ => throw new Exception("AddI: Expected two FloatValue on top of stack")
            }
            case SubI => stack match {
                case FloatValue(b) :: FloatValue(a) :: tail => (FloatValue(a - b) :: tail, env)
                case _ => throw new Exception("SubI: Expected two FloatValue on top of stack")
            }
            case MultI => stack match {
                case FloatValue(b) :: FloatValue(a) :: tail => (FloatValue(a * b) :: tail, env)
                case _ => throw new Exception("MultI: Expected two FloatValue on top of stack")
            }
            case DivI => stack match {
                case FloatValue(b) :: FloatValue(a) :: tail =>
                if (b == 0) {
                    throw new Exception("DivI: Division by zero")
                } 
                (FloatValue(a / b) :: tail, env)
                case _ => throw new Exception("DivI: Expected two FloatValue on top of stack")
            }
            case ExpI => stack match {
                case FloatValue(a) :: tail => (FloatValue(math.exp(a)) :: tail, env)
                case _ => throw new Exception("ExpI: Expected FloatValue on top of stack")
            }
            case LogI => stack match {
                case FloatValue(a) :: tail =>
                if (a <= 0) {
                    throw new Exception("LogI: Argument must be positive")
                }
                (FloatValue(math.log(a)) :: tail, env)
                case _ => throw new Exception("LogI: Expected FloatValue on top of stack")
            }
            case SinI => stack match {
                case FloatValue(a) :: tail => (FloatValue(math.sin(a)) :: tail, env)
                case _ => throw new Exception("SinI: Expected FloatValue on top of stack")
            }
            case CosI => stack match {
                case FloatValue(a) :: tail => (FloatValue(math.cos(a)) :: tail, env)
                case _ => throw new Exception("CosI: Expected FloatValue on top of stack")
            }
            case GeqI => stack match {
                case FloatValue(b) :: FloatValue(a) :: tail => (BoolValue(a >= b) :: tail, env)
                case _ => throw new Exception("GeqI: Expected two FloatValue on top of stack")
            }
            case EqI => stack match {
                case FloatValue(b) :: FloatValue(a) :: tail => (BoolValue(a == b) :: tail, env)
                case _ => throw new Exception("EqI: Expected two FloatValue on top of stack")
            }
            case NotI => stack match {
                case BoolValue(a) :: tail => (BoolValue(!a) :: tail, env)
                case _ => throw new Exception("NotI: Expected BoolValue on top of stack")
            }
            case PopI => stack match {
                case _ :: tail => (tail, env)
                case _ => throw new Exception("PopI: Stack underflow")
            }
            
            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}