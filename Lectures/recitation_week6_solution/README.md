# Week 6 Recitation, Spring 2023

This week, we'll look at:
- how to work with Scala's build tool (sbt) for working with local projects
- Writing/Running unit tests
- Importing Libraries

To do so, we'll finish an implementation of a simple pantry app,
with the following features:

Print out inventory
Update quantity of an item
Load inventory from json

## Quick reference for sbt:

- build.sbt - project definitions, dependencies
- `sbt run` - builds and runs the program
- `sbt test` - builds program, runs tests
- `sbt compile` - build the program

## Recap - Curried Functions

A curried function is a function that can be given some of its arguments, and returns a function that expects the remaining arguments.

As a simple example, `add` takes 2 numbers, and adds them. However, we can call `add` with one argument, and get a function that expects the second argument.

```scala 
val add(x:Int)(y:Int): = {
    x + y
}

// We can partially apply this by calling:
val add_five = add(5)(_)
```

## Example - Adding,using, and testing a library using SBT

When j is entered as a command, we want to allow the user to provide a path to a JSON file containing an inventory to load and use as our current pantry stock. 

To do so, we could make our own JSON parser, but this is a common format, and we want to reuse code rather than reinventing the wheel and having to test our parser too.

For this, we'll use the `circe` library in Scala (v. 0.14.1). [Link](https://circe.github.io/circe/)

To add a library, we modify build.sbt, and add it as a library dependency (see build.sbt)

We then import the necessary packages (see Pantry.scala), and can use the library.

## Exercise - Printing out the inventory

### Part A - Implementation
When a user enters l as a command, we want to allow the user to enter a character indicating how to sort, and sort based on this.
The set of valid flags is given, so let's implement this command.

### Part B - Testing
Now that we've implemented the command, let's test that showing the list sorted does not modify our actual inventory.

To do this, add a test in PantrySpec.scala

### Part C - Testing Continued

Now that we know how to write tests, and have seen 2, let's test another functionality of our project, the updating of an element

Let's add another test to PantrySpec.scala


## Pantry App Commands
For reference, all commands of the pantry app are collected here

- `l`: list the items in stock. This command then expects another parameter, for how to sort the list.
    - `n`: sort by name
    - `q`: sort by quantity
    - `i`: list in order (by id)
- `j`: Load new inventory from a `.json` file, discarding the previous inventory. This command then expects a file to load.
- any index: Given a number, if it is a valid index, the command will expect a new quantity, and update the item at that index.
- `quit`: exits the app.