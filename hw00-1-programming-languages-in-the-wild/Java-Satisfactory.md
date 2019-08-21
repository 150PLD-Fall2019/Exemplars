# Programming Languages in the Wild
1) For discussion, you chose the programming language: Java 

2) This language is: General Purpose

3) Is this language Turing complete?:  Yes

## Uses 
4) For what killer app, projects, or historical reason is this language best known or used?: 
Java is best known for its virtual machine. 
The Java virtual machine allows the same program to be used on multiple devices
and architectures with modification. 
Java programmers expect consistent behavior of their programs despite architectural differences. 
The virtual machine includes a garbage collector that handles memory management for the programmer;
the garbage collector releases memory occupied by objects that are no longer reachable from the executing program.

5) Is the answer to 4, which the language is known for, due to language features, community support or libraries, or something else?:
The portability of Java depends on both the compilation to Java Byte Code -- an intermediate representation (IR) -- and to support for versions of the virtual machine to execute the IR on different architectures. To achieve consistent behavior, the maintainers of Java also maintain a standard library to act as a unifying basis for most Java programs. To support efficient garbage collection, the maintainers of Java and the research community at invest in designing and improving garbage collection algorithms.

6) As a programmer, what is easy to do in the language?:
As anobject-oriented language, it is easy to express heirarchies of code that depend on one another either through inheritance or encapsulation. An object inherits code from a parent object when its corresponding class sub-types the parent object's class.
An object encapsulates another object when it contains that object as a field, typically private, and delegates to that object field as necessary. The Java type system supports these features, and help check and enforce whether an object will have any particular code to reuse statically.   

7) What does the method to do the answer to 6 look like in general purpose languages like c or java?:
In languages without objects, like c, you must use functions to reuse code and structs to establish data heirarchies. 
The burden is put on the programmer to create a design pattern for how they are reusing or passing code around, either by name
or with function pointers. 
These patterns must also be checked and enforced by the programmer instead of by a type system.


8) Does the programmer have to give up anything to gain the benefits of this language?:
To simplify the type system, Java does not recommend you break the class and object abstractions. 
So, the fun pointer arithmetic and other tricks that are available to programmers in c are unavailable behind the abstraction boundry. Also, unlike c++, Java does not allow for multiple inheritance (multiple parent classes) to avoid potential namespace collisions.
In idiomatic Java, algorithms become spread across multiple classes in order to best utilize reuse while allowing for customizability. In functional languages, customizability would be handled by passing functions as first class parameters. However, recent versions of Java have added first class functions, albeit with an unweildy syntax when compared to popular functional languages when having to represent the feature with class and object analogs. 
 
