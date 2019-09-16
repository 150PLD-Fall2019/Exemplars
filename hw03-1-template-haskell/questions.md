1. What is compile-time meta-programming?

2. What can compile-time meta-programming be used for?

3. What is a splice and how is it notated in this paper?

4. What is a quasi-quotation and how is it notated?

5. How is the code a metaprogram generates represented? What interface for writing metaprograms is presented to the user?

6. What is reification and why is it useful?

7. Explain what the problem is in the cross2b function and why cross2a doesn’t have the same problem.

8. Why is the code f x = $(zipN x) illegal at the top-level?

9. What happens, under what conditions, and when if a Template Haskell 
program can diverge for some input.

10. Explain how Haskell processes groups of declarations that contain splices and why it adopts this
approach.

11. Explain how the function qIO :: IO a -> Q a makes it so compiling your program can delete your
entire file store.

12. What are original names and what are they used for?
