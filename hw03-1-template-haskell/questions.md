1. What is compile-time meta-programming?

2. What can compile-time meta-programming be used for?

3. What is a splice and how is it notated in this paper?

4. What is a quasi-quotation and how is it notated?

5. How does the gen function work? How would you modify the definition of gen to support floats as an
additional kind of format instructions?

6. Why can’t we write the function _sel_ using the quotation notation?

7. What is reification and why is it useful?

8. Explain what the problem is in the cross2b function and why cross2a doesn’t have the same problem.

9. Why is the code f x = $(zipN x) illegal at the top-level?

10. What happens, under what conditions, and when if a Template Haskell 
program can diverge for some input.

11. Explain how Haskell processes groups of declarations that contain splices and why it adopts this
approach.

12. Explain how the function qIO :: IO a -> Q a makes it so compiling your program can delete your
entire file store.

13. What are original names and what are they used for?
