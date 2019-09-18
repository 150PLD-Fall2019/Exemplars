Troll - User Story
My user is a dungeon master, DM, for a dungeons and dragons group.
They are designing an enemy and want to know what the chances are their players
will hit the enemy using their most typical actions.

Player X's most typical action has them roll a d20 and add 5 proficency to hit.
If X misses, then they can expend one of their skill points to add a d4 to the roll.

The DM goes to the web interface for troll.
They want to know how likely X will hit the enemy if the enemy has an armor class
of 18. In this case, X can hit the enemy if their roll and modifies sum to 18 or 
higher.

The DM enters the expression

```
roll := 1d20 + 5 ; if 18 > roll then roll + 1d4 else roll
``` 
and clicks "Calculate probabilities".
From the table that is displayed, they see the average value the player will
roll is 17.6 with a spread of 4.8.
The DM doesn't recall statistics in detail, and becomes frustrated with 
parsing the graph, but they understand that most values are 4.8 away from
the mean of 17.6. so most rolls from X will be between 12.8 and 22.4. This
makes the enemy seem easy enough to hit at least half the time, 
and so the DM decides to include the enemy in their plans.


The DM notes the highest probability is 18, and anticipates that these
"barely hit" moments can make the encounter more exciting.
