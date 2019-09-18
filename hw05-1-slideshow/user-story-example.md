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
roll := 1d20 ; if 18 > roll then roll + 1d4 else roll
`` 
and clicks "Calculate probabilities".
From the table that is displayed, they see the average value the player will
roll is 12.6 with a spread of 5.3.
The DM doesn't recall statistics in detail, and becomes frustrated with 
parsing the graph, but they understand that most values are 5.3 away from
the mean of 12.6. so most rolls from X will be between 7.3 and 17.9. This
makes the enemy seem very difficult to hit, and so the DM decides to adjust
the enemy's armor class.
