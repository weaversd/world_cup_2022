---
title: "How this works"
output: html_document
---
<style>
.center1 {
  display: block;
  margin-left: auto;
  margin-right: auto;
  width: 20%;
}

.center2 {
  display: block;
  margin-left: auto;
  margin-right: auto;
  width: 25%;
}
</style>


## Code
Source code can be found on [GitHub](https://github.com/weaversd/world_cup_2022)  

## How to use 
### Schedule  
The games will be updated as soon as they finish and the scores become official. For custom use, download, edit, and upload the scores template. Only edit the scores and penalty columns. You can also fill in knock-out stage teams in the home and away columns. The uploaded file will auto-fill the results in relevant tabs, and subsequent simulations will take those scores into account. This can be used to see how your team's chances change based on other results.  

### Simulations  
Simulations are based on the Elo scores for each team (see method below). To provide custom Elo scores, download, edit, and upload the Elo template in the schedule tab. The updated Elo scores will be used for any subsequent simulations.

## Simulation Method
To predict the chances that each team makes it to different stages in the tournament, this app will simulate the tournament *n* times, and count the results each time. Each tournament simulation boils down to single game simulations. Single game simulations are based on [Elo scores](https://en.wikipedia.org/wiki/World_Football_Elo_Ratings), using a method that includes the possibility for draws, described in [these](http://soccerwizardbetting.blogspot.com/2015/03/predicting-draws-with-elo-model-part-1.html) [two](http://soccerwizardbetting.blogspot.com/2015/04/predicting-draws-with-elo-model-part-2.html) blog posts. The default Elo scores are pulled from [eloratings.net](https://www.eloratings.net/2022_World_Cup), which uses a method similar to the FIFA world rankings, but that takes into account goal difference, home field advantage, and significance of games.

The formula for determining winning expectancy for the team with the higher Elo ranking is:


<img src="www/eq_1.png" alt="drawing" width="100" class="center1"/>  

where *W<sub>e</sub>* is winning expectancy, and *dr* is the difference between the Elo ratings for the two teams. However, this doesn't take into account draws.   

To allow for the possibility of games ending in a tie, I used the draw rate of all world cup games from 2002 to 2018, which was 23.5%. I than back calculated a *d* value that was scaled to the Elo ratings which is essentially the 'gray' area in between winning and losing.


<img src="www/eq_2.png" alt="drawing" width="100" class="center2"/>
Simplifying gives:

<img src="www/eq_3.png" alt="drawing" width="100" class="center2"/> 

Then the winning expectancy for the team with the higher Elo rating is:

<img src="www/eq_4.png" alt="drawing" width="100" class="center1"/>

and the winning expectancy for the team with the lower Elo rating is:

<img src="www/eq_5.png" alt="drawing" width="100" class="center1"/>

The draw expectancy is simply:

<img src="www/eq_6.png" alt="drawing" width="100" class="center1"/>


Therefore, these equations were used to predict the result of each game based on the Elo scores of each team. 

To assign a goal value to each game, I calculated the percent of World Cup games from 2002 to 2018 that ended with a total goal value of each value, 0-8. Then for each predicted game, I randomly assigned a goal total based on the historical rates for each total goal number. Then based on the game outcome (win, loss, or draw) I randomly picked a score that worked for that outcome where the total goals added up to the chosen total. If the game was a knockout game and the score ended in a draw, I randomly chose a number between 2 and 5 for each team for their penalty score. If those two numbers were the same, I added one penalty to the home team, and declared them the winner.

Each game in the tournament that does not already have a score is simulated in this manner, and the outcome of the tournament is saved. After the total number of simulations is carried out, the results for each team are counted and divided by the total simulations, and percentage 'chance' for each outcome is obtained and can be plotted.  

To calculate some level of uncertainty, the total simulations are broken up into 10 blocks, and the outcome of each block is averaged, and the standard deviation of the 10 outcomes is calculated. These standard deviation values can be seen by hovering the mouse over each datapoint in the plots. 

-----

Created by Simon D. Weaver  
Graduate Student, University of Notre Dame
