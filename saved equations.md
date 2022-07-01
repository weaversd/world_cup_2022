---
title: "saved equations"
author: "Simon D Weaver"
date: '2022-07-01'
output: html_document
---

$$W_{e} = \frac{1}{{10^\frac{-\delta r}{400}}+1}$$

where $W_{e}$ is winning expectancy, and $\delta r$ is the difference between the Elo ratings for the two teams. However, this doesn't take into account draws.   

$$\frac{1}{{10^\frac{-d}{400}}+1}-\frac{1}{{10^\frac{-d}{400}}+1} = 0.235$$

\begin{align*}
d = & 400*ln \left(-\frac{0.235 + 1}{0.235 - 1}\right) \\
d = & 83.202
\end{align*}  

Then the winning expectancy for the team with the higher Elo rating is:
$$W_{ea} = \frac{1}{{10^\frac{83.202-\delta r}{400}}+1}$$

and the winning expectancy for the team with the lower Elo rating is:
$$W_{eb} = \frac{1}{{10^\frac{83.202+\delta r}{400}}+1}$$

The draw expectancy is simply:

$$D_{e}=1-(W_{ea}+W_{eb})$$