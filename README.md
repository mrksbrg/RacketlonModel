# RacketlonModel
Model of racketlon matches

Use ratings from the English Racketlon Association. I recommend the ggplot2 package for visualizing the results.

Example usage:
> result <- play_match(100000, 15546,	17022,	16379,	14126, 16296,	16861, 16119,	15096)
> library(ggplot2)
> qplot(result, binwidth=1)
