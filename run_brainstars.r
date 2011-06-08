source("brainstars.r")

# Search API
receptors     <- getBrainStarsSearch("receptor/10,5")
num.receptors <- getBrainStarsSearch("receptor/count")
cat(receptors, "\n")
cat(num.receptors, "\n")

# Marker API
r.json <- getBrainStarsMarker("high/LS/count")
cat(r.json, "\n")

# Expression API
expression <- getBrainStarsExpression("1439627_at") 
print(expression)

# Graph API
getBrainStarsFigure("1439627_at", "exprgraph")
getBrainStarsFigure("1439627_at", "exprmap")
getBrainStarsFigure("1439627_at", "switchgraph")
#getBrainStarsFigure("1439627_at", "switchhist")
getBrainStarsFigure("1439627_at", "switchmap")