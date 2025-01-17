# a python script
import pandas
import seaborn
import matplotlib
f = "https://raw.githubusercontent.com/difiore/ADA-datasets/master/zombies.csv"
z = pandas.read_csv(f, index_col=0)
print(z.head())
seaborn.boxplot(x="gender", y="weight", data=z)
p = seaborn.swarmplot(x="gender", y="weight", data=z, size=2, color=".3", linewidth=0)
