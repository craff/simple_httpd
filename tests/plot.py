import csv
import matplotlib.pyplot as plt
import os

def bars(name, title):
    file= open(name)
    df = [list(r) for r in list(csv.reader(file))]

    fig, ax = plt.subplots(layout='constrained')
    offset = 0

    width = 0.25

    titles = [ line[0] for line in df[1:] ]
    labels = df[0][1:len(df[0]) - 1] # ignore max

    tbl  = [ line[1:] for line in df[1:] ]
    tbl = [  [ round (float(x[0:len(x)-2]) if x[len(x)-2:len(x)] == "µs"
               else float(x[0:len(x)-2]) * 1e3  if x[len(x)-2:len(x)] == "ms"
                      else float(x[0:len(x)-1]) * 1e6, 0) for x in line ] for line in tbl ]


    for i in range(0,len(tbl)):
        x = [ j * len(df) * width + width * i for j in range(0,len(labels)) ]
        y = [ tbl[i][j] for j in range(0,len(labels)) ]
        label = df[0][i]
        rects = ax.bar(x, y, width, label=titles[i])
        ax.bar_label(rects, padding=3)

    ax.legend(loc='upper left')
    ax.set_ylabel('Time (µs)')
    ax.set_xticks( [(j + 0.4) * len(df) * width for j in range(0,len(labels)) ] , labels)
    ax.set_title(title)

    name = os.path.splitext(name)[0] + ".svg"
    plt.savefig(name)
    #plt.show()

bars('timings/static.csv', 'Latencies for static files')
bars('timings/dynamic.csv', 'Latencies for php/chaml files')
