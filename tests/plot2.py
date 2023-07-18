import csv
import matplotlib.pyplot as plt
import os

def bars(name, title):
    file= open(name)
    df = [list(r) for r in list(csv.reader(file))]

    xs = [ float(line[1]) for line in df[1:] ]
    files = []; index=0
    for i in range(1,len(df)):
        if files == [] or files[index-1] != df[i][0]:
            files.append(df[i][0])
            index+=1

    labels = df[0][2:len(df[0])]
    nb_files = len(files)
    nb_runs = (len(df) - 1) // nb_files

    tbl  = [ line[2:] for line in df[1:] ]
    tbl = [  [ round (float(x), 0) for x in line ] for line in tbl ]


    for i in range(0,nb_files):
        fig, ax = plt.subplots(layout='constrained')

        x = [ xs[i*nb_runs + j] for j in range(0,nb_runs) ]
        for k in range(0,len(labels)):
                y = [ tbl[i*nb_runs + j][k] for j in range(0,nb_runs) ]
                label = labels[k]
                color = ("#0000ff" if "vfs" in label
                         else ("#ff00ff" if "simple_httpd" in label
                               else ("#ff0000" if "nginx" in label
                                     else "#00ff00")))
                dashes = ([4,3] if "ssl" in label else (None, None))
                curve = ax.plot(x, y, label=label,color=color,dashes=dashes)

        ax.legend(loc='upper right')
        ax.set_ylabel('Number of request per second')
        ax.set_xlabel("Number of simultaneous connections")
        ax.set_xticks( x, [ int(i) for i in x])

        l = files[i].split("_")
        fulltitle = title if len(l) <= 1 else title + " (" + l[1] + " files)"
        ax.set_title(fulltitle)

        fname = os.path.splitext(name)[0] + "_" + files[i] + ".svg"
        plt.savefig(fname)
        #plt.show()

bars('timings/bench.csv', 'Requests per seconds for static files')
bars('timings/bench_chaml.csv', 'Request per seconds for php/chaml files')
