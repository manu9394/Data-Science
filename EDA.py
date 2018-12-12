
# coding: utf-8

# In[103]:


def Graph(x,path,index="" ): ## Adding additional argument for specifying directory
    y = index
    import pandas as pd
    import os
    if path == "":
        Dir = os.getcwd()
    else:
        Dir = path    
    os.chdir(str(Dir))
    import numpy as np
    import matplotlib.pyplot as plt
    L=[]
    if y != "":
        y=list(y)
        l=list(x.columns)
        subplname = " Graph of "
        yhist = " Frequency of "
        xhist = " Values of "
        for i in l:
            if l.index(i) in y:
                L.append(i)
                if np.dtype(x[i]) == "O":
                    x[i].value_counts().plot(kind="barh",color="blue",fontsize=13)
                    plt.xlabel(yhist + i)
                    plt.ylabel(xhist + i)
                    plt.title(i)
                    plt.savefig(i + ".jpg")
                    plt.close()
                else:
                    ax1 = plt.subplot2grid((40,10),(1,0),rowspan=10,colspan=10)
                    ax2 = plt.subplot2grid((40,10),(13,0),rowspan=29,colspan=10)
                    ax2.hist(x[i])
                    ax1.boxplot(x[i],vert = False)
                    plt.suptitle(subplname + i)
                    ax2.set_xlabel(xhist + i)
                    ax2.set_ylabel(yhist + i)
                    plt.savefig(i + ".jpg")
                    plt.close()
        writer = pd.ExcelWriter('Description.xlsx')
        x[L].describe().to_excel(writer,'Sheet1')
        writer.save()
        
    
    elif y=="":
        l=x.columns
        subplname = " Graph of "
        yhist = " Frequency of "
        xhist = " Values of "
        for i in l:
            if np.dtype(x[i]) == "O":
                x[i].value_counts().plot(kind="barh",color="blue",fontsize=13)
                plt.xlabel(yhist + i)
                plt.ylabel(xhist + i)
                plt.title(i)
                plt.savefig(i + ".jpg")
                plt.close()
            else:
                ax1 = plt.subplot2grid((40,10),(1,0),rowspan=10,colspan=10)
                ax2 = plt.subplot2grid((40,10),(13,0),rowspan=29,colspan=10)
                ax2.hist(x[i])
                ax1.boxplot(x[i],vert = False)
                plt.suptitle(subplname + i)
                ax2.set_xlabel(xhist + i)
                ax2.set_ylabel(yhist + i)
                plt.savefig(i + ".jpg")
                plt.close()
        writer = pd.ExcelWriter('Description.xlsx')
        x[l].describe().to_excel(writer,'Sheet1')
        writer.save()
      

