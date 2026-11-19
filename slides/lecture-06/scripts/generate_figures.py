from pathlib import Path
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
root=Path(__file__).resolve().parents[1]
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':16,'svg.fonttype':'none','text.color':'#253447','axes.labelcolor':'#253447'})
for file,ys in [('01-parallel',[[40,50,60],[55,65,75]]),('02-converging',[[40,55,70],[60,65,70]]),('03-crossing',[[45,60,75],[75,60,45]])]:
 fig,ax=plt.subplots(figsize=(11,3.8));fig.subplots_adjust(left=.1,right=.78,bottom=.18,top=.93)
 for y,col,marker,label in zip(ys,['#4a6fa5','#c74440'],['o','s'],['4 ετών','5 ετών']):
  ax.plot(range(3),y,color=col,marker=marker,lw=3,ms=9,label=label)
  for i,v in enumerate(y):ax.annotate(str(v),(i,v),xytext=(0,10 if label=='5 ετών' else -22),textcoords='offset points',ha='center',color=col,fontsize=14)
 ax.set_xticks(range(3),['Αφήγηση','Εικόνες','Κούκλες']);ax.set_ylim(25,90);ax.set_yticks([30,45,60,75,90]);ax.set_ylabel('Μέση βαθμολογία (0–100)');ax.set_xlim(-.15,2.15)
 ax.spines[['top','right']].set_visible(False);ax.grid(axis='y',alpha=.2);ax.legend(loc='center left',bbox_to_anchor=(1.02,.5),frameon=False)
 fig.savefig(root/'figures'/f'{file}.svg');plt.close(fig)
