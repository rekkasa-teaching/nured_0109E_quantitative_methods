"""Reproduce the simulated CSV, numerical results and lecture figures.
Python 3; numpy, scipy, matplotlib. Run from any directory.
"""
from pathlib import Path
import csv, json
import numpy as np
from scipy import stats
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'figures'
OUT.mkdir(exist_ok=True)
rng = np.random.default_rng(20261104)
n = 30
codes = ['Α', 'Β', 'Γ']
labels = ['Α: Αφήγηση', 'Β: Εικόνες', 'Γ: Κούκλες']
groups = [np.round(rng.normal(mu, 10, n), 1) for mu in [50, 51, 63]]
assert all(np.all((g >= 0) & (g <= 100)) for g in groups)
with (ROOT/'anova_study.csv').open('w', encoding='utf-8-sig', newline='') as f:
    w=csv.writer(f); w.writerow(['child_id','story_method','comprehension_score'])
    for j,g in enumerate(groups):
        for i,y in enumerate(g): w.writerow([j*n+i+1,codes[j],y])
y = np.concatenate(groups); means = np.array([g.mean() for g in groups]); grand=y.mean()
ssb=sum(n*(m-grand)**2 for m in means); ssw=sum(sum((g-g.mean())**2) for g in groups)
sst=ssb+ssw; dfw=len(y)-3; mse=ssw/dfw
F=(ssb/2)/mse; p=stats.f.sf(F,2,dfw)
eta=ssb/sst; omega=(ssb-2*mse)/(sst+mse)
res=np.concatenate([g-g.mean() for g in groups])
lev=stats.levene(*groups,center='mean'); bf=stats.levene(*groups,center='median'); sh=stats.shapiro(res)
tuk=stats.tukey_hsd(*groups); ci=tuk.confidence_interval()
pairs=[]
for a,b in [(1,0),(2,0),(2,1)]:
    t=(means[a]-means[b])/np.sqrt(mse*(2/n)); raw=2*stats.t.sf(abs(t),dfw)
    pairs.append(dict(pair=f'{codes[a]} − {codes[b]}',diff=means[a]-means[b],lower=ci.low[a,b],upper=ci.high[a,b],p=tuk.pvalue[a,b],raw=raw,bonf=min(3*raw,1)))
order=np.argsort([x['raw'] for x in pairs]); adj=0
for rank,i in enumerate(order):
    adj=max(adj,min(1,(3-rank)*pairs[i]['raw'])); pairs[i]['holm']=adj
result=dict(N=len(y),n=n,means=means.tolist(),sds=[g.std(ddof=1) for g in groups],min=[g.min() for g in groups],max=[g.max() for g in groups],ssb=ssb,ssw=ssw,sst=sst,mse=mse,dfw=dfw,F=F,p=p,eta=eta,omega=omega,levene_F=lev.statistic,levene_p=lev.pvalue,brown_forsythe_F=bf.statistic,brown_forsythe_p=bf.pvalue,shapiro_W=sh.statistic,shapiro_p=sh.pvalue,pairs=pairs)
(ROOT/'results.json').write_text(json.dumps(result,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps(result,ensure_ascii=False,indent=2))
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':16,'axes.labelsize':16,'axes.titlesize':18,'svg.fonttype':'none'})
navy='#1e3a5f'; blue='#4a6fa5'; red='#c74440'; green='#249786'; gold='#e2a552'
colors=[blue,green,gold]
def finish(fig,name):
    fig.tight_layout(); fig.savefig(OUT/name,bbox_inches='tight'); plt.close(fig)
def clean(ax):
    ax.spines[['top','right']].set_visible(False); ax.grid(axis='y',alpha=.15); ax.set_axisbelow(True)
fig,ax=plt.subplots(figsize=(11,3.5)); ax.barh(['Συνολική\nμεταβλητότητα'],[40],color=red,label='Μεταξύ ομάδων: 40'); ax.barh(['Συνολική\nμεταβλητότητα'],[30],left=[40],color=blue,label='Μέσα στις ομάδες: 30'); ax.text(20,0,'57,1%',ha='center',va='center',color='white',fontsize=25); ax.text(55,0,'42,9%',ha='center',va='center',color='white',fontsize=25); ax.set_xlim(0,70); ax.set_xlabel('Άθροισμα τετραγώνων (SS)'); ax.legend(loc='lower center',bbox_to_anchor=(.5,1.02),ncol=2,frameon=False); clean(ax); finish(fig,'01-eta.svg')
fig,ax=plt.subplots(figsize=(11,4.1)); m=np.arange(1,21); ax.plot(m,100*(1-.95**m),color=red,lw=3); ax.axhline(5,color=blue,ls='--',label='5% ανά έλεγχο'); ax.scatter([3,6,10],[100*(1-.95**z) for z in [3,6,10]],color=navy); ax.set_xticks([1,3,6,10,15,20]); ax.set_xlabel('Αριθμός ανεξάρτητων ελέγχων'); ax.set_ylabel('Πιθανότητα ≥1 σφάλματος (%)'); ax.legend(frameon=False); clean(ax); finish(fig,'02-fwer.svg')
fig,ax=plt.subplots(figsize=(11,4.5)); ax.boxplot(groups,positions=[1,2,3],widths=.4,showfliers=False,medianprops={'color':navy,'linewidth':2}); jitter=np.random.default_rng(42)
for j,g in enumerate(groups):
    ax.scatter(j+1+jitter.uniform(-.14,.14,len(g)),g,s=26,alpha=.6,color=colors[j]); ax.scatter(j+1,g.mean(),marker='D',s=85,c=red,zorder=4)
ax.set_xticks([1,2,3],labels); ax.set_ylabel('Βαθμολογία κατανόησης (0–100)'); ax.set_ylim(15,95); clean(ax); finish(fig,'03-data.svg')
fig,axes=plt.subplots(1,2,figsize=(11,4.3)); stats.probplot(res,plot=axes[0]); axes[0].set_title('Q–Q διάγραμμα καταλοίπων'); axes[0].set_xlabel('Θεωρητικά κανονικά ποσοστημόρια'); axes[0].set_ylabel('Ταξινομημένα κατάλοιπα'); axes[0].get_lines()[0].set_color(blue); axes[0].get_lines()[1].set_color(red)
for j,g in enumerate(groups): axes[1].scatter(np.full(n,means[j]),g-means[j],c=colors[j],s=28,alpha=.7)
axes[1].axhline(0,color=navy,lw=1); axes[1].set_title('Κατάλοιπα και προβλεπόμενες τιμές'); axes[1].set_xlabel('Μέσος ομάδας'); axes[1].set_ylabel('Κατάλοιπο')
for ax in axes: clean(ax)
finish(fig,'04-diagnostics.svg')
fig,ax=plt.subplots(figsize=(11,4.1))
for j,d in enumerate(pairs): ax.errorbar(d['diff'],2-j,xerr=[[d['diff']-d['lower']],[d['upper']-d['diff']]],fmt='o',color=red if d['p']<.05 else blue,capsize=7,ms=8,lw=3)
ax.axvline(0,color=navy,ls='--'); ax.set_yticks([2,1,0],[d['pair'] for d in pairs]); ax.set_xlabel('Διαφορά μέσων — ταυτόχρονα 95% ΔΕ Tukey'); ax.set_ylim(-.5,2.5); clean(ax); finish(fig,'05-tukey.svg')
assert np.isclose(sst, sum((y-grand)**2))
assert np.isclose(F, stats.f_oneway(*groups).statistic)
assert all((d['lower']>0)==(d['p']<.05) for d in pairs)
