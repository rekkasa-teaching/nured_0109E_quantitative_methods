#!/usr/bin/env python3
"""Generate reproducible SVG illustrations for the Greek one-way ANOVA Quarto lecture.
Requires: numpy, matplotlib, scipy. Synthetic teaching example, not field data.
"""
from pathlib import Path
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy.stats import f as f_dist

OUT = Path(__file__).resolve().parents[1] / 'figures'
OUT.mkdir(exist_ok=True)
plt.rcParams.update({
    'font.family': 'DejaVu Sans', 'font.size': 17, 'axes.titlesize': 21,
    'axes.labelsize': 17, 'xtick.labelsize': 16, 'ytick.labelsize': 15,
    'svg.fonttype': 'none', 'savefig.transparent': False,
})
NAVY='#1e3a5f'; BLUE='#4a6fa5'; RED='#c74440'; GREEN='#249786'
GOLD='#e2a552'; INK='#253447'; GRAY='#8995a5'; LIGHT='#ecf1f6'; BORDER='#d7dfe9'
COL=[BLUE,GREEN,GOLD]; LABELS=['Αφήγηση','Εικόνες','Κουκλοθέατρο']
BASE=[np.array([2,3,4,5,6]),np.array([4,5,6,7,8]),np.array([6,7,8,9,10])]
WIDE=[np.array([0,2,4,6,8]),np.array([2,4,6,8,10]),np.array([4,6,8,10,12])]
MEANS=np.array([np.mean(g) for g in BASE]); GRAND=float(np.concatenate(BASE).mean())
assert list(MEANS)==[4,6,8] and GRAND==6


def fig_ax(sz=(12,5.3), xmin=.48, xmax=3.52, ymin=.5, ymax=12.25):
    fig, ax=plt.subplots(figsize=sz, dpi=144)
    fig.patch.set_facecolor('white');ax.set_facecolor('white')
    ax.set_xlim(xmin,xmax);ax.set_ylim(ymin,ymax)
    ax.spines[['top','right','left']].set_visible(False)
    ax.spines['bottom'].set_color(BORDER)
    ax.set_yticks(np.arange(0,13,2));ax.set_xticks([1,2,3],LABELS)
    ax.tick_params(axis='both',length=0,pad=10,colors=INK)
    ax.grid(axis='y',color=LIGHT,lw=1,zorder=0)
    ax.set_ylabel('Βαθμολογία ανάκλησης (0–12)', labelpad=13, color=INK)
    return fig,ax

def dots(ax,data=BASE,size=145,alpha=.92):
    for j,grp in enumerate(data):
        offsets=np.array([-0.09,-0.045,0,.045,.09])
        ax.scatter(np.full(len(grp),j+1)+offsets,grp,s=size,c=COL[j],
                   edgecolor='white',linewidth=1.6,alpha=alpha,zorder=5)

def mean_markers(ax, data=BASE, explain=True):
    for j, grp in enumerate(data):
        m=grp.mean(); col=COL[j]
        ax.plot([j+.75,j+1.25],[m,m],color=col,lw=5,solid_capstyle='round',zorder=7)
        if explain:
            ax.text(j+1.3,m,f'μ = {m:g}',ha='left',va='center',color=col,fontweight='bold',fontsize=16)

def finalize(fig,path):
    fig.tight_layout(pad=1.4)
    fig.savefig(OUT/path,format='svg',bbox_inches='tight',facecolor='white')
    plt.close(fig)

# 01: Raw group observations, with means labelled
fig,ax=fig_ax();dots(ax);mean_markers(ax)
ax.set_title('Τρεις ανεξάρτητες ομάδες, πέντε παιδιά σε καθεμία',fontweight='bold',color=NAVY,pad=15)
ax.text(.55,11.5,'● Παιδί',color=INK,fontsize=15)
ax.text(.55,10.8,'▬ Μέσος όρος ομάδας',color=NAVY,fontsize=15)
finalize(fig,'01-groups-means.svg')

# 02: Group means and grand mean
fig,ax=fig_ax();dots(ax);mean_markers(ax)
ax.axhline(GRAND,color=NAVY,ls=(0,(6,4)),lw=2.4,zorder=3)
ax.text(3.5,GRAND+.33,'Γενικός μέσος = 6',color=NAVY,ha='right',fontweight='bold',fontsize=16)
ax.set_title('Οι μέσοι των ομάδων και ο γενικός μέσος',fontweight='bold',color=NAVY,pad=15)
finalize(fig,'02-means-grand.svg')

# 03: Within-group devs
fig,ax=fig_ax();
for j,grp in enumerate(BASE):
    for i,y in enumerate(grp):
        x=j+1 + [-.09,-.045,0,.045,.09][i]
        ax.plot([x,x],[y,grp.mean()],color=BLUE,lw=3,alpha=.7,zorder=2)
mean_markers(ax);dots(ax)
ax.text(1.42,10.9,'Κάθε μπλε τμήμα: από το παιδί στον μέσο της ομάδας του',
        ha='left',va='center',fontsize=16,color=BLUE)
ax.set_title('SSW: μεταβλητότητα μέσα στις ομάδες',fontweight='bold',color=NAVY,pad=15)
finalize(fig,'03-within-deviations.svg')

# 04: Between means deviations
fig,ax=fig_ax();dots(ax,alpha=.35)
ax.axhline(GRAND,color=NAVY,ls=(0,(6,4)),lw=2.4,zorder=3)
for j,m in enumerate(MEANS):
    ax.plot([j+1,j+1],[GRAND,m],color=RED,lw=7,alpha=.85,zorder=6)
    ax.scatter(j+1,m,marker='D',c=RED,s=210,edgecolor='white',linewidth=1.5,zorder=8)
    ax.text(j+1.12,m+(.34 if m != 6 else .55),f'μ{j+1} = {m:g}',color=RED,fontsize=16,fontweight='bold')
ax.text(3.5,GRAND-.65,'Γενικός μέσος = 6',color=NAVY,ha='right',fontsize=15)
ax.set_title('SSB: απόσταση μέσων ομάδων από τον γενικό μέσο',fontweight='bold',color=NAVY,pad=15)
finalize(fig,'04-between-deviations.svg')

# 05: Total deviations to global mean
fig,ax=fig_ax();
for j,grp in enumerate(BASE):
    for i,y in enumerate(grp):
        x=j+1+[-.09,-.045,0,.045,.09][i]
        ax.plot([x,x],[y,GRAND],color=GRAY,lw=2.6,zorder=2,alpha=.75)
dots(ax);ax.axhline(GRAND,color=NAVY,ls=(0,(6,4)),lw=2.4,zorder=3)
ax.text(3.5,GRAND+.35,'Γενικός μέσος = 6',ha='right',color=NAVY,fontsize=16,fontweight='bold')
ax.set_title('SST: κάθε παιδί σε σχέση με τον γενικό μέσο',fontweight='bold',color=NAVY,pad=15)
finalize(fig,'05-total-deviations.svg')

# 06: Single-observation geometric decomposition, exact additive signed deviation
fig,ax=plt.subplots(figsize=(12,4.8),dpi=144)
fig.patch.set_facecolor('white');ax.set_facecolor('white')
ax.set_xlim(-.15,6.5);ax.set_ylim(1.2,7.7)
for h,name,c in [(2,'Παιδί: y = 2',GRAY),(4,'Μέσος αφήγησης: 4',BLUE),(6,'Γενικός μέσος: 6',NAVY)]:
    ax.hlines(h,1.0,5.7,color=c,lw=2.2,ls='--' if h==6 else '-',alpha=.75)
    ax.text(5.85,h,name,va='center',fontsize=16,color=c,fontweight='bold' if h==6 else 'normal')
# arrows
for x,y0,y1,c,label in [(1.65,2,4,BLUE,'μέσα: −2'),(3.0,4,6,RED,'μεταξύ: −2'),(4.35,2,6,NAVY,'συνολικά: −4')]:
    ax.annotate('',xy=(x,y0),xytext=(x,y1),arrowprops={'arrowstyle':'-|>','lw':4,'color':c})
    ax.text(x-.15,(y0+y1)/2,label,fontsize=16,color=c,rotation=90,va='center',ha='right')
ax.text(2.9,7.2,'yᵢⱼ − μ = (yᵢⱼ − μⱼ) + (μⱼ − μ)',ha='center',fontsize=21,color=NAVY,fontweight='bold')
ax.axis('off');finalize(fig,'06-one-child-decomposition.svg')

# 07: Squared sums, numeric decomposition
fig,ax=plt.subplots(figsize=(12,3.8),dpi=144)
fig.patch.set_facecolor('white');ax.set_facecolor('white')
ax.barh(1,30,color=BLUE,height=.58,label='SSW = 30',left=0)
ax.barh(1,40,color=RED,height=.58,label='SSB = 40',left=30)
ax.text(15,1,'SSW = 30',ha='center',va='center',color='white',fontweight='bold',fontsize=24)
ax.text(50,1,'SSB = 40',ha='center',va='center',color='white',fontweight='bold',fontsize=24)
ax.annotate('',xy=(0,.44),xytext=(70,.44),arrowprops=dict(arrowstyle='<->',lw=2,color=NAVY))
ax.text(35,.2,'SST = 70',ha='center',va='center',fontsize=24,color=NAVY,fontweight='bold')
ax.set_xlim(-3,73);ax.set_ylim(-.1,1.75);ax.axis('off')
ax.set_title('Συνολική = μεταξύ + εντός ομάδων',fontsize=22,color=NAVY,fontweight='bold')
finalize(fig,'07-ss-partition.svg')

# 08: contrast F value for low vs high within variation
fig,axes=plt.subplots(1,2,figsize=(13,5.2),dpi=144,sharey=True)
fig.patch.set_facecolor('white')
for ax, data, ft, p, ttl in zip(axes,[BASE,WIDE],[8,2],[f_dist.sf(8,2,12),f_dist.sf(2,2,12)],
                               ['Μικρότερη διασπορά μέσα στις ομάδες','Μεγαλύτερη διασπορά μέσα στις ομάδες']):
    ax.set_facecolor('white');ax.set_xlim(.6,3.4);ax.set_ylim(-.6,12.9)
    ax.set_xticks([1,2,3],['Αφήγηση','Εικόνες','Κούκλες']);ax.set_yticks(np.arange(0,13,2))
    ax.tick_params(axis='both',length=0,labelsize=13,pad=8)
    for sp in ax.spines.values(): sp.set_visible(False)
    ax.grid(axis='y',color=LIGHT,lw=1)
    dots(ax,data,size=105)
    for j,g in enumerate(data):
        m=g.mean();ax.hlines(m,j+.75,j+1.25,color=COL[j],lw=4,zorder=6)
    ax.set_title(ttl,fontsize=16,color=NAVY,fontweight='bold',pad=12)
    ax.text(2,.3,f'F = {ft}     p = {p:.3f}',ha='center',va='bottom',fontsize=18,
            color=NAVY,fontweight='bold',bbox=dict(boxstyle='round,pad=.4',facecolor='white',edgecolor=BORDER))
axes[0].set_ylabel('Βαθμολογία',fontsize=16,color=INK)
fig.suptitle('Ίδιοι μέσοι (4, 6, 8) — διαφορετικό F',fontsize=23,color=NAVY,fontweight='bold',y=1.02)
fig.tight_layout(pad=1.2,w_pad=1.5)
fig.savefig(OUT/'08-f-contrast.svg',format='svg',bbox_inches='tight',facecolor='white');plt.close(fig)

# 09: F density null distribution
fig,ax=plt.subplots(figsize=(12,4.8),dpi=144)
fig.patch.set_facecolor('white');ax.set_facecolor('white')
x=np.linspace(.0001,14,1300); y=f_dist.pdf(x,2,12)
ax.plot(x,y,color=NAVY,lw=4)
ax.axvspan(8,13.3,color=RED,alpha=.065,zorder=0)
ax.fill_between(x,y,where=x>=8,color=RED,alpha=.55)
crit=f_dist.ppf(.95,2,12)
ax.axvline(crit,ls='--',lw=2.4,color=GRAY)
ax.axvline(8,ls='--',lw=3.2,color=RED)
ax.text(7.78,.62,'F = 8',color=RED,fontsize=19,fontweight='bold',rotation=90,va='bottom')
ax.text(crit+.2,.25,f'F₍κρίσιμο₎ ≈ {crit:.2f}',color=GRAY,fontsize=17,rotation=90,va='bottom')
ax.annotate('p ≈ 0,006\n(δεξιά ουρά)',xy=(9.25,.004),xytext=(9.2,.24),fontsize=19,color=RED,
            ha='center',arrowprops={'arrowstyle':'->','color':RED,'lw':2})
ax.set_xlim(0,13.3);ax.set_ylim(0,.98);ax.set_xlabel('Τιμές F υπό τη μηδενική υπόθεση',color=INK,labelpad=9)
ax.set_ylabel('Πυκνότητα',color=INK)
ax.grid(axis='y',color=LIGHT);ax.spines[['top','right']].set_visible(False)
ax.set_title('Κατανομή F με 2 και 12 βαθμούς ελευθερίας',fontsize=21,color=NAVY,fontweight='bold',pad=13)
finalize(fig,'09-f-distribution.svg')

# 10: anova mean square parts simple area chart for F ratio
fig,ax=plt.subplots(figsize=(11,4.3),dpi=144)
fig.patch.set_facecolor('white');ax.set_facecolor('white')
vals=[20,2.5]; xx=[0,1]; bars=ax.bar(xx,vals,width=.58,color=[RED,BLUE],zorder=3)
for bar,v in zip(bars,vals):
    ax.text(bar.get_x()+bar.get_width()/2,v+.7,str(v).replace('.',','),ha='center',fontsize=24,fontweight='bold',color=NAVY)
ax.set_xticks(xx,['MSB (μεταξύ)','MSW (εντός)']);ax.set_ylabel('Μέσο τετράγωνο',color=INK)
ax.set_ylim(0,24);ax.set_xlim(-.7,1.7);ax.grid(axis='y',color=LIGHT);ax.set_axisbelow(True)
ax.spines[['top','right','left']].set_visible(False);ax.tick_params(length=0,labelsize=17)
ax.set_title('Ο λόγος 20 / 2,5 = 8',fontsize=24,color=NAVY,fontweight='bold')
finalize(fig,'10-mean-squares.svg')

print('SVG figures:',len(list(OUT.glob('*.svg'))))
