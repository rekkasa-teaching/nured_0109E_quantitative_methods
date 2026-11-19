"""Reproduce the teaching data and reference calculations (numpy, pandas, scipy)."""
from pathlib import Path
import numpy as np
import pandas as pd
from scipy import stats
import json,itertools
root=Path(__file__).resolve().parents[1]
rng=np.random.default_rng(6102026)
methods=['Αφήγηση','Εικόνες','Κούκλες'];ages=['4 ετών','5 ετών']
mu=[[45,60,70],[63,67,71]]
rows=[]
for a,age in enumerate(ages):
 for m,method in enumerate(methods):
  for y in rng.normal(mu[a][m],10,30):
   rows.append([method,age,round(float(y),1)])
df=pd.DataFrame(rows,columns=['story_method','age_group','comprehension_score']).sample(frac=1,random_state=6).reset_index(drop=True)
df.insert(0,'child_id',np.arange(1,len(df)+1))
assert df.comprehension_score.between(0,100).all()
df.to_csv(root/'two_way_anova.csv',index=False,encoding='utf-8-sig')
# Re-read the delivered CSV: all reference results use its rounded values.
df=pd.read_csv(root/'two_way_anova.csv')
g=df.groupby(['age_group','story_method']).comprehension_score
cells=g.agg(['count','mean','std','median','min','max']).reindex(pd.MultiIndex.from_product([ages,methods]))
means=cells['mean'].to_numpy().reshape(2,3); grand=df.comprehension_score.mean()
ss_a=30*2*np.sum((means.mean(axis=0)-grand)**2)
ss_b=30*3*np.sum((means.mean(axis=1)-grand)**2)
ss_ab=30*np.sum((means-means.mean(axis=0)[None,:]-means.mean(axis=1)[:,None]+grand)**2)
res=df.comprehension_score-g.transform('mean');sse=float(np.sum(res**2));mse=sse/174
anova=[]
for name,ss,d in [('Τρόπος',ss_a,2),('Ηλικία',ss_b,1),('Τρόπος × Ηλικία',ss_ab,2)]:
 f=ss/d/mse;anova.append(dict(term=name,SS=ss,df=d,MS=ss/d,F=f,p=stats.f.sf(f,d,174),partial_eta2=ss/(ss+sse)))
labels=[f'{m} / {a}' for a in ages for m in methods];flat=means.flatten();critical=stats.studentized_range.ppf(.95,6,174);se=np.sqrt(mse/30)
post=[]
for i,j in itertools.combinations(range(6),2):
 delta=flat[i]-flat[j];p=stats.studentized_range.sf(abs(delta)/se,6,174)
 post.append(dict(first=labels[i],second=labels[j],difference=delta,low=delta-critical*se,high=delta+critical*se,p=p,within_age=(i//3==j//3)))
f_model=(ss_a+ss_b+ss_ab)/5/mse
results=dict(N=180,cells=cells.reset_index().rename(columns={'level_0':'age_group','level_1':'story_method'}).to_dict('records'),anova=anova,SSE=sse,MSE=mse,overall_F=f_model,overall_p=stats.f.sf(f_model,5,174),posthoc=post,levene_p=stats.levene(*[v.values for _,v in g],center='mean').pvalue,shapiro_p=stats.shapiro(res).pvalue)
(root/'results.json').write_text(json.dumps(results,ensure_ascii=False,indent=2))
print(cells.round(2).to_string());print(json.dumps({k:v for k,v in results.items() if k not in ['cells','posthoc']},ensure_ascii=False,indent=2))
