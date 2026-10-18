"""Reproduce the approved Figure 6.8A sample means.

Invoked by 05_reproduce_thesis_figures.R. Requires NumPy. Private empirical
inputs and simulation intermediates stay in the ignored data directory.
The linear Gaussian model allows the sample-mean noise to be simulated
directly, with innovation SD sigma / sqrt(N). This has exactly the same
distribution as averaging N independently simulated individuals.
"""
from pathlib import Path
import csv
import json
import sys
import numpy as np

build = Path(sys.argv[1])
inputs = json.loads((build / 'inputs.json').read_text())
VD, VP, VS, CDP, CDS, CPS = inputs['moments']
u, v, w = VD-CDS**2/VS, VP-CPS**2/VS, CDP-CDS*CPS/VS
hist = np.genfromtxt(build / 'stress_distribution.csv', delimiter=',', names=True)
prob = hist['count'] / hist['count'].sum()
strengths = np.arange(101) / 100
n, dt, scale, seed = 500, .01, .3, 2026101807
rng = np.random.default_rng(seed)
rows = []
max_residual = 0.
for a in np.round(np.arange(14)*.05, 2):
    ads = (CDS-a*CPS)/VS
    apd = (2*w-a*v)/u
    aps = (CPS-apd*CDS)/VS
    sigma = np.sqrt([2*(u-a*w), 2*(v-apd*w)])
    A = np.array([[-1., a], [apd, -1.]])
    B = np.eye(2) + dt*A
    f = np.array([ads, aps])
    C = np.array([[u, w], [w, v]])
    max_residual = max(max_residual, float(abs(A@C+C@A.T+np.diag(sigma**2)).max()))
    baseline = rng.choice(hist['value'], size=(len(strengths), n), p=prob)
    stress_means = baseline.mean(axis=1)*(1-strengths)+inputs['S_min']*strengths
    noise = np.zeros((len(strengths), 2))
    g = np.zeros(2)
    for t in range(2, 501):
        noise = noise@B.T + np.sqrt(dt/n)*sigma*rng.standard_normal(noise.shape)
        g = B@g + dt*f
        if t in (100, 300, 500):
            for i, q in enumerate(strengths):
                for k, variable in enumerate(('D', 'P')):
                    rows.append(dict(alpha_dp=float(a), intervention=float(q), time=t,
                                     Variable=variable, scale=scale,
                                     mean=float(g[k]*stress_means[i]+scale*noise[i, k])))
assert max_residual < 1e-12
with (build / 'A-means.csv').open('w', newline='') as handle:
    writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
print(f'A complete, N={n}, seed={seed}, noise scale={scale}, residual={max_residual:.3g}')
