Golec de Zavala, A. (2023) Mindfulness  reduces the link between collective narcissism and prejudice

Reserach conditions

RECODE audio (2=1) (3=-1) (1=0) INTO cond.
EXECUTE.
-1 control 0 - mindfulness 1 mindfulness with gratitude

Collective narcissism (Golec de Zavala et al., 2009)

COMPUTE CNSC=mean(cn_r1,cn_r2,cn_r3,cn_r4,cn_r5).
EXECUTE.

RELIABILITY
  /VARIABLES=cn_r1 cn_r2 cn_r3 cn_r4 cn_r5
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=SCALE
  /SUMMARY=TOTAL.

Ingroup satisfaction (Leach et al., 2008)

COMPUTE IGSAT=mean(group_pride_r1,group_pride_r2,group_pride_r3,group_pride_r4).
EXECUTE.

RELIABILITY
  /VARIABLES=group_pride_r1 group_pride_r2 group_pride_r3 group_pride_r4
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=SCALE
  /SUMMARY=TOTAL.


State mindfulness (Tanay & Bernstein, 2013)

I noticed physical sensations come and go.
I noticed some pleasant and unpleasant physical sensations.
I noticed various sensations caused by my surroundings (e.g., heat, coolness, the
wind on my face).
I clearly physically felt what was going on in my body.
I felt in contact with my body.

COMPUTE smindbs=mean(state_mindfulness_scale_r21,state_mindfulness_scale_r20,
    state_mindfulness_scale_r19,state_mindfulness_scale_r18,state_mindfulness_scale_r17,
    state_mindfulness_scale_r16).
EXECUTE.

RELIABILITY
  /VARIABLES=state_mindfulness_scale_r16 state_mindfulness_scale_r17
    state_mindfulness_scale_r18 state_mindfulness_scale_r19 state_mindfulness_scale_r20
    state_mindfulness_scale_r21 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=SCALE
  /SUMMARY=TOTAL.

Trait mindfulness (Brown & Ryan, 2003)

COMPUTE tmind=mean(tmind1,tmind2,tmind3,tmind4,tmind5,tmind6,tmind7,tmind8,tmind9,tmind10,
    tmind11,tmind12,tmind13,tmind14,tmind15).
EXECUTE.

RELIABILITY
  /VARIABLES=tmind1 tmind2 tmind3 tmind4 tmind5 tmind6 tmind7 tmind8 tmind9 tmind10
    tmind11 tmind12 tmind13 tmind14 tmind15
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=SCALE
  /SUMMARY=TOTAL.

anti-Semitism  (Wójcik, Lewicka & Bilewicz, 2011)

COMPUTE antiSems=mean(skala2r1,skala2r2,skala2r3,skala2r4,skala2r5).
EXECUTE.
