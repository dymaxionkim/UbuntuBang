## SCREW
* 스크류 토크를 간단히 구하는 스크립트 입니다.
* Sage Math로 작성된 버전이 있습니다.
* 스크류 자체의 마찰력 및 물체 이송시 물체와 바닥면의 마찰계수를 고려하였습니다.
* 스크류 자체의 마찰계수를 조정함으로써 볼스크류/리드스크류 모두 적용 가능합니다.

## CODE on SageMath
```
#########################################
# How much Lead Screw Torque V04 (English)
# 20150415 DH,Kim
# dymaxion.kim@gmail.com
#########################################

@interact
def GearParameter(
    Fi = slider(0,50,0.1,35.0,'Axial Force on Screw Nut [kgf]'),
    us = slider(0,0.5,0.01,0.1,'Friction Factor on Screw Nut'),
    W = slider(0,30,0.01,10,'Mass on Screw [kgf]'),
    uw = slider(0,0.5,0.01,0.3,'Friction Factor between Mass and Ground'),
    D = slider(0.1,20,0.1,15,'Pitch Diameter of Screw [mm]'),
    P = slider(0.1,30,0.1,30,'Pitch of Screw [mm]')
    ):

    ### Physical Constant
    g = 9806.65 # Gravity Acceleration [mm/s^2]

    ### Axial Force
    F = 50 # Max Axial Force on Screw Nut in [kgf]
    F_total = F + uw*W # Total Axial Force [kgf]
    Fi_total = Fi + uw*W # Total Axial Force [kgf]

    ### Screw (ref : http://www.engineersedge.com/hardware/metric-external-thread-sizes1.htm )
    R = D*pi # Lead of Screw [mm]
    alpha = atan(P/R)  # Lead Angle of Screw [Rad]
    nu = (1-us*tan(alpha))/(1+us/tan(alpha)) # Efficiency of Screw

    ### Torque in [kgf.cm]
    T = F_total*R/(2*pi*nu)  # Torque in [kgf.mm]
    T = T*0.1  # Torque in [kgf.cm]
    Ti = Fi_total*R/(2*pi*nu)  # Torque in [kgf.mm]
    Ti = Ti*0.1  # Torque in [kgf.cm]
    
    t = var('t')
    DATA = parametric_plot( (t/1000,(0.1*(t+uw*W)*R/(2*pi*nu))/1000), (t,0,int(F_total)*1000), axes_labels=['Total Axial Force\n$[kgf]$','Screw Torque\n$[kgf.cm]$'])
    DATA += line([(Fi_total,0), (Fi_total,Ti)], color='red')
    DATA += line([(0,Ti), (Fi_total,Ti)], color='red')
    DATA += point((Fi_total,Ti), color='green', size=100)
    Ti = float(Ti)
    html(r'$Given\ Total\ Axial\ Force = %s [kgf]$<br>'%latex(Fi_total))
    html(r'$Required\ Screw\ Torque = %s [kgf.cm]$'%latex(Ti))
    show(DATA, figsize=7, aspect_ratio='automatic', gridlines='automatic')
```

## 모델
![SCREW](http://4.bp.blogspot.com/-iD2t6aPmZ6g/VS34aBQ5ubI/AAAAAAAAa-w/oM9bUoEYSMU/s1600/20150415_ScrewTorque.png)

* 온라인에서 직접 계산 ::: http://dymaxionkim.blogspot.kr/search/label/SAGE

## 결과 예시
![SCREW_Output](https://cloud.githubusercontent.com/assets/12775748/11821108/526286ac-a3ab-11e5-8e87-b2c87eaa5107.png)

* 상단의 슬라이드바를 움직여서 간단하게 입력 파라미터를 조절해 줄 수 있습니다.

|입력 파라미터                    | 설명                                         |
|---------------------------------|----------------------------------------------|
| Axial Force on Screw Nut [kgf]  | 스크류의 너트에 걸리는 축력 즉 저지하는 외력 |
| Friction Factor on Screw Nut    | 스크류와 너트간의 마찰계수                   |
| Mass on Screw [kgf]             | 스크류의 너트에 붙어있는 질량                |
| Friction Factor between Mass and Ground | 질량체와 바닥면의 마찰계수           |
| Pitch Diameter of Screw [mm]    | 스크류의 피치 직경                           |
| Pitch of Screw [mm]	          | 스크류의 피치 치수 (리드값)                  |


