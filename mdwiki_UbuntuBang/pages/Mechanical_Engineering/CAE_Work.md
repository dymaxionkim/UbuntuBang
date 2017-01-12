
# CAE_Work (실습 예제들)

## Dodaam_Education

### 윈도우 PC에 해석 소프트웨어 설치
* https://gist.github.com/dymaxionkim/f3949116dc25d8bb677cc1f633dc1047

### Ex01 (예제1)
* https://gist.github.com/dymaxionkim/7011f032c805d06e16fb4cf1b4b43ece


----------
## Legacy

### GMSH - ElmerFEM - ParaView
* http://dymaxionkim.blogspot.kr/2015/04/open-source-cae-toolchain-3.html
* GMSH 및 ElmerFEM으로 이어지는 작업 흐름대로 기본적인 선형(Linear) 구조해석을 실시한 것입니다.


### CFD with Elmer
* http://dymaxionkim.blogspot.kr/2015/04/open-source-cae-toolchain-5-cfd-with.html
* 가장 기본적인 관로유동(Internal Flow)에 대한 정적(Static) 유체해석을 실시한 것입니다.
* ElmerGUI 자체적으로 제공하는 VTK 뷰어와, ParaView 모두 사용하여 결과를 확인해 보았습니다.


### CFD with Elmer with a Body
* http://dymaxionkim.blogspot.kr/2015/04/open-source-cae-toolchain-6-cfd-with.html
* 바다속의 잠수함을 상정한 모델을 이용하여, 기본적인 외부유동(External Flow)를 정적(Static)으로 해석해 본 것입니다.  아무래도 계산시 발산(Divergence) 확률이 너무 높았으므로, 유체 속도를 크게 낮춰서 겨우 깨끗한 유선(Flow Line)을 얻을 수 있었습니다.
* 아울러, Post Processot로 사용한 ParaView의 사용 방법도 조금 더 익숙해 질 수 있었습니다.



### CFD with Elmer with Transient
* http://dymaxionkim.blogspot.kr/2015/04/open-source-cae-toolchain-8-cfd-with.html
*  열전달(Heat Transfer) 및 유체동역학(Navier-Stokes Equation)을 연성(Connect)시켜 계산하는
다중물리현상(Multi Physics) 및 천이(Transient) 현상을 모사 가능한지 확인해 본 것입니다.  유선(Flow Line)이 깨끗하게 나오지는 못했습니다.  이유는 압력차를 좀 과하게 줘서, 유체의 흐름 속도가 지나치게 빠르기 때문인 듯 합니다.
* 이 결과를 통해, 전자제품 내부의 냉각팬에 의한 강제대류 해석을 믿을만한 정밀도로 얻어내기 위해서는 적절한 난류모델의 적용이 필수적이라는 점을 알 수 있습니다.
[](https://youtu.be/CbpazcU72L4)


### CFD with Elmer by MultiPhysics Natural Convection & Transient
* http://dymaxionkim.blogspot.kr/2015/04/open-source-cae-toolchain-9-cfd-with.html
* 온도차에 의한 레일리 버나드 대류 열전달 현상을 Navier-Stokes Equation(유체동역학), Bussinesq(부력), Gravity(중력), 그리고 Heat Source Obstacle(발열물체) 조건을 줘서 다중물리현상(Multi Physics) 시뮬레이션을 해 본 것입니다.
[](https://youtu.be/Dp6M6J6wAIc)


### FEM with Elmer by Linear Elascticity & Scanning time varient displacement
* http://dymaxionkim.blogspot.kr/2015/04/open-source-cae-toolchain-10-fem-with.html
* ElmerGUI 상에서 MATC 및 Scaning 조건을 주는 과정을 시연한 시뮬레이션입니다.
[](https://youtu.be/czrIwudVzdI)


### Salome - Elmer
* http://dymaxionkim.blogspot.kr/2015/04/open-source-cae-toolchain-11-salomeelmer.html
* 리눅스에 Salome-Meca를 설치하고, Salome에서 Mesh 생성을 수행하였습니다.  어떤 Pre-Processor가 궁합이 좋은지 모색하기 위한 과정입니다.
* 만들어진 메쉬 모델을 ElmerGUI로 불러들여 열전도 정적해석을 수행한 시뮬레이션입니다.


### FEM with Elmer by Linear Elascticity and Scanning time varient normal force
* http://dymaxionkim.blogspot.kr/2015/05/open-source-cae-toolchain-13-fem-with.html
* ElmerGUI 환경에서 MATC 및 Scaning 조건을 주는 과정을 시연한 시뮬레이션입니다.
[](https://youtu.be/yC13uiLwPlc)



### GMSH - ELMER FEM - PARAVIEW 작업 흐름 강좌
* http://dymaxionkim.blogspot.kr/2015/05/gmsh-elmer-fem-paraview.html

* Gmsh 사용 방법 (매쉬 생성)
[](https://youtu.be/qts-H6novo0)

* Elmer 사용 방법 (선형 탄성 해석)
[](https://youtu.be/NDuQikyDDlg)

* Paraview 및 문서작성의 예
[](https://youtu.be/q_PAZZLYq-4)


### Karman Vortex CFD with Elmer
* http://dymaxionkim.blogspot.kr/2015/06/karman-vortex-cfd-with-elmer.html
* Navier-Stokes Equation(유체동역학방정식), Non-Compressible(비압축성), Laminar(층류) 조건으로 가운데 장애물을 만난 물의 흐름이 적절한 레이놀즈 계수에 의해 와류(Karman Vortex) 현상을 나타내는 것을 시뮬레이션하였습니다.
* 층류 조건에서 레이놀즈 계수가 어떤 임계점을 벗어나게 되면, 와류가 발생하며, 이보다 더 벗어나게 되면 본격적으로 난류(Turbulance)가 발생하게 되는데, 이때 CFD에서 계산 실패 확률이 비약적으로 높아지게 됩니다.  왜냐면 난류 영역 내에서의 유체 거동이 너무 급격해지기 때문에 왠만한 Time Step으로는 연속성(Continuity)를 확보할 수 없어 전산유체역학상의 차분 방정식의 해를 구할 수 없게 되기 때문입니다.  따라서 이상적으로 볼 때, 계산 실패를 만났을 때는 그보다 최소 수백만배 이상 더 짧은 Time Step으로 가져가야만 계산 실패를 극복할 수 있을 확률이 높아집니다.  이것은 곧, 계산시간이 수백만배 더 많아지고 또 계산 결과 데이타 용량도 수백만배 더 많아져야 한다는 이야기가 됩니다.  현실성이 없어진다는 이야기입니다.
* 따라서 난류 조건이 확실할 때는 적절한 난류 모델을 적용하여 계산을 더 단순화하고 대신 디테일한 난류 영역의 유체 거동의 관찰을 포기합니다.  그것이 현재의 유체동역학의 상황이죠.
[](https://youtu.be/nkyaWMNCFlQ)

### Natural Convection CFD with Elmer
* http://dymaxionkim.blogspot.kr/2015/06/natural-convection-cfd-with-elmer.html
* Navier-Stokes Equation(유체동역학방정식), Non-Compressible(비압축성)으로 바닥면을 가열하였을때 발생하는 자연대류(Natural Convection) 현상을 시뮬레이션하였습니다.
* 시간에 따라 500샷 가량 얻어내어 동영상을 만들었습니다.
* 2D 모델이며, 모델링 및 매쉬 생성은 GMSH를 사용하였습니다.
[](https://youtu.be/iA1dHt3BniE)

### Transient Linear Elastic with Elmer
* http://dymaxionkim.blogspot.kr/2015/06/transient-linear-elastic-with-elmer.html
* 초기조건으로 변위를 줬다가 제거해 줌으로써 외팔보를 자유진동하게 하고, 이것은 Transient 조건으로 시간에 따라 스냅샷을 3000개 정도 얻어내어 동역학적인 결과를 얻어낸 것입니다.
[](https://youtu.be/RA9CPvdPU0Y)
[](https://youtu.be/HGqn0NhYFFc)

### Mechanical Contact Analysis with Elmer
* http://dymaxionkim.blogspot.kr/2015/06/mechanical-contact-analysis-with-elmer.html
* 이 예제는 최근에 Elmer에 추가된 멀티바디 접촉 경계조건(Contac BC)를 적용하여 2개의 물체가 서로 꾹 눌려서 서로 반력을 받는 현상을 시뮬레이션한 것입니다.
* Scanning 조건을 사용해서 200개의 샷(Shot)을 얻어내어 동영상을 얻어내었습니다.
* PostProcessor는 ParaView를 사용하였습니다.
[](https://youtu.be/we3vLBMvJ_8)
