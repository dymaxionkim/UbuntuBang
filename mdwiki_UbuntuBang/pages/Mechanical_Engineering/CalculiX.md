# CalculiX

## 개요
* 독일의 제트엔진 회사인 MTU에 재직중이던 귀도라는 엔지니어가 개발한 자체 해석기.
* 퇴사 후에도 계속 버전을 올려가면서 오픈소스로 유지.
* 오픈소스 구조해석기 중에서 Code_Aster 다음으로 완성도가 높다.

## 특징
* 아주 단순무식한 구성.  해석 실행파일 ccx와, 뷰어 실행파일 cgx 달랑 2개로 구성된다고 보면 된다.
* 덕분에 상대적으로 사용법을 익히기가 쉬울 수 있다. (단순하므로)
* 직접 소스코드를 빌드해서 설치해야 하고, 의존성 있는 라이브러리 등을 모두 수동으로 구성해 줘야 하는 등 굉장히 번거롭다는 단점이 있었으나, 최근에는 이런 점을 해결한 제3자에 의한 패키지가 몇가지 안정적으로 나오고 있으므로 이것을 이용하면 된다.
* Elmer는 멀티피직스 특성이 강하다 보니, 구조해석 부분의 기능이 상대적으로 소홀하기 때문에 아쉬움이 좀 있다.  구조해석 부분을 특화해 나가려면 여기에 집중된 CalculiX를 사용해서 보완하는 것도 좋은 전략일 것이다.
* Code_Aster는 최초 진입장벽이 CaluliX보다 상대적으로 좀 더 높다.  몇가지 요인이 있는데, (1)온라인 입문 자료 부족 (2)프랑스어의 압박 (3)인풋파일 구성 문법의 생소함 등이다.  (1)(2)번 문제는 시간이 지나면서 점차 나아지고 있는 것 같다.
* 기타
  1. 인풋파일은 Abaqus와 유사한 .inp 파일로, 비교적 생성 및 편집이 용이하다. CalculiX Launcher를 사용하면 Salome에서 만들어진 .unv 파일을 간단히 .inp 파일로 변환할 수 있다.
  2. 아웃풋 파일은 호환성에 문제가 좀 있다.  전용 포멧인 .frd 파일은 cgx에서만 읽을 수 있기 때문에, 이것은 gmsh 또는 paraview에서 읽어들이기 위해서는 파일 포멧을 변환해 주어야 한다.  그런데 아직 이 부분에 대한 개발이 전반적으로 부실하다.  아쉬운대로 cgx에서 필요한 그림이나 동영상을 추출해 낼 수는 있다.
  3. CaculiX가 사용하는 편미분방정식 솔버는 Arpack이다.  멀티코어 지원하는 초고속(?)의 안정적인 포트란 코드로서, Elmer에서 디폴트로 사용하는 솔버들보다 속도 면에서 상당히 빨라 보인다.  그럼에도 불구하고 Abaqus 같이 잘 튜닝된 상용 코드 보다는 느리다.

## 튜토리얼
* MIT 검색엔진에서 유용한 것들을 찾아볼 수 있다.
* http://search.mit.edu/search?client=mithome&site=mit&output=xml_no_dtd&proxystylesheet=mithome&num=15&submit=Search&as_q=calculix
* http://web.mit.edu/calculix_v2.7/CalculiX/cgx_2.7/doc/cgx/cgx.html


## 윈도우 환경에 설치
* 3가지 방법이 있다.
  1. 윈도우 환경에서 직접 소스코드를 빌드 : 시간이 남아돈다면 해 보자.
  2. bConverged사에서 제공해 주는 패키지 설치 : 그냥 받아다가 설치하면 곧바로 사용 가능하다.  ( http://www.bconverged.com/products.php )  다만 아직 최신 버전이 반영되어 있지는 못한 것 같다.
  3. CalculiX Launcher 윈도우 버전 설치 : 러시아 쪽 사람이 제공하는 것 같은데, 이쪽을 더 추천하고 싶다.  http://calculixforwin.blogspot.kr/2015/05/calculix-launcher.html  이곳에서 Launcher-02 for Windows 32-64 bit 라고 써져있는 링크로 들어가서 다운로드 받으면 된다.  압축을 원하는 장소에 풀고, 안에 들어있는 설명서를 보고 몇가지 조정만 해 주면 된다.

* 개인적으로, 최초 학습 목적이 아니고 실제 업무에 적용해 보려고 한다면 윈도우 환경에서 뭔가 해 보려는 생각 자체를 버리는 것이 좋다고 생각한다.  낭비요소와 제약조건이 너무 많기 때문이다.

## 리눅스 환경에 설치
* 여러가지 방법이 있다.
  1. 리눅스 환경에서 직접 소스코드를 빌드 : [참고1](http://www.libremechanics.com/?q=node/9), [참고2](http://www.dhondt.de/), [참고3](http://nicklj.com/?p=639), [참고4](http://greyattic.blogspot.kr/2014/02/devtip-ubuntu-calculix.html)
  2. 리눅스용 바이너리 파일을 직접 다운로드 받아서 실행 : [참고2](http://www.dhondt.de/)에서 Linux wxwcutable을 다운로드.  의존성 있는 Rapack 같은 라이브러리들은 따로 설치해 줘야 하고, 심볼릭 링크 등을 직접 설정해 줘야 하기 때문에 번거롭다.
  3. FreeCAD 0.16 이상의 버전을 설치 : 기본적으로 FreeCAD 안에 ccx와 Netgen이 통합되어 있다.  그러나 cgx는 누락되어 있기 때문에 불완전하고, ccx는 최신버전은 아닌 것 같다.  [참고](http://www.freecadweb.org/wiki/index.php?title=FEM_Install)  cgx를 따로 설치해서 연결하는 것은 [이쪽](http://forum.freecadweb.org/viewtopic.php?t=10789)으로 참고.
  4. CalculiX Launcher 리눅스 버전 설치 : 러시아 쪽 사람이 제공하는 것 같은데, 이쪽을 더 추천하고 싶다.  http://calculixforwin.blogspot.kr/2015/05/calculix-launcher.html  이곳에서 Launcher-02 for Linux (Ubuntu tested) 64 bit 라고 써져있는 링크로 들어가서 다운로드 받으면 된다.  압축을 원하는 장소에 푼다.  최신버전으로 패치하려면 NEW!!! 2016/07/12 Launcher-03 beta (32&64bit linux) updates for Launcher -02 라고 써져있는 링크를 추가로 다운로드 받아서, 압축을 풀어서 덮어써 주면 된다.  설명서를 보고 파일네임,디렉토리네임 등을 조정해 준다.
  5. CalculiX Extras : https://www.openaircraft.com/calculix-extras/  CUDA 기능을 사용할 수 있는 확장판이다.

## CalculiX Launcher (Lazarus Launcher)
* http://calculixforwin.blogspot.kr/2015/05/calculix-launcher.html 이 페이지를 자세히 보면, 마지막에 Salome-meca 리눅스용 배포본과, 또 Salome Platform 윈도우 배포본도 함께 제공해 준다.  윈도우 환경에서는 이것을 설치하는 것이 좋은 것 같다.
* 설치 및 사용 방법을 간단히 보인 동영상 클립도 있으므로 참고가 된다. https://youtu.be/ViLf4Z4VE1Y
* 그리고 이 사람의 개인 유튜브 채널을 구독하는 것도 좋겠다.  https://www.youtube.com/channel/UCiAhHLXtYGcQC9eZIzd_COg

## 결과 파일 .frd를 .vtu로 변환
* CalculiX에서 해석을 끝내면 결과파일은 cgx전용의 .frd 포멧으로 만들어진다.
* Paraview에서 결과를 보려면 .vtu 파일이 필요하므로, 변환기가 필요하다.
* [윈도우용 변환기1](http://caelinux.com/CMS/index.php?option=com_kunena&func=view&catid=6&id=8189&Itemid=300025)
* [윈도우용 변환기2](https://sourceforge.net/projects/candystore/)
* [윈도우용 변환기3](https://sourceforge.net/projects/vtkpost/) ::: 제일 완성도가 높음
* 리눅스용 변환기는 아직 충분히 성숙된 것을 찾을 수가 없다.
