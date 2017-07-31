# GMSH


### 소개
* <http://geuz.org/gmsh/>
* 벨기에의 쟝-프랑수아 르마클레(Jean-François Remacle) 교수가 젊을 때 부터 이걸 계속 개발해서 무료로 공개해 왔다.  이 아저씨는 [대머리](https://yt3.ggpht.com/-f2bdRUTIWaA/AAAAAAAAAAI/AAAAAAAAAAA/2TcwimtMFTo/s900-c-k-no/photo.jpg)인데 [유튜브 개인채널](https://www.youtube.com/user/pilpoil3000)을 보니까 아무래도 헤비메탈 매니아인 듯 하다.  온통 헤비메탈 음악만 있다.  90년대에 젊은 시절을 보낸 세대 답다.
* 아무튼 이 프로그램의 핵심 기능은 유한요소해석(FEM)에 적합한 요소망(Mesh)을 만들어주는 것이다.
* 그 외에 몇가지 잡기능이 더 붙어있긴 한데, 그냥 없는셈 치는게 속 편하다.
* 오픈소스 매쉬 소프트웨어 중에서 가장 사용자가 많다.  보통 관련 전공 대학원생들이 애용하는 듯 하다.


## 배경지식
* Gmsh는 기본적으로 바텀업(Bottom-up) 모델링 기법을 따른다.  즉, 점-선-면-볼륨 순서로 올려가면서 쌓듯이 그려나가는 것이다.  따라서 제일 먼저 점부터 그려줘야 한다.
* Gmsh 모델링 방법은, GUI 도구들을 사용해서 대화식(Interactive)으로 모델링 작업하는 것도 가능하고, 직접 .geo 파일을 편집기로 코딩해 넣어서 Gmsh로 불러들이는 것도 가능하다.
* Gmsh 자체적으로 Open-Cascade CAD 커널의 기본적인 기능을 내장하고 있기 때무에, STEP 또는 BREP 포멧의 3D 데이타를 직업 읽어들이는 것도 가능하다.  다만 이때는 자체적인 .geo 파일안에 해당 데이타를 내장하지는 못하고, 점-선 요소들의 정보를 추출해내지는 못한다.  또한 가끔 경우에 따라 버그로 인해서 형상이 왜곡되는 경우가 있으므로, 로딩 후에 형상 체크를 해 보는 것이 좋겠다.  주로 필렛(Fillet) 면의 둥근 형상을 제대로 완벽하게 읽어들이지 못하는 것 같다.
* 어쨌든 형상을 구성하고 나서, 이것을 매쉬로 만들어낼 때 적절한 옵션을 다양하게 줄 수 있다.  기본적으로 GUI 상에서 모두 가능하다.  옵션 중에서 상당수는 .geo 파일 안에 포함시킬 수도 있고, 또 터미널 명령으로 gmsh를 주면서 인수로 옵션을 줄 수 있는 사항들도 있다.

### 장점
* Open-Cascade 커널을 사용하여 STEP 포멧의 3D 형상 데이타를 직접 읽어들여 매쉬 생성 가능하다.
* 사실상 독립 실행파일 하나만 있어서, 외부 라이브러리 의존성이 없어서 에러없이 쓰기 편하다.
* 단순한 형상은 텍스트 포멧의 .geo 파일을 작성하거나 GUI 도구들을 이용해서 신속하게 만들어낼 수 있다.
* 단순한 형상의 경우에는 사각형(또는 육면체) 요소망으로 만들어낼 수도 있다.
* 터미널 명령어 옵션을 줘서 .geo 파일에서 직접 .msh 파일로 만들어줄 수 있으므로, 스크립트로 다루기 좋다.
* .msh 포멧은 텍스트 파일이라서 직접 열어서 보고 분석 가능할 뿐만 아니라, 여러 오픈소스 해석기들이 읽어들이도록 연계시키기 좋은 편이다.

### 단점
* 여기서 Multi-Body를 만들어내면 Elmer에서 잘 못 읽어들이는 듯 하다.
* Multi-Body Assembley STEP 파일을 못 읽는 것 같다.  오로지 Single Body만 확실히 된다.
* Single Core CPU만 사용하도록 프로그래밍이 되어 있어서, Multi Core CPU가 있어도 매쉬 생성할 때 속도 향상에 전혀 도움이 안된다.
* 때문에 요소 개수가 아주 많은 대규모 모델을 만들어낼 때는 아무래도 좀 무리가 있는 것 같다.
* 전문적인 상용 매쉬 소프트웨어보다는 좀 기능이 떨어지지만, 그래도 여전히 꽤 쓸만하다.
* FLTK라는 오픈소스 GUI 프레임웍으로 만들어서 그런지 디자인이 좀 허접해 보이고, 메뉴 체계가 처음에는 좀 생소할 수 있다.

### 배경지식
* 삼각형(또는 사면체) 매쉬를 만들어내는 알고리즘은, Netgen 등의 기여로 인해 오픈소스 쪽에서도 상당히 신뢰성있게 잘 발달되어 있다.
* 여기서 Recombine 조작을 통해 사각형(또는 육면체) 매쉬를 만들어낼 수도 있는데, 이때는 약간 신뢰성이 떨어진다.  형상이 단순하다면 해 볼만 하다.
* 격자들이 예쁘게 잘 정렬된 구조화된 매쉬(Structured Mesh)를 만들어내려면 Transfinite 알고리즘을 이용한 조작을 해 줘야 하는데, 제약조건이 꽤 있으므로 단순한 형상에서만 실질적으로 유효하다.  Transfinite 조작이 실패하면 에러메시지를 내놓고 자기가 알아서 비정형 매쉬(Non-Structured Mesh)를 생성한다.
* 구조해석을 할 때는 반드시 2nd Order 이상으로 매쉬를 생성해야 해석 결과의 정확성을 기할 수 있다.  디폴트로는 1st Order 매쉬 생성으로 잡혀있기 때문에 까먹지 말고 유의할 필요가 있다.

### 설치 방법
* 첫번째 방법은 Ubuntu의 기본 저장소에 이미 있는 것을 그냥 설치하는 것이다.  터미널에서 아래의 명령을 주면 끝.
```bash
sudo apt-get install gmsh
```
* 첫번째 방법의 단점은 조금 뒤쳐진 버전이 설치된다는 것이다.  최신버전을 사용하고 싶다면 직접 다운로드 받는 두번째 방법을 사용하면 된다.  가급적 최신버전을 사용하는 두번째 방법을 추천한다.
```
wget http://geuz.org/gmsh/bin/Linux/gmsh-2.11.0-Linux64.tgz
sudo tar -xzvf gmsh*.tgz
rm gmsh*.tgz
sudo mv gmsh* /opt/gmsh
sudo ln -s /opt/gmsh/bin/gmsh /usr/bin/gmsh

* 그럼 아무 데서나 `gmsh` 명령을 주면 그 자리에서 실행된다.