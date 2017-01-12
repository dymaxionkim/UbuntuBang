
# Jupyter
* Jupyter가 뭔지는 인터넷 검색 요망!


## Jupyter Notebook을 외부에서 접속 가능하도록 설정하기

* http://jupyter-notebook.readthedocs.org/en/latest/public_server.html#running-a-public-notebook-server
* http://jupyter-notebook.readthedocs.org/en/latest/public_server.html#securing-a-notebook-server

* Jupyter Notebook을 로컬컴퓨터에서만 이용하기에는 아까우므로, 외부에서 어디서나 접속 가능하도록 해서 활용도를 높여 봅시다.
* 새로운 Jupyter Notebook 설정 프로파일을 생성합니다.
```
jupyter notebook --generate-config
```
* 그러면 `/home/dong/.jupyter/jupyter_notebook_config.py` 파일이 새로 생겨난 것을 확인할 수 있습니다.  이 파일을 편집기로 열어 봅시다.  대략 500줄을 좀 넘어가는 코드가 나옵니다.  그런데 대부분 주석이죠.  필요한 부분을 수정하면 됩니다.

* 먼저 패스워드 관련 부분.  일단 패스워드를 해쉬코드로 생성해 내야 합니다.  이와 관련된 기본적인 파이썬 함수가 있으므로, 파이썬 상태에서 이 함수를 이용해서 원하는걸 얻어내야 합니다.  터미널에 `python` 쳐서 파이썬 REPL로 들어간 다음,
```
from notebook.auth import passwd
passwd()
```
* 위와 같이 명령을 때려주면 비밀번호를 치라고 나오는데 확인차 2번 쳐 주면 해쉬코드로 된 비밀번호 코드가 나옵니다.  이걸 긁어다가, 'jupyter_notebook_config.py' 파일을 편집기로 열어서 다음과 같이 패스워드 아규먼트값을 할당해 줍니다.  물론 이 값 앞에 있는 '#' 주석 기호는 삭제해서 해제해 주는 것을 잊지 말구요.
```
c.NotebookApp.password = u'sha1:67c9e60bb8b6:9ffede0825894254b2e042ea597d771089e11aed'
```
* 그리고 SSL을 설정해서 쓰도록 해 주는 것도 보안상 좋은데, 여기서는 일단 생략합니다.  필요하다면 [도큐먼트](http://jupyter-notebook.readthedocs.org/en/latest/public_server.html#securing-a-notebook-server)를 참고하면 됩니다.

* 이제, 'jupyter_notebook_config.py' 파일에서 다음 부분을 더 적용해 줍니다.  이중에서 포트 번호는 원하는걸로 넣고요.
```
# Set ip to '*' to bind on all interfaces (ips) for the public server
c.NotebookApp.ip = '*'
# The port the notebook server will listen on.
c.NotebookApp.port = 8100
# Whether to open in a browser after starting. The specific browser used is
# platform dependent and determined by the python standard library `webbrowser`
# module, unless it is overridden using the --browser (NotebookApp.browser)
# configuration option.
c.NotebookApp.open_browser = False
```
* 이제 설정파일 편집은 됐고, 지정해 준 포트로 외부 접속이 되도록 방화벽 설정이 되어 있는지 확인해서 풀어줄 필요가 있습니다.  현재 사용중인 UbuntuBang은 방화벽이 없으므로 생략.  또 프록시 관련된 것도 생략.
* 우분투 계열의 다른 배포판을 사용할 경우, 기본적으로 방화벽이 켜져 있는 경우에는 다음 명령으로 열어줄 수 있습니다.
```
sudo ufw allow 포트번호
```
* 이제 남은 건, 집에서 보통 공유기를 사용하게 되는데 공유기 부분의 설정을 건드려주면 좀 더 편리해 질 것입니다.  ipTime 공유기의 경우, 포트포워드 설정 부분을 건드려서 외부에서 접속할 때, 예를 들어 여기서 지정된 8100 포트를 80 포트로 포워딩되록 바꿔서 편리하게 해 준다던가...  또 ipTime의 특수기능으로 DDNS 설정에 들어가서 호스트 이름을 '지정한이름.iptime.org'로 등록해 줘서 가정용 유동ip 상황일때도 도메인 네임으로 편하게 접속할 수 있도록 해 준다던가 하는 것들을 건드려주면 되겠습니다.
* 현재의 Jupyter Notebook은 싱글유저만 되는데, 멀티유저로 서비스하려면 JupyterHub를 설치해서 운용해야 합니다.  다만 이런식으로 퍼블릭하게 나가려면 좀 더 제대로 된 서버를 구축하고, JupyterHub는 Docker 컨테이너로 운용을 한다던가 하는 등의 관리 전략을 세우면 될 것 같습니다만...  여기서의 목표는 서버 관리자가 되는게 아니므로 이정도까지만 하겠습니다.  다만 외부 인터넷 연결되는 서버 운용시 외부 해커 침입에는 유의해 주는 것이 좋겠습니다.

## Jupyter Notebook을 정적 웹서버로도 활용하기
* 이건 일종의 팁 같은 걸로 볼 수 있을 듯 합니다.
* Jupyter Notebook은 기본적으로 자신을 실행시킨 계정의 홈 디렉토리에서 시작합니다.  예를 들어 dong 이라는 계정으로 별다른 옵션 없이 실행하면 `/home/dong/`에서 그 아래의 모든 디렉토리를 볼 수 있죠.
* 그럼 `/home/dong/index.html` 파일을 하나 적당히 넣어놓고 이걸 호스팅 할 수 있지 않을까요?  가능한 것 같습니다.
* Jupyter Notebook 서비스의 URL은 보통 'http://주소:포트' 이렇게 됩니다.  (예: http://dymaxionkim.iptime.org:9000 )
* 그런데 이렇게 주소를 입력하면 자동으로 'http://주소:포트/tree' 로 들어가 집니다.  (예: http://dymaxionkim.iptime.org:9000/tree ) 여기서 서비스가 이루어지죠.
* 그럼 주소를 변경해서, 'tree' 부분을 'files'로 변경해 봅니다.  'http://주소:포트/files/index.html' (예: http://dymaxionkim.iptime.org:9000/files/index.html )  이런 식으로요.
* 물론 이때 `/home/dong/index.html` 파일을 만들어 둔 상태로요.  그러면 이 파일로 웹호스팅이 됩니다.
* 응용예로, '`/home/dong/MDwiki/' 디렉토리를 만들어서 그 안에 MDwiki를 넣어두고, `/home/dong/index.html` 파일에는 편집기로 내용을 아래와 같이 해서 저장해 두면...
```
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<!--
     Override `MDwiki` below with your default language and country code
  -->
<meta http-equiv="refresh" content="0; url=MDwiki/" />
<title>Loading</title>
</head>
<body>
<h1>쫌만 기둘려!</h1>
</body>
</html>
```
* `/home/dong/index.html`  파일이 웹에 띄워지면서 그 아래에 있는 'MDwiki/' 디렉토리로 옯겨가서 그 안의 '/home/dong/MDwiki/index.html' 파일을 로드해 줍니다.  그러면 MDwiki 서비스가 되는거죠.



# Jupyter Extensions 설치하기

## Gist Extension

### Gist?
* Github에서 제공하는 서비스.
* 소스코드 조각을 예쁘게 꾸며서 고정된 주소를 부여해 준다.
* 이 주소를 이용해서 웹에서 링크해 주면 된다.

### Gist & Jupyter
* Jupyter Notebook에서 문서를 작성하면 .ipynb 확장자의 파일이 된다.
* JSON 형식을 가진 텍스트 파일이다.
* Github에서는 이 파일을 자동으로 해석,렌더링해서 보여주는 기능이 있다.
* 따라서 이 파일을 Gist에 올리면 그냥 하나의 웹페이지가 되고, 주소가 부여된다.
* Gist에 올린 다음, 개인 블로그 등에 링크/임베드 시키면 웹 게시 끝.

### Gist로 .ipynb 파일 업로드
* 만들어진 파일을 매번 하나하나 Github에서 업로드 하는 것이 매우 귀챦다.
* 버튼만 누르면 현재의 문서를 Gist에 업로드해주는 것은 없을까?
* 당연히 있다!
* 이 기능을 해 주는 Jupyter Extensions를 찾아보자.

### Jupyter Extensions for Gist Upload
* 검색을 해 보면 가장 눈에 띄는 것이 2종류 있다.

> https://github.com/minrk/ipython_extensions#gist

> https://github.com/ipython-contrib/IPython-notebook-extensions

* 둘 중 골라서 설치하면 된다.

### Jupyter Gist Extension 설치 (첫번째 것)
* 아래의 문서는 Jupyter Notebook에서 작성되었고, Gist에 업로드되어 있다.

http://nbviewer.jupyter.org/gist/dymaxionkim/d9a2ab620303982df1af?flush_cache=true

* 다만 이 익스텐션은, Python 스크립트에 의해서 설치되는 방식이라서 실패 확률이 높은 것 같다.
* 권장하지 않는다.

### Jupyter Gist Extension 설치 (두번째 것)

* 대신 이것을 사용하는 것이 더 낫겠다.
* 다음 설치 명령을 주면 설치된다.
```
pip install https://github.com/ipython-contrib/IPython-notebook-extensions/archive/master.zip --user
```
* Python Notebook을 종료했다가 다시 시작한다.

* 환경 설정을 위해서는, 웹브라우저 주소창에 다음 주소로 들어간다.
```
http://localhost:8888/nbextensions
```
* 만일 위 주소로 들어갔는데, 빈 화면만 보인다면, 다음 터미널 명령으로 설치본을 업그레이드 해 본다.
```
pip install --upgrade https://github.com/ipython-contrib/IPython-notebook-extensions/archive/master.zip --user
```
* 업그레이드 하면서 터미널에 어떤 문제가 있는지 메시지가 뜬다.  보통 퍼미션 문제이므로, 메시지를 잘 보고 해당 파일의 퍼미션이 root로 되어 있을 경우에는 풀어주고 다시 시도한다.  그렇게 하면 해결이 된다.
* 여러가지 Extension들이 상당히 많이 있는데, 이 중에서 `Gist-it`을 선택한다.
* `Gist-it`을 활성화하기 위해서는 인증토큰을 설정해 줘야 한다.
* 인증토큰을 위해서 우선 웹브라우저에 새 탭을 열고 다음 주소로 들어간다.
```
https://github.com/settings/applications
```
* 그리고 이제, 좌측 메뉴 중에서 'Personal access tokens'를 누르면 새로운 페이지가 뜬다.
* 여기서 'Generate new token' 버튼을 누른다.
* 'Token description'을 적당히 적어주고, 'Select scopes' 카테고리에서 'gist'가 체크되어 있는지 확인후 'Generate token' 버튼을 눌러준다.
* 이제 Pernoal access token이 만들어졌다. 코드를 긁어다 복사해서, `Gist-it`의 인증코드 요구 팝업창에 넣어주면 된다. (약간의 시차가 필요할 수도 있으므로, 코드 생성 이후 조금 기다렸다 하는 것이 좋겠음.)
* 그리고, 토큰을 복사해 넣어준 입력칸 바로 밑에 `Gists default to public.`의 라디오 버튼을 체크해 준다.
* Public으로 해 줘야 남에게 보여줄 때 그냥 보여지게 할 수 있기 때문이다.
* 별도의 저장 버튼은 없고, Activate 버튼이 흐리게 되어 있으면 이미 Activate 상태라고 보면 될것같다.  그냥 빠져나오면 된다.

### Gist it!

* 이제 Jupyter Notebook에 보면 Gist-it 버튼이 하나 추가된 것을 확인할 수 있다.
* 이 버튼을 누르면 팝업이 뜨는데, `Gist ID` 창은 그냥 비워둔 채로 실행하면 된다.
* `Gist ID`는 기존에 해당 문서의 Gist가 이미 있을 경우에, 그 번호를 써 넣어 주면 버전을 올려주는 역할이다.
* Gist가 올라갔으면, 다음 주소로 링크해서 활용하면 된다. (`GIST_ID`는 얻어진 GIST ID 번호를 넣어준다.)
```
https://gist.github.com/dymaxionkim/GIST_ID
```

### Gist 사용의 의미

* 기존에는, .ipynb 파일을 html로 변환한 후 그것을 웹서버에 업로드하고, 업로드 된 주소를 링크시켜 게시하는 과정을 거치게 된다.
* 그러나, 개인 웹사이트가 없는 경우에는 안정적으로 마땅히 업로드 할 만한 곳이 없다는 문제가 있다.
* 또 매번 수동으로 업로드 하는 일도 꽤 귀챦게 된다.
* Gist로 이 과정을 한 번에 끝내고, 공유 및 검색이 편하도록 해 줄 수 있다.


## RISE Extension

### RISE?

* https://github.com/adl/RISE
* Jupyter에 애드온으로 설치하는 익스텐션 중의 하나.
* Jupyter로 작성한 .ipynb 파일을 그대로 프리젠테이션 용으로 쓸 수 있게 해 준다.
* Jupyter Notebook 안에 통합되어 버튼이 추가된다.
* 본 강의자료는 RISE로 프리젠테이션 되고 있다.
* Reveal.js 기반

### RISE 설치 (1)

* Git 설치 (이미 설치되어 있으면 생략) : [참조](http://dymaxionkim.github.io/mdwiki/#!pages/Code/Github.md)
```
sudo apt-get install git
```

* Github의 저장소에서 내 컴퓨터로 Clone해 오기
```
git clone https://github.com/adl/RISE.git
```

* Clone 해 온 폴더로 이동
```
cd ~/github/RISE
```

* 설치 명령 입력
```
python setup.py install
```

* Jupyter를 다시 시작한다!


### RISE 설치 (2)
* anaconda를 통해서 Python 플랫폼을 구축했을 경우에는...
* jupyter-nbextension enable rise --py --sys-prefix


### RISE 사용의 의미

* 기존에는, nbconvert 명령을 이용해서 일일이 ipynb 파일을 슬라이드 html 파일로 변환했다.
* RISE를 사용해서 Notebook 상에서 그냥 곧바로 슬라이드를 보여주고, 내용을 수정하거나 직접 실행하면서 진행이 가능하다.


## 프리젠테이션을 그대로 PDF로 인쇄하기

* 참고 : https://github.com/damianavila/RISE/issues/127
* RISE를 사용하는 상태에서는 인쇄가 곤란하다.  대신 다음 방법을 사용할 수 있다.
* 먼저 가상 PDF 프린터가 존재해야 한다.
* 웹브라우저는 HTML5가 지원되는 모던 웹 브라우저여야 한다. (보통 구글크롬에서 잘 된다고 하는데, 내 경우엔 크롬에선 레이아웃이 엉망이 되고 대신 파이어폭스에서 안 깨지고 잘 되었다.)
* 터미널 명령으로 다음을 친다.
```
jupyter nbconvert --to slides 문서이름.ipynb --post serve
```
* 그러면 웹브라우저가 실행되면서 슬라이드가 포스팅될 것이다.
* 포스팅된 웹브라우저 주소창의 주소는 다음과 같은 형식으로 되어 있다.
```
http://127.0.0.1:8000/문서이름.slides.html#/
```
* 여기서 다음과 같이 마지막 문구인 `#/`를 `?print-pdf#/`로 대체해 주고 엔터.
```
http://127.0.0.1:8000/문서이름.slides.html?print-pdf#/
```
* 이제 웹브라우저로 인쇄버튼을 누르고 미리보기에서 다음 설정을 고친다.

> 종이를 '가로'로 설정한다.

> 여백은 '없음'으로 해서 없앤다.

> 배경 그래픽은 체크해서 인쇄에 반영되도록 한다.

* 그리고 가상 PDF 프린터를 선택해서 파일로 인쇄를 하면 끝.


## Jupyter 및 Julia를 윈도우 환경에서 설치하기

* 참고 : http://quant-econ.net/jl/learning_julia.html
* Scientific Python 배포판 중의 하나인 Anaconda를 설치하고, Julia를 직접 설치 후 IJulia를 깔아주면 된다.
* 단점은...
  1. 엉뚱하게 다른 소프트웨어의 동작이 이상해질 수 있다.  내 경우엔, PTC CREO 2.0이 실행되지 않아서 일을 할 수 없는 경우가 있었다.  원인은 path로 잡힌 것들 중에서 양쪽에서 모두 쓰는 C라이브러리가 있었던 모양인데, Path 때문에 CREO 쪽에서 Anaconda 내의 라이브러리를 불러오다 보니 Crush된 경우.  이때는 답이 없다.  그냥 리눅스에서 사용하는게 맘 편하다.
  2. Anaconda 패포판에 의지하다 보니, 아무래도 업데이트가 한발씩 늦다.
* 장점은...
  1. 전에는 Anaconda 배포판이 32비트 버전만 제공된다는 단점이 있었는데, 최근에 보니 전부 64비트 버전도 전격 제공하고 있다.  이 부분은 박수!
  2. Anaconda 배포판에만 붙어 나오는 몇가지 유틸리티들이 함께 기본사양으로 들어가 있다.  예를 들어 프리젠테이션 툴 같은것.  대충 보니 꽤 좋다.
  3. 설치하는데 삽질을 안해도 된다.  일단 받아다 깔면 작동이 된다.
  
