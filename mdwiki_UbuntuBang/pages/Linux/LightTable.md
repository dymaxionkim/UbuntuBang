# LightTable

### LightTable ?
* 텍스트 편집기다.  Clojure라는 언어를 주로 사용해서 만들었다고 한다.
* Chris Granger라는 사람이 KickStarter 크라우드 펀딩을 받아서 개발했고 오픈소스화했다.
* 코딩하는 사람들이 애용하는 편집기로는 Vim, Emacs 같은 오래된 것들부터 Sublime Text, Atom 등등해서 여러가지 많은데 각자 장단점이 있다.  제일 인기가 많다는  Sublime Text의 경우에는, 한글입력이 잘 안되고 유료버전이 따로 존재하는 등의 제약 때문에 사용하고 싶은 마음이 생기지 않았다.
* 그중에서도 LightTable은 최신기술로 개발된 오픈소스 에디터이므로 앞으로 전망이 좋은 듯 하다.  또한, Julia Lang을 위한 전용 편집기인 Juno가 LightTable로 만들어져 있기 때문에 활용성도 높을 것 같다.
* 기본적인 사상은, (1)단순+간단  (2)크로스플랫폼  (3)단축키권장 등인 듯 하다.  [여기](https://vimeo.com/36579366)서 영감을 받았다고 한다.

### 장점
* 약간 전위적인 기술을 적용하여 개발된 최신 툴이다. 새로운 시도는 항상 엔지니어를 자극한다.
* Eval 기능이 있다. 한 줄씩 실행해 보면서 문제를 찾기 좋다.
* 쓸데없는 잡기능이 없고 단순한 디자인으로 심리적인 부담을 주지 않는다.
* Julia 관련 플러그인이 제공된다.

### 단점
* 살짝 느린 감이 있다.  하지만 대규모 프로젝트를 할 일은 없으므로 상관없다.
* 아직 자잘한 버그들이 있는 것 같다.  하지만 사용상 문제는 거의 없다.

### 배경지식
* LightTable과 흔히 비교되는 최신 에디터들로는 Brackets, Atom, Sublime Text 정도가 있는 듯 하다.
* 이중에서 Sublime Text는 가장 성숙한 선배에 해당한다.  하지만 상업적 성격이 가장 강하므로 사용하지 않기로 했다.
* Brackets, Atom은 LightTable과 구조적으로 닮았다.  즉 내부적으로 [Node.js 및 Webkit을 이용한 일종의 웹앱](http://nwjs.io/)이라고 한다.  즉 로컬 서버가 생성되어 로컬 클라이언트와 접속한 후 일종의 웹브라우저로 화면에 내용을 보여준다.  이런 형태는 아직은 조금 실험적인 성격이 강한 단계라고 생각되나, 현존 기술중 가장 궁극적인 형태의 앱 구조가 아닐까 싶다.  다만 덕분에 속도가 느려진 것이다.  속도 문제는 아마 Node.js 서버단 쪽이 발전하면서 개선될지도 모른다.
* 클라이언트 단의 프론트엔드 쪽에서 사용자가 편집한 코드의 Highlight 효과가 있는 웹에디터로 [CodeMirror](http://codemirror.net/)라는 Javascript 코드를 갖다 쓴다고 한다.  여기서도 속도 저하 요인이 있다고 하는데, 역시 계속 개선되지 않을까 기대한다.
* LightTable의 가장 독자적인 특징은, Clojure라는 언어로 개발되었다는 점이다.  소프트웨어 공학에 정통하지 못해서 다는 이해하지 못하나, 알아낸 사실만 나열해 보자면...  Clojure는 오랫동안 해커들이 칭송해 왔지만 대중화에 계속 실패해왔던 Lisp 언어의 가장 최신판의 한 종류에 해당하는 듯 하다.  요즘 소프트웨어 개발쪽을 보니까 함수형 언어 및 그 원조격인 Lisp에 대한 관심과 발전 경향이 뚜렷하게 보이기 시작하고 있다.
* Clojure의 구현체는 현재 (1)Java가상머신 위에 올라타서 돌아가는 놈 (2)Javascript 위에 올라타서 돌아가는 놈의 2가지가 있는 것 같은데, LightTable은 당연히 둘 다 활용해서 개발된 것 같다.
* Lisp의 제일 큰 특징은, 모든 것을 데이타로 취급해서 리스트화하여 마음대로 다룰 수 있다는 점이라고 한다.  그래서 그런지, LightTable의 Setting에 들어가보면 설정파일 텍스트 문서를 직접 편집하는 형태로 되어 있는데, 그 내용을 보면 그냥 Clojure의 소스코드 형태 그대로다.  이걸 S-Expression이라고 한다는데, 마치 XML 형태로 된 설정파일과 같은 기능을 하는 느낌이다.  XML과 문법만 다르다.  얼른 보기에는 S-Expression 쪽이 더 간결해 보인다.
* 아무튼 이와 같이 상당히 전위적인 개념으로 설계된 에디터라는 점은 확실히 알겠다.  Julia 코딩용으로 플러그인도 있겠다 해서 딱 맞는다.
* 또한 Markdown 문서를 렌더링해서 보여주어 편집을 도와주는 플러그인도 있어서, Markdown 문서 편집할 때도 쓸 수 있다.
* 기타 Clojure, Python, Haskel, Javascript 등등해서 지원하는 언어들이 여럿 있어서 필요하다면 쓸 수 있다.
* Git 관련 플러그인도 있긴 한데, 아직 버전도 낮고 사용법도 파악하지 못해서 일단 보류...
* 아무튼 최신 에디터 중에서 가장 해커 스럽다는 느낌이 드는 물건이다.

### 설치 방법 1
* 다운로드 : <http://lighttable.com/>
* 압축풀기 : 아무데나 풀어도 되지만 가급적 root 권한으로 `/opt/LightTable`으로 넣는 것이 좋겠다.
* 실행파일 심볼릭 링크 : 터미널에서 `sudo ln -s /opt/LightTable/LightTable /usr/bin/LightTable` 명령을 준다.
* 그럼 아무 데서나 `LightTable` 명령을 주면 그 자리에서 실행된다.

### 설치 방법 2
* 참고기사 : <http://itsfoss.com/install-lighttable-ubuntu/>
* 다음과 같이 PPA 저장소를 등록하고 설치한다.
```
sudo add-apt-repository ppa:dr-akulavich/lighttable
sudo apt-get update
sudo apt-get install lighttable-installer
```

### 단축키
* Auto-Completion : `Tab`
* Evaluation : `Ctrl+Enter`
* Save File : `Ctrl+s`
* Commands : `Ctrl+Space`
* 단축키 설정을 수정하고자 할 때는, Commands에서 `Settings: User keymap` 및 `Settings: Default keymap` 해서 적절히 수정해서 저장하면 된다.

### 줄번호 표시하게 설정
* Commands에서 `Settings: User behaviors` 해서 나오 설정에서, 다음 내용을 추가한다.
```
 ;; 라인 넘버 보이게 설정 추가
 [:editor :lt.objs.editor/line-numbers]
```

### 폰트 변경
* Commands에서 `Settings: User behaviors` 해서 나오 설정에서, 다음 내용을 추가한다.  폰트, 사이즈, 줄간격은 알아서 적당히 수정하면 된다.
```
 ;; 폰트 수정 추가
 [:editor :lt.objs.style/font-settings "나눔고딕코딩" 13 1]
```

### 마크다운 편집하는 플러그인 설치 방법
* 터미널에서 `sudo LightTable`해서 루트 권한으로 실행할 것.
* Commands에서 `Plugins: Show plugin Manager` 해서 `markdown` 검색한 다음에 Install하면 됨.
* LightTable을 종료하고, 다시 사용자권한으로 `LightTable` 실행.
* 아무 .md 파일 하나 만들어서 내용을 써 넣어 본 후, Commands에서 `Markdown: watch this editor for changes` 해 주면 랜더링된 화면이 나온다.  랜더링 화면은 편집하면 즉시 반영된다.  끝!

### 기타 자세한 설명서
* [표준설명서](http://docs.lighttable.com/)
* [일본사람이 쓴 간단한 설정 방법 설명서](http://translate.google.com/translate?hl=ko&sl=auto&tl=ko&u=http%3A%2F%2Fqiita.com%2Ftenten0213%2Fitems%2F4089179cbc88bbdd950c)
