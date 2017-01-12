# Atom

### Atom ?
* Github에서 제공하는 에디터.
* 웹킷+node.js 프레임웤인 엘렉트론 기반이다.
* 최근에 Julia의 공식 에디터가 LightTable에서 Atom으로 바뀌었다.

### 장점
* 강력한 에디터를 지향하는 야심이 보인다.

### 단점
* 덕분에 좀 지저분한 느낌이다.


### 설치 방법 1
* deb 패키지 다운로드 : <https://atom.io/>

### 설치 방법 2
* 참고기사 : <http://www.webupd8.org/2014/05/install-atom-text-editor-in-ubuntu-via-ppa.html>
* 다음과 같이 비공식 PPA 저장소를 등록하고 설치한다.
```
sudo add-apt-repository ppa:webupd8team/atom
sudo apt-get update
sudo apt-get install atom
```

### Juno 패키지 설치
* Atom에 플러그인되는 Julia 개발환경이다.
* `Ctrl+,` 단축키를 치면 Setting 화면이 나온다.  여기서 uber-juno를 검색해서 깔아준다.
* 이때 진행하다가 중간에 실패하게 되는데, 이유는 Julia 패키지 업데이트가 다 안되어 있기 때문이다.
* 먼저 터미널에서 cmake를 설치해 준다. (업데이트할 때 MbedTLS라는 패키지가 빌드될 때 사용하게 됨)
```
sudo apt-get install cmake
```
* 이후 터미널에서 julia를 쳐서 Julia 상태로 들어간 후, `Pkg.update()` 명령을 쳐서 업데이트를 진행해 준다.
* 그리고 Atom을 실행하면 마저 진행된다.  진행 과정은 Atom이 예쁘게 메시지를 띄워서 고지해 준다.
* 다 되고 나면, 그 다음부터는 Atom이 Julia를 위한 환경이 된다.  이제부터는 여기서 직접 Julia를 즐기기가 좋다.