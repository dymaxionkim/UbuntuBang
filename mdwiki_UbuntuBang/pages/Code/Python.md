# Python


## 패키지 관리

* pip 패키지들을 모두 한꺼번에 업데이트 ( http://blog.kimtae.xyz/171 )
```
sudo pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs pip install -U
```



## Anaconda로 Python 환경을 구축할 경우

### 개요
* 기본적으로 시스템 구축시 설명된 Scientific Python 및 Julia 환경 구축 방법은, 사실 여러가지 단점이 많다.
* 우선 버전관리 및 패키지 관리가 번거로운 편이고, 설치 자체도 하나하나 다 해줘야 하므로 번거롭다.
* 또 시스템에 따라 에러가 발생하면 추적해서 그걸 고쳐주는 등의 번거로움 또한 있다.
* 물론 이 과정을 통해 시스템의 구성과 각 모듈들의 역할에 대해 더 깊이 이해할 수 있다는 장점도 있다.
* 이렇게 기본적인 설치 방법 말고, Anaconda 패키지를 통해 한 방에 설치하는 방법을 기술해 본다.
* 실무적으로 사용하기 위해서는 이 방법이 좀 더 나은 것 같다.

### Anaconda 설명서
* https://docs.continuum.io/
* http://conda.pydata.org/docs/get-started.html

### Anaconda 설치
* 다운로드 받기 : http://continuum.io/downloads.html
* 다운로드 받은 거 실행하기
* Path 잡아주기 : `l3afpad ~/.bashrc`해서 다음 구문 추가 (경로는 자신의 경로에 맞게 수정)
     ```
     export PATH="/home/username/anaconda/bin:$PATH"
     ```
### Anaconda 시운전
* 한글 번역 : http://egloos.zum.com/mataeoh/v/7052271
  1. 업데이트
  ```
  conda update conda
  conda update anaconda
  ```
  2. 자신의 주력 환경 생성 (환경이름=Basic, 프로그램이름=ipython)
  ```
  conda create --name Basic ipython
  ```
  3. 생성한 환경으로 전환
  ```
  source activate Basic
  ```
  4. 전환한 환경의 비활성화
  ```
  source deactivate
  ```


### Anaconda 상에서 Julia 설치 및 설정
* 설명서 : http://quant-econ.net/jl/getting_started.html
* Conda.jl 설치 (julia 패키지 소스를 github가 아닌 Conda로 할 수 있음)
  1. julia 실행
  2. 다음 명령 실행 / [참고](https://github.com/Luthaf/Conda.jl)
  ```
  Pkg.add("Conda")
  ```
* PyPlot 설치 (자동으로 MatPlotLib도 설치됨)
  1. julia 실행
  2. 다음 명령 실행 / [참고](https://github.com/stevengj/PyPlot.jl)
  ```
  ENV["PYTHON"]=""
  Pkg.add("PyCall")
  Pkg.build("PyCall")
  Pkg.add("PyPlot")
  ```
* 기타 기본적인 패키지 설치
  1. julia 실행
  2. 다음 명령 실행
  ```
  Pkg.init()
  Pkg.add("Jewel")
  Pkg.add("Images")
  Pkg.update()
  quit()
  ```

