# Docker

![Docker](https://www.docker.com/sites/all/themes/docker/assets/images/logo.png)

## Ref.
* 가장 빨리 만나는 도커 ::: http://www.pyrasis.com/private/2014/11/30/publish-docker-for-the-really-impatient-book
* DockerHub ::: https://hub.docker.com
* 도커 한국어 문서 모음 ::: https://github.com/remotty/documents.docker.co.kr
* 과학기술 파이썬 툴체인 가상화 ::: http://raspberry-pi.xwmooc.org/raspberry-pi-programming-science-docker.html
* 도커 사용법 예제와 함께 설명 ::: https://github.com/leeplay/study/tree/master/docker/thedockerbook
* Doker run 특성 관련 ::: https://bestna.wordpress.com/2014/11/10/docker-container-run-%EC%9D%B4%EC%95%BC%EA%B8%B0/
* 사용법 아주 짧게 요약 ::: https://github.com/koojh89/wiki/wiki/docker
* 치트 시트 ::: https://gist.github.com/nacyot/8366310

## Docker ?
* 이게 뭔지는 정확하게 구글링해서 알아보면 됩니다.
* 간단히 말해, 필요한 운영체제 환경을 격리시켜서 모듈화하는 개념이죠.  그런데, 빠르고 용량도 적게 먹고, 또 사용법만 습득하면 의외로 간단합니다.
* 다른 사람들이 미리 이미지를 떠 놓은 것들을 https://hub.docker.com 에서 공유하기 때문에, 필요한 환경을 검색해서 가져다 쓰면 됩니다.

## 응용
* JupyterHub 서비스
* RedMine 프로젝트 관리 도구 서비스
* PDM 서비스

뭐 이런 것들을 따로따로 Docker로 묶어서 제공한다면 관리가 상당히 편해지지 않을까 생각되네요.  실제 사용 방법은 천천히 익혀봐야 겠습니다.

## 설치 방법 1
Note: 쉽게 가기

* 참고 : http://www.liquidweb.com/kb/how-to-install-docker-on-ubuntu-14-04-lts/
* Ubuntu 14.04에서는 apt-get으로 곧바로 설치가 가능하므로 엄청나게 편합니다.
```
sudo apt-get update
sudo apt-get install docker.io
```
* 심볼릭 링크로 실행 명령어를 docker로 잡아주고 또 패스도 잡아줘야 한다고 합니다.
```
sudo ln -sf /usr/bin/docker.io /usr/local/bin/docker
sudo sed -i '$acomplete -F _docker docker' /etc/bash_completion.d/docker.io
```
* 그리고 서버가 부팅될 때 도커가 자동으로 실행되도록 하고 싶다면 아래 명령을 쓰면 된다고 합니다. (원할때만)
```
sudo update-rc.d docker.io defaults
```

## 설치 방법 2
Note: 조금 복잡하지만 정석대로 가기.  저는 이 방식으로 설치해 보았습니다.

* 참고 : http://docs.docker.com/engine/installation/ubuntulinux/
* 아마 이쪽이 더 최신버전을 한템포 더 빨리 반영하기가 좋은 것 같습니다. (도커에서 직접 제공하는 저장소에서 설치하기 때문)
* 우선 아래 명령으로 apt 저장소 소스를 새로 등록해 줍니다.
```
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
```
해서 GPG인증키를 주고,
```
sudo leafpad /etc/apt/sources.list.d/docker.list
```
해서 편집기에서 다음 내용을 직접 써넣어 줍니다.
```
deb https://apt.dockerproject.org/repo ubuntu-trusty main
```
그리고 업데이트 한 번 해 주고
```
sudo apt-get update
```
혹시 기존에 먼저 깔려 있는게 있다면 청소해 주고
```
sudo apt-get purge lxc-docker
```
이제 어떤 버전들이 제공되는지 확인해 봅니다.
```
sudo apt-cache policy docker-engine
```
* 그리고 `linux-image-extra` 패키지를 설치해야 한다고 합니다.
```
sudo apt-get update
sudo apt-get install linux-image-extra-$(uname -r)
```
* 이제 도커에 필요한 환경은 만들었으므로 설치를 합니다.
```
sudo apt-get update
sudo apt-get install docker-engine
```
* 설치가 끝났으면 도커 데몬을 시작하고, 기본적으로 들어있는 'Hello-world' 이미지를 시작해서 잘 동작하는지 최초 확인을 합니다.
```
sudo service docker start
sudo docker run hello-world
```
'hello-world'라는 이미지는 도커로 실행하면 자신이 실행되었다는 메시지를 뱉아놓고 곧바로 종료되는 확인용 이미지인 것 같습니다.  때문에 이 이미지를 run 해 보니까 나오는 메시지는 다음과 같더군요.  자기가 어떻게 동작했는지 설명을 해 주네요.
```
Hello from Docker.
This message shows that your installation appears to be working correctly.
To generate this message, Docker took the following steps:
 1. The Docker client contacted the Docker daemon.
 2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
 3. The Docker daemon created a new container from that image which runs the
    executable that produces the output you are currently reading.
 4. The Docker daemon streamed that output to the Docker client, which sent it
    to your terminal.
To try something more ambitious, you can run an Ubuntu container with:
 $ docker run -it ubuntu bash
Share images, automate workflows, and more with a free Docker Hub account:
 https://hub.docker.com
For more examples and ideas, visit:
 https://docs.docker.com/userguide/
```
* 도커 유저그룹에 사용자 아이디를 가입시킵니다.  도커는 슈퍼유저 권한을 항상 사용해야 하므로, docker라는 유저그룹에 슈퍼유저 권한을 주고 필요한 사용자를 여기에 집어넣어 쓴다는 개념인 듯 합니다.
```
sudo usermod -aG docker 아이디
```
이후에 로그아웃 했다가 다시 로그인 하면 docker 유저그룹으로서 적용이 됩니다.
* 그리고 이제 `sudo` 명령을 생략하고 명령을 때려서 확인해 봅니다.
```
docker run hello-world
```

## RedMine 이미지 땡겨오기
* 여러가지 예제가 많겠지만, 여기서는 프로젝트 관리용 유틸리티인 RedMine 이미지를 땡겨와 보겠습니다.  Bitnami라는 회사에서 베타버전으로 최근에 제공해 주기 시작한 패키지화된 스택을 땡겨옵니다. (참고 : https://bitnami.com/stack/redmine )
* 그리고 설치 순서는 이곳( https://hub.docker.com/r/bitnami/redmine/ )을 참고해서 그대로 해 봅니다.  이미지를 땡겨오고, 그 이미지에 역시 Bitnami에서 만들어서 Github에 공유해 둔 Dockerfile을 빌드해서 먹여주는 순서입니다.
```
docker pull bitnami/redmine:latest
```

## 땡겨온 RedMine 이미지 실행해 보기
* 이렇게 땡겨온 후 곧바로 실행해 보면 됩니다.  (더 자세한 실행 옵션은 설명서 보면서 확인.  여기서는 일단 -d 옵션을 주고, 또 9999로 포트포워딩을 하도록 -p 옵션을 주었습니다.)
```
docker run -d --name=redmine -p 9999:80 bitnami/redmine
```
* 현재 실행중인 컨테이너들의 목록을 보는 명령은,
```
docker ps
```
* 이제 웹브라우저에 다음 주소를 쳐서 RedMine 서비스가 실행되는지 확인해 봅시다.
```
http://localhost:9999/
```
* Bitnami RedMine의 디폴트 아이디는 'user', 패스워드는 'bitnami'임을 확인합시다.  이제 RedMine 서비스를 사용해 보면 됩니다.
* 이렇게 일단 사용은 가능한데, 몇가지 문제가 더 남아있습니다.  이 이미지의 경우에는, DB 같은 것들이 다 포함되어 있어서, 내용을 변경하거나 채워넣으면 내부의 DB에 변화가 일어나는데, 이것을 따로 commit 해서 새로운 이미지로 만들어놓지 않으면 그대로 다 날아가 버립니다.
* 때문에 이상적으로는, DB나 파일저장소는 도커 이미지 안에 들어있는걸 쓰지 말고 외부의 것을 갖다 쓰는 방식으로 구성하고, 도커 컨테이너에는 순수하게 어플리케이션만 돌아가도록 해서 이미지 백업을 안 해도 되도록 구성하는게 낫지 않나 합니다.

* 백그라운드 실행, 컨테이너 이름 결정, 외부와 디렉토리 공유, 포트포워딩 조건을 줘서 제대로 실행해 보면...
```
docker run -d --name=redmine -v ~/redmine-files:/opt/redmine/apps -e USER_UID=`id -u` -p 9999:80 -p 443:443 bitnami/redmine
```
* 새로운 터미널을 만들어서 안을 둘러보거나 할 때 이걸로 하면 되고..
```
docker exec -it redmine /bin/bash
```
* 로그파일을 생성해서 상황을 볼 때는 이걸로.. (물론 아래 경로의 디렉토리는 다 만들어줘야 할 듯)
```
docker exec -it redmine /opt/bitnami/scripts/logs.sh apache
docker exec -it redmine /opt/bitnami/scripts/logs.sh mysql
```
* 웹서비스를 중단했다가 다시 시작하려면 이걸 때리면 됩니다.
```
docker exec -it redmine /services.sh stop
docker exec -it redmine /services.sh start
```

* 본 예제에서 사용된 Bitnami RedMine의 경우, 컨테이너를 Stop 했다가 다시 Start 시켜보니깐 다시 살아나는데 문제가 있는 듯 합니다.  조사해 보니, 아파치 등 부팅 직후 자동실행되는 서비스들이 올라타는 Bash쉘이 EXIT 되어 버리기 때문에 재기동할 때 서비스가 자동으로 실행되지 않는 문제 같습니다.
* 따라서, 현재로서는 stop 및 start가 잘 안되므로, 이걸 피해서 운용하는 수 밖에 없는 듯 합니다.  일단 핵심 데이타(DB)는 외부로 빼내주고, 핵심 설정 내용들을 얼려주기 위해 commit을 사용하여 새로운 업데이트된 이미지를 생성하는 것이 정석일 듯 합니다.


## 기타 기본적인 관리 명령어들
* 현재 다운로드된 이미지들의 목록 보기 ::: `docker images`
* 현재 다운로드된 이미지들의 목록을 자세하게 보기 (의존성이 걸려있는 숨겨진 이미지들까지 다 보기) ::: `docker images -a`
* 이미지 삭제하기 ::: `docker rmi 이미지이름`
* 현재 실행중인 컨테이너들의 목록 보기 ::: `docker ps`
* 모든 컨테이너들의 목록 보기 (중지된 것들까지 다 보기) ::: `docker ps -a`
* 현재 실행중인 컨테이너 중지하기 ::: `docker stop 이름`
* 현재 실행중인 컨테이너 죽이기 ::: `docker kill 이름`
* 현재 중지상태인 컨테이너 삭제하기 ::: `docker rm 이름`
* 현재 중지상태인 컨테이너 시작하기 ::: `docker start 이름`
* 컨테이너 외부에서 컨테이너 내부로 명령 때려주기 ::: `docker exec 이름 명령`
* 컨테이너 내부를 뒤져보고 싶을 때 실행 방법 ::: `docker run -it 이름 /bin/bash`
