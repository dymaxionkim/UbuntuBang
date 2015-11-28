# UbuntuBang
_Automation of Making OpenBox &amp; Tint2 Desktop based on Ubuntu Server 14.04_


## 소개
* 우분투 기반 경량 리눅스 데스크탑을 빠르게 구성하기 위한 자동화 툴입니다.
* 사용되는 유틸리티들 및 사용자 환경은 작성자의 취향에 따라 임의로 선택된 것입니다.


## 특징
* VirtualBox 가상머신에 설치하는 것을 상정하여 구성하였습니다.
* 기계설계,해석을 위한 리눅스 머신을 구성하는데 초점을 두었습니다.
* 일반적인 웹서버 운용이나 멀티미디어 데스크탑을 구성하기 위한 시스템은 아닙니다.


## 주요 유틸리티
* Ubuntu Server 14.04
* Xorg, OpenBox
* obmenu, lxappearance, xcompmgr, nitrogen, tint2, numlockx, conky
* leafpad, pcmanfm, lxtask, gmrun, gnome-screenshot, Dasom Jeongeum
* etc ...


## 상세한 설명
* http://dymaxionkim.github.io/mdwiki/#!pages/Linux/Setup_OS.md
* http://dymaxionkim.github.io/mdwiki/#!pages/Linux/Setup_Apps.md
* http://dymaxionkim.github.io/mdwiki/#!pages/Linux/Setup_Dev.md


## 사용방법
* Ubuntu Server 14.04를 VirtualBox 가상머신에 설치합니다.  가상머신의 하드디스크 사이즈는 20GB 이상으로 합니다.
* 최초 부팅하고 로그인 합니다.
* 인터넷 연결이 되어 있는지 확인합니다.
* 설치된 Ubuntu의 VirtualBox 메뉴에서 `장치` 안에 있는 `게스트 확장 CD 이미지 삽입`을 반드시 선택해 줍니다.
* 다음의 명령어를 차례대로 쳐넣습니다.
```
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install git
mkdir ~/github
cd ~/github
git init
git clone https://github.com/dymaxionkim/UbuntuBang.git
cd UbuntuBang
chmod +x ./UbuntuBang.sh
sudo sh UbuntuBang.sh
```
* 위의 과정이 다 끝나면 다음 명령을 쳐서 재부팅을 한 번 해 줍니다.
```
sudo reboot now
```

* 그리고 X윈도우를 실행해서 잘 되는지 확인합니다.
```
startx
```

* X윈도우상에서 터미널 에뮬레이터를 실행시킨 후, 다음 명령을 써서 바탕화면을 원하는대로 설정합니다.
```
nitrogen
```

## 유의사항
* 사운드 드라이버는 설치하지 않습니다.  필요시 별도로 잡아주어야 합니다.
* LightDM은 설치하지 않습니다.  필요시 별도로 설치하면 됩니다.
* 스크린세이버는 설치하지 않습니다.  필요시 별도로 설치하면 됩니다.
