# UbuntuBang
_Automation of Making OpenBox &amp; Tint2 Desktop based on Ubuntu Server 14.04_

* Wiki : <http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/index.html>


![Numix Theme](https://cloud.githubusercontent.com/assets/12775748/11458097/81a3e3f4-96fc-11e5-8d66-7a009bf36585.png)

![Terminator Terminal Emulator](https://cloud.githubusercontent.com/assets/12775748/11458099/852898e4-96fc-11e5-8eae-90a0b5f22e00.png)

![FreeCAD and Netgen](https://cloud.githubusercontent.com/assets/12775748/11458100/9000d1e6-96fc-11e5-96a5-0d7cc24c7e91.png)

![ElmerGUI](https://cloud.githubusercontent.com/assets/12775748/11458103/a1ec281a-96fc-11e5-886c-3c88c63ea891.png)

![Jupyter Notebook, Atom Editor, PCManFM File Mangager](https://cloud.githubusercontent.com/assets/12775748/11455961/8b4a237a-96b9-11e5-90fc-0890beeb737e.png)


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
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Setup_OS.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Setup_Apps.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Setup_Dev.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Maintenance.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Tips.md


## 사용방법
* Ubuntu Server 14.04를 VirtualBox 가상머신에 설치합니다.  가상머신의 하드디스크 사이즈는 20GB 이상으로 합니다.
* 최초 부팅하고 로그인 합니다.
* 인터넷 연결이 되어 있는지 확인합니다.
* 설치된 Ubuntu의 VirtualBox 메뉴에서 `장치` 안에 있는 `게스트 확장 CD 이미지 삽입`을 반드시 선택해 줍니다. (주의할 것!)

![virtualbox_01](https://cloud.githubusercontent.com/assets/12775748/11455936/9c9c6e7c-96b8-11e5-9bcf-d993aa4788c6.png)

* 이후에 다음의 명령어를 차례대로 쳐넣습니다.
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

* 그리고 원한다면, Python/Jupyter 개발환경을 구성하는 쉘스크립트를 실행시킵니다.
```
cd ~/github/UbuntuBang
chmod +x ./UbuntuBang_Jupyter.sh
sudo sh UbuntuBang_Jupyter.sh
```

* 위의 과정이 다 끝나면 다음 명령을 쳐서 재부팅을 한 번 해 줍니다.
```
sudo reboot now
```

* 그리고 X윈도우를 실행해서 잘 되는지 확인합니다.
```
startx
```

* X윈도우 상에서 Jupyter가 잘 실행되고, Python2/Python3 커널이 잘 올라가 있는지 확인해 봅니다.

* 끝!  나머지 환경설정은 알아서~



## 유의사항
* 사운드 드라이버는 설치하지 않습니다.  필요시 별도로 잡아주어야 합니다.
* LightDM은 설치하지 않습니다.  필요시 별도로 설치하면 됩니다.
* 스크린세이버는 설치하지 않습니다.  필요시 별도로 설치하면 됩니다.
* 한컴오피스 뷰어는 직접 수동으로 따로 설치하도록 합시다.

