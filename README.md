# UbuntuBang
_Automation of Making OpenBox &amp; Tint2 Desktop based on Ubuntu Server_

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
* Ubuntu Server
* Xorg, OpenBox
* obmenu, lxappearance, xcompmgr, nitrogen, tint2, numlockx
* pluma, pcmanfm, htop, gnome-screenshot, uim byeoru
* etc ...


## 상세한 설명
* 구버전인 Ubuntu 14.04 기준으로 작성된 것이므로 현재는 잘 안 맞을 수도 있습니다.  참고만 해 주세요~
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Setup_OS.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Setup_Apps.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Setup_Dev.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Maintenance.md
* http://dymaxionkim.github.io/UbuntuBang/mdwiki_UbuntuBang/#!pages/Linux/Tips.md


## 사용방법
* Ubuntu Server 16.04를 VirtualBox 가상머신에 설치합니다.  가상머신의 하드디스크 사이즈는 30GB 이상으로 합니다.
* 최초 부팅하고 로그인 합니다.
* 인터넷 연결이 되어 있는지 확인합니다.
* 이후에 다음의 명령어를 차례대로 쳐넣습니다.

```bash
mkdir ~/git
cd ~/git
git init
git clone https://github.com/dymaxionkim/UbuntuBang.git
cd UbuntuBang
chmod +x ./UbuntuBang16.04.sh
sudo sh UbuntuBang16.04.sh
```

* 그리고 원한다면, Python/Jupyter 개발환경을 구성하는 쉘스크립트를 실행시킵니다. (아직 미검증)
```bash
cd ~/github/UbuntuBang
chmod +x ./UbuntuBang_Jupyter.sh
sudo sh UbuntuBang_Jupyter.sh
```

* 위의 과정이 다 끝나면 다음 명령을 쳐서 재부팅을 한 번 해 줍니다.
```bash
sudo reboot now
```

* 그리고 X윈도우를 실행해서 잘 되는지 확인합니다.
```bash
startx
```

* X윈도우 상에서 Jupyter가 잘 실행되고, Python2/Python3 커널이 잘 올라가 있는지 확인해 봅니다.

* 끝!  나머지 환경설정은 알아서~





```bash
wget https://raw.githubusercontent.com/dymaxionkim/UbuntuBang/master/UbuntuBang18.04.sh
chmod +x ./UbuntuBang18.04.sh
sudo sh ./UbuntuBang18.04.sh
sudo reboot now

startx
tint2 in terminal one time
obmenu in terminal and set for applications
lxappearance in terminal and set for Numix theme
nitrogen --restore in terminal and set wallpaper
ibus preference for Hangul input method
```

