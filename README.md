# UbuntuBang
_Automation of Making OpenBox &amp; Tint2 Desktop based on Ubuntu Server_


## 소개
* 우분투 기반 경량 리눅스 데스크탑을 빠르게 구성하기 위한 자동화 툴입니다.
* 사용되는 유틸리티들 및 사용자 환경은 작성자의 취향에 따라 임의로 선택된 것입니다.


## 특징
* VirtualBox 가상머신에 설치하는 것을 상정하여 구성하였습니다.
* 기계설계,해석을 위한 리눅스 머신을 구성하는데 초점을 두었습니다.
* 일반적인 웹서버 운용이나 멀티미디어 데스크탑을 구성하기 위한 시스템은 아닙니다.


## 주요 유틸리티
* OS : Ubuntu Server 18.04
* Xorg, OpenBox, obmenu, lxappearance, compton, feh, tint2, numlockx, qlipper
* pluma, atom, mscode
* pcmanfm, htop, scrot, nimf, file-roller, cups-pdf, evince, convertall, qalculate, rofi, stacer ...


## 사용방법
* Ubuntu Server 18.04를 VirtualBox 가상머신에 설치합니다.  가상머신의 하드디스크 사이즈는 80GB 이상으로 합니다.
* 최초 부팅하고 로그인 합니다.
* 인터넷 연결이 되어 있는지 확인합니다.
* 이후에 다음의 명령어를 차례대로 쳐넣습니다.

```bash
mkdir ~/git
cd ~/git
git init
git clone https://github.com/dymaxionkim/UbuntuBang.git
cd UbuntuBang
chmod +x ./UbuntuBang18.04.sh
sudo sh UbuntuBang18.04.sh
```

* 위의 과정이 다 끝나면 다음 명령을 쳐서 재부팅을 한 번 해 줍니다.

```bash
sudo reboot now
```

* 그리고 X윈도우를 실행해서 잘 되는지 확인합니다.

```bash
startx
```

* 이제 데스크탑 관련 각종 설정을 해 줍니다.

```bash
nimf-settings
obconf
lxappearance
obkey
obmenu
```

* 기타 유틸리티들의 설치 스크립트를 마저 실행해 줍니다.

```bash
chmod +x ./Utilities.sh
chmod +x ./pyenv.sh
sudo sh Utilities.sh
./pyenv.sh
```

* 끝!  나머지 환경는 알아서~




