# Maintenance


## 콘솔 스크린세이버 끄기

* Ubuntu Server를 설치하면, 기본 셋팅으로 콘솔 상태에서 10분 후에 자동으로 스크린이 꺼지도록 스크린세이버 설정이 되어 있습니다.  독립적인 PC에서라면 상관없겠지만, VirtualBox에서 운용할 때는 이게 필요 없으므로 콘솔 스크린세이버 기능을 죽이도록 합시다.

* 먼저 사용자 계정의 bash 설정을 변경합니다.	
`nano ~/.bashrc`해서 `setterm -blank 0` 추가.  이때 setterm 이라는 것은 터미널 셋팅을 바꿔주는 유틸리티인데, 이걸 터미널 시작될 때 자동으로 실행하라는 의미입니다.

* 그리고 전체 계정(Multiuser Runlevel)의 기본 쉘 설정을 변경합니다.
`sudo nano /etc/rc.local`해서 `setterm -blank 0` 추가

* 그래도 혹시 모르니깐 GRUB 부트로더 로딩될 때의 설정도 변경합니다.
`sudo nano /etc/default/grub`해서 `GRUB_CMDLINE_LINUX_DEFAULT=""` 파라미터를 `GRUB_CMDLINE_LINUX_DEFAULT="consoleblank=0"`으로 변경하고 나온 후, 터미널 명령으로 `sudo update-grub`해서 grub 설정을 먹여줍니다.

* 그리고 `sudo reboot now` 해서 재부팅.

* 참고한 출처 : http://superuser.com/questions/152347/change-linux-console-screen-blanking-behavior/154388#154388


## X윈도우 스크린세이버 끄기

* 콘솔 말고도, X윈도우 역시 일정 시간이 지나면 스크린이 자동으로 꺼지도록 디폴트 설정 되어 있습니다.  VirtualBox에서 운용할 때, 종종 꺼진 스크린을 다시 살릴 때 시스템이 먹통이 되는 경우가 보이므로 이것도 없애려고 합니다.

* `sudo nano /etc/X11/xinit/xinitrc`로 들어간 후 `xset s off -dpms`를 추가해 줍니다.


## VirtualBox 심리스 모드 버그 대처

* 현재 버전(V5.0.1)의 경우, 운용해 보니깐 '설정 - 사용자 인터페이스 - 전체 화면 / 심리스 모드에서 보이기' 옵션이 기본으로 체크되어 있습니다.  전체화면 모드에서도 아래쪽에 관리메뉴가 있어서 호스트 윈도우 쪽과 왔다갔다 전환이 편합니다.
* 그런데 리눅스 상태에서 오랫동안 뒀다가 다시 들어가 보니깐 먹통이 되는 현상이 발견됩니다.  정확한 원인은 모르겠는데  아무튼 버그입니다.
* 조금 불편하더라도 `전체 화면 / 심리스 모드에서 보이기` 옵션을 체크 해제하고 사용해 보니, 먹통이 되는 현상이 사라졌습니다.
* 이 버그는 리눅스 쪽이 아니고 VirtualBox 쪽의 버그로 생각됩니다.  VirtualBox가 업데이트되면 계속 잘 업데이트해 주는 것이 좋겠습니다.


## 호스트OS(윈도우) 쪽의 폴더 공유하기

* 전제조건은, (1)VirtualBox 게스트 확장이 반드시 설치되어 있어야 하고, (2)가상머신의 설정에서 원하는 폴더를 공유하겠다고 설정되어 있어야 합니다.
* 이후에 부팅해서 `/media` 안에 들어가 보면 `sf_share` 식으로 `sf_`라는 두문자가 붙은 디렉토리가 생겨있음을 알 수 있습니다.
* 그런데 이것의 접근권한은 `vboxsf` 유저그룹에게만 주어져 있는데, 만일 내 계정이 여기에 속해있지 않으면 접근할 수 없습니다.  슈퍼유저 상태에서만 접근이 가능하므로 불편합니다.
* 따라서 터미널에서 다음의 명령어를 쳐서 현재 나의 아이디를 `vboxsf`으로 넣어 줍니다.  여기서 `dong`부분에 현재 나의 계정을 넣어주면 됩니다.
```
sudo usermod -G vboxsf -a dong
```
* 그리고 재부팅.
* 이제 `/media/sf_share`으로 접근해서 읽고 쓰기가 가능한지 확인합니다.  잘 된다면 OK.
* 만일 매번 `/media/sf_share`으로 접근하기가 귀챦다면, 아래 처럼 심볼릭 링크를 걸어줘서 쓰면 편합니다.
```
ln -s /media/sf_share ~/sf_share
```


## SAMBA로 네트워크 드라이브 연결하기

* ip주소가 //123.45.67.89인 원격지의 어느 컴퓨터에 네트워크 공유되는 디렉토리 share가 있는 경우를 봅니다.  share 디렉토리 접근을 위한 ID와 PW도 주어져 있다고 합시다.
```
ip주소 : //123.45.67.89/share
ID : testid
PW : testpw
```

* 일단 시험삼아 마운트해 봅니다.  이때 만일 디렉토리 중에 빈 칸이 있을 경우에는, 전체 경로를 따옴표("...")로 감싸면 됩니다.
```
sudo mkdir /media/test
sudo mount -t cifs -o user=testid,pass=testpw //123.45.67.89/share /media/share
```

* 마운트 되는 것이 확인되면, 부팅할 때 자동으로 마운트되도록 설정합니다.  `/etc/fstab` 설정파일을 열어서 편집하면 됩니다.
```
sudo l3afpad /etc/fstab
```

* 여기에 다음 내용을 추가합니다.  이때 `uid=dong` 부분은 현재 리눅스의 계정을 적어주면 읽고쓰기 권한을 획득할 수 있습니다.
* 그리고, 만일 디렉토리 중에 빈 칸이 있을 경우에는, 빈 칸을 `\040`으로 대체해서 써 넣어 빈 칸이 없도록 해 주면 됩니다.
```
//123.45.67.89/share /media/share cifs defaults,username=testid,pass=testpw,uid=dong,iocharset=utf8 0 0
```

* 재부팅해서 자동으로 마운트 되어 있는지 확인하면 끝!


## Alsa 사운드 드라이버 설치하기
* 필요한 경우에는 사운드 드라이버를 설치하면 됩니다.
* 나의 VirtualBox에서는 디폴트 오디오로 Windows Direct Sound로 ICH AC97이 잡혀있습니다.  리눅스 게스트 OS 쪽에서는 `lspci` 명령을 때리면 PCI 디바이스들 목록이 나오고 이 중에서 사운드 디바이스가 뭔지 보입니다.  확인해 보니 `Multimedia audio controller: Intel Corporation 82801AA AC'97 Audio Controller (rev 01)`으로 나옵니다.  역시 VirtualBox에서 정해준 것과 동일하고, 사운드 하드웨어 자체는 잘 돌아가고 있다는 것을 알 수 있습니다.
* 사운드 드라이버는 Alsa 라는 것을 주로 사용하는 모양입니다.  드라이버 설치하고, 이게 적용되게 하려면 audio 유저그룹에 현재의 아이디를 넣어줘야 한다고 하므로 그것까지 해 줍니다.
```
sudo apt-get install alsa alsa-tools
sudo adduser 아이디 audio
```
* 그리고 재부팅.
* 재부팅 후에 startx하고 터미널 에뮬레이터에서 `alsamixer` 명령을 치면 볼륨조절 화면이 나옵니다.  좌우 화살표로 각 장치를 옮기면서 상하 화살표로 볼륨을 조절할 수 있고, 각 장치 볼륨바의 아래쪽에 `MM`이라는 표시는 Mute 상태라는 뜻이므로 키보드 `m`글자를 쳐 줘서 Mute를 풀어줍니다.
* 그리고 youtube 같은데 들어가서 소리가 잘 나오는지 확인해 봅니다.
* 추가적으로, Tint2 태스크바에 볼륨조절 아이콘이 상주하도록 하려면 아래와 같이 해당 패키지를 설치해 주고,
```
sudo apt-get install volumeicon-alsa
```
* 이걸 항상 백그라운드로 작동하도록 autostart에 추가해 줍니다.
```
leafpad ~/.config/openbox/autostart
```
해서 편집기에서 `volumeicon &`를 써주고 저장하면 끝.


## 구글드라이브 동기화 소프트웨어 'Drive'

* 참고1 : http://www.howtogeek.com/196635/an-official-google-drive-for-linux-is-here-sort-of-maybe-this-is-all-well-ever-get/
* 참고2 : https://github.com/odeke-em/drive
* 작년에, 구글소속 프로그래머인 [Burcu Dogan](https://github.com/rakyll)이라는 여성 프로그래머가 구글 드라이브 개발팀 소속으로 개발한 거라고 합니다.  지금은 바빠서 이 프로젝트를 다른 사람에게 넘겼나 봅니다.
* 이전에는 개발단계여서 Go Lang을 깔고 그 위에 소스를 직접 클론해서 패스 잡고 적용해 줘야 했으나, 지금은 리눅스 배포본별로 바이너리를 생성해서 제공해 줍니다.
( 플랫폼별 패키지 제공 설명서 : https://github.com/odeke-em/drive/blob/master/platform_packages.md )
* 일단 우분투 계통은 다음 명령으로 설치가 되네요.
```
sudo add-apt-repository ppa:twodopeshaggy/drive
sudo apt-get update
sudo apt-get install drive
```
* 설치 후에, 초기 환경 설정 과정은 다음과 같습니다.
```
drive init ~/gdrive
```
* 그러면 구글 인증 코드 따내는 URL이 나오는데, 그걸 긁어서 복사한 다음에 브라우저로 가서 구글계정으로 로그인하면 인증 코드가 나옵니다.  인증코드를 역시 긁어다 복사해서 터미널에 Paste하면 됩니다.  이제 구글드라이브의 자료들을 땡겨옵니다.
* 전체를 땡겨오려면,
```
cd ~/gdrive
drive pull
```
해 줘 봅니다.  이때 만일 구글드라이브에 사진 등이 많은 자료들이 저장되어 있다면 에러가 날 확률이 높아질 것 같습니다.  용량 문제도 있을 수 있고요.
* 때문에 특정 폴더만 땡겨오려면 다음 처럼 하면 되는 것 같습니다.
```
drive pull 경로(폴더명)
```
* 기타 더 자세한 사용법은 설명서 찾아서 잘 연구해 봅시다. 형식이 git 명령과 매우 유사합니다. Dogan이라는 개발자의 성격을 엿볼 수 있는 듯 합니다.
* 항상 상주하면서 실시간으로 백그라운드 싱크시켜주는 기능은 개발자가 의도적으로 배제한 듯 합니다.
* 구글드라이브 기본용량이 대략 15GB 정도 되기 때문에, 나의 VirtualBox에 할당해 둔 용량이 충분치 않다면 사용하지 않도록 유의하는 것이 좋겠습니다.


## 부팅 직후 터미널에 한글 메시지가 다 깨져서 보여서 짜증날 때
* 부팅 직후의 터미널은 그래픽 상태가 아니므로 ASCII 문자만 보이기 때문에, 한글 같은 것들은 다 깨져서 나옵니다.  무슨 메시지인지 확인이 안 되어 불편합니다.
* 다음 유틸리티(프레임버퍼 터미널)를 설치해서, 그래픽 모드로 터미널이 보이도록 하면 됩니다.
```
sudo apt-get install fbterm
```
* 사용 명령어는 `fbterm` 입니다.  명령을 치면 터미널이 그래픽모드로 바뀌면서 한글 메시지 확인이 됩니다.  다만 이것을 디폴트로 사용하지는 않겠습니다.  약간이라도 메모리를 아끼는게 좋아서 입니다. (개인선택)  필요할 때만 사용하면 되겠죠.


## 'apt-get update' 명령을 사용할 때 에러 발생시 대처방법
* 네트워크 상태가 좋지 않거나 기타등등의 이유로 가끔 'apt-get update' 명령이 실패할 때가 있습니다.  자주 보이는 에러메시지는 대체로 'BADSIG GPG errors' 내지는 '해시 합이 맞지 않습니다' 같은 것들입니다.  이럴때는 우분투 패키지 저장소 목록 자체를 완전히 날려버리고 새로 update해서 구성하면 해결될 때가 많다고 합니다.  절차는 아래와 같습니다.
```
sudo apt-get clean
sudo mv -r /var/lib/apt/lists /var/lib/apt/lists.old
sudo mkdir -p /var/lib/apt/lists/partial
sudo apt-get clean
sudo apt-get update
```

* 간단히 말해, 기존의 우분투 패키지 저장소 목록를 .old 이름으로 일단 백업해 놓고, 원래의 디렉토리 'list' 및 그 밑의 하위 디렉토리인 'partial'을 새로 만듭니다.  빈 디렉토리들이 되는거죠.  깨끗하게 비운 것입니다.

* 'apt-get clean'이라는 명령 역시 깨끗하게 비우라는 것인데, 앞뒤로 확인하듯이 한번씩 명령을 줘서 확실하게 합니다.

* 이후에 update를 시도하는 것입니다.  그러면 새롭게 받아진 저장소 목록으로 해당 디렉토리들이 채워집니다.  이렇게 하면 중간에 인증(GPG)나 에러체크(해시 합) 관련해서 오류가 있는 파일들이 있을 확률이 크게 낮아지므로 문제를 해결할 수 있는 경우가 많습니다.



## UbuntuBang 쪽에서 네트워크 설정 변경 (고정아이피로 하기)
* 가정용 공유기로 외부에서 접근 가능하게 접속해 주면, 포트포워딩 같은걸 해야 하므로 리눅스 쪽이 공유기로 접속되는 내부 아이피를 고정아이피로 바꿔줄 필요가 있습니다.  재부팅할때마다 바뀌면 곤란하니까요.
* 전제조건으로, VirtualBox에서 운용중인 리눅스의 네트워크 설정이 VirtualBox 쪽에서 반드시 '브릿지어댑터'로 되어 있어야 합니다.  'NAT'로 되어 있을 경우에는 호스트 단인 윈도우 쪽에 종속된 아이피를 사용하게 되므로 외부와 연결되는 서버 운용이 안됩니다.
* 일단 아래 네트워크 설정 파일을 편집기로 엽니다.
```
sudo leafpad /etc/network/interfaces
```
* 열어보니 아래와 같은 내용이 보입니다.  eth0 랜카드를 auto로 뭐 어쩌구 해라 대충 이런 내용이겠죠.
```
# This file describes the network interfaces available on your system
# and how to activate them. For more information, see interfaces(5).
# The loopback network interface
auto lo
iface lo inet loopback
# The primary network interface
auto eth0
iface eth0 inet dhcp
```
* 이걸 지우고(불안하면 다름 이름으로 카피해서 백업해 두고) 아래 내용으로 바꿔줍니다.  물론 address는 원하는 아이피로 바꿔줘야겠고, netmask 및 게이트웨이도 확인해 줍니다.  공유기를 사용할 경우에는 공유기에서 정보 확인해서 적용합니다.
```
auto lo
iface lo inet loopback
auto eth0
iface eth0 inet static
address 192.168.0.9
netmask 255.255.255.0
gateway 110.35.26.1
```
* 그리고 네트워크 재 시작 명령.
```
sudo /etc/init.d/networking restart
```
