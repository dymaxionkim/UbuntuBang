
# LibreOffice

## WPS Office

* 무료로 리눅스에 설치가능한 오피스 중에서, MS-Office와 가장 호환성이 좋고 완성도가 높은 것은 WPS Ofiice (구 Kings Office) 입니다.  이곳( http://wps-community.org/ )에서 다운로드 받아다가 설치해서 사용할 수 있습니다.

* 64bit DEB 패키지를 선택해서 받아다가 `sudo dpkg -i wps*.deb` 명령을 줘서 설치하면 됩니다.  설치되는 장소는 '/opt'안에 배치가 됩니다.

* 실행 명령어는, 스프레드시트는 `et`, 워드프로세서는 `wps`, 프리젠테이션은 `wpp` 입니다.  설치후에 `obmenu`로 메뉴상에서 실행시킬 수 있도록 편집해 주면 더 좋겠죠.

* 최초 실행시에, 뭔가 팝업창이 하나 떠서 경고문구가 나오는데, 이것은 무슨 기호 용으로 사용하는 폰트가 없다는 것입니다.  해당 폰트를 인터넷 검색해서 설치해 줘도 좋고, 귀챦으면 그냥 무시해도 됩니다.  [개인적으로 저장해 놓은 해당 폰트 다운로드](https://drive.google.com/open?id=0B3VzdmodvgcIdkZnT3VqRnpHTDQ)  새로운 폰트를 풀어넣을 곳은 몇군데 있지만, 제일 간단하게는 `~/.fonts` 디렉토리를 만들어넣고 거기에 폰트 파일들을 넣으면 됩니다.

* 다만, 이것은 오픈소스가 아니라 프리웨어이기 때문에, LibreOffice를 디폴트로 삼았습니다.



## LibreOffice 테마 바꾸기

* LibreOffice를 사용할 때 아이콘 같은 것들이 좀 촌스러우므로 세련되게 바꿔 봅니다.

* 출처 :  http://www.webupd8.org/2013/06/try-new-libreoffice-flat-icon-set.html

* 일단 아래의 명령을 통해 스타일 및 아이콘팩을 설치하고...

```
sudo apt-get install libreoffice-style-crystal
cd /tmp && wget https://github.com/hotice/myfiles/raw/master/images_flat.zip
sudo cp images_flat.zip /usr/share/libreoffice/share/config/images_crystal.zip
```

* LibreOffice를 시작해서 'Tools - Options' 메뉴로 들어간 다음, 팝업창이 뜨면 'LibreOffice - View - Icon Size and Style'에서 아이콘 테마를 디폴트의 "Tango"에서 "Crystal"로 바꿔줍니다.  그러면 끝.
